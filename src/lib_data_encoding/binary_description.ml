(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let unopt_lazy func = function
  | None -> func ()
  | Some x -> x

type recursives = string list
type references = { descriptions : (string * Binary_schema.toplevel_encoding) list } [@@unwrapped]

(* Simple Union find implementation, there are several optimizations
   that give UF it's usual time complexity that could be added.
   If this is a bottleneck, they're easy to add. *)
module UF : sig
  type t
  val add : t -> Binary_schema.description -> unit
  val find : t -> string -> Binary_schema.description
  val union : t -> new_cannonical:Binary_schema.description -> existing:string -> unit
  val empty : unit -> t
  val pp : Format.formatter -> t -> unit
end = struct
  open Binary_schema
  type ele = Ref of string | Root of description
  type t = (string, ele) Hashtbl.t
  let add t x = Hashtbl.replace t x.name (Root x)
  let rec find tbl key =
    match Hashtbl.find tbl key with
    | Ref s -> find tbl s
    | Root desc -> desc

  let union tbl ~new_cannonical ~existing =
    add tbl new_cannonical ;
    let root = find tbl existing in
    if root.name = new_cannonical.name
    then ()
    else Hashtbl.replace tbl root.name (Ref new_cannonical.name)

  let empty () = Hashtbl.create 128

  let pp ppf tbl =
    Format.fprintf ppf "@[<v 2>UF:@,%a@]"
      (fun ppf ->
         (Hashtbl.iter (fun k v ->
              Format.fprintf ppf "'%s' ---> %a@,"
                k (fun ppf -> function
                    | Root { name } -> Format.fprintf ppf "Root '%s'" name
                    | Ref s -> Format.fprintf ppf "Ref '%s'" s) v))) tbl
end

let fixup_references uf =
  let open Binary_schema in
  let rec fixup_layout = function
    | Ref s -> Ref (UF.find uf s).name
    | Enum (i, name) -> Enum (i, (UF.find uf name).name)
    | Seq layout -> Seq (fixup_layout layout)
    | (Zero_width
      | Int _
      | Bool
      | RangedInt (_, _)
      | RangedFloat (_, _)
      | Float
      | Bytes
      | String) as enc -> enc in
  let field = function
    | Named_field (name, kind, layout) ->
        Named_field (name, kind, fixup_layout layout)
    | Anonymous_field (kind, layout) ->
        Anonymous_field (kind, fixup_layout layout)
    | Dynamic_field i ->
        Dynamic_field i
    | (Option_indicator_field _) as field -> field in
  function
  | Obj { fields } -> Obj { fields = List.map field fields }
  | Cases ({ cases } as x) ->
      Cases { x with
              cases = List.map
                  (fun (i, name, fields) ->
                     (i, name, List.map field fields)) cases }
  | (Int_enum _ as ie) -> ie

let z_reference_name = "Z.t"

let z_reference_description =
  "A variable length sequence of bytes, encoding a Zarith number. \
   Each byte has a running unary size bit: the most significant bit of \
   each byte tells is this is the last byte in the sequence (0) or if \
   there is more to read (1). The second most significant bit of the \
   first byte is reserved for the sign (positive if zero). Size and \
   sign bits ignored, data is then the binary representation of the \
   absolute value of the number in little endian order."

let z_encoding =
  Binary_schema.Obj { fields = [ Named_field ("Z.t", `Dynamic, Bytes) ] }

let add_z_reference uf { descriptions } =
  UF.add uf { name = z_reference_name ;
              description = Some z_reference_description } ;
  { descriptions = (z_reference_name, z_encoding) :: descriptions }

let n_reference_name = "N.t"

let n_reference_description =
  "A variable length sequence of bytes, encoding a Zarith number. \
   Each byte has a running unary size bit: the most significant bit of \
   each byte tells is this is the last byte in the sequence (0) or if \
   there is more to read (1). Size bits ignored, data is then the binary \
   representation of the absolute value of the number in little endian order."

let n_encoding =
  Binary_schema.Obj { fields = [ Named_field ("N.t", `Dynamic, Bytes) ] }

let add_n_reference uf { descriptions } =
  UF.add uf { name = n_reference_name ;
              description = Some n_reference_description } ;
  { descriptions = (n_reference_name, n_encoding) :: descriptions }

let describe (type x) ?toplevel_name (encoding : x Encoding.t) =
  let open Encoding in
  let uf = UF.empty () in
  let uf_add_name name =
    UF.add uf { name ; description = None } in
  let add_reference name description { descriptions } =
    { descriptions = (name, description) :: descriptions } in
  let new_reference =
    let x = ref ~-1 in
    fun () ->
      x := !x + 1 ;
      let name = "X_" ^ string_of_int !x in
      uf_add_name name ;
      name in
  let extract_dynamic :
    type x. x Encoding.desc -> Binary_schema.field_descr list * x Encoding.desc =
    function
    | Dynamic_size { encoding } -> ([ Dynamic_field 1 ], encoding.encoding)
    | enc -> ([], enc) in
  let rec field_descr :
    type a. recursives -> references ->
    a Encoding.field -> Binary_schema.field_descr list * references =
    fun recursives references -> function
      | Req { name ; encoding = ({ encoding } as enc) }
      | Dft { name ; encoding = ({ encoding } as enc) } ->
          let (dynamics, field) = extract_dynamic encoding in
          let (layout, references) = layout recursives references field in
          (dynamics @ [ Named_field (name, classify enc, layout) ], references)
      | Opt { kind = `Variable ; name ; encoding = { encoding } } ->
          let (layout, references) = layout recursives references encoding in
          ([ Named_field (name, `Variable, layout) ], references)
      | Opt { kind = `Dynamic ; name ; encoding = { encoding } } ->
          let (dynamics, field) = extract_dynamic encoding in
          let (layout, references) = layout recursives references field in
          (Binary_schema.Option_indicator_field name :: dynamics @ [ Named_field (name, `Dynamic, layout) ], references)
  and obj fields =
    Binary_schema.Obj { fields }
  and union :
    type a. recursives -> references -> Kind.t -> Binary_size.tag_size -> a case list -> string * references=
    fun recursives references kind size cases ->
      let cases =
        List.sort (fun (t1, _) (t2, _) -> Compare.Int.compare t1 t2) @@
        TzList.filter_map
          (function
            | Case { tag = Json_only } -> None
            | (Case { tag = Tag tag } as case) -> Some (tag, case))
          cases in
      let tag_field =
        Binary_schema.Named_field ("Tag", `Fixed (Binary_size.tag_size size), Int (size :> Binary_schema.integer_extended)) in
      let (cases, references) =
        List.fold_right
          (fun (tag, Case case) (cases, references) ->
             let fields, references = fields recursives references case.encoding.encoding in
             ((tag, case.name, tag_field :: fields) :: cases, references))
          cases
          ([], references) in
      let name = new_reference () in
      let references =
        add_reference
          name
          (Cases { kind ;
                   tag_size = size ;
                   cases }) references in
      (name, references)
  and describe  : type b. ?description:string -> name:string ->
    recursives -> references -> b desc -> string * references =
    fun ?description ~name recursives references encoding ->
      let new_cannonical = { Binary_schema.name ; description } in
      UF.add uf new_cannonical ;
      let layout, references = layout recursives references encoding in
      begin
        match layout with
        | Ref ref_name ->
            UF.union uf ~existing:ref_name ~new_cannonical ;
            (ref_name, references)
        | layout ->
            UF.add uf new_cannonical ;
            (name,
             add_reference name
               (obj [ Anonymous_field (classify { encoding ; json_encoding = None }, layout) ])
               references)
      end
  and enum : type a. (a, _) Hashtbl.t -> a array -> _ = fun tbl encoding_array ->
    (Binary_size.range_to_size ~minimum:0 ~maximum:(Array.length encoding_array),
     List.map
       (fun i -> (i, fst @@ Hashtbl.find tbl encoding_array.(i)))
       Utils.Infix.(0 -- ((Array.length encoding_array) - 1)))
  and fields :
    type b. recursives -> references ->
    b Encoding.desc -> Binary_schema.fields * references =
    fun recursives references -> function
      | Obj field ->
          field_descr recursives references field
      | Objs { left ; right } ->
          let (left_fields, references) =
            fields recursives references left.encoding in
          let (right_fields, references) = fields recursives references right.encoding in
          (left_fields @ right_fields, references)
      | Null -> ([ Anonymous_field (`Fixed 0, Zero_width) ], references)
      | Empty -> ([ Anonymous_field (`Fixed 0, Zero_width) ], references)
      | Ignore -> ([ Anonymous_field (`Fixed 0, Zero_width) ], references)
      | Constant _ -> ([ Anonymous_field (`Fixed 0, Zero_width) ], references)
      | Dynamic_size { encoding } ->
          let (fields, refs) = fields recursives references encoding.encoding in
          (Dynamic_field (List.length fields) :: fields, refs)
      | Conv { encoding } ->
          fields recursives references encoding.encoding
      | Describe { id = name ; description ; encoding } ->
          let (name, references) = describe ~name ?description recursives references encoding.encoding in
          ([ Anonymous_field (classify encoding, Ref name) ], references)
      | Splitted { encoding } ->
          fields recursives references encoding.encoding
      | Delayed func ->
          fields recursives references (func ()).encoding
      | List { encoding } ->
          let (layout, references) = layout recursives references encoding in
          ([ Anonymous_field (`Variable, Seq layout) ],
           references)
      | Array { encoding } ->
          let (layout, references) = layout recursives references encoding in
          ([ Anonymous_field (`Variable, Seq layout) ],
           references)
      | Bytes kind ->
          ([ Anonymous_field ((kind :> Kind.t), Bytes) ], references)
      | String kind ->
          ([ Anonymous_field ((kind :> Kind.t), String) ], references)
      | (String_enum (tbl, encoding_array) as encoding) ->
          let size, cases = enum tbl encoding_array in
          let name = new_reference () in
          ([ Anonymous_field (classify { encoding ; json_encoding = None }, Ref name) ],
           add_reference name (Int_enum { size ; cases }) references)
      | Tup ({ encoding } as enc) ->
          let (layout, references) = layout recursives references encoding in
          ([ Anonymous_field (classify enc, layout) ], references)
      | Tups { left ; right } ->
          let (fields1, references) = fields recursives references left.encoding in
          let (fields2, references) = fields recursives references right.encoding in
          (fields1 @ fields2, references)
      | Union { kind ; tag_size ; cases } ->
          let name, references = union recursives references kind tag_size cases in
          ([ Anonymous_field (kind, Ref name) ], references)
      | (Mu { kind ; name ; description ; fix } as encoding) ->
          let kind = (kind :> Kind.t) in
          if List.mem name recursives
          then ([ Anonymous_field (kind, Ref name) ], references)
          else
            let { encoding } = fix { encoding ; json_encoding = None } in
            let (name, references) = describe ~name ?description (name :: recursives) references encoding in
            ([ Anonymous_field (kind, Ref name) ], references)
      | encoding ->
          let layout, references = layout recursives references encoding in
          ([ Anonymous_field (classify (make encoding), layout) ], references)
  and layout :
    type c. recursives -> references ->
    c Encoding.desc -> Binary_schema.layout * references =
    fun recursives references -> function
      | Null -> (Zero_width, references)
      | Empty -> (Zero_width, references)
      | Ignore -> (Zero_width, references)
      | Constant _ -> (Zero_width, references)
      | Bool -> (Bool, references)
      | Int8 -> (Int `Int8, references)
      | Uint8 -> (Int `Uint8, references)
      | Int16 -> (Int `Int16, references)
      | Uint16 -> (Int `Uint16, references)
      | Int31 -> (RangedInt (~-1073741824, 1073741823), references)
      | Int32 -> (Int `Int32, references)
      | Int64 -> (Int `Int64, references)
      | N ->
          (Ref n_reference_name,
           add_n_reference uf references)
      | Z ->
          (Ref z_reference_name,
           add_z_reference uf references)
      | RangedInt { minimum ; maximum } ->
          (RangedInt (minimum, maximum), references)
      | RangedFloat { minimum ; maximum } ->
          (RangedFloat (minimum, maximum), references)
      | Float ->
          (Float, references)
      | Bytes _kind ->
          (Bytes, references)
      | String _kind ->
          (String, references)
      | String_enum (tbl, encoding_array) ->
          let name = new_reference () in
          let size, cases = enum tbl encoding_array in
          let references = add_reference name (Int_enum { size ; cases }) references in
          (Enum (size, name), references)
      | Array data ->
          let (descr, references) = layout recursives references data.encoding in
          (Seq descr, references)
      | List data ->
          let layout, references =
            layout recursives references data.encoding in
          (Seq layout, references)
      | (Obj _) as enc ->
          let name = new_reference () in
          let fields, references = fields recursives references enc in
          let references = add_reference name (obj fields) references in
          (Ref name, references)
      | Objs { left ; right } ->
          let name = new_reference () in
          let fields1, references = fields recursives references left.encoding in
          let fields2, references = fields recursives references right.encoding in
          let references = add_reference name (obj (fields1 @ fields2)) references in
          (Ref name, references)
      | Tup { encoding } ->
          layout recursives references encoding
      | (Tups _ as descr) ->
          let fields, references = fields recursives references descr in
          let name = new_reference () in
          let references = add_reference name (obj fields) references in
          (Ref name, references)
      | Union { kind ; tag_size ; cases } ->
          let name, references = union recursives references kind tag_size cases in
          (Ref name, references)
      | Mu { name ; description ; fix } as encoding ->
          if List.mem name recursives
          then (Ref name, references)
          else
            let { encoding } = fix { encoding ; json_encoding = None } in
            let (name, references) = describe ~name ?description (name :: recursives) references encoding in
            (Ref name, references)
      | Conv { encoding } ->
          layout recursives references encoding.encoding
      | Describe { id = name ; description ; encoding } ->
          let name, references =
            describe ~name ?description recursives references encoding.encoding in
          (Ref name, references)
      | Splitted { encoding } ->
          layout recursives references encoding.encoding
      | (Dynamic_size _) as encoding ->
          let fields, references = fields recursives references encoding in
          let name = new_reference () in
          UF.add uf { name ; description = None } ;
          (Ref name, add_reference name (obj fields) references)
      | Check_size { encoding } ->
          layout recursives references encoding.encoding
      | Delayed func ->
          layout recursives references (func ()).encoding in
  let toplevel_name = Option.unopt ~default:"Toplevel encoding" toplevel_name in
  uf_add_name toplevel_name ;
  let fields, references = fields [] { descriptions = [] } encoding.encoding in
  let rev_references = (toplevel_name, obj fields) :: references.descriptions in
  let dedup_canonicalize =
    let tbl : (Binary_schema.toplevel_encoding, Binary_schema.description) Hashtbl.t = Hashtbl.create 100 in
    let rec help prev_len acc = function
      | [] ->
          let fixedup =
            List.map
              (fun (desc, layout) -> (desc, fixup_references uf layout))
              acc in
          if List.length fixedup = prev_len
          then
            List.map
              (fun (name, layout) ->
                 (UF.find uf name, layout))
              fixedup
          else
            begin
              Hashtbl.clear tbl ;
              help (List.length fixedup) [] fixedup
            end
      | (name, layout) :: tl ->
          match Hashtbl.find_opt tbl layout with
          | None ->
              let desc = UF.find uf name in
              begin
                Hashtbl.add tbl layout desc ;
                help prev_len ((desc.name, layout) :: acc) tl
              end
          | Some original_desc ->
              begin
                UF.union uf
                  ~new_cannonical:original_desc
                  ~existing:name ;
                help prev_len acc tl
              end
    in help 0 [] in
  let filtered =
    List.filter
      (fun (name, encoding) ->
         match encoding with
         | Binary_schema.Obj { fields = [ Anonymous_field (_, Ref reference) ] } ->
             UF.union uf ~new_cannonical:(UF.find uf name) ~existing:reference ;
             false
         | _ -> true)
      rev_references in
  dedup_canonicalize filtered
