/*****************************************************************************/
/*                                                                           */
/* Open Source License                                                       */
/* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     */
/*                                                                           */
/* Permission is hereby granted, free of charge, to any person obtaining a   */
/* copy of this software and associated documentation files (the "Software"),*/
/* to deal in the Software without restriction, including without limitation */
/* the rights to use, copy, modify, merge, publish, distribute, sublicense,  */
/* and/or sell copies of the Software, and to permit persons to whom the     */
/* Software is furnished to do so, subject to the following conditions:      */
/*                                                                           */
/* The above copyright notice and this permission notice shall be included   */
/* in all copies or substantial portions of the Software.                    */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*/
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   */
/* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*/
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   */
/* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       */
/* DEALINGS IN THE SOFTWARE.                                                 */
/*                                                                           */
/*****************************************************************************/

type recursives = list(string);

[@unwrapped]
type references = {
  descriptions: list((string, Binary_schema.toplevel_encoding)),
};

/* Simple Union find implementation, there are several optimizations
   that give UF it's usual time complexity that could be added.
   If this is a bottleneck, they're easy to add. */
module UF: {
  type t;

  let add: (t, Binary_schema.description) => unit;

  let find: (t, string) => Binary_schema.description;

  let union:
    (t, ~new_canonical: Binary_schema.description, ~existing: string) => unit;

  let empty: unit => t;
} = {
  open Binary_schema;

  type ele =
    | Ref(string)
    | Root(description);

  type t = Hashtbl.t(string, ele);

  let add = (t, x) => Hashtbl.replace(t, x.title, Root(x));

  let rec find = (tbl, key) =>
    switch (Hashtbl.find(tbl, key)) {
    | Ref(s) => find(tbl, s)
    | Root(desc) => desc
    };

  let union = (tbl, ~new_canonical, ~existing) => {
    add(tbl, new_canonical);
    let root = find(tbl, existing);
    if (root.title == new_canonical.title) {
      ();
    } else {
      Hashtbl.replace(tbl, root.title, Ref(new_canonical.title));
    };
  };

  let empty = () => Hashtbl.create(128);
};

let fixup_references = uf => {
  open Binary_schema;
  let rec fixup_layout =
    fun
    | Ref(s) => Ref(UF.find(uf, s).title)
    | [@implicit_arity] Enum(i, name) =>
      [@implicit_arity] Enum(i, UF.find(uf, name).title)
    | [@implicit_arity] Seq(layout, len) =>
      [@implicit_arity] Seq(fixup_layout(layout), len)
    | (
        Zero_width | Int(_) | Bool | [@implicit_arity] RangedInt(_, _) |
        [@implicit_arity] RangedFloat(_, _) |
        Float |
        Bytes |
        String |
        Padding
      ) as enc => enc;

  let field =
    fun
    | [@implicit_arity] Named_field(name, kind, layout) =>
      [@implicit_arity] Named_field(name, kind, fixup_layout(layout))
    | [@implicit_arity] Anonymous_field(kind, layout) =>
      [@implicit_arity] Anonymous_field(kind, fixup_layout(layout))
    | (Dynamic_size_field(_) | Optional_field(_)) as field => field;

  fun
  | Obj({fields}) => Obj({fields: List.map(field, fields)})
  | Cases({cases, _} as x) =>
    Cases({
      ...x,
      cases:
        List.map(
          ((i, name, fields)) => (i, name, List.map(field, fields)),
          cases,
        ),
    })
  | Int_enum(_) as ie => ie;
};

let z_reference_name = "Z.t";

let z_reference_description = "A variable length sequence of bytes, encoding a Zarith number. Each byte has a running unary size bit: the most significant bit of each byte tells is this is the last byte in the sequence (0) or if there is more to read (1). The second most significant bit of the first byte is reserved for the sign (positive if zero). Size and sign bits ignored, data is then the binary representation of the absolute value of the number in little endian order.";

let z_encoding =
  Binary_schema.Obj({
    fields: [[@implicit_arity] Named_field("Z.t", `Dynamic, Bytes)],
  });

let add_z_reference = (uf, {descriptions}) => {
  UF.add(
    uf,
    {title: z_reference_name, description: Some(z_reference_description)},
  );
  {descriptions: [(z_reference_name, z_encoding), ...descriptions]};
};

let n_reference_name = "N.t";

let n_reference_description = "A variable length sequence of bytes, encoding a Zarith number. Each byte has a running unary size bit: the most significant bit of each byte tells is this is the last byte in the sequence (0) or if there is more to read (1). Size bits ignored, data is then the binary representation of the absolute value of the number in little endian order.";

let n_encoding =
  Binary_schema.Obj({
    fields: [[@implicit_arity] Named_field("N.t", `Dynamic, Bytes)],
  });

let add_n_reference = (uf, {descriptions}) => {
  UF.add(
    uf,
    {title: n_reference_name, description: Some(n_reference_description)},
  );
  {descriptions: [(n_reference_name, n_encoding), ...descriptions]};
};

let dedup_canonicalize = uf => {
  let tbl:
    Hashtbl.t(Binary_schema.toplevel_encoding, Binary_schema.description) = (
    Hashtbl.create(100):
      Hashtbl.t(Binary_schema.toplevel_encoding, Binary_schema.description)
  );

  let rec help = (prev_len, acc) =>
    fun
    | [] => {
        let fixedup =
          List.map(
            ((desc, layout)) => (desc, fixup_references(uf, layout)),
            acc,
          );

        if (List.length(fixedup) == prev_len) {
          List.map(
            ((name, layout)) => (UF.find(uf, name), layout),
            fixedup,
          );
        } else {
          Hashtbl.clear(tbl);
          help(List.length(fixedup), [], fixedup);
        };
      }
    | [(name, layout), ...tl] =>
      switch (Hashtbl.find_opt(tbl, layout)) {
      | None =>
        let desc = UF.find(uf, name);
        Hashtbl.add(tbl, layout, desc);
        help(prev_len, [(desc.title, layout), ...acc], tl);
      | Some(original_desc) =>
        UF.union(uf, ~new_canonical=original_desc, ~existing=name);
        help(prev_len, acc, tl);
      };

  help(0, []);
};

type pdesc =
  | P(Encoding.desc('x)): pdesc;

let describe = (type x, encoding: Encoding.t(x)) => {
  open Encoding;
  let uf = UF.empty();
  let uf_add_name = title => UF.add(uf, {title, description: None});
  let add_reference = (name, description, {descriptions}) => {
    descriptions: [(name, description), ...descriptions],
  };

  let new_reference = {
    let x = ref(- 1);
    () => {
      x := x^ + 1;
      let name = "X_" ++ string_of_int(x^);
      uf_add_name(name);
      name;
    };
  };

  let may_new_reference =
    fun
    | None => new_reference()
    | Some(name) => {
        uf_add_name(name);
        name;
      };

  let rec extract_dynamic:
    type x.
      (option(string), Encoding.desc(x)) =>
      (option(Binary_size.unsigned_integer), option(string), pdesc) =
    ref_name =>
      fun
      | Conv({encoding, _}) => extract_dynamic(ref_name, encoding.encoding)
      | Describe({id: ref_name, encoding, _}) =>
        extract_dynamic(Some(ref_name), encoding.encoding)
      | Splitted({encoding, _}) =>
        extract_dynamic(ref_name, encoding.encoding)
      | Delayed(f) => extract_dynamic(ref_name, f().encoding)
      | Dynamic_size({kind, encoding}) => (
          Some(kind),
          ref_name,
          P(encoding.encoding),
        )
      | enc => (None, ref_name, P(enc));

  let rec field_descr:
    type a.
      (recursives, references, Encoding.field(a)) =>
      (list(Binary_schema.field_descr), references) =
    (recursives, references) =>
      fun
      | Req({name, encoding: {encoding, _}, _})
      | Dft({name, encoding: {encoding, _}, _}) => {
          let (dynamics, ref_name, P(field)) =
            extract_dynamic(None, encoding);
          let (layout, references) =
            layout(ref_name, recursives, references, field);

          if (layout == Zero_width) {
            ([], references);
          } else {
            let field_descr =
              [@implicit_arity]
              Binary_schema.Named_field(name, classify_desc(field), layout);

            switch (dynamics) {
            | Some(kind) => (
                [
                  [@implicit_arity] Dynamic_size_field(ref_name, 1, kind),
                  field_descr,
                ],
                references,
              )
            | None => ([field_descr], references)
            };
          };
        }
      | Opt({kind: `Variable, name, encoding: {encoding, _}, _}) => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [[@implicit_arity] Named_field(name, `Variable, layout)],
            references,
          );
        }
      | Opt({kind: `Dynamic, name, encoding: {encoding, _}, _}) => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              Binary_schema.Optional_field(name),
              [@implicit_arity]
              Named_field(name, classify_desc(encoding), layout),
            ],
            references,
          );
        }
  and obj = fields => Binary_schema.Obj({fields: fields})
  and union:
    type a.
      (
        option(string),
        recursives,
        references,
        Kind.t,
        Binary_size.tag_size,
        list(case(a))
      ) =>
      (string, references) =
    (ref_name, recursives, references, kind, size, cases) => {
      let cases =
        List.sort(((t1, _), (t2, _)) =>
          (compare: (int, int) => int)(t1, t2)
        ) @@
        List.fold_left(
          (acc, case) =>
            switch (case) {
            | Case({tag: Json_only, _})
            | Lazy_case({tag: Json_only, _}) => acc
            | Case({tag: Tag(tag), _})
            | Lazy_case({tag: Tag(tag), _}) => [(tag, case), ...acc]
            },
          [],
          cases,
        );

      let tag_field =
        [@implicit_arity]
        Binary_schema.Named_field(
          "Tag",
          `Fixed(Binary_size.tag_size(size)),
          Int((size :> Binary_schema.integer_extended)),
        );

      let (cases, references) =
        List.fold_right(
          ((tag, case), (cases, references)) => {
            let fields = encoding =>
              fields(None, recursives, references, encoding);
            let (fields, references) =
              switch (case) {
              | Case({encoding, _}) => fields(encoding.encoding)
              | Lazy_case({encoding, _}) =>
                fields(Lazy.force(encoding).encoding)
              };

            let title =
              switch (case) {
              | Case({title, _}) => title
              | Lazy_case({title, _}) => title
              };

            (
              [(tag, Some(title), [tag_field, ...fields]), ...cases],
              references,
            );
          },
          cases,
          ([], references),
        );

      let name = may_new_reference(ref_name);
      let references =
        add_reference(
          name,
          Cases({kind, tag_size: size, cases}),
          references,
        );

      (name, references);
    }
  and describe:
    type b.
      (
        ~description: string=?,
        ~title: string,
        string,
        recursives,
        references,
        desc(b)
      ) =>
      (string, references) =
    (~description=?, ~title, name, recursives, references, encoding) => {
      let new_canonical = {Binary_schema.title, description};
      UF.add(uf, new_canonical);
      let (layout, references) =
        layout(None, recursives, references, encoding);
      switch (layout) {
      | Ref(ref_name) =>
        UF.union(uf, ~existing=ref_name, ~new_canonical);
        (ref_name, references);
      | layout =>
        UF.add(uf, new_canonical);
        (
          name,
          add_reference(
            name,
            obj([
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ]),
            references,
          ),
        );
      };
    }
  and enum: type a. (Hashtbl.t(a, _), array(a)) => _ =
    (tbl, encoding_array) => (
      Binary_size.range_to_size(
        ~minimum=0,
        ~maximum=Array.length(encoding_array),
      ),
      List.map(
        i => (i, fst @@ Hashtbl.find(tbl, encoding_array[i])),
        List.init(Array.length(encoding_array), i => i),
      ),
    )
  and fields:
    type b.
      (option(string), recursives, references, Encoding.desc(b)) =>
      (Binary_schema.fields, references) =
    (ref_name, recursives, references) =>
      fun
      | Obj(field) => field_descr(recursives, references, field)
      | Objs({left, right, _}) => {
          let (left_fields, references) =
            fields(None, recursives, references, left.encoding);

          let (right_fields, references) =
            fields(None, recursives, references, right.encoding);

          (left_fields @ right_fields, references);
        }
      | Null => (
          [[@implicit_arity] Anonymous_field(`Fixed(0), Zero_width)],
          references,
        )
      | Empty => (
          [[@implicit_arity] Anonymous_field(`Fixed(0), Zero_width)],
          references,
        )
      | Ignore => (
          [[@implicit_arity] Anonymous_field(`Fixed(0), Zero_width)],
          references,
        )
      | Constant(_) => (
          [[@implicit_arity] Anonymous_field(`Fixed(0), Zero_width)],
          references,
        )
      | Dynamic_size({kind, encoding}) => {
          let (fields, refs) =
            fields(None, recursives, references, encoding.encoding);

          (
            [
              [@implicit_arity]
              Dynamic_size_field(None, List.length(fields), kind),
              ...fields,
            ],
            refs,
          );
        }
      | Check_size({encoding, _}) =>
        fields(ref_name, recursives, references, encoding.encoding)
      | Conv({encoding, _}) =>
        fields(ref_name, recursives, references, encoding.encoding)
      | Describe({id: name, encoding, _}) =>
        fields(Some(name), recursives, references, encoding.encoding)
      | Splitted({encoding, _}) =>
        fields(ref_name, recursives, references, encoding.encoding)
      | Delayed(func) =>
        fields(ref_name, recursives, references, func().encoding)
      | [@implicit_arity] List(len, {encoding, _}) => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(`Variable, [@implicit_arity] Seq(layout, len)),
            ],
            references,
          );
        }
      | [@implicit_arity] Array(len, {encoding, _}) => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(`Variable, [@implicit_arity] Seq(layout, len)),
            ],
            references,
          );
        }
      | Bytes(kind) => (
          [[@implicit_arity] Anonymous_field((kind :> Kind.t), Bytes)],
          references,
        )
      | String(kind) => (
          [[@implicit_arity] Anonymous_field((kind :> Kind.t), String)],
          references,
        )
      | [@implicit_arity] Padded({encoding: e, _}, n) => {
          let (fields, references) =
            fields(ref_name, recursives, references, e);
          (
            fields
            @ [[@implicit_arity] Named_field("padding", `Fixed(n), Padding)],
            references,
          );
        }
      | [@implicit_arity] String_enum(tbl, encoding_array) as encoding => {
          let (size, cases) = enum(tbl, encoding_array);
          let name = may_new_reference(ref_name);
          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), Ref(name)),
            ],
            add_reference(name, Int_enum({size, cases}), references),
          );
        }
      | Tup({encoding, _}) => {
          let (layout, references) =
            layout(ref_name, recursives, references, encoding);

          if (layout == Zero_width) {
            ([], references);
          } else {
            (
              [
                [@implicit_arity]
                Anonymous_field(classify_desc(encoding), layout),
              ],
              references,
            );
          };
        }
      | Tups({left, right, _}) => {
          let (fields1, references) =
            fields(None, recursives, references, left.encoding);

          let (fields2, references) =
            fields(None, recursives, references, right.encoding);

          (fields1 @ fields2, references);
        }
      | Union({kind, tag_size, cases}) => {
          let (name, references) =
            union(None, recursives, references, kind, tag_size, cases);

          (
            [[@implicit_arity] Anonymous_field(kind, Ref(name))],
            references,
          );
        }
      | Mu({kind, name, title, description, fix}) as encoding => {
          let kind = (kind :> Kind.t);
          let title =
            switch (title) {
            | Some(title) => title
            | None => name
            };
          if (List.mem(name, recursives)) {
            (
              [[@implicit_arity] Anonymous_field(kind, Ref(name))],
              references,
            );
          } else {
            let {encoding, _} = fix({encoding, json_encoding: None});
            let (name, references) =
              describe(
                ~title,
                ~description?,
                name,
                [name, ...recursives],
                references,
                encoding,
              );

            (
              [[@implicit_arity] Anonymous_field(kind, Ref(name))],
              references,
            );
          };
        }
      | Bool as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Int8 as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Uint8 as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Int16 as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Uint16 as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Int31 as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Int32 as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Int64 as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | N as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Z as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | RangedInt(_) as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | RangedFloat(_) as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
      | Float as encoding => {
          let (layout, references) =
            layout(None, recursives, references, encoding);

          (
            [
              [@implicit_arity]
              Anonymous_field(classify_desc(encoding), layout),
            ],
            references,
          );
        }
  and layout:
    type c.
      (option(string), recursives, references, Encoding.desc(c)) =>
      (Binary_schema.layout, references) =
    (ref_name, recursives, references) =>
      fun
      | Null => (Zero_width, references)
      | Empty => (Zero_width, references)
      | Ignore => (Zero_width, references)
      | Constant(_) => (Zero_width, references)
      | Bool => (Bool, references)
      | Int8 => (Int(`Int8), references)
      | Uint8 => (Int(`Uint8), references)
      | Int16 => (Int(`Int16), references)
      | Uint16 => (Int(`Uint16), references)
      | Int31 => (
          [@implicit_arity] RangedInt(- 1073741824, 1073741823),
          references,
        )
      | Int32 => (Int(`Int32), references)
      | Int64 => (Int(`Int64), references)
      | N => (Ref(n_reference_name), add_n_reference(uf, references))
      | Z => (Ref(z_reference_name), add_z_reference(uf, references))
      | RangedInt({minimum, maximum}) => (
          [@implicit_arity] RangedInt(minimum, maximum),
          references,
        )
      | RangedFloat({minimum, maximum}) => (
          [@implicit_arity] RangedFloat(minimum, maximum),
          references,
        )
      | Float => (Float, references)
      | Bytes(_kind) => (Bytes, references)
      | String(_kind) => (String, references)
      | Padded(_) as enc => {
          let name = may_new_reference(ref_name);
          let (fields, references) =
            fields(None, recursives, references, enc);
          let references = add_reference(name, obj(fields), references);
          (Ref(name), references);
        }
      | [@implicit_arity] String_enum(tbl, encoding_array) => {
          let name = may_new_reference(ref_name);
          let (size, cases) = enum(tbl, encoding_array);
          let references =
            add_reference(name, Int_enum({size, cases}), references);

          ([@implicit_arity] Enum(size, name), references);
        }
      | [@implicit_arity] Array(len, data) => {
          let (descr, references) =
            layout(None, recursives, references, data.encoding);

          ([@implicit_arity] Seq(descr, len), references);
        }
      | [@implicit_arity] List(len, data) => {
          let (layout, references) =
            layout(None, recursives, references, data.encoding);

          ([@implicit_arity] Seq(layout, len), references);
        }
      | Obj(Req({encoding: {encoding, _}, _}))
      | Obj(Dft({encoding: {encoding, _}, _})) =>
        layout(ref_name, recursives, references, encoding)
      | Obj(Opt(_)) as enc => {
          let name = may_new_reference(ref_name);
          let (fields, references) =
            fields(None, recursives, references, enc);
          let references = add_reference(name, obj(fields), references);
          (Ref(name), references);
        }
      | Objs({left, right, _}) => {
          let name = may_new_reference(ref_name);
          let (fields1, references) =
            fields(None, recursives, references, left.encoding);

          let (fields2, references) =
            fields(None, recursives, references, right.encoding);

          let references =
            add_reference(name, obj(fields1 @ fields2), references);

          (Ref(name), references);
        }
      | Tup({encoding, _}) =>
        layout(ref_name, recursives, references, encoding)
      | Tups(_) as descr => {
          let name = may_new_reference(ref_name);
          let (fields, references) =
            fields(None, recursives, references, descr);
          let references = add_reference(name, obj(fields), references);
          (Ref(name), references);
        }
      | Union({kind, tag_size, cases}) => {
          let (name, references) =
            union(ref_name, recursives, references, kind, tag_size, cases);

          (Ref(name), references);
        }
      | Mu({name, title, description, fix, _}) as encoding => {
          let title =
            switch (title) {
            | Some(title) => title
            | None => name
            };
          if (List.mem(name, recursives)) {
            (Ref(name), references);
          } else {
            let {encoding, _} = fix({encoding, json_encoding: None});
            let (name, references) =
              describe(
                name,
                ~title,
                ~description?,
                [name, ...recursives],
                references,
                encoding,
              );

            (Ref(name), references);
          };
        }
      | Conv({encoding, _}) =>
        layout(ref_name, recursives, references, encoding.encoding)
      | Describe({id: name, encoding, _}) =>
        layout(Some(name), recursives, references, encoding.encoding)
      | Splitted({encoding, _}) =>
        layout(ref_name, recursives, references, encoding.encoding)
      | Dynamic_size(_) as encoding => {
          let name = may_new_reference(ref_name);
          let (fields, references) =
            fields(None, recursives, references, encoding);

          UF.add(uf, {title: name, description: None});
          (Ref(name), add_reference(name, obj(fields), references));
        }
      | Check_size({encoding, _}) =>
        layout(ref_name, recursives, references, encoding.encoding)
      | Delayed(func) =>
        layout(ref_name, recursives, references, func().encoding);

  let (fields, references) =
    fields(None, [], {descriptions: []}, encoding.encoding);

  uf_add_name("");
  let (_, toplevel) =
    List.hd(dedup_canonicalize(uf, [("", obj(fields))]));
  let filtered =
    List.filter(
      ((name, encoding)) =>
        switch (encoding) {
        | Binary_schema.Obj({
            fields: [[@implicit_arity] Anonymous_field(_, Ref(reference))],
          }) =>
          UF.union(
            uf,
            ~new_canonical=UF.find(uf, name),
            ~existing=reference,
          );
          false;
        | _ => true
        },
      references.descriptions,
    );

  let fields = List.rev(dedup_canonicalize(uf, filtered));
  {Binary_schema.toplevel, fields};
};
