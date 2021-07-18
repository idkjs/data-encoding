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

type json = [
  | `O(list((string, json)))
  | `Bool(bool)
  | `Float(float)
  | `A(list(json))
  | `Null
  | `String(string)
];

type schema = Json_schema.schema;

type pair_builder = {
  build:
    'a 'b.
    (Encoding.Kind.t, Encoding.t('a), Encoding.t('b)) =>
    Encoding.t(('a, 'b)),

};

exception Parse_error(string);

let wrap_error = (f, str) =>
  try(f(str)) {
  | exn => raise([@implicit_arity] Json_encoding.Cannot_destruct([], exn))
  };

let int64_encoding =
  Json_encoding.(
    def(
      "int64",
      ~title="64 bit integers",
      ~description="Decimal representation of 64 bit integers",
    ) @@
    conv(Int64.to_string, wrap_error(Int64.of_string), string)
  );

let n_encoding =
  Json_encoding.(
    def(
      "positive_bignum",
      ~title="Positive big number",
      ~description="Decimal representation of a positive big number",
    ) @@
    conv(
      z => {
        if (Z.sign(z) < 0) {
          invalid_arg("negative natural");
        };
        Z.to_string(z);
      },
      s => {
        let n = Z.of_string(s);
        if (Z.sign(n) < 0) {
          raise(
            [@implicit_arity]
            Json_encoding.Cannot_destruct([], Failure("negative natural")),
          );
        };
        n;
      },
      string,
    )
  );

let z_encoding =
  Json_encoding.(
    def(
      "bignum",
      ~title="Big number",
      ~description="Decimal representation of a big number",
    ) @@
    conv(Z.to_string, Z.of_string, string)
  );

let bytes_jsont = {
  open Json_encoding;
  let schema =
    Json_schema.(
      create({
        title: None,
        description: None,
        default: None,
        enum: None,
        kind:
          String({
            pattern: Some("^[a-zA-Z0-9]+$"),
            min_length: 0,
            max_length: None,
          }),
        format: None,
        id: None,
      })
    );

  conv(
    ~schema,
    Hex.of_bytes,
    wrap_error(Hex.to_bytes),
    conv((`Hex(h)) => h, h => `Hex(h), string),
  );
};

let check_utf8 = s =>
  Uutf.String.fold_utf_8(
    (valid, _pos) =>
      fun
      | `Uchar(_) => valid
      | `Malformed(_) => false,
    true,
    s,
  );

let raw_string_encoding = {
  open Json_encoding;
  let utf8_case =
    case(
      string,
      s =>
        if (check_utf8(s)) {
          Some(s);
        } else {
          None;
        },
      s => s,
    );

  let obj_case =
    case(
      obj1(
        req(
          "invalid_utf8_string",
          array(ranged_int(~minimum=0, ~maximum=255, "byte")),
        ),
      ),
      s => Some(Array.init(String.length(s), i => Char.code(s.[i]))),
      a => String.init(Array.length(a), i => Char.chr(a[i])),
    );

  def(
    "unistring",
    ~title="Universal string representation",
    ~description=
      "Either a plain UTF8 string, or a sequence of bytes for strings that contain invalid byte sequences.",
    union([utf8_case, obj_case]),
  );
};

let rec lift_union: type a. Encoding.t(a) => Encoding.t(a) =
  e =>
    Encoding.(
      switch (e.encoding) {
      | Conv({proj, inj, encoding: e, schema}) =>
        switch (lift_union(e)) {
        | {encoding: Union({kind, tag_size, cases}), _} =>
          make @@
          Union({
            kind,
            tag_size,
            cases:
              List.map(
                fun
                | Case({
                    title,
                    description,
                    encoding,
                    proj: proj',
                    inj: inj',
                    tag,
                  }) =>
                  Case({
                    encoding,
                    title,
                    description,
                    proj: x => proj'(proj(x)),
                    inj: x => inj(inj'(x)),
                    tag,
                  })
                | Lazy_case({
                    title,
                    description,
                    encoding,
                    proj: proj',
                    inj: inj',
                    tag,
                  }) =>
                  Case({
                    encoding: Lazy.force(encoding),
                    title,
                    description,
                    proj: x => proj'(proj(x)),
                    inj: x => inj(inj'(x)),
                    tag,
                  }),
                cases,
              ),
          })
        | e => make @@ Conv({proj, inj, encoding: e, schema})
        }
      | Objs({kind, left, right}) =>
        lift_union_in_pair(
          {build: (kind, left, right) => make @@ Objs({kind, left, right})},
          kind,
          left,
          right,
        )
      | Tups({kind, left, right}) =>
        lift_union_in_pair(
          {build: (kind, left, right) => make @@ Tups({kind, left, right})},
          kind,
          left,
          right,
        )
      | _ => e
      }
    )

and lift_union_in_pair:
  type a b.
    (pair_builder, Encoding.Kind.t, Encoding.t(a), Encoding.t(b)) =>
    Encoding.t((a, b)) =
  (b, p, e1, e2) =>
    Encoding.(
      switch (lift_union(e1), lift_union(e2)) {
      | (e1, {encoding: Union({tag_size, cases, _}), _}) =>
        make @@
        Union({
          kind: `Dynamic /* ignored */,
          tag_size,
          cases:
            List.map(
              fun
              | Case({title, description, encoding: e2, proj, inj, tag}) =>
                Case({
                  encoding: lift_union_in_pair(b, p, e1, e2),
                  title,
                  description,
                  proj: ((x, y)) =>
                    switch (proj(y)) {
                    | None => None
                    | Some(y) => Some((x, y))
                    },
                  inj: ((x, y)) => (x, inj(y)),
                  tag,
                })
              | Lazy_case({title, description, encoding: e2, proj, inj, tag}) =>
                Case({
                  encoding: lift_union_in_pair(b, p, e1, Lazy.force(e2)),
                  title,
                  description,
                  proj: ((x, y)) =>
                    switch (proj(y)) {
                    | None => None
                    | Some(y) => Some((x, y))
                    },
                  inj: ((x, y)) => (x, inj(y)),
                  tag,
                }),
              cases,
            ),
        })
      | ({encoding: Union({tag_size, cases, _}), _}, e2) =>
        make @@
        Union({
          kind: `Dynamic /* ignored */,
          tag_size,
          cases:
            List.map(
              fun
              | Case({title, description, encoding: e1, proj, inj, tag}) =>
                Case({
                  encoding: lift_union_in_pair(b, p, e1, e2),
                  title,
                  description,
                  proj: ((x, y)) =>
                    switch (proj(x)) {
                    | None => None
                    | Some(x) => Some((x, y))
                    },
                  inj: ((x, y)) => (inj(x), y),
                  tag,
                })
              | Lazy_case({title, description, encoding: e1, proj, inj, tag}) =>
                Case({
                  encoding: lift_union_in_pair(b, p, Lazy.force(e1), e2),
                  title,
                  description,
                  proj: ((x, y)) =>
                    switch (proj(x)) {
                    | None => None
                    | Some(x) => Some((x, y))
                    },
                  inj: ((x, y)) => (inj(x), y),
                  tag,
                }),
              cases,
            ),
        })
      | (e1, e2) => b.build(p, e1, e2)
      }
    );

let rec json: type a. Encoding.desc(a) => Json_encoding.encoding(a) =
  Encoding.(
    Json_encoding.(
      fun
      | Null => null
      | Empty => empty
      | Constant(s) => constant(s)
      | Ignore => unit
      | Int8 => ranged_int(~minimum=- (1 lsl 7), ~maximum=1 lsl 7 - 1, "int8")
      | Uint8 => ranged_int(~minimum=0, ~maximum=1 lsl 8 - 1, "uint8")
      | Int16 =>
        ranged_int(~minimum=- (1 lsl 15), ~maximum=1 lsl 15 - 1, "int16")
      | Uint16 => ranged_int(~minimum=0, ~maximum=1 lsl 16 - 1, "uint16")
      | RangedInt({minimum, maximum}) =>
        ranged_int(~minimum, ~maximum, "rangedInt")
      | Int31 => int
      | Int32 => int32
      | Int64 => int64_encoding
      | N => n_encoding
      | Z => z_encoding
      | Bool => bool
      | Float => float
      | RangedFloat({minimum, maximum}) =>
        ranged_float(~minimum, ~maximum, "rangedFloat")
      | String(`Fixed(expected)) => {
          let check = s => {
            let found = String.length(s);
            if (found != expected) {
              raise(
                [@implicit_arity]
                Cannot_destruct(
                  [],
                  [@implicit_arity]
                  Unexpected(
                    Format.asprintf("string (len %d)", found),
                    Format.asprintf("string (len %d)", expected),
                  ),
                ),
              );
            };
            s;
          };

          conv(check, check, raw_string_encoding);
        }
      | String(_) => raw_string_encoding
      | [@implicit_arity] Padded(e, _) => get_json(e)
      | Bytes(`Fixed(expected)) => {
          let check = s => {
            let found = Bytes.length(s);
            if (found != expected) {
              raise(
                [@implicit_arity]
                Cannot_destruct(
                  [],
                  [@implicit_arity]
                  Unexpected(
                    Format.asprintf("string (len %d)", found),
                    Format.asprintf("string (len %d)", expected),
                  ),
                ),
              );
            };
            s;
          };

          conv(check, check, bytes_jsont);
        }
      | Bytes(_) => bytes_jsont
      | [@implicit_arity] String_enum(tbl, _) =>
        string_enum(
          Hashtbl.fold((a, (str, _), acc) => [(str, a), ...acc], tbl, []),
        )
      | [@implicit_arity] Array(_, e) => array(get_json(e)) /* FIXME TODO enforce max_length */
      | [@implicit_arity] List(_, e) => list(get_json(e))
      | Obj(f) => obj1(field_json(f))
      | Objs({left, right, _}) =>
        merge_objs(get_json(left), get_json(right))
      | Tup(e) => tup1(get_json(e))
      | Tups({left, right, _}) =>
        merge_tups(get_json(left), get_json(right))
      | Conv({proj, inj, encoding: e, schema}) =>
        conv(~schema?, proj, inj, get_json(e))
      | Describe({id, title, description, encoding: e}) =>
        def(id, ~title?, ~description?, get_json(e))
      | Mu({name, fix, _}) as ty =>
        mu(name, json_encoding => get_json @@ fix(make(~json_encoding, ty)))
      | Union({cases, _}) => union(List.map(case_json, cases))
      | Splitted({json_encoding, _}) => json_encoding
      | Dynamic_size({encoding: e, _}) => get_json(e)
      | Check_size({encoding, _}) => get_json(encoding)
      | Delayed(f) => get_json(f())
    )
  )

and field_json: type a. Encoding.field(a) => Json_encoding.field(a) =
  Json_encoding.(
    fun
    | Encoding.Req({name, encoding: e, title, description}) =>
      req(~title?, ~description?, name, get_json(e))
    | Encoding.Opt({name, encoding: e, title, description, kind: _}) =>
      opt(~title?, ~description?, name, get_json(e))
    | Encoding.Dft({name, encoding: e, default: d, title, description}) =>
      dft(~title?, ~description?, name, get_json(e), d)
  )

and case_json: type a. Encoding.case(a) => Json_encoding.case(a) =
  Json_encoding.(
    fun
    | Encoding.Case({encoding: e, proj, inj, tag: _, title, description}) =>
      case(~title, ~description?, get_json(e), proj, inj)
    | Encoding.Lazy_case({encoding: e, proj, inj, tag: _, title, description}) =>
      case(~title, ~description?, get_json @@ Lazy.force(e), proj, inj)
  )

and get_json: type a. Encoding.t(a) => Json_encoding.encoding(a) =
  e =>
    switch (e.json_encoding) {
    | None =>
      let json_encoding = json(lift_union(e).encoding);
      e.json_encoding = Some(json_encoding);
      json_encoding;
    | Some(json_encoding) => json_encoding
    };

let convert = get_json;

type path = list(path_item)

and path_item = [
  | /** A field in an object. */
    `Field(string)
  | /** An index in an array. */
    `Index(int)
  | /** Any / every field or index. */
    `Star
  | /** The next element after an array. */
    `Next
];

include Json_encoding;

let construct = (e, v) => construct(get_json(e), v);

let destruct = (e, v) => destruct(get_json(e), v);

let schema = (~definitions_path=?, e) =>
  schema(~definitions_path?, get_json(e));

let cannot_destruct = fmt =>
  Format.kasprintf(
    msg => raise([@implicit_arity] Cannot_destruct([], Failure(msg))),
    fmt,
  );

type t = json;

let to_string = (~newline=false, ~minify=?, j) =>
  Format.asprintf(
    "%a%s",
    Json_repr.(pp(~compact=?minify, (module Ezjsonm))),
    j,
    if (newline) {"\n"} else {""},
  );

let pp = Json_repr.(pp((module Ezjsonm)));

let from_string = s =>
  switch (Ezjsonm.from_string("[" ++ s ++ "]")) {
  | exception ([@implicit_arity] Ezjsonm.Parse_error(_, msg)) => Error(msg)
  | `A([json]) => Ok(json)
  | _ => Error("Malformed value")
  };

let encoding = {
  let binary: Encoding.t(Json_repr.ezjsonm) = (
    Encoding.conv(
      json =>
        Json_repr.convert(
          (module Json_repr.Ezjsonm),
          (module Json_repr_bson.Repr),
          json,
        )
        |> Json_repr_bson.bson_to_bytes
        |> Bytes.to_string,
      s =>
        try(
          Bytes.of_string(s)
          |> Json_repr_bson.bytes_to_bson(~copy=false)
          |> Json_repr.convert(
               (module Json_repr_bson.Repr),
               (module Json_repr.Ezjsonm),
             )
        ) {
        | [@implicit_arity] Json_repr_bson.Bson_decoding_error(msg, _, _) =>
          raise(Parse_error(msg))
        },
      Encoding.string,
    ):
      Encoding.t(Json_repr.ezjsonm)
  );

  let json = Json_encoding.any_ezjson_value;
  Encoding.raw_splitted(~binary, ~json);
};

let schema_encoding =
  Encoding.conv(Json_schema.to_json, Json_schema.of_json, encoding);
