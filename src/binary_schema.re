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

/* Two helper functions */
let filter_cons = (xs, x) =>
  switch (x) {
  | None => xs
  | Some(x) => [x, ...xs]
  };

let filter_map = (f, l) =>
  List.rev(List.fold_left((acc, x) => filter_cons(acc, f(x)), [], l));

open Encoding;

type integer_extended = [ Binary_size.integer | `Int32 | `Int64];

type field_descr =
  | Named_field(string, Kind.t, layout)
  | Anonymous_field(Kind.t, layout)
  | Dynamic_size_field(option(string), int, Binary_size.unsigned_integer)
  | Optional_field(string)

and layout =
  | Zero_width
  | Int(integer_extended)
  | Bool
  | RangedInt(int, int)
  | RangedFloat(float, float)
  | Float
  | Bytes
  | String
  | Enum(Binary_size.integer, string)
  | Seq(layout, option(int)) /* For arrays and lists */
  | Ref(string)
  | Padding

and fields = list(field_descr)

and toplevel_encoding =
  | Obj({fields})
  | Cases({
      kind: Kind.t,
      tag_size: Binary_size.tag_size,
      cases: list((int, option(string), fields)),
    })
  | Int_enum({
      size: Binary_size.integer,
      cases: list((int, string)),
    })

and description = {
  title: string,
  description: option(string),
};

type t = {
  toplevel: toplevel_encoding,
  fields: list((description, toplevel_encoding)),
};

module Printer_ast = {
  type table = {
    headers: list(string),
    body: list(list(string)),
  };

  type t =
    | Table(table)
    | Union(Binary_size.tag_size, list((description, table)));

  let pp_size = ppf =>
    fun
    | `Fixed(size) =>
      Format.fprintf(
        ppf,
        "%d byte%s",
        size,
        if (size == 1) {
          "";
        } else {
          "s";
        },
      )
    | `Variable => Format.fprintf(ppf, "Variable")
    | `Dynamic => Format.fprintf(ppf, "Determined from data");

  let pp_int = (ppf, int: integer_extended) =>
    Format.fprintf(
      ppf,
      "%s",
      switch (int) {
      | `Int16 => "signed 16-bit integer"
      | `Int31 => "signed 31-bit integer"
      | `Uint30 => "unsigned 30-bit integer"
      | `Int32 => "signed 32-bit integer"
      | `Int64 => "signed 64-bit integer"
      | `Int8 => "signed 8-bit integer"
      | `Uint16 => "unsigned 16-bit integer"
      | `Uint8 => "unsigned 8-bit integer"
      },
    );

  let rec pp_layout = ppf =>
    fun
    | Zero_width => ()
    | Int(integer) => Format.fprintf(ppf, "%a", pp_int, integer)
    | Bool => Format.fprintf(ppf, "boolean (0 for false, 255 for true)")
    | [@implicit_arity] RangedInt(minimum, maximum) when minimum <= 0 =>
      Format.fprintf(
        ppf,
        "%a in the range %d to %d",
        pp_int,
        (Binary_size.range_to_size(~minimum, ~maximum) :> integer_extended),
        minimum,
        maximum,
      )
    | [@implicit_arity] RangedInt(minimum, maximum) /* when minimum > 0 */ =>
      Format.fprintf(
        ppf,
        "%a in the range %d to %d (shifted by %d)",
        pp_int,
        (Binary_size.range_to_size(~minimum, ~maximum) :> integer_extended),
        minimum,
        maximum,
        minimum,
      )
    | [@implicit_arity] RangedFloat(minimum, maximum) =>
      Format.fprintf(
        ppf,
        "double-precision floating-point number, in the range %f to %f",
        minimum,
        maximum,
      )
    | Float => Format.fprintf(ppf, "double-precision floating-point number")
    | Bytes => Format.fprintf(ppf, "bytes")
    | String => Format.fprintf(ppf, "bytes")
    | Ref(reference) => Format.fprintf(ppf, "$%s", reference)
    | Padding => Format.fprintf(ppf, "padding")
    | [@implicit_arity] Enum(size, reference) =>
      Format.fprintf(
        ppf,
        "%a encoding an enumeration (see %s)",
        pp_int,
        (size :> integer_extended),
        reference,
      )
    | [@implicit_arity] Seq(data, len) => {
        Format.fprintf(ppf, "sequence of ");
        switch (len) {
        | None => ()
        | Some(len) => Format.fprintf(ppf, "at most %d ", len)
        };
        switch (data) {
        | Ref(reference) => Format.fprintf(ppf, "$%s", reference)
        | _ => pp_layout(ppf, data)
        };
      };

  let pp_tag_size = (ppf, tag) =>
    Format.fprintf(ppf, "%s") @@
    (
      switch (tag) {
      | `Uint8 => "8-bit"
      | `Uint16 => "16-bit"
      }
    );

  let field_descr = () => {
    let reference = ref(0);
    let string_of_layout = Format.asprintf("%a", pp_layout);
    let anon_num = () => {
      let value = reference^;
      reference := value + 1;
      string_of_int(value);
    };

    let is_zero_size_kind =
      fun
      | `Fixed(0) => true
      | _ => false;
    fun
    | [@implicit_arity] Named_field(name, kind, desc) =>
      Some([
        name,
        Format.asprintf("%a", pp_size, kind),
        string_of_layout(desc),
      ])
    | [@implicit_arity] Dynamic_size_field(Some(name), 1, size) =>
      Some([
        Format.asprintf("# bytes in field \"%s\"", name),
        Format.asprintf(
          "%a",
          pp_size,
          `Fixed(Binary_size.integer_to_size(size)),
        ),
        string_of_layout(Int((size :> integer_extended))),
      ])
    | [@implicit_arity] Dynamic_size_field(None, 1, size) =>
      Some([
        Format.asprintf("# bytes in next field"),
        Format.asprintf(
          "%a",
          pp_size,
          `Fixed(Binary_size.integer_to_size(size)),
        ),
        string_of_layout(Int((size :> integer_extended))),
      ])
    | [@implicit_arity] Dynamic_size_field(_, i, size) =>
      Some([
        Format.asprintf("# bytes in next %d fields", i),
        Format.asprintf(
          "%a",
          pp_size,
          `Fixed(Binary_size.integer_to_size(size)),
        ),
        string_of_layout(Int((size :> integer_extended))),
      ])
    | [@implicit_arity] Anonymous_field(kind, desc) =>
      if (!is_zero_size_kind(kind)) {
        Some([
          "Unnamed field " ++ anon_num(),
          Format.asprintf("%a", pp_size, kind),
          string_of_layout(desc),
        ]);
      } else {
        None;
      }
    | Optional_field(name) =>
      Some([
        Format.asprintf("? presence of field \"%s\"", name),
        Format.asprintf("%a", pp_size, `Fixed(1)),
        string_of_layout(Bool),
      ]);
  };

  let binary_table_headers = ["Name", "Size", "Contents"];

  let enum_headers = ["Case number", "Encoded string"];

  let toplevel = ((descr, encoding)) =>
    switch (encoding) {
    | Obj({fields}) => (
        descr,
        Table({
          headers: binary_table_headers,
          body: filter_map(field_descr(), fields),
        }),
      )
    | Cases({kind, tag_size, cases}) => (
        {
          title:
            Format.asprintf(
              "%s (%a, %a tag)",
              descr.title,
              pp_size,
              kind,
              pp_tag_size,
              tag_size,
            ),
          description: descr.description,
        },
        [@implicit_arity]
        Union(
          tag_size,
          List.map(
            ((tag, name, fields)) =>
              (
                {
                  title:
                    switch (name) {
                    | Some(name) => Format.asprintf("%s (tag %d)", name, tag)
                    | None => Format.asprintf("Tag %d", tag)
                    },
                  description: None,
                },
                {
                  headers: binary_table_headers,
                  body: filter_map(field_descr(), fields),
                },
              ),
            cases,
          ),
        ),
      )
    | Int_enum({size, cases}) => (
        {
          title:
            Format.asprintf(
              "%s (Enumeration: %a):",
              descr.title,
              pp_int,
              (size :> integer_extended),
            ),
          description: descr.description,
        },
        Table({
          headers: enum_headers,
          body:
            List.map(((num, str)) => [string_of_int(num), str], cases),
        }),
      )
    };
};

module Printer = {
  let rec pad = (char, ppf) =>
    fun
    | 0 => ()
    | n => {
        Format.pp_print_char(ppf, char);
        pad(char, ppf, n - 1);
      };

  let pp_title = (level, ppf, title) => {
    let char =
      if (level == 1) {
        '*';
      } else if (level == 2) {
        '=';
      } else {
        '`';
      };
    let sub = String.map(_ => char, title);
    Format.fprintf(ppf, "%s@ %s@\n@\n", title, sub);
  };

  let pp_table = (ppf, {Printer_ast.headers, body}) => {
    let max_widths =
      List.fold_left(
        List.map2((len, str) => max(String.length(str), len)),
        List.map(String.length, headers),
        body,
      );

    let pp_row = (pad_char, ppf) =>
      Format.fprintf(ppf, "|%a", ppf =>
        List.iter2(
          (width, str) =>
            Format.fprintf(
              ppf,
              " %s%a |",
              str,
              pad(pad_char),
              width - String.length(str),
            ),
          max_widths,
        )
      );

    let pp_line = (c, ppf) =>
      Format.fprintf(ppf, "+%a", ppf =>
        List.iter2(
          (width, _str) => Format.fprintf(ppf, "%a+", pad(c), width + 2),
          max_widths,
        )
      );

    Format.fprintf(
      ppf,
      "%a@\n%a@\n%a@\n%a@\n@\n",
      pp_line('-'),
      headers,
      pp_row(' '),
      headers,
      pp_line('='),
      headers,
      Format.pp_print_list(
        ~pp_sep=(ppf, ()) => Format.fprintf(ppf, "@\n"),
        (ppf, s) =>
          Format.fprintf(ppf, "%a@\n%a", pp_row(' '), s, pp_line('-'), s),
      ),
      body,
    );
  };

  let pp_option_nl = ppf =>
    fun
    | Some(s) => Format.fprintf(ppf, "%s@\n@\n", s)
    | None => ();

  let pp_toplevel = ppf =>
    fun
    | Printer_ast.Table(table) => pp_table(ppf, table)
    | [@implicit_arity] Union(_tag_size, tables) =>
      Format.fprintf(
        ppf,
        "%a",
        ppf =>
          Format.pp_print_list(
            ~pp_sep=(ppf, ()) => Format.fprintf(ppf, "@\n"),
            (ppf, (descr, table)) =>
              Format.fprintf(
                ppf,
                "%a%a%a",
                pp_title(2),
                descr.title,
                pp_option_nl,
                descr.description,
                pp_table,
                table,
              ),
            ppf,
          ),
        tables,
      );

  let pp = (ppf, {toplevel, fields}) => {
    let (_, toplevel) =
      Printer_ast.toplevel(({title: "", description: None}, toplevel));

    Format.fprintf(
      ppf,
      "%a@\n%a",
      pp_toplevel,
      toplevel,
      Format.pp_print_list(
        ~pp_sep=(ppf, ()) => Format.fprintf(ppf, "@\n"),
        (ppf, (descr, toplevel)) =>
          Format.fprintf(
            ppf,
            "%a%a%a",
            pp_title(1),
            descr.title,
            pp_option_nl,
            descr.description,
            pp_toplevel,
            toplevel,
          ),
      ),
      List.map(Printer_ast.toplevel, fields),
    );
  };
};

module Encoding = {
  let description_encoding =
    conv(
      ({title, description}) => (title, description),
      ((title, description)) => {title, description},
      obj2(req("title", string), opt("description", string)),
    );

  let integer_cases = [
    ("Int16", `Int16),
    ("Int8", `Int8),
    ("Uint16", `Uint16),
    ("Uint8", `Uint8),
  ];

  let integer_encoding: encoding(Binary_size.integer) = (
    string_enum(integer_cases): encoding(Binary_size.integer)
  );

  let integer_extended_encoding =
    string_enum([("Int64", `Int64), ("Int32", `Int32), ...integer_cases]);

  let layout_encoding =
    mu("layout", layout =>
      union([
        case(
          ~title="Zero_width",
          Tag(0),
          obj1(req("kind", constant("Zero_width"))),
          fun
          | Zero_width => Some()
          | _ => None,
          () =>
          Zero_width
        ),
        case(
          ~title="Int",
          Tag(1),
          obj2(
            req("size", integer_extended_encoding),
            req("kind", constant("Int")),
          ),
          fun
          | Int(integer) => Some((integer, ()))
          | _ => None,
          ((integer, _)) =>
          Int(integer)
        ),
        case(
          ~title="Bool",
          Tag(2),
          obj1(req("kind", constant("Bool"))),
          fun
          | Bool => Some()
          | _ => None,
          () =>
          Bool
        ),
        case(
          ~title="RangedInt",
          Tag(3),
          obj3(
            req("min", int31),
            req("max", int31),
            req("kind", constant("RangedInt")),
          ),
          fun
          | [@implicit_arity] RangedInt(min, max) => Some((min, max, ()))
          | _ => None,
          ((min, max, _)) =>
          [@implicit_arity] RangedInt(min, max)
        ),
        case(
          ~title="RangedFloat",
          Tag(4),
          obj3(
            req("min", float),
            req("max", float),
            req("kind", constant("RangedFloat")),
          ),
          fun
          | [@implicit_arity] RangedFloat(min, max) => Some((min, max, ()))
          | _ => None,
          ((min, max, ())) =>
          [@implicit_arity] RangedFloat(min, max)
        ),
        case(
          ~title="Float",
          Tag(5),
          obj1(req("kind", constant("Float"))),
          fun
          | Float => Some()
          | _ => None,
          () =>
          Float
        ),
        case(
          ~title="Bytes",
          Tag(6),
          obj1(req("kind", constant("Bytes"))),
          fun
          | Bytes => Some()
          | _ => None,
          () =>
          Bytes
        ),
        case(
          ~title="String",
          Tag(7),
          obj1(req("kind", constant("String"))),
          fun
          | String => Some()
          | _ => None,
          () =>
          String
        ),
        case(
          ~title="Enum",
          Tag(8),
          obj3(
            req("size", integer_encoding),
            req("reference", string),
            req("kind", constant("Enum")),
          ),
          fun
          | [@implicit_arity] Enum(size, cases) => Some((size, cases, ()))
          | _ => None,
          ((size, cases, _)) =>
          [@implicit_arity] Enum(size, cases)
        ),
        case(
          ~title="Seq",
          Tag(9),
          obj3(
            req("layout", layout),
            req("kind", constant("Seq")),
            opt("max_length", int31),
          ),
          fun
          | [@implicit_arity] Seq(layout, len) => Some((layout, (), len))
          | _ => None,
          ((layout, (), len)) =>
          [@implicit_arity] Seq(layout, len)
        ),
        case(
          ~title="Ref",
          Tag(10),
          obj2(req("name", string), req("kind", constant("Ref"))),
          fun
          | Ref(layout) => Some((layout, ()))
          | _ => None,
          ((name, ())) =>
          Ref(name)
        ),
        case(
          ~title="Padding",
          Tag(11),
          obj1(req("kind", constant("Padding"))),
          fun
          | Padding => Some()
          | _ => None,
          () =>
          Padding
        ),
      ])
    );

  let kind_enum_cases = () => [
    case(
      ~title="Dynamic",
      Tag(0),
      obj1(req("kind", constant("Dynamic"))),
      fun
      | `Dynamic => Some()
      | _ => None,
      () =>
      `Dynamic
    ),
    case(
      ~title="Variable",
      Tag(1),
      obj1(req("kind", constant("Variable"))),
      fun
      | `Variable => Some()
      | _ => None,
      () =>
      `Variable
    ),
  ];

  let kind_t_encoding =
    def("schema.kind") @@
    union([
      case(
        ~title="Fixed",
        Tag(2),
        obj2(req("size", int31), req("kind", constant("Float"))),
        fun
        | `Fixed(n) => Some((n, ()))
        | _ => None,
        ((n, _)) =>
        `Fixed(n)
      ),
      ...kind_enum_cases(),
    ]);

  let unsigned_integer_encoding =
    string_enum([
      ("Uint30", `Uint30),
      ("Uint16", `Uint16),
      ("Uint8", `Uint8),
    ]);

  let field_descr_encoding = {
    let dynamic_layout_encoding = dynamic_size(layout_encoding);
    def("schema.field") @@
    union([
      case(
        ~title="Named_field",
        Tag(0),
        obj4(
          req("name", string),
          req("layout", dynamic_layout_encoding),
          req("data_kind", kind_t_encoding),
          req("kind", constant("named")),
        ),
        fun
        | [@implicit_arity] Named_field(name, kind, layout) =>
          Some((name, layout, kind, ()))
        | _ => None,
        ((name, kind, layout, _)) =>
        [@implicit_arity] Named_field(name, layout, kind)
      ),
      case(
        ~title="Anonymous_field",
        Tag(1),
        obj3(
          req("layout", dynamic_layout_encoding),
          req("kind", constant("anon")),
          req("data_kind", kind_t_encoding),
        ),
        fun
        | [@implicit_arity] Anonymous_field(kind, layout) =>
          Some((layout, (), kind))
        | _ => None,
        ((kind, _, layout)) =>
        [@implicit_arity] Anonymous_field(layout, kind)
      ),
      case(
        ~title="Dynamic_field",
        Tag(2),
        obj4(
          req("kind", constant("dyn")),
          opt("name", string),
          req("num_fields", int31),
          req("size", unsigned_integer_encoding),
        ),
        fun
        | [@implicit_arity] Dynamic_size_field(name, i, size) =>
          Some(((), name, i, size))
        | _ => None,
        (((), name, i, size)) =>
        [@implicit_arity] Dynamic_size_field(name, i, size)
      ),
      case(
        ~title="Optional_field",
        Tag(3),
        obj2(
          req("kind", constant("option_indicator")),
          req("name", string),
        ),
        fun
        | Optional_field(s) => Some(((), s))
        | _ => None,
        (((), s)) =>
        Optional_field(s)
      ),
    ]);
  };

  let tag_size_encoding =
    string_enum([("Uint16", `Uint16), ("Uint8", `Uint8)]);

  let binary_description_encoding =
    union([
      case(
        ~title="Obj",
        Tag(0),
        obj1(req("fields", list(dynamic_size(field_descr_encoding)))),
        fun
        | Obj({fields}) => Some(fields)
        | _ => None,
        fields =>
        Obj({fields: fields})
      ),
      case(
        ~title="Cases",
        Tag(1),
        obj3(
          req("tag_size", tag_size_encoding),
          req("kind", dynamic_size(kind_t_encoding)),
          req(
            "cases",
            list(
              def("union case") @@
              conv(
                ((tag, name, fields)) => (tag, fields, name),
                ((tag, fields, name)) => (tag, name, fields),
              ) @@
              obj3(
                req("tag", int31),
                req("fields", list(dynamic_size(field_descr_encoding))),
                opt("name", string),
              ),
            ),
          ),
        ),
        fun
        | Cases({kind, tag_size, cases}) => Some((tag_size, kind, cases))
        | _ => None,
        ((tag_size, kind, cases)) =>
        Cases({kind, tag_size, cases})
      ),
      case(
        ~title="Int_enum",
        Tag(2),
        obj2(
          req("size", integer_encoding),
          req("cases", list(tup2(int31, string))),
        ),
        fun
        | Int_enum({size, cases}) => Some((size, cases))
        | _ => None,
        ((size, cases)) =>
        Int_enum({size, cases})
      ),
    ]);

  let encoding =
    conv(
      ({toplevel, fields}) => (toplevel, fields),
      ((toplevel, fields)) => {toplevel, fields},
    ) @@
    obj2(
      req("toplevel", binary_description_encoding),
      req(
        "fields",
        list(
          obj2(
            req("description", description_encoding),
            req("encoding", binary_description_encoding),
          ),
        ),
      ),
    );
};

let encoding = Encoding.encoding;

let pp = Printer.pp;
