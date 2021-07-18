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

/** Test expected errors while deserializing data. */;

open Data_encoding;
open Helpers;
open Types;

let not_enough_data =
  fun
  | Binary.Read_error(Not_enough_data) => true
  | _ => false;

let extra_bytes =
  fun
  | Binary.Read_error(Extra_bytes) => true
  | _ => false;

let trailing_zero =
  fun
  | Binary.Read_error(Trailing_zero) => true
  | _ => false;

let invalid_int =
  fun
  | Binary.Read_error(Invalid_int(_)) => true
  | [@implicit_arity] Json_encoding.Cannot_destruct([], Failure(_)) => true
  | _ => false;

let invalid_string_length =
  fun
  | [@implicit_arity]
    Json_encoding.Cannot_destruct(
      [],
      [@implicit_arity]
      Json_encoding.Unexpected("string (len 9)", "string (len 4)"),
    ) =>
    true
  | [@implicit_arity]
    Json_encoding.Cannot_destruct(
      [],
      [@implicit_arity]
      Json_encoding.Unexpected("bytes (len 9)", "bytes (len 4)"),
    ) =>
    true
  | Binary.Read_error(Extra_bytes) => true
  | _ => false;

let missing_case =
  fun
  | [@implicit_arity]
    Json_encoding.Cannot_destruct([], Json_encoding.No_case_matched(_)) =>
    true
  | Binary.Read_error(Unexpected_tag(_)) => true
  | _ => false;

let missing_enum =
  fun
  | [@implicit_arity]
    Json_encoding.Cannot_destruct([], Json_encoding.Unexpected(_)) =>
    true
  | Binary.Read_error(No_case_matched) => true
  | _ => false;

let list_too_long =
  fun
  | Binary.Read_error(List_too_long) => true
  | _ => false;

let array_too_long =
  fun
  | Binary.Read_error(Array_too_long) => true
  | _ => false;

let json = (~expected=_ => true, read_encoding, json, ()) =>
  check_raises(expected, () => ignore(Json.destruct(read_encoding, json)));

let bson = (~expected=_ => true, read_encoding, bson, ()) =>
  check_raises(expected, () => ignore(Bson.destruct(read_encoding, bson)));

let binary = (~expected=_ => true, read_encoding, bytes, ()) =>
  check_raises(expected, () =>
    ignore(Binary.of_bytes_exn(read_encoding, bytes))
  );

let stream = (~expected=_ => true, read_encoding, bytes, ()) => {
  let len_data = Bytes.length(bytes);
  for (sz in 1 to max(1, len_data)) {
    let name = Format.asprintf("stream (%d)", sz);
    switch (chunked_read(sz, read_encoding, bytes)) {
    | Binary.Success(_) =>
      Alcotest.failf("%s failed: expecting exception, got success.", name)
    | Binary.Await(_) => Alcotest.failf("%s failed: not enough data", name)
    | Binary.Error(error) when expected(Binary.Read_error(error)) => ()
    | Binary.Error(error) =>
      Alcotest.failf(
        "@[<v 2>%s failed: read error@ %a@]",
        name,
        Binary.pp_read_error,
        error,
      )
    };
  };
};

let minimal_stream =
    (~expected=_ => true, expected_read, read_encoding, bytes, ()) => {
  let name = "minimal_stream";
  switch (streamed_read(read_encoding, bytes)) {
  | (Binary.Success(_), _) =>
    Alcotest.failf("%s failed: expecting exception, got success.", name)
  | (Binary.Await(_), _) =>
    Alcotest.failf("%s failed: not enough data", name)
  | (Binary.Error(error), count)
      when expected(Binary.Read_error(error)) && count == expected_read =>
    ()
  | (Binary.Error(error), count) =>
    Alcotest.failf(
      "@[<v 2>%s failed: read error after reading %d. @ %a@]",
      name,
      count,
      Binary.pp_read_error,
      error,
    )
  };
};

let all = (~expected=?, name, write_encoding, read_encoding, value) => {
  let json_value = Json.construct(write_encoding, value);
  let bson_value = Bson.construct(write_encoding, value);
  let bytes_value = Binary.to_bytes_exn(write_encoding, value);
  [
    (name ++ ".json", `Quick, json(~expected?, read_encoding, json_value)),
    (name ++ ".bson", `Quick, bson(~expected?, read_encoding, bson_value)),
    (
      name ++ ".bytes",
      `Quick,
      binary(~expected?, read_encoding, bytes_value),
    ),
    (
      name ++ ".stream",
      `Quick,
      stream(~expected?, read_encoding, bytes_value),
    ),
  ];
};

let all_ranged_int = (minimum, maximum) => {
  let encoding = ranged_int(minimum, maximum);
  let signed =
    switch (Data_encoding__Binary_size.range_to_size(~minimum, ~maximum)) {
    | `Int31
    | `Int8
    | `Int16 => true
    | `Uint8
    | `Uint16
    | `Uint30 => false
    };

  let write_encoding =
    splitted(
      ~json=ranged_int(minimum - 1, maximum + 1),
      ~binary=
        if (signed) {
          ranged_int(minimum - 1, maximum + 1);
        } else {
          ranged_int(minimum, maximum + 1);
        },
    );

  let name = Format.asprintf("ranged_int.%d", minimum);
  all(
    ~expected=invalid_int,
    name ++ ".max",
    write_encoding,
    encoding,
    maximum + 1,
  )
  @ (
    if (signed) {
      all(
        ~expected=invalid_int,
        name ++ ".min",
        write_encoding,
        encoding,
        minimum - 1,
      );
    } else {
      let json_value = Json.construct(write_encoding, minimum - 1);
      let bson_value = Bson.construct(write_encoding, minimum - 1);
      [
        (
          name ++ "min.json",
          `Quick,
          json(~expected=invalid_int, encoding, json_value),
        ),
        (
          name ++ "min..bson",
          `Quick,
          bson(~expected=invalid_int, encoding, bson_value),
        ),
      ];
    }
  );
};

let all_ranged_float = (minimum, maximum) => {
  let encoding = ranged_float(minimum, maximum);
  let name = Format.asprintf("ranged_float.%f", minimum);
  all(name ++ ".min", float, encoding, minimum -. 1.)
  @ all(name ++ ".max", float, encoding, maximum +. 1.);
};

let test_bounded_string_list = {
  let expected =
    fun
    | Data_encoding__Binary_error.Read_error(Size_limit_exceeded) => true
    | _ => false;

  let test = (name, ~total, ~elements, v, expected_read, expected_read') => {
    let bytes = Binary.to_bytes_exn(Variable.list(string), v);
    let vbytes = Binary.to_bytes_exn(list(string), v);
    [
      (
        "bounded_string_list." ++ name,
        `Quick,
        binary(~expected, bounded_list(~total, ~elements, string), bytes),
      ),
      (
        "bounded_string_list_stream." ++ name,
        `Quick,
        stream(
          ~expected,
          dynamic_size(bounded_list(~total, ~elements, string)),
          vbytes,
        ),
      ),
      (
        "bounded_string_list_minimal_stream." ++ name,
        `Quick,
        minimal_stream(
          ~expected,
          expected_read,
          dynamic_size(bounded_list(~total, ~elements, string)),
          vbytes,
        ),
      ),
      (
        "bounded_string_list_minimal_stream." ++ name,
        `Quick,
        minimal_stream(
          ~expected,
          expected_read',
          check_size(
            total + 4,
            dynamic_size(Variable.list(check_size(elements, string))),
          ),
          vbytes,
        ),
      ),
    ];
  };

  test("a", ~total=0, ~elements=0, [""], 4, 4)
  @ test("b1", ~total=3, ~elements=4, [""], 4, 4)
  @ test("b2", ~total=4, ~elements=3, [""], 4, 4)
  @ test("c1", ~total=19, ~elements=4, ["", "", "", "", ""], 4, 4)
  @ test("c2", ~total=20, ~elements=3, ["", "", "", "", ""], 4, 4)
  @ test("d1", ~total=20, ~elements=5, ["", "", "", "", "a"], 4, 4)
  @ test("d2", ~total=21, ~elements=4, ["", "", "", "", "a"], 24, 24)
  @ test("e", ~total=30, ~elements=10, ["ab", "c", "def", "gh", "ijk"], 4, 4);
};

let tests =
  all_ranged_int(100, 400)
  @ all_ranged_int(19000, 19253)
  @ all_ranged_int(- 100, 300)
  @ all_ranged_int(- 300_000_000, 300_000_000)
  @ all_ranged_float(-. 100., 300.)
  @ all(
      "string.fixed",
      ~expected=invalid_string_length,
      string,
      Fixed.string(4),
      "turlututu",
    )
  @ all("string.bounded", string, Bounded.string(4), "turlututu")
  @ all(
      "bytes.fixed",
      ~expected=invalid_string_length,
      bytes,
      Fixed.bytes(4),
      Bytes.of_string("turlututu"),
    )
  @ all(
      "bytes.bounded",
      bytes,
      Bounded.bytes(4),
      Bytes.of_string("turlututu"),
    )
  @ all(
      "unknown_case.B",
      ~expected=missing_case,
      union_enc,
      mini_union_enc,
      B("2"),
    )
  @ all(
      "unknown_case.E",
      ~expected=missing_case,
      union_enc,
      mini_union_enc,
      E,
    )
  @ all("enum.missing", ~expected=missing_enum, enum_enc, mini_enum_enc, 4)
  @ test_bounded_string_list
  @ [
    (
      "n.truncated",
      `Quick,
      binary(~expected=not_enough_data, n, Bytes.of_string("�")),
    ),
    (
      "n.trailing_zero",
      `Quick,
      binary(~expected=trailing_zero, n, Bytes.of_string("�\000")),
    ),
    (
      "n.trailing_zero2",
      `Quick,
      binary(~expected=trailing_zero, n, Bytes.of_string("�\000")),
    ),
    (
      "z.truncated",
      `Quick,
      binary(~expected=not_enough_data, z, Bytes.of_string("�")),
    ),
    (
      "z.trailing_zero",
      `Quick,
      binary(~expected=trailing_zero, z, Bytes.of_string("�\000")),
    ),
    (
      "z.trailing_zero2",
      `Quick,
      binary(~expected=trailing_zero, z, Bytes.of_string("��\000")),
    ),
    (
      "dynamic_size.empty",
      `Quick,
      binary(
        ~expected=not_enough_data,
        dynamic_size(Variable.string),
        Bytes.of_string(""),
      ),
    ),
    (
      "dynamic_size.partial_size",
      `Quick,
      binary(
        ~expected=not_enough_data,
        dynamic_size(Variable.string),
        Bytes.of_string("\000\000"),
      ),
    ),
    (
      "dynamic_size.incomplete_data",
      `Quick,
      binary(
        ~expected=not_enough_data,
        dynamic_size(Variable.string),
        Bytes.of_string("\000\000\000\004\000\000"),
      ),
    ),
    (
      "dynamic_size.outer-garbage",
      `Quick,
      binary(
        ~expected=extra_bytes,
        dynamic_size(Variable.string),
        Bytes.of_string("\000\000\000\001\000\000"),
      ),
    ),
    (
      "dynamic_size.inner-garbage",
      `Quick,
      binary(
        ~expected=extra_bytes,
        dynamic_size(uint8),
        Bytes.of_string("\000\000\000\002\000\000"),
      ),
    ),
    (
      "list.truncated",
      `Quick,
      binary(
        ~expected=not_enough_data,
        list(~max_length=1, int8),
        Bytes.of_string("\000\000\000\002*"),
      ),
    ),
    (
      "list.too_long",
      `Quick,
      binary(
        ~expected=list_too_long,
        list(~max_length=1, int8),
        Bytes.of_string("\000\000\000\002**"),
      ),
    ),
    (
      "array.truncated",
      `Quick,
      binary(
        ~expected=not_enough_data,
        array(~max_length=1, int8),
        Bytes.of_string("\000\000\000\002*"),
      ),
    ),
    (
      "array.too_long",
      `Quick,
      binary(
        ~expected=array_too_long,
        array(~max_length=1, int8),
        Bytes.of_string("\000\000\000\002**"),
      ),
    ),
  ];
