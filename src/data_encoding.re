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

module Encoding = {
  include Encoding;

  let splitted = (~json, ~binary) =>
    raw_splitted(~json=Json.convert(json), ~binary);

  let assoc = enc => {
    let json = Json_encoding.assoc(Json.convert(enc));
    let binary = list(tup2(string, enc));
    raw_splitted(~json, ~binary);
  };

  module Bounded = {
    let string = length =>
      raw_splitted(
        ~binary={
          let kind = Binary_size.unsigned_range_to_size(length);
          check_size(length + Binary_size.integer_to_size(kind)) @@
          dynamic_size(~kind, Variable.string);
        },
        ~json=
          Json_encoding.(
            conv(
              s => {
                if (String.length(s) > length) {
                  invalid_arg("oversized string");
                };
                s;
              },
              s => {
                if (String.length(s) > length) {
                  raise(
                    [@implicit_arity]
                    Cannot_destruct(
                      [],
                      Invalid_argument("oversized string"),
                    ),
                  );
                };
                s;
              },
              string,
            )
          ),
      );

    let bytes = length =>
      raw_splitted(
        ~binary={
          let kind = Binary_size.unsigned_range_to_size(length);
          check_size(length + Binary_size.integer_to_size(kind)) @@
          dynamic_size(~kind, Variable.bytes);
        },
        ~json=
          Json_encoding.(
            conv(
              s => {
                if (Bytes.length(s) > length) {
                  invalid_arg("oversized string");
                };
                s;
              },
              s => {
                if (Bytes.length(s) > length) {
                  raise(
                    [@implicit_arity]
                    Cannot_destruct(
                      [],
                      Invalid_argument("oversized string"),
                    ),
                  );
                };
                s;
              },
              Json.bytes_jsont,
            )
          ),
      );
  };

  type lazy_state('a) =
    | Value('a)
    | Bytes(Bytes.t)
    | Both(Bytes.t, 'a);

  type lazy_t('a) = {
    mutable state: lazy_state('a),
    encoding: t('a),
  };

  let force_decode = le =>
    switch (le.state) {
    | Value(value) => Some(value)
    | [@implicit_arity] Both(_, value) => Some(value)
    | Bytes(bytes) =>
      switch (Binary_reader.of_bytes_opt(le.encoding, bytes)) {
      | Some(expr) =>
        le.state = [@implicit_arity] Both(bytes, expr);
        Some(expr);
      | None => None
      }
    };

  let force_bytes = le =>
    switch (le.state) {
    | Bytes(bytes) => bytes
    | [@implicit_arity] Both(bytes, _) => bytes
    | Value(value) =>
      let bytes = Binary_writer.to_bytes_exn(le.encoding, value);
      le.state = [@implicit_arity] Both(bytes, value);
      bytes;
    };

  let lazy_encoding = encoding => {
    let binary =
      Encoding.conv(
        force_bytes,
        bytes => {state: Bytes(bytes), encoding},
        Encoding.bytes,
      );

    let json =
      Encoding.conv(
        le =>
          switch (force_decode(le)) {
          | Some(r) => r
          | None => raise(Exit)
          },
        value => {state: Value(value), encoding},
        encoding,
      );

    splitted(~json, ~binary);
  };

  let make_lazy = (encoding, value) => {encoding, state: Value(value)};

  let apply_lazy = (~fun_value, ~fun_bytes, ~fun_combine, le) =>
    switch (le.state) {
    | Value(value) => fun_value(value)
    | Bytes(bytes) => fun_bytes(bytes)
    | [@implicit_arity] Both(bytes, value) =>
      fun_combine(fun_value(value), fun_bytes(bytes))
    };
};

include Encoding;
module With_version = With_version;
module Registration = Registration;
module Json = Json;
module Bson = Bson;
module Binary_schema = Binary_schema;

module Binary = {
  include Binary_error;
  include Binary_length;
  include Binary_writer;
  include Binary_reader;
  include Binary_stream_reader;

  let describe = Binary_description.describe;
};

type json = Json.t;

let json = Json.encoding;

type json_schema = Json.schema;

let json_schema = Json.schema_encoding;

type bson = Bson.t;
