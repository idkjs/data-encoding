/*****************************************************************************/
/*                                                                           */
/* Open Source License                                                       */
/* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               */
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

type id = string;

type t =
  | Record({
      encoding: Encoding.t('a),
      description: option(string),
      pp: option((Format.formatter, 'a) => unit),
    })
    : t;

module EncodingTable = Map.Make(String);

let table = ref(EncodingTable.empty);

let description = (Record({description, _})) => description;

let json_schema = (Record({encoding, _})) => {
  let json_schema = Json.schema(encoding);
  json_schema;
};

let binary_schema = (Record({encoding, _})) => {
  let binary_schema = Binary_description.describe(encoding);
  binary_schema;
};

let json_pretty_printer = (Record({encoding, pp, _}), fmt, json) =>
  switch (pp) {
  | Some(pp) =>
    let json = Json.destruct(encoding, json);
    Format.fprintf(fmt, "%a", pp, json);
  | None => Format.fprintf(fmt, "%a", Json.pp, json)
  };

let binary_pretty_printer = (Record({encoding, pp, _}), fmt, bytes) => {
  let data = Binary_reader.of_bytes_exn(encoding, bytes);
  switch (pp) {
  | Some(pp) => Format.fprintf(fmt, "%a", pp, data)
  | None =>
    let json = Json.construct(encoding, data);
    Format.fprintf(fmt, "%a", Json.pp, json);
  };
};

let rec lookup_id_descr = ({encoding, _}: Encoding.t('a)) =>
  switch (encoding) {
  | Splitted({encoding, _})
  | Dynamic_size({encoding, _})
  | Check_size({encoding, _}) => lookup_id_descr(encoding)
  | Describe({id, description, _}) => Some((id, description))
  | _ => None
  };

let register = (~pp=?, encoding) =>
  switch (lookup_id_descr(encoding)) {
  | None =>
    invalid_arg("Data_encoding.Registration.register: non def(in)ed encoding")
  | Some((id, description)) =>
    table :=
      EncodingTable.update(
        id,
        fun
        | None => {
            let record = Record({encoding, description, pp});
            Some(record);
          }
        | Some(_) =>
          Format.kasprintf(
            invalid_arg,
            "Encoding %s previously registered",
            id,
          ),
        table^,
      )
  };

let find = id => EncodingTable.find_opt(id, table^);

let list = () => EncodingTable.bindings(table^);

let bytes_of_json = (Record({encoding, _}), json) => {
  let data = Json.destruct(encoding, json);
  Binary_writer.to_bytes_opt(encoding, data);
};

let json_of_bytes = (Record({encoding, _}), bytes) =>
  switch (Binary_reader.of_bytes_opt(encoding, bytes)) {
  | Some(v) => Some(Json.construct(encoding, v))
  | None => None
  };
