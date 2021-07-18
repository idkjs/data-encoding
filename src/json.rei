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

/** This is for use *within* the data encoding library only. Instead, you should
    use the corresponding module intended for use: {!Data_encoding.Json}. */;

type json = [
  | `O(list((string, json)))
  | `Bool(bool)
  | `Float(float)
  | `A(list(json))
  | `Null
  | `String(string)
];

type t = json;

type schema = Json_schema.schema;

let convert: Encoding.t('a) => Json_encoding.encoding('a);

let schema: (~definitions_path: string=?, Encoding.t('a)) => schema;

let encoding: Encoding.t(json);

let schema_encoding: Encoding.t(schema);

let construct: (Encoding.t('t), 't) => json;

let destruct: (Encoding.t('t), json) => 't;

type path = list(path_item)

and path_item = [ | `Field(string) | `Index(int) | `Star | `Next];

exception Cannot_destruct((path, exn));

exception Unexpected(string, string);

exception No_case_matched(list(exn));

exception Bad_array_size(int, int);

exception Missing_field(string);

exception Unexpected_field(string);

let print_error:
  (~print_unknown: (Format.formatter, exn) => unit=?, Format.formatter, exn) =>
  unit;

let cannot_destruct: format4('a, Format.formatter, unit, 'b) => 'a;

let wrap_error: ('a => 'b, 'a) => 'b;

let from_string: string => result(json, string);

let to_string: (~newline: bool=?, ~minify: bool=?, json) => string;

let pp: (Format.formatter, json) => unit;

let bytes_jsont: Json_encoding.encoding(Bytes.t);
