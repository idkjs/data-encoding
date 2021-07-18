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

/** This is for use *within* the data encoding library only. */;

type integer_extended = [ Binary_size.integer | `Int32 | `Int64];

type field_descr =
  | Named_field(string, Encoding.Kind.t, layout)
  | Anonymous_field(Encoding.Kind.t, layout)
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
      kind: Encoding.Kind.t,
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

let pp: (Format.formatter, t) => unit;

let encoding: Encoding.t(t);
