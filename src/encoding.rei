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
    use the corresponding module intended for use: {!Data_encoding.Encoding}. */;

module Kind: {
  type t = [ | `Fixed(int) | `Dynamic | `Variable];

  type length = [ | `Fixed(int) | `Variable];

  type enum = [ | `Dynamic | `Variable];

  let combine: (string, t, t) => t;

  let merge: (t, t) => t;

  let merge_list: (Binary_size.tag_size, list(t)) => t;
};

type case_tag =
  | Tag(int)
  | Json_only;

type desc('a) =
  | Null: desc(unit)
  | Empty: desc(unit)
  | Ignore: desc(unit)
  | Constant(string): desc(unit)
  | Bool: desc(bool)
  | Int8: desc(int)
  | Uint8: desc(int)
  | Int16: desc(int)
  | Uint16: desc(int)
  | Int31: desc(int)
  | Int32: desc(Int32.t)
  | Int64: desc(Int64.t)
  | N: desc(Z.t)
  | Z: desc(Z.t)
  | RangedInt({
      minimum: int,
      maximum: int,
    })
    : desc(int)
  | RangedFloat({
      minimum: float,
      maximum: float,
    })
    : desc(float)
  | Float: desc(float)
  | Bytes(Kind.length): desc(Bytes.t)
  | String(Kind.length): desc(string)
  | Padded(t('a), int): desc('a)
  | String_enum(Hashtbl.t('a, (string, int)), array('a)): desc('a)
  | Array(option(int), t('a)): desc(array('a))
  | List(option(int), t('a)): desc(list('a))
  | Obj(field('a)): desc('a)
  | Objs({
      kind: Kind.t,
      left: t('a),
      right: t('b),
    })
    : desc(('a, 'b))
  | Tup(t('a)): desc('a)
  | Tups({
      kind: Kind.t,
      left: t('a),
      right: t('b),
    })
    : desc(('a, 'b))
  | Union({
      kind: Kind.t,
      tag_size: Binary_size.tag_size,
      cases: list(case('a)),
    })
    : desc('a)
  | Mu({
      kind: Kind.enum,
      name: string,
      title: option(string),
      description: option(string),
      fix: t('a) => t('a),
    })
    : desc('a)
  | Conv({
      proj: 'a => 'b,
      inj: 'b => 'a,
      encoding: t('b),
      schema: option(Json_schema.schema),
    })
    : desc('a)
  | Describe({
      id: string,
      title: option(string),
      description: option(string),
      encoding: t('a),
    })
    : desc('a)
  | Splitted({
      encoding: t('a),
      json_encoding: Json_encoding.encoding('a),
      is_obj: bool,
      is_tup: bool,
    })
    : desc('a)
  | Dynamic_size({
      kind: Binary_size.unsigned_integer,
      encoding: t('a),
    })
    : desc('a)
  | Check_size({
      limit: int,
      encoding: t('a),
    })
    : desc('a)
  | Delayed(unit => t('a)): desc('a)

and field(_) =
  | Req({
      name: string,
      encoding: t('a),
      title: option(string),
      description: option(string),
    })
    : field('a)
  | Opt({
      name: string,
      kind: Kind.enum,
      encoding: t('a),
      title: option(string),
      description: option(string),
    })
    : field(option('a))
  | Dft({
      name: string,
      encoding: t('a),
      default: 'a,
      title: option(string),
      description: option(string),
    })
    : field('a)

and case('a) =
  | Case({
      title: string,
      description: option(string),
      encoding: t('a),
      proj: 't => option('a),
      inj: 'a => 't,
      tag: case_tag,
    })
    : case('t)
  | Lazy_case({
      title: string,
      description: option(string),
      encoding: Lazy.t(t('a)),
      proj: 't => option('a),
      inj: 'a => 't,
      tag: case_tag,
    })
    : case('t)

and t('a) = {
  encoding: desc('a),
  mutable json_encoding: option(Json_encoding.encoding('a)),
};

type encoding('a) = t('a);

let make: (~json_encoding: Json_encoding.encoding('a)=?, desc('a)) => t('a);

let null: encoding(unit);

let empty: encoding(unit);

let unit: encoding(unit);

let constant: string => encoding(unit);

let int8: encoding(int);

let uint8: encoding(int);

let int16: encoding(int);

let uint16: encoding(int);

let int31: encoding(int);

let int32: encoding(int32);

let int64: encoding(int64);

let n: encoding(Z.t);

let z: encoding(Z.t);

let ranged_int: (int, int) => encoding(int);

let ranged_float: (float, float) => encoding(float);

let bool: encoding(bool);

let string: encoding(string);

let bytes: encoding(Bytes.t);

let float: encoding(float);

let option: encoding('a) => encoding(option('a));

let result: (encoding('a), encoding('b)) => encoding(result('a, 'b));

let string_enum: list((string, 'a)) => encoding('a);

let is_obj: encoding('a) => bool;

let is_tup: encoding('a) => bool;

module Fixed: {
  let string: int => encoding(string);

  let bytes: int => encoding(Bytes.t);

  let add_padding: (encoding('a), int) => encoding('a);
};

module Variable: {
  let string: encoding(string);

  let bytes: encoding(Bytes.t);

  let array: (~max_length: int=?, encoding('a)) => encoding(array('a));

  let list: (~max_length: int=?, encoding('a)) => encoding(list('a));
};

let dynamic_size:
  (~kind: Binary_size.unsigned_integer=?, encoding('a)) => encoding('a);

let check_size: (int, encoding('a)) => encoding('a);

let delayed: (unit => encoding('a)) => encoding('a);

let req:
  (~title: string=?, ~description: string=?, string, encoding('t)) =>
  field('t);

let opt:
  (~title: string=?, ~description: string=?, string, encoding('t)) =>
  field(option('t));

let varopt:
  (~title: string=?, ~description: string=?, string, encoding('t)) =>
  field(option('t));

let dft:
  (~title: string=?, ~description: string=?, string, encoding('t), 't) =>
  field('t);

let obj1: field('f1) => encoding('f1);

let obj2: (field('f1), field('f2)) => encoding(('f1, 'f2));

let obj3:
  (field('f1), field('f2), field('f3)) => encoding(('f1, 'f2, 'f3));

let obj4:
  (field('f1), field('f2), field('f3), field('f4)) =>
  encoding(('f1, 'f2, 'f3, 'f4));

let obj5:
  (field('f1), field('f2), field('f3), field('f4), field('f5)) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5));

let obj6:
  (
    field('f1),
    field('f2),
    field('f3),
    field('f4),
    field('f5),
    field('f6)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6));

let obj7:
  (
    field('f1),
    field('f2),
    field('f3),
    field('f4),
    field('f5),
    field('f6),
    field('f7)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7));

let obj8:
  (
    field('f1),
    field('f2),
    field('f3),
    field('f4),
    field('f5),
    field('f6),
    field('f7),
    field('f8)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8));

let obj9:
  (
    field('f1),
    field('f2),
    field('f3),
    field('f4),
    field('f5),
    field('f6),
    field('f7),
    field('f8),
    field('f9)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9));

let obj10:
  (
    field('f1),
    field('f2),
    field('f3),
    field('f4),
    field('f5),
    field('f6),
    field('f7),
    field('f8),
    field('f9),
    field('f10)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10));

let tup1: encoding('f1) => encoding('f1);

let tup2: (encoding('f1), encoding('f2)) => encoding(('f1, 'f2));

let tup3:
  (encoding('f1), encoding('f2), encoding('f3)) =>
  encoding(('f1, 'f2, 'f3));

let tup4:
  (encoding('f1), encoding('f2), encoding('f3), encoding('f4)) =>
  encoding(('f1, 'f2, 'f3, 'f4));

let tup5:
  (
    encoding('f1),
    encoding('f2),
    encoding('f3),
    encoding('f4),
    encoding('f5)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5));

let tup6:
  (
    encoding('f1),
    encoding('f2),
    encoding('f3),
    encoding('f4),
    encoding('f5),
    encoding('f6)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6));

let tup7:
  (
    encoding('f1),
    encoding('f2),
    encoding('f3),
    encoding('f4),
    encoding('f5),
    encoding('f6),
    encoding('f7)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7));

let tup8:
  (
    encoding('f1),
    encoding('f2),
    encoding('f3),
    encoding('f4),
    encoding('f5),
    encoding('f6),
    encoding('f7),
    encoding('f8)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8));

let tup9:
  (
    encoding('f1),
    encoding('f2),
    encoding('f3),
    encoding('f4),
    encoding('f5),
    encoding('f6),
    encoding('f7),
    encoding('f8),
    encoding('f9)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9));

let tup10:
  (
    encoding('f1),
    encoding('f2),
    encoding('f3),
    encoding('f4),
    encoding('f5),
    encoding('f6),
    encoding('f7),
    encoding('f8),
    encoding('f9),
    encoding('f10)
  ) =>
  encoding(('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10));

let merge_objs: (encoding('o1), encoding('o2)) => encoding(('o1, 'o2));

let merge_tups: (encoding('a1), encoding('a2)) => encoding(('a1, 'a2));

let array: (~max_length: int=?, encoding('a)) => encoding(array('a));

let list: (~max_length: int=?, encoding('a)) => encoding(list('a));

let case:
  (
    ~title: string,
    ~description: string=?,
    case_tag,
    encoding('a),
    't => option('a),
    'a => 't
  ) =>
  case('t);

let lazy_case:
  (
    ~title: string,
    ~description: string=?,
    case_tag,
    Lazy.t(encoding('a)),
    't => option('a),
    'a => 't
  ) =>
  case('t);

let union:
  (~tag_size: [ | `Uint8 | `Uint16]=?, list(case('t))) => encoding('t);

let def:
  (string, ~title: string=?, ~description: string=?, encoding('a)) =>
  encoding('a);

let conv:
  ('a => 'b, 'b => 'a, ~schema: Json_schema.schema=?, encoding('b)) =>
  encoding('a);

let mu:
  (
    string,
    ~title: string=?,
    ~description: string=?,
    encoding('a) => encoding('a)
  ) =>
  encoding('a);

let classify: encoding('a) => [ | `Fixed(int) | `Dynamic | `Variable];

let classify_desc: desc('a) => [ | `Fixed(int) | `Dynamic | `Variable];

let raw_splitted:
  (~json: Json_encoding.encoding('a), ~binary: encoding('a)) => encoding('a);
