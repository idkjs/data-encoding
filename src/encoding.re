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

module Kind = {
  type t = [ | `Fixed(int) | `Dynamic | `Variable];

  type length = [ | `Fixed(int) | `Variable];

  type enum = [ | `Dynamic | `Variable];

  let combine = (name): ((t, t) => t) =>
    (k1, k2) =>
      switch (k1, k2) {
      | (`Fixed(n1), `Fixed(n2)) => `Fixed(n1 + n2)
      | (`Dynamic, `Dynamic)
      | (`Fixed(_), `Dynamic)
      | (`Dynamic, `Fixed(_)) => `Dynamic
      | (`Variable, `Fixed(_))
      | (`Dynamic | `Fixed(_), `Variable) => `Variable
      | (`Variable, `Dynamic) =>
        Printf.ksprintf(
          invalid_arg,
          "Cannot merge two %s when the left element is of variable length and the right one of dynamic length. You should use the reverse order, or wrap the second one with Data_encoding.dynamic_size.",
          name,
        )
      | (`Variable, `Variable) =>
        Printf.ksprintf(
          invalid_arg,
          "Cannot merge two %s with variable length. You should wrap one of them with Data_encoding.dynamic_size.",
          name,
        )
      };

  let merge: (t, t) => t = (
    (k1, k2) =>
      switch (k1, k2) {
      | (`Fixed(n1), `Fixed(n2)) when n1 == n2 => `Fixed(n1)
      | (`Fixed(_), `Fixed(_)) => `Dynamic
      | (`Dynamic, `Dynamic)
      | (`Fixed(_), `Dynamic)
      | (`Dynamic, `Fixed(_)) => `Dynamic
      | (`Variable, `Dynamic | `Fixed(_))
      | (`Dynamic | `Fixed(_), `Variable)
      | (`Variable, `Variable) => `Variable
      }:
      (t, t) => t
  );

  let merge_list = (sz): (list(t) => t) =>
    fun
    | [] => assert(false) /* should be rejected by Data_encoding.union */
    | [k, ...ks] =>
      switch (List.fold_left(merge, k, ks)) {
      | `Fixed(n) => `Fixed(n + Binary_size.tag_size(sz))
      | k => k
      };
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

let rec classify: type a. t(a) => Kind.t = e => classify_desc(e.encoding)

and classify_desc: type a. desc(a) => Kind.t =
  e =>
    switch (e) {
    /* Fixed */
    | Null => `Fixed(0)
    | Empty => `Fixed(0)
    | Constant(_) => `Fixed(0)
    | Bool => `Fixed(Binary_size.bool)
    | Int8 => `Fixed(Binary_size.int8)
    | Uint8 => `Fixed(Binary_size.uint8)
    | Int16 => `Fixed(Binary_size.int16)
    | Uint16 => `Fixed(Binary_size.uint16)
    | Int31 => `Fixed(Binary_size.int31)
    | Int32 => `Fixed(Binary_size.int32)
    | Int64 => `Fixed(Binary_size.int64)
    | N => `Dynamic
    | Z => `Dynamic
    | RangedInt({minimum, maximum}) =>
      `Fixed(
        Binary_size.(integer_to_size @@ range_to_size(~minimum, ~maximum)),
      )
    | Float => `Fixed(Binary_size.float)
    | RangedFloat(_) => `Fixed(Binary_size.float)
    /* Tagged */
    | Bytes(kind) => (kind :> Kind.t)
    | String(kind) => (kind :> Kind.t)
    | [@implicit_arity] Padded({encoding, _}, n) =>
      switch (classify_desc(encoding)) {
      | `Fixed(m) => `Fixed(n + m)
      | _ => assert(false) /* by construction (see [Fixed.padded]) */
      }
    | [@implicit_arity] String_enum(_, cases) =>
      `Fixed(Binary_size.(integer_to_size @@ enum_size(cases)))
    | Obj(Opt({kind, _})) => (kind :> Kind.t)
    | Objs({kind, _}) => kind
    | Tups({kind, _}) => kind
    | Union({kind, _}) => (kind :> Kind.t)
    | Mu({kind, _}) => (kind :> Kind.t)
    /* Variable */
    | Ignore => `Fixed(0)
    | Array(_) => `Variable
    | List(_) => `Variable
    /* Recursive */
    | Obj(Req({encoding, _})) => classify(encoding)
    | Obj(Dft({encoding, _})) => classify(encoding)
    | Tup(encoding) => classify(encoding)
    | Conv({encoding, _}) => classify(encoding)
    | Describe({encoding, _}) => classify(encoding)
    | Splitted({encoding, _}) => classify(encoding)
    | Dynamic_size(_) => `Dynamic
    | Check_size({encoding, _}) => classify(encoding)
    | Delayed(f) => classify(f())
    };

let make = (~json_encoding=?, encoding) => {encoding, json_encoding};

module Fixed = {
  let string = n => {
    if (n <= 0) {
      invalid_arg(
        "Cannot create a string encoding of negative or null fixed length.",
      );
    };
    make @@ String(`Fixed(n));
  };

  let bytes = n => {
    if (n <= 0) {
      invalid_arg(
        "Cannot create a byte encoding of negative or null fixed length.",
      );
    };
    make @@ Bytes(`Fixed(n));
  };

  let add_padding = (e, n) => {
    if (n <= 0) {
      invalid_arg(
        "Cannot create a padding of negative or null fixed length.",
      );
    };
    switch (classify(e)) {
    | `Fixed(_) => make @@ [@implicit_arity] Padded(e, n)
    | _ => invalid_arg("Cannot pad non-fixed size encoding")
    };
  };
};

let rec is_zeroable: type t. encoding(t) => bool =
  e =>
    /* Whether an encoding can ever produce zero-byte of encoding. It is dangerous
       to place zero-size elements in a collection (list/array) because
       they are indistinguishable from the absence of elements. */
    switch (e.encoding) {
    /* trivially true */
    | Null => true /* always true */
    | Empty => true /* always true */
    | Ignore => true /* always true */
    | Constant(_) => true /* always true */
    /* trivially false */
    | Bool => false
    | Int8 => false
    | Uint8 => false
    | Int16 => false
    | Uint16 => false
    | Int31 => false
    | Int32 => false
    | Int64 => false
    | N => false
    | Z => false
    | RangedInt(_) => false
    | RangedFloat(_) => false
    | Float => false
    | Bytes(_) => false
    | String(_) => false
    | Padded(_) => false
    | String_enum(_) => false
    /* true in some cases, but in practice always protected by Dynamic */
    | Array(_) => true /* 0-element array */
    | List(_) => true /* 0-element list */
    /* represented as whatever is inside: truth mostly propagates */
    | Obj(Req({encoding: e, _})) => is_zeroable(e) /* represented as-is */
    | Obj(Opt({kind: `Variable, _})) => true /* optional field omitted */
    | Obj(Dft({encoding: e, _})) => is_zeroable(e) /* represented as-is */
    | Obj(_) => false
    | Objs({left, right, _}) => is_zeroable(left) && is_zeroable(right)
    | Tup(e) => is_zeroable(e)
    | Tups({left, right, _}) => is_zeroable(left) && is_zeroable(right)
    | Union(_) => false /* includes a tag */
    /* other recursive cases: truth propagates */
    | Mu({kind: `Dynamic, _}) => false /* size prefix */
    | Mu({kind: `Variable, fix, _}) => is_zeroable(fix(e))
    | Conv({encoding, _}) => is_zeroable(encoding)
    | Describe({encoding, _}) => is_zeroable(encoding)
    | Splitted({encoding, _}) => is_zeroable(encoding)
    | Check_size({encoding, _}) => is_zeroable(encoding)
    /* Unscrutable: true by default */
    | Delayed(f) => is_zeroable(f())
    /* Protected against zeroable */
    | Dynamic_size(_) => false
    };

/* always some data for size */

module Variable = {
  let string = make @@ String(`Variable);

  let bytes = make @@ Bytes(`Variable);

  let check_not_variable = (name, e) =>
    switch (classify(e)) {
    | `Variable =>
      Printf.ksprintf(
        invalid_arg,
        "Cannot insert variable length element in %s. You should wrap the contents using Data_encoding.dynamic_size.",
        name,
      )
    | `Dynamic
    | `Fixed(_) => ()
    };

  let check_not_zeroable = (name, e) =>
    if (is_zeroable(e)) {
      Printf.ksprintf(
        invalid_arg,
        "Cannot insert potentially zero-sized element in %s.",
        name,
      );
    } else {
      ();
    };

  let array = (~max_length=?, e) => {
    check_not_variable("an array", e);
    check_not_zeroable("an array", e);
    let encoding = make @@ [@implicit_arity] Array(max_length, e);
    switch (classify(e), max_length) {
    | (`Fixed(n), Some(max_length)) =>
      let limit = n * max_length;
      make @@ Check_size({limit, encoding});
    | (_, _) => encoding
    };
  };

  let list = (~max_length=?, e) => {
    check_not_variable("a list", e);
    check_not_zeroable("a list", e);
    let encoding = make @@ [@implicit_arity] List(max_length, e);
    switch (classify(e), max_length) {
    | (`Fixed(n), Some(max_length)) =>
      let limit = n * max_length;
      make @@ Check_size({limit, encoding});
    | (_, _) => encoding
    };
  };
};

let dynamic_size = (~kind=`Uint30, e) =>
  make @@ Dynamic_size({kind, encoding: e});

let check_size = (limit, encoding) => make @@ Check_size({limit, encoding});

let delayed = f => make @@ Delayed(f);

let null = make @@ Null;

let empty = make @@ Empty;

let unit = make @@ Ignore;

let constant = s => make @@ Constant(s);

let bool = make @@ Bool;

let int8 = make @@ Int8;

let uint8 = make @@ Uint8;

let int16 = make @@ Int16;

let uint16 = make @@ Uint16;

let int31 = make @@ Int31;

let int32 = make @@ Int32;

let ranged_int = (minimum, maximum) => {
  let minimum = min(minimum, maximum)
  and maximum = max(minimum, maximum);
  if (minimum < - (1 lsl 30) || 1 lsl 30 - 1 < maximum) {
    invalid_arg("Data_encoding.ranged_int");
  };
  make @@ RangedInt({minimum, maximum});
};

let ranged_float = (minimum, maximum) => {
  let minimum = min(minimum, maximum)
  and maximum = max(minimum, maximum);
  make @@ RangedFloat({minimum, maximum});
};

let int64 = make @@ Int64;

let n = make @@ N;

let z = make @@ Z;

let float = make @@ Float;

let string = dynamic_size(Variable.string);

let bytes = dynamic_size(Variable.bytes);

let array = (~max_length=?, e) =>
  dynamic_size(Variable.array(~max_length?, e));

let list = (~max_length=?, e) =>
  dynamic_size(Variable.list(~max_length?, e));

let string_enum =
  fun
  | [] => invalid_arg("data_encoding.string_enum: cannot have zero cases")
  | [_case] =>
    invalid_arg(
      "data_encoding.string_enum: cannot have a single case, use constant instead",
    )
  | [_, ..._] as cases => {
      let arr = Array.of_list(List.map(snd, cases));
      let tbl = Hashtbl.create(Array.length(arr));
      List.iteri(
        (ind, (str, a)) => Hashtbl.add(tbl, a, (str, ind)),
        cases,
      );
      make @@ [@implicit_arity] String_enum(tbl, arr);
    };

let conv = (proj, inj, ~schema=?, encoding) =>
  make @@ Conv({proj, inj, encoding, schema});

let def = (id, ~title=?, ~description=?, encoding) =>
  make @@ Describe({id, title, description, encoding});

let req = (~title=?, ~description=?, n, t) =>
  Req({name: n, encoding: t, title, description});

let opt = (~title=?, ~description=?, n, encoding) => {
  let kind =
    switch (classify(encoding)) {
    | `Variable => `Variable
    | `Fixed(_)
    | `Dynamic => `Dynamic
    };

  Opt({name: n, kind, encoding, title, description});
};

let varopt = (~title=?, ~description=?, n, encoding) =>
  Opt({name: n, kind: `Variable, encoding, title, description});

let dft = (~title=?, ~description=?, n, t, d) =>
  Dft({name: n, encoding: t, default: d, title, description});

let raw_splitted = (~json, ~binary) =>
  make @@
  Splitted({
    encoding: binary,
    json_encoding: json,
    is_obj: false,
    is_tup: false,
  });

let rec is_obj: type a. t(a) => bool =
  e =>
    switch (e.encoding) {
    | Obj(_) => true
    | Objs(_) /* by construction */ => true
    | Conv({encoding: e, _}) => is_obj(e)
    | Dynamic_size({encoding: e, _}) => is_obj(e)
    | Union({cases, _}) =>
      List.for_all(
        fun
        | Case({encoding: e, _}) => is_obj(e)
        | Lazy_case({encoding: e, _}) => is_obj(Lazy.force(e)),
        cases,
      )
    | Empty => true
    | Ignore => true
    | Mu({fix, _}) => is_obj(fix(e))
    | Splitted({is_obj, _}) => is_obj
    | Delayed(f) => is_obj(f())
    | Describe({encoding, _}) => is_obj(encoding)
    | _ => false
    };

let rec is_tup: type a. t(a) => bool =
  e =>
    switch (e.encoding) {
    | Tup(_) => true
    | Tups(_) /* by construction */ => true
    | Conv({encoding: e, _}) => is_tup(e)
    | Dynamic_size({encoding: e, _}) => is_tup(e)
    | Union({cases, _}) =>
      List.for_all(
        fun
        | Case({encoding: e, _}) => is_tup(e)
        | Lazy_case({encoding: e, _}) => is_tup(Lazy.force(e)),
        cases,
      )
    | Mu({fix, _}) => is_tup(fix(e))
    | Splitted({is_tup, _}) => is_tup
    | Delayed(f) => is_tup(f())
    | Describe({encoding, _}) => is_tup(encoding)
    | _ => false
    };

let raw_merge_objs = (left, right) => {
  let kind = Kind.combine("objects", classify(left), classify(right));
  make @@ Objs({kind, left, right});
};

let obj1 = f1 => make @@ Obj(f1);

let obj2 = (f2, f1) => raw_merge_objs(obj1(f2), obj1(f1));

let obj3 = (f3, f2, f1) => raw_merge_objs(obj1(f3), obj2(f2, f1));

let obj4 = (f4, f3, f2, f1) => raw_merge_objs(obj2(f4, f3), obj2(f2, f1));

let obj5 = (f5, f4, f3, f2, f1) =>
  raw_merge_objs(obj1(f5), obj4(f4, f3, f2, f1));

let obj6 = (f6, f5, f4, f3, f2, f1) =>
  raw_merge_objs(obj2(f6, f5), obj4(f4, f3, f2, f1));

let obj7 = (f7, f6, f5, f4, f3, f2, f1) =>
  raw_merge_objs(obj3(f7, f6, f5), obj4(f4, f3, f2, f1));

let obj8 = (f8, f7, f6, f5, f4, f3, f2, f1) =>
  raw_merge_objs(obj4(f8, f7, f6, f5), obj4(f4, f3, f2, f1));

let obj9 = (f9, f8, f7, f6, f5, f4, f3, f2, f1) =>
  raw_merge_objs(obj1(f9), obj8(f8, f7, f6, f5, f4, f3, f2, f1));

let obj10 = (f10, f9, f8, f7, f6, f5, f4, f3, f2, f1) =>
  raw_merge_objs(obj2(f10, f9), obj8(f8, f7, f6, f5, f4, f3, f2, f1));

let merge_objs = (o1, o2) =>
  if (is_obj(o1) && is_obj(o2)) {
    raw_merge_objs(o1, o2);
  } else {
    invalid_arg("Json_encoding.merge_objs");
  };

let raw_merge_tups = (left, right) => {
  let kind = Kind.combine("tuples", classify(left), classify(right));
  make @@ Tups({kind, left, right});
};

let tup1 = e1 => make @@ Tup(e1);

let tup2 = (e2, e1) => raw_merge_tups(tup1(e2), tup1(e1));

let tup3 = (e3, e2, e1) => raw_merge_tups(tup1(e3), tup2(e2, e1));

let tup4 = (e4, e3, e2, e1) => raw_merge_tups(tup2(e4, e3), tup2(e2, e1));

let tup5 = (e5, e4, e3, e2, e1) =>
  raw_merge_tups(tup1(e5), tup4(e4, e3, e2, e1));

let tup6 = (e6, e5, e4, e3, e2, e1) =>
  raw_merge_tups(tup2(e6, e5), tup4(e4, e3, e2, e1));

let tup7 = (e7, e6, e5, e4, e3, e2, e1) =>
  raw_merge_tups(tup3(e7, e6, e5), tup4(e4, e3, e2, e1));

let tup8 = (e8, e7, e6, e5, e4, e3, e2, e1) =>
  raw_merge_tups(tup4(e8, e7, e6, e5), tup4(e4, e3, e2, e1));

let tup9 = (e9, e8, e7, e6, e5, e4, e3, e2, e1) =>
  raw_merge_tups(tup1(e9), tup8(e8, e7, e6, e5, e4, e3, e2, e1));

let tup10 = (e10, e9, e8, e7, e6, e5, e4, e3, e2, e1) =>
  raw_merge_tups(tup2(e10, e9), tup8(e8, e7, e6, e5, e4, e3, e2, e1));

let merge_tups = (t1, t2) =>
  if (is_tup(t1) && is_tup(t2)) {
    raw_merge_tups(t1, t2);
  } else {
    invalid_arg("Tezos_serial.Encoding.merge_tups");
  };

let conv3 = ty =>
  conv(((c, b, a)) => (c, (b, a)), ((c, (b, a))) => (c, b, a), ty);

let obj3 = (f3, f2, f1) => conv3(obj3(f3, f2, f1));

let tup3 = (f3, f2, f1) => conv3(tup3(f3, f2, f1));

let conv4 = ty =>
  conv(
    ((d, c, b, a)) => ((d, c), (b, a)),
    (((d, c), (b, a))) => (d, c, b, a),
    ty,
  );

let obj4 = (f4, f3, f2, f1) => conv4(obj4(f4, f3, f2, f1));

let tup4 = (f4, f3, f2, f1) => conv4(tup4(f4, f3, f2, f1));

let conv5 = ty =>
  conv(
    ((e, d, c, b, a)) => (e, ((d, c), (b, a))),
    ((e, ((d, c), (b, a)))) => (e, d, c, b, a),
    ty,
  );

let obj5 = (f5, f4, f3, f2, f1) => conv5(obj5(f5, f4, f3, f2, f1));

let tup5 = (f5, f4, f3, f2, f1) => conv5(tup5(f5, f4, f3, f2, f1));

let conv6 = ty =>
  conv(
    ((f, e, d, c, b, a)) => ((f, e), ((d, c), (b, a))),
    (((f, e), ((d, c), (b, a)))) => (f, e, d, c, b, a),
    ty,
  );

let obj6 = (f6, f5, f4, f3, f2, f1) => conv6(obj6(f6, f5, f4, f3, f2, f1));

let tup6 = (f6, f5, f4, f3, f2, f1) => conv6(tup6(f6, f5, f4, f3, f2, f1));

let conv7 = ty =>
  conv(
    ((g, f, e, d, c, b, a)) => ((g, (f, e)), ((d, c), (b, a))),
    (((g, (f, e)), ((d, c), (b, a)))) => (g, f, e, d, c, b, a),
    ty,
  );

let obj7 = (f7, f6, f5, f4, f3, f2, f1) =>
  conv7(obj7(f7, f6, f5, f4, f3, f2, f1));

let tup7 = (f7, f6, f5, f4, f3, f2, f1) =>
  conv7(tup7(f7, f6, f5, f4, f3, f2, f1));

let conv8 = ty =>
  conv(
    ((h, g, f, e, d, c, b, a)) =>
      (((h, g), (f, e)), ((d, c), (b, a))),
    ((((h, g), (f, e)), ((d, c), (b, a)))) =>
      (h, g, f, e, d, c, b, a),
    ty,
  );

let obj8 = (f8, f7, f6, f5, f4, f3, f2, f1) =>
  conv8(obj8(f8, f7, f6, f5, f4, f3, f2, f1));

let tup8 = (f8, f7, f6, f5, f4, f3, f2, f1) =>
  conv8(tup8(f8, f7, f6, f5, f4, f3, f2, f1));

let conv9 = ty =>
  conv(
    ((i, h, g, f, e, d, c, b, a)) =>
      (i, (((h, g), (f, e)), ((d, c), (b, a)))),
    ((i, (((h, g), (f, e)), ((d, c), (b, a))))) =>
      (i, h, g, f, e, d, c, b, a),
    ty,
  );

let obj9 = (f9, f8, f7, f6, f5, f4, f3, f2, f1) =>
  conv9(obj9(f9, f8, f7, f6, f5, f4, f3, f2, f1));

let tup9 = (f9, f8, f7, f6, f5, f4, f3, f2, f1) =>
  conv9(tup9(f9, f8, f7, f6, f5, f4, f3, f2, f1));

let conv10 = ty =>
  conv(
    ((j, i, h, g, f, e, d, c, b, a)) =>
      ((j, i), (((h, g), (f, e)), ((d, c), (b, a)))),
    (((j, i), (((h, g), (f, e)), ((d, c), (b, a))))) =>
      (j, i, h, g, f, e, d, c, b, a),
    ty,
  );

let obj10 = (f10, f9, f8, f7, f6, f5, f4, f3, f2, f1) =>
  conv10(obj10(f10, f9, f8, f7, f6, f5, f4, f3, f2, f1));

let tup10 = (f10, f9, f8, f7, f6, f5, f4, f3, f2, f1) =>
  conv10(tup10(f10, f9, f8, f7, f6, f5, f4, f3, f2, f1));

let check_cases = (tag_size, cases) => {
  if (cases == []) {
    invalid_arg("Data_encoding.union: empty list of cases.");
  };
  let max_tag =
    switch (tag_size) {
    | `Uint8 => 256
    | `Uint16 => 256 * 256
    };
  ignore @@
  List.fold_left(
    (others, case) => {
      let tag =
        switch (case) {
        | Case({tag, _}) => tag
        | Lazy_case({tag, _}) => tag
        };

      switch (tag) {
      | Json_only => others
      | Tag(tag) =>
        if (List.mem(tag, others)) {
          Format.kasprintf(
            invalid_arg,
            "The tag %d appears twice in an union.",
            tag,
          );
        };
        if (tag < 0 || max_tag <= tag) {
          Format.kasprintf(invalid_arg, "The tag %d is invalid.", tag);
        };
        [tag, ...others];
      };
    },
    [],
    cases,
  );
};

let union = (~tag_size=`Uint8, cases) => {
  check_cases(tag_size, cases);
  let kinds =
    List.map(
      fun
      | Case({encoding, _}) => classify(encoding)
      | Lazy_case(_) => `Variable,
      cases,
    );

  let kind = Kind.merge_list(tag_size, kinds);
  make @@ Union({kind, tag_size, cases});
};

let case = (~title, ~description=?, tag, encoding, proj, inj) =>
  Case({title, description, encoding, proj, inj, tag});

let lazy_case = (~title, ~description=?, tag, encoding, proj, inj) =>
  Lazy_case({title, description, encoding, proj, inj, tag});

let rec is_nullable: type t. encoding(t) => bool =
  e =>
    switch (e.encoding) {
    | Null => true
    | Empty => false
    | Ignore => true
    | Constant(_) => false
    | Bool => false
    | Int8 => false
    | Uint8 => false
    | Int16 => false
    | Uint16 => false
    | Int31 => false
    | Int32 => false
    | Int64 => false
    | N => false
    | Z => false
    | RangedInt(_) => false
    | RangedFloat(_) => false
    | Float => false
    | Bytes(_) => false
    | String(_) => false
    | [@implicit_arity] Padded(e, _) => is_nullable(e)
    | String_enum(_) => false
    | Array(_) => false
    | List(_) => false
    | Obj(_) => false
    | Objs(_) => false
    | Tup(_) => false
    | Tups(_) => false
    | Union({cases, _}) =>
      List.exists(
        fun
        | Case({encoding: e, _}) => is_nullable(e)
        | Lazy_case({encoding: e, _}) => is_nullable(Lazy.force(e)),
        cases,
      )
    | Mu({fix, _}) => is_nullable(fix(e))
    | Conv({encoding: e, _}) => is_nullable(e)
    | Describe({encoding: e, _}) => is_nullable(e)
    | Splitted({json_encoding, _}) =>
      Json_encoding.is_nullable(json_encoding)
    | Dynamic_size({encoding: e, _}) => is_nullable(e)
    | Check_size({encoding: e, _}) => is_nullable(e)
    | Delayed(_) => true
    };

let option = ty => {
  if (is_nullable(ty)) {
    invalid_arg("Data_encoding.option: cannot nest nullable encodings");
  };
  /* TODO add a special construct `Option` in the GADT */
  union(
    ~tag_size=`Uint8,
    [
      case(Tag(1), ty, ~title="Some", x => x, x => Some(x)),
      case(
        Tag(0),
        null,
        ~title="None",
        fun
        | None => Some()
        | Some(_) => None,
        () =>
        None
      ),
    ],
  );
};

let mu = (name, ~title=?, ~description=?, fix) => {
  let kind =
    try({
      let precursor =
        make @@ Mu({kind: `Dynamic, name, title, description, fix});

      switch (classify @@ fix(precursor)) {
      | `Fixed(_)
      | `Dynamic => `Dynamic
      | `Variable => raise(Exit)
      };
    }) {
    | Exit
    | _ /* TODO variability error */ =>
      let precursor =
        make @@ Mu({kind: `Variable, name, title, description, fix});

      ignore(classify @@ fix(precursor));
      `Variable;
    };

  make @@ Mu({kind, name, title, description, fix});
};

let result = (ok_enc, error_enc) =>
  union(
    ~tag_size=`Uint8,
    [
      case(
        Tag(1),
        ok_enc,
        ~title="Ok",
        fun
        | Ok(x) => Some(x)
        | Error(_) => None,
        x =>
        Ok(x)
      ),
      case(
        Tag(0),
        error_enc,
        ~title="Result",
        fun
        | Ok(_) => None
        | Error(x) => Some(x),
        x =>
        Error(x)
      ),
    ],
  );
