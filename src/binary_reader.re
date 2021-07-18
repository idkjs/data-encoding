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

open Binary_error;

let raise = e => raise(Read_error(e));

type state = {
  buffer: Bytes.t,
  mutable offset: int,
  mutable remaining_bytes: int,
  mutable allowed_bytes: option(int),
};

let check_allowed_bytes = (state, size) =>
  switch (state.allowed_bytes) {
  | Some(len) when len < size => raise(Size_limit_exceeded)
  | Some(len) => Some(len - size)
  | None => None
  };

let check_remaining_bytes = (state, size) => {
  if (state.remaining_bytes < size) {
    raise(Not_enough_data);
  };
  state.remaining_bytes - size;
};

let read_atom = (size, conv, state) => {
  let offset = state.offset;
  state.remaining_bytes = check_remaining_bytes(state, size);
  state.allowed_bytes = check_allowed_bytes(state, size);
  state.offset = state.offset + size;
  conv(state.buffer, offset);
};

/** Reader for all the atomic types. */
module Atom = {
  let uint8 = read_atom(Binary_size.uint8, TzEndian.get_uint8);

  let uint16 = read_atom(Binary_size.int16, TzEndian.get_uint16);

  let int8 = read_atom(Binary_size.int8, TzEndian.get_int8);

  let int16 = read_atom(Binary_size.int16, TzEndian.get_int16);

  let int32 = read_atom(Binary_size.int32, TzEndian.get_int32);

  let int64 = read_atom(Binary_size.int64, TzEndian.get_int64);

  let float = read_atom(Binary_size.float, TzEndian.get_double);

  let bool = state => int8(state) != 0;

  let uint30 =
    read_atom(Binary_size.uint30) @@
    (
      (buffer, ofs) => {
        let v = Int32.to_int(TzEndian.get_int32(buffer, ofs));
        if (v < 0) {
          raise(Invalid_int({min: 0, v, max: 1 lsl 30 - 1}));
        };
        v;
      }
    );

  let int31 =
    read_atom(Binary_size.int31) @@
    ((buffer, ofs) => Int32.to_int(TzEndian.get_int32(buffer, ofs)));

  let int =
    fun
    | `Int31 => int31
    | `Int16 => int16
    | `Int8 => int8
    | `Uint30 => uint30
    | `Uint16 => uint16
    | `Uint8 => uint8;

  let ranged_int = (~minimum, ~maximum, state) => {
    let read_int =
      switch (Binary_size.range_to_size(~minimum, ~maximum)) {
      | `Int8 => int8
      | `Int16 => int16
      | `Int31 => int31
      | `Uint8 => uint8
      | `Uint16 => uint16
      | `Uint30 => uint30
      };

    let ranged = read_int(state);
    let ranged =
      if (minimum > 0) {
        ranged + minimum;
      } else {
        ranged;
      };
    if (!(minimum <= ranged && ranged <= maximum)) {
      raise(Invalid_int({min: minimum, v: ranged, max: maximum}));
    };
    ranged;
  };

  let ranged_float = (~minimum, ~maximum, state) => {
    let ranged = float(state);
    if (!(minimum <= ranged && ranged <= maximum)) {
      raise(Invalid_float({min: minimum, v: ranged, max: maximum}));
    };
    ranged;
  };

  let rec read_z = (res, value, bit_in_value, state) => {
    let byte = uint8(state);
    let value = value lor (byte land 0x7F) lsl bit_in_value;
    let bit_in_value = bit_in_value + 7;
    let (bit_in_value, value) =
      if (bit_in_value < 8) {
        (bit_in_value, value);
      } else {
        Buffer.add_char(res, Char.unsafe_chr(value land 0xFF));
        (bit_in_value - 8, value lsr 8);
      };

    if (byte land 0x80 == 0x80) {
      read_z(res, value, bit_in_value, state);
    } else {
      if (bit_in_value > 0) {
        Buffer.add_char(res, Char.unsafe_chr(value));
      };
      if (byte == 0x00) {
        raise(Trailing_zero);
      };
      Z.of_bits(Buffer.contents(res));
    };
  };

  let n = state => {
    let first = uint8(state);
    let first_value = first land 0x7F;
    if (first land 0x80 == 0x80) {
      read_z(Buffer.create(100), first_value, 7, state);
    } else {
      Z.of_int(first_value);
    };
  };

  let z = state => {
    let first = uint8(state);
    let first_value = first land 0x3F;
    let sign = first land 0x40 != 0;
    if (first land 0x80 == 0x80) {
      let n = read_z(Buffer.create(100), first_value, 6, state);
      if (sign) {
        Z.neg(n);
      } else {
        n;
      };
    } else {
      let n = Z.of_int(first_value);
      if (sign) {
        Z.neg(n);
      } else {
        n;
      };
    };
  };

  let string_enum = (arr, state) => {
    let read_index =
      switch (Binary_size.enum_size(arr)) {
      | `Uint8 => uint8
      | `Uint16 => uint16
      | `Uint30 => uint30
      };

    let index = read_index(state);
    if (index >= Array.length(arr)) {
      raise(No_case_matched);
    };
    arr[index];
  };

  let fixed_length_bytes = length =>
    read_atom(length) @@ ((buf, ofs) => Bytes.sub(buf, ofs, length));

  let fixed_length_string = length =>
    read_atom(length) @@ ((buf, ofs) => Bytes.sub_string(buf, ofs, length));

  let tag =
    fun
    | `Uint8 => uint8
    | `Uint16 => uint16;
};

/** Main recursive reading function, in continuation passing style. */

let rec read_rec: type ret. (Encoding.t(ret), state) => ret =
  (e, state) =>
    Encoding.(
      switch (e.encoding) {
      | Null => ()
      | Empty => ()
      | Constant(_) => ()
      | Ignore => ()
      | Bool => Atom.bool(state)
      | Int8 => Atom.int8(state)
      | Uint8 => Atom.uint8(state)
      | Int16 => Atom.int16(state)
      | Uint16 => Atom.uint16(state)
      | Int31 => Atom.int31(state)
      | Int32 => Atom.int32(state)
      | Int64 => Atom.int64(state)
      | N => Atom.n(state)
      | Z => Atom.z(state)
      | Float => Atom.float(state)
      | Bytes(`Fixed(n)) => Atom.fixed_length_bytes(n, state)
      | Bytes(`Variable) =>
        Atom.fixed_length_bytes(state.remaining_bytes, state)
      | String(`Fixed(n)) => Atom.fixed_length_string(n, state)
      | String(`Variable) =>
        Atom.fixed_length_string(state.remaining_bytes, state)
      | [@implicit_arity] Padded(e, n) =>
        let v = read_rec(e, state);
        ignore(Atom.fixed_length_string(n, state): string);
        v;
      | RangedInt({minimum, maximum}) =>
        Atom.ranged_int(~minimum, ~maximum, state)
      | RangedFloat({minimum, maximum}) =>
        Atom.ranged_float(~minimum, ~maximum, state)
      | [@implicit_arity] String_enum(_, arr) => Atom.string_enum(arr, state)
      | [@implicit_arity] Array(max_length, e) =>
        let max_length =
          switch (max_length) {
          | Some(l) => l
          | None => max_int
          };
        let l = read_list(Array_too_long, max_length, e, state);
        Array.of_list(l);
      | [@implicit_arity] List(max_length, e) =>
        let max_length =
          switch (max_length) {
          | Some(l) => l
          | None => max_int
          };
        read_list(List_too_long, max_length, e, state);
      | Obj(Req({encoding: e, _})) => read_rec(e, state)
      | Obj(Dft({encoding: e, _})) => read_rec(e, state)
      | Obj(Opt({kind: `Dynamic, encoding: e, _})) =>
        let present = Atom.bool(state);
        if (!present) {
          None;
        } else {
          Some(read_rec(e, state));
        };
      | Obj(Opt({kind: `Variable, encoding: e, _})) =>
        if (state.remaining_bytes == 0) {
          None;
        } else {
          Some(read_rec(e, state));
        }
      | Objs({kind: `Fixed(sz), left, right}) =>
        ignore(check_remaining_bytes(state, sz): int);
        ignore(check_allowed_bytes(state, sz): option(int));
        let left = read_rec(left, state);
        let right = read_rec(right, state);
        (left, right);
      | Objs({kind: `Dynamic, left, right}) =>
        let left = read_rec(left, state);
        let right = read_rec(right, state);
        (left, right);
      | Objs({kind: `Variable, left, right}) =>
        read_variable_pair(left, right, state)
      | Tup(e) => read_rec(e, state)
      | Tups({kind: `Fixed(sz), left, right}) =>
        ignore(check_remaining_bytes(state, sz): int);
        ignore(check_allowed_bytes(state, sz): option(int));
        let left = read_rec(left, state);
        let right = read_rec(right, state);
        (left, right);
      | Tups({kind: `Dynamic, left, right}) =>
        let left = read_rec(left, state);
        let right = read_rec(right, state);
        (left, right);
      | Tups({kind: `Variable, left, right}) =>
        read_variable_pair(left, right, state)
      | Conv({inj, encoding, _}) => inj(read_rec(encoding, state))
      | Union({tag_size, cases, _}) =>
        let ctag = Atom.tag(tag_size, state);
        let case =
          try(
            List.find(
              fun
              | Case({tag: Tag(tag), _})
              | Lazy_case({tag: Tag(tag), _}) => tag == ctag
              | Case({tag: Json_only, _})
              | Lazy_case({tag: Json_only, _}) => false,
              cases,
            )
          ) {
          | Not_found => raise(Unexpected_tag(ctag))
          };

        switch (case) {
        | Case({encoding, inj, _}) => inj(read_rec(encoding, state))
        | Lazy_case({encoding, inj, _}) =>
          inj(read_rec(Lazy.force(encoding), state))
        };
      | Dynamic_size({kind, encoding: e}) =>
        let sz = Atom.int(kind, state);
        let remaining = check_remaining_bytes(state, sz);
        state.remaining_bytes = sz;
        ignore(check_allowed_bytes(state, sz): option(int));
        let v = read_rec(e, state);
        if (state.remaining_bytes != 0) {
          raise(Extra_bytes);
        };
        state.remaining_bytes = remaining;
        v;
      | Check_size({limit, encoding: e}) =>
        let old_allowed_bytes = state.allowed_bytes;
        let limit =
          switch (state.allowed_bytes) {
          | None => limit
          | Some(current_limit) => min(current_limit, limit)
          };

        state.allowed_bytes = Some(limit);
        let v = read_rec(e, state);
        let allowed_bytes =
          switch (old_allowed_bytes) {
          | None => None
          | Some(old_limit) =>
            let remaining =
              switch (state.allowed_bytes) {
              | None => assert(false)
              | Some(remaining) => remaining
              };

            let read = limit - remaining;
            Some(old_limit - read);
          };

        state.allowed_bytes = allowed_bytes;
        v;
      | Describe({encoding: e, _}) => read_rec(e, state)
      | Splitted({encoding: e, _}) => read_rec(e, state)
      | Mu({fix, _}) => read_rec(fix(e), state)
      | Delayed(f) => read_rec(f(), state)
      }
    )

and read_variable_pair:
  type left right.
    (Encoding.t(left), Encoding.t(right), state) => (left, right) =
  (e1, e2, state) =>
    switch (Encoding.classify(e1), Encoding.classify(e2)) {
    | (`Dynamic | `Fixed(_), `Variable) =>
      let left = read_rec(e1, state);
      let right = read_rec(e2, state);
      (left, right);
    | (`Variable, `Fixed(n)) =>
      if (n > state.remaining_bytes) {
        raise(Not_enough_data);
      };
      state.remaining_bytes = state.remaining_bytes - n;
      let left = read_rec(e1, state);
      assert(state.remaining_bytes == 0);
      state.remaining_bytes = n;
      let right = read_rec(e2, state);
      assert(state.remaining_bytes == 0);
      (left, right);
    | _ => assert(false)
    }

/* Should be rejected by [Encoding.Kind.combine] */
and read_list: type a. (read_error, int, Encoding.t(a), state) => list(a) =
  (error, max_length, e, state) => {
    let rec loop = (max_length, acc) =>
      if (state.remaining_bytes == 0) {
        List.rev(acc);
      } else if (max_length == 0) {
        raise(error);
      } else {
        let v = read_rec(e, state);
        loop(max_length - 1, [v, ...acc]);
      };

    loop(max_length, []);
  };

/** ******************** */;

/** Various entry points */;

let read_exn = (encoding, buffer, ofs, len) => {
  let state = {
    buffer,
    offset: ofs,
    remaining_bytes: len,
    allowed_bytes: None,
  };

  let v = read_rec(encoding, state);
  (state.offset, v);
};

let read = (encoding, buffer, ofs, len) =>
  try(Ok(read_exn(encoding, buffer, ofs, len))) {
  | Read_error(err) => Error(err)
  };

let read_opt = (encoding, buffer, ofs, len) =>
  try(Some(read_exn(encoding, buffer, ofs, len))) {
  | Read_error(_) => None
  };

let of_bytes_exn = (encoding, buffer) => {
  let len = Bytes.length(buffer);
  let state = {buffer, offset: 0, remaining_bytes: len, allowed_bytes: None};

  let v = read_rec(encoding, state);
  if (state.offset != len) {
    raise(Extra_bytes);
  };
  v;
};

let of_bytes = (encoding, buffer) =>
  try(Ok(of_bytes_exn(encoding, buffer))) {
  | Read_error(err) => Error(err)
  };

let of_bytes_opt = (encoding, buffer) =>
  try(Some(of_bytes_exn(encoding, buffer))) {
  | Read_error(_) => None
  };
