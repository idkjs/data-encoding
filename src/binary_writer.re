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

let raise = error => raise(Write_error(error));

/** Imperative state of the binary writer. */

type state = {
  /** The buffer where to write. */
  mutable buffer: Bytes.t,
  /** The offset of the next byte to be written in [buffer]. */
  mutable offset: int,
  /** Maximum number of bytes that are allowed to be write in [buffer]
      (after [offset]) before to fail (None = unlimited). */
  mutable allowed_bytes: option(int),
};

let check_allowed_bytes = (state, size) =>
  switch (state.allowed_bytes) {
  | Some(len) when len < size => raise(Size_limit_exceeded)
  | Some(len) => state.allowed_bytes = Some(len - size)
  | None => ()
  };

/** [may_resize state size] will first ensure there is enough
    space in [state.buffer] for writing [size] bytes (starting at
    [state.offset]).

    When the buffer does not have enough space for writing [size] bytes,
    but still has enough [allowed_bytes], it will replace the buffer
    with a buffer large enough.

    @raise [Binary_error.Write_error Size_limit_exceeded] when there is
           not enough allowed bytes to write [size] bytes. */

let may_resize = (state, size) => {
  check_allowed_bytes(state, size);
  let buffer_len = Bytes.length(state.buffer);
  if (buffer_len - state.offset < size) {
    let new_buffer = Bytes.create(max(2 * buffer_len, buffer_len + size));
    Bytes.blit(state.buffer, 0, new_buffer, 0, state.offset);
    state.buffer = new_buffer;
  };
  state.offset = state.offset + size;
};

/** Writer for all the atomic types. */
module Atom = {
  let check_int_range = (min, v, max) =>
    if (v < min || max < v) {
      raise(Invalid_int({min, v, max}));
    };

  let check_float_range = (min, v, max) =>
    if (v < min || max < v) {
      raise(Invalid_float({min, v, max}));
    };

  let set_int = (kind, buffer, ofs, v) =>
    switch (kind) {
    | `Int31
    | `Uint30 => TzEndian.set_int32(buffer, ofs, Int32.of_int(v))
    | `Int16
    | `Uint16 => TzEndian.set_int16(buffer, ofs, v)
    | `Int8
    | `Uint8 => TzEndian.set_int8(buffer, ofs, v)
    };

  let int = (kind, state, v) => {
    check_int_range(
      Binary_size.min_int(kind),
      v,
      Binary_size.max_int(kind),
    );
    let ofs = state.offset;
    may_resize(state, Binary_size.integer_to_size(kind));
    set_int(kind, state.buffer, ofs, v);
  };

  let int8 = int(`Int8);

  let uint8 = int(`Uint8);

  let int16 = int(`Int16);

  let uint16 = int(`Uint16);

  let uint30 = int(`Uint30);

  let int31 = int(`Int31);

  let bool = (state, v) => uint8(state, if (v) {255} else {0});

  let int32 = (state, v) => {
    let ofs = state.offset;
    may_resize(state, Binary_size.int32);
    TzEndian.set_int32(state.buffer, ofs, v);
  };

  let int64 = (state, v) => {
    let ofs = state.offset;
    may_resize(state, Binary_size.int64);
    TzEndian.set_int64(state.buffer, ofs, v);
  };

  let ranged_int = (~minimum, ~maximum, state, v) => {
    check_int_range(minimum, v, maximum);
    let v =
      if (minimum >= 0) {
        v - minimum;
      } else {
        v;
      };
    switch (Binary_size.range_to_size(~minimum, ~maximum)) {
    | `Uint8 => uint8(state, v)
    | `Uint16 => uint16(state, v)
    | `Uint30 => uint30(state, v)
    | `Int8 => int8(state, v)
    | `Int16 => int16(state, v)
    | `Int31 => int31(state, v)
    };
  };

  let n = (state, v) => {
    if (Z.sign(v) < 0) {
      raise(Invalid_natural);
    };
    if (Z.equal(v, Z.zero)) {
      uint8(state, 0x00);
    } else {
      let bits = Z.numbits(v);
      let get_chunk = (pos, len) => Z.to_int(Z.extract(v, pos, len));
      let length = Binary_length.n_length(v);
      let offset = state.offset;
      may_resize(state, length);
      for (i in 0 to length - 1) {
        let pos = i * 7;
        let chunk_len =
          if (i == length - 1) {
            bits - pos;
          } else {
            7;
          };
        TzEndian.set_int8(
          state.buffer,
          offset + i,
          (
            if (i == length - 1) {
              0x00;
            } else {
              0x80;
            }
          )
          lor get_chunk(pos, chunk_len),
        );
      };
    };
  };

  let z = (state, v) => {
    let sign = Z.sign(v) < 0;
    let bits = Z.numbits(v);
    if (Z.equal(v, Z.zero)) {
      uint8(state, 0x00);
    } else {
      let v = Z.abs(v);
      let get_chunk = (pos, len) => Z.to_int(Z.extract(v, pos, len));
      let length = Binary_length.z_length(v);
      let offset = state.offset;
      may_resize(state, length);
      TzEndian.set_int8(
        state.buffer,
        offset,
        (if (sign) {0x40} else {0x00})
        lor (
          if (bits > 6) {
            0x80;
          } else {
            0x00;
          }
        )
        lor get_chunk(0, 6),
      );
      for (i in 1 to length - 1) {
        let pos = 6 + (i - 1) * 7;
        let chunk_len =
          if (i == length - 1) {
            bits - pos;
          } else {
            7;
          };
        TzEndian.set_int8(
          state.buffer,
          offset + i,
          (
            if (i == length - 1) {
              0x00;
            } else {
              0x80;
            }
          )
          lor get_chunk(pos, chunk_len),
        );
      };
    };
  };

  let float = (state, v) => {
    let ofs = state.offset;
    may_resize(state, Binary_size.float);
    TzEndian.set_double(state.buffer, ofs, v);
  };

  let ranged_float = (~minimum, ~maximum, state, v) => {
    check_float_range(minimum, v, maximum);
    float(state, v);
  };

  let string_enum = (tbl, arr, state, v) => {
    let value =
      try(snd(Hashtbl.find(tbl, v))) {
      | Not_found => raise(No_case_matched)
      };

    switch (Binary_size.enum_size(arr)) {
    | `Uint30 => uint30(state, value)
    | `Uint16 => uint16(state, value)
    | `Uint8 => uint8(state, value)
    };
  };

  let fixed_kind_bytes = (length, state, s) => {
    if (Bytes.length(s) != length) {
      raise(
        Invalid_bytes_length({expected: length, found: Bytes.length(s)}),
      );
    };
    let ofs = state.offset;
    may_resize(state, length);
    Bytes.blit(s, 0, state.buffer, ofs, length);
  };

  let fixed_kind_string = (length, state, s) => {
    if (String.length(s) != length) {
      raise(
        Invalid_string_length({expected: length, found: String.length(s)}),
      );
    };
    let ofs = state.offset;
    may_resize(state, length);
    Bytes.blit_string(s, 0, state.buffer, ofs, length);
  };

  let tag =
    fun
    | `Uint8 => uint8
    | `Uint16 => uint16;
};

/** Main recursive writing function. */

let rec write_rec: type a. (Encoding.t(a), state, a) => unit =
  (e, state, value) =>
    Encoding.(
      switch (e.encoding) {
      | Null => ()
      | Empty => ()
      | Constant(_) => ()
      | Ignore => ()
      | Bool => Atom.bool(state, value)
      | Int8 => Atom.int8(state, value)
      | Uint8 => Atom.uint8(state, value)
      | Int16 => Atom.int16(state, value)
      | Uint16 => Atom.uint16(state, value)
      | Int31 => Atom.int31(state, value)
      | Int32 => Atom.int32(state, value)
      | Int64 => Atom.int64(state, value)
      | N => Atom.n(state, value)
      | Z => Atom.z(state, value)
      | Float => Atom.float(state, value)
      | Bytes(`Fixed(n)) => Atom.fixed_kind_bytes(n, state, value)
      | Bytes(`Variable) =>
        let length = Bytes.length(value);
        Atom.fixed_kind_bytes(length, state, value);
      | String(`Fixed(n)) => Atom.fixed_kind_string(n, state, value)
      | String(`Variable) =>
        let length = String.length(value);
        Atom.fixed_kind_string(length, state, value);
      | [@implicit_arity] Padded(e, n) =>
        write_rec(e, state, value);
        Atom.fixed_kind_string(n, state, String.make(n, '\000'));
      | RangedInt({minimum, maximum}) =>
        Atom.ranged_int(~minimum, ~maximum, state, value)
      | RangedFloat({minimum, maximum}) =>
        Atom.ranged_float(~minimum, ~maximum, state, value)
      | [@implicit_arity] String_enum(tbl, arr) =>
        Atom.string_enum(tbl, arr, state, value)
      | [@implicit_arity] Array(Some(max_length), _e)
          when Array.length(value) > max_length =>
        raise(Array_too_long)
      | [@implicit_arity] Array(_, e) =>
        Array.iter(write_rec(e, state), value)
      | [@implicit_arity] List(Some(max_length), _e)
          when List.length(value) > max_length =>
        raise(List_too_long)
      | [@implicit_arity] List(_, e) =>
        List.iter(write_rec(e, state), value)
      | Obj(Req({encoding: e, _})) => write_rec(e, state, value)
      | Obj(Opt({kind: `Dynamic, encoding: e, _})) =>
        switch (value) {
        | None => Atom.bool(state, false)
        | Some(value) =>
          Atom.bool(state, true);
          write_rec(e, state, value);
        }
      | Obj(Opt({kind: `Variable, encoding: e, _})) =>
        switch (value) {
        | None => ()
        | Some(value) => write_rec(e, state, value)
        }
      | Obj(Dft({encoding: e, _})) => write_rec(e, state, value)
      | Objs({left, right, _}) =>
        let (v1, v2) = value;
        write_rec(left, state, v1);
        write_rec(right, state, v2);
      | Tup(e) => write_rec(e, state, value)
      | Tups({left, right, _}) =>
        let (v1, v2) = value;
        write_rec(left, state, v1);
        write_rec(right, state, v2);
      | Conv({encoding: e, proj, _}) => write_rec(e, state, proj(value))
      | Union({tag_size, cases, _}) =>
        let rec write_case = (
          fun
          | [] => raise(No_case_matched)
          | [Case({tag: Json_only, _}), ...tl]
          | [Lazy_case({tag: Json_only, _}), ...tl] => write_case(tl)
          | [Case({encoding: e, proj, tag: Tag(tag), _}), ...tl] =>
            switch (proj(value)) {
            | None => write_case(tl)
            | Some(value) =>
              Atom.tag(tag_size, state, tag);
              write_rec(e, state, value);
            }
          | [Lazy_case({encoding: e, proj, tag: Tag(tag), _}), ...tl] => {
              let e = Lazy.force(e);
              switch (proj(value)) {
              | None => write_case(tl)
              | Some(value) =>
                Atom.tag(tag_size, state, tag);
                write_rec(e, state, value);
              };
            }
        );

        write_case(cases);
      | Dynamic_size({kind, encoding: e}) =>
        let initial_offset = state.offset;
        /* place holder for [size] */
        Atom.int(kind, state, 0);
        write_with_limit(Binary_size.max_int(kind), e, state, value);
        /* patch the written [size] */
        Atom.set_int(
          kind,
          state.buffer,
          initial_offset,
          state.offset - initial_offset - Binary_size.integer_to_size(kind),
        );
      | Check_size({limit, encoding: e}) =>
        write_with_limit(limit, e, state, value)
      | Describe({encoding: e, _}) => write_rec(e, state, value)
      | Splitted({encoding: e, _}) => write_rec(e, state, value)
      | Mu({fix, _}) => write_rec(fix(e), state, value)
      | Delayed(f) => write_rec(f(), state, value)
      }
    )

and write_with_limit: type a. (int, Encoding.t(a), state, a) => unit =
  (limit, e, state, value) => {
    /* backup the current limit */
    let old_limit = state.allowed_bytes;
    /* install the new limit (only if smaller than the current limit) */
    let limit =
      switch (state.allowed_bytes) {
      | None => limit
      | Some(old_limit) => min(old_limit, limit)
      };

    state.allowed_bytes = Some(limit);
    write_rec(e, state, value);
    /* restore the previous limit (minus the read bytes) */
    switch (old_limit) {
    | None => state.allowed_bytes = None
    | Some(old_limit) =>
      let remaining =
        switch (state.allowed_bytes) {
        | None => assert(false)
        | Some(len) => len
        };

      let read = limit - remaining;
      state.allowed_bytes = Some(old_limit - read);
    };
  };

/** ******************** */;

/** Various entry points */;

let write_exn = (e, v, buffer, offset, len) => {
  /* By hardcoding [allowed_bytes] with the buffer length,
     we ensure that [write] will never reallocate the buffer. */
  let state = {buffer, offset, allowed_bytes: Some(len)};
  write_rec(e, state, v);
  state.offset;
};

let write = (e, v, buffer, offset, len) =>
  try(Ok(write_exn(e, v, buffer, offset, len))) {
  | Write_error(err) => Error(err)
  };

let write_opt = (e, v, buffer, offset, len) =>
  try(Some(write_exn(e, v, buffer, offset, len))) {
  | Write_error(_) => None
  };

let to_bytes_exn = (~buffer_size=128, e, v) =>
  switch (Encoding.classify(e)) {
  | `Fixed(n) =>
    /* Preallocate the complete buffer */
    let state = {
      buffer: Bytes.create(n),
      offset: 0,
      allowed_bytes: Some(n),
    };

    write_rec(e, state, v);
    state.buffer;
  | `Dynamic
  | `Variable =>
    /* Preallocate a minimal buffer and let's not hardcode a
       limit to its extension. */
    let state = {
      buffer: Bytes.create(buffer_size),
      offset: 0,
      allowed_bytes: None,
    };

    write_rec(e, state, v);
    Bytes.sub(state.buffer, 0, state.offset);
  };

let to_bytes_opt = (~buffer_size=?, e, v) =>
  try(Some(to_bytes_exn(~buffer_size?, e, v))) {
  | Write_error(_) => None
  };

let to_bytes = (~buffer_size=?, e, v) =>
  try(Ok(to_bytes_exn(~buffer_size?, e, v))) {
  | Write_error(err) => Error(err)
  };
