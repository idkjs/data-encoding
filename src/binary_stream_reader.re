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

/** Persistent state of the binary reader. */

type state = {
  /** All the remaining data to be read. */
  stream: Binary_stream.t,
  /** Total number of bytes that should be from 'stream' (None =
      unlimited). Reading less bytes should raise [Extra_bytes] and
      trying to read more bytes should raise [Not_enough_data]. */
  remaining_bytes: option(int),
  /** Maximum number of bytes that are allowed to be read from 'stream'
      before to fail (None = unlimited). */
  allowed_bytes: option(int),
  /** Total number of bytes that has been read from [stream] since the
      beginning. */
  total_read: int,
};

/** Return type for the function [read_rec]. See [Data_encoding] for its
    description. */

type status('ret) =
  | Success({
      result: 'ret,
      size: int,
      stream: Binary_stream.t,
    })
  | Await(Bytes.t => status('ret))
  | Error(read_error);

let check_remaining_bytes = (state, size) =>
  switch (state.remaining_bytes) {
  | Some(len) when len < size => raise(Not_enough_data)
  | Some(len) => Some(len - size)
  | None => None
  };

let check_allowed_bytes = (state, size) =>
  switch (state.allowed_bytes) {
  | Some(len) when len < size => raise(Size_limit_exceeded)
  | Some(len) => Some(len - size)
  | None => None
  };

/** [read_atom resume size conv state k] reads [size] bytes from [state],
    pass it to [conv] to be decoded, and finally call the continuation [k]
    with the decoded value and the updated state.

    The function [conv] is also allowed to raise [Read_error err].
    In that case the exception is caught and [Error err] is returned.

    If there is not enough [remaining_bytes] to be read in [state], the
    function returns [Error Not_enough_data] instead of calling
    the continuation.

    If there is not enough [allowed_bytes] to be read in [state], the
    function returns [Error Size_limit_exceeded] instead of calling
    the continuation.

    If there is not enough bytes to be read in [state], the function
    returns [Await resume] instead of calling the continuation. */

let read_atom = (resume, size, conv, state, k) =>
  switch (
    {
      let remaining_bytes = check_remaining_bytes(state, size);
      let allowed_bytes = check_allowed_bytes(state, size);
      let (res, stream) = Binary_stream.read(state.stream, size);
      (
        conv(res.buffer, res.ofs),
        {
          remaining_bytes,
          allowed_bytes,
          stream,
          total_read: state.total_read + size,
        },
      );
    }
  ) {
  | exception (Read_error(error)) => Error(error)
  | exception Binary_stream.Need_more_data => Await(resume)
  | v => k(v)
  };

/* tail call */

/** Reader for all the atomic types. */
module Atom = {
  let uint8 = r => read_atom(r, Binary_size.uint8, TzEndian.get_uint8);

  let uint16 = r => read_atom(r, Binary_size.int16, TzEndian.get_uint16);

  let int8 = r => read_atom(r, Binary_size.int8, TzEndian.get_int8);

  let int16 = r => read_atom(r, Binary_size.int16, TzEndian.get_int16);

  let int32 = r => read_atom(r, Binary_size.int32, TzEndian.get_int32);

  let int64 = r => read_atom(r, Binary_size.int64, TzEndian.get_int64);

  let float = r => read_atom(r, Binary_size.float, TzEndian.get_double);

  let bool = (resume, state, k) =>
    int8(resume, state) @@ (((v, state)) => k((v != 0, state)));

  let uint30 = r =>
    read_atom(r, Binary_size.uint30) @@
    (
      (buffer, ofs) => {
        let v = Int32.to_int(TzEndian.get_int32(buffer, ofs));
        if (v < 0) {
          raise(Invalid_int({min: 0, v, max: 1 lsl 30 - 1}));
        };
        v;
      }
    );

  let int31 = r =>
    read_atom(r, Binary_size.int31) @@
    ((buffer, ofs) => Int32.to_int(TzEndian.get_int32(buffer, ofs)));

  let int =
    fun
    | `Int31 => int31
    | `Int16 => int16
    | `Int8 => int8
    | `Uint30 => uint30
    | `Uint16 => uint16
    | `Uint8 => uint8;

  let ranged_int = (~minimum, ~maximum, resume, state, k) => {
    let read_int =
      switch (Binary_size.range_to_size(~minimum, ~maximum)) {
      | `Int8 => int8
      | `Int16 => int16
      | `Int31 => int31
      | `Uint8 => uint8
      | `Uint16 => uint16
      | `Uint30 => uint30
      };

    read_int(resume, state) @@
    (
      ((ranged, state)) => {
        let ranged =
          if (minimum > 0) {
            ranged + minimum;
          } else {
            ranged;
          };
        if (!(minimum <= ranged && ranged <= maximum)) {
          Error(Invalid_int({min: minimum, v: ranged, max: maximum}));
        } else {
          k((ranged, state));
        };
      }
    );
  };

  let ranged_float = (~minimum, ~maximum, resume, state, k) =>
    float(resume, state) @@
    (
      ((ranged, state)) =>
        if (!(minimum <= ranged && ranged <= maximum)) {
          Error(Invalid_float({min: minimum, v: ranged, max: maximum}));
        } else {
          k((ranged, state));
        }
    );

  let rec read_z = (res, value, bit_in_value, state, k) => {
    let resume = buffer => {
      let stream = Binary_stream.push(buffer, state.stream);
      read_z(res, value, bit_in_value, {...state, stream}, k);
    };

    uint8(resume, state) @@
    (
      ((byte, state)) => {
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
          read_z(res, value, bit_in_value, state, k);
        } else {
          if (bit_in_value > 0) {
            Buffer.add_char(res, Char.unsafe_chr(value));
          };
          if (byte == 0x00) {
            raise(Trailing_zero);
          };
          k((Z.of_bits(Buffer.contents(res)), state));
        };
      }
    );
  };

  let n = (resume, state, k) =>
    uint8(resume, state) @@
    (
      ((first, state)) => {
        let first_value = first land 0x7F;
        if (first land 0x80 == 0x80) {
          read_z(Buffer.create(100), first_value, 7, state, k);
        } else {
          k((Z.of_int(first_value), state));
        };
      }
    );

  let z = (resume, state, k) =>
    uint8(resume, state) @@
    (
      ((first, state)) => {
        let first_value = first land 0x3F;
        let sign = first land 0x40 != 0;
        if (first land 0x80 == 0x80) {
          read_z(Buffer.create(100), first_value, 6, state) @@
          (
            ((n, state)) =>
              k((
                if (sign) {
                  Z.neg(n);
                } else {
                  n;
                },
                state,
              ))
          );
        } else {
          let n = Z.of_int(first_value);
          k((
            if (sign) {
              Z.neg(n);
            } else {
              n;
            },
            state,
          ));
        };
      }
    );

  let string_enum = (arr, resume, state, k) => {
    let read_index =
      switch (Binary_size.enum_size(arr)) {
      | `Uint8 => uint8
      | `Uint16 => uint16
      | `Uint30 => uint30
      };

    read_index(resume, state) @@
    (
      ((index, state)) =>
        if (index >= Array.length(arr)) {
          Error(No_case_matched);
        } else {
          k((arr[index], state));
        }
    );
  };

  let fixed_length_bytes = (length, r) =>
    read_atom(r, length) @@ ((buf, ofs) => Bytes.sub(buf, ofs, length));

  let fixed_length_string = (length, r) =>
    read_atom(r, length) @@
    ((buf, ofs) => Bytes.sub_string(buf, ofs, length));

  let tag =
    fun
    | `Uint8 => uint8
    | `Uint16 => uint16;
};

let rec skip = (n, state, k) => {
  let resume = buffer => {
    let stream = Binary_stream.push(buffer, state.stream);
    try(skip(n, {...state, stream}, k)) {
    | Read_error(err) => Error(err)
    };
  };

  Atom.fixed_length_string(n, resume, state) @@
  (((_, state): (string, _)) => k(state));
};

/** Main recursive reading function, in continuation passing style. */

let rec read_rec:
  type next ret.
    (bool, Encoding.t(next), state, ((next, state)) => status(ret)) =>
    status(ret) =
  (whole, e, state, k) => {
    let resume = buffer => {
      let stream = Binary_stream.push(buffer, state.stream);
      try(read_rec(whole, e, {...state, stream}, k)) {
      | Read_error(err) => Error(err)
      };
    };

    open Encoding;
    assert(
      Encoding.classify(e) != `Variable || state.remaining_bytes != None,
    );
    switch (e.encoding) {
    | Null => k(((), state))
    | Empty => k(((), state))
    | Constant(_) => k(((), state))
    | Ignore => k(((), state))
    | Bool => Atom.bool(resume, state, k)
    | Int8 => Atom.int8(resume, state, k)
    | Uint8 => Atom.uint8(resume, state, k)
    | Int16 => Atom.int16(resume, state, k)
    | Uint16 => Atom.uint16(resume, state, k)
    | Int31 => Atom.int31(resume, state, k)
    | Int32 => Atom.int32(resume, state, k)
    | Int64 => Atom.int64(resume, state, k)
    | N => Atom.n(resume, state, k)
    | Z => Atom.z(resume, state, k)
    | Float => Atom.float(resume, state, k)
    | Bytes(`Fixed(n)) => Atom.fixed_length_bytes(n, resume, state, k)
    | Bytes(`Variable) =>
      let size = remaining_bytes(state);
      Atom.fixed_length_bytes(size, resume, state, k);
    | String(`Fixed(n)) => Atom.fixed_length_string(n, resume, state, k)
    | String(`Variable) =>
      let size = remaining_bytes(state);
      Atom.fixed_length_string(size, resume, state, k);
    | [@implicit_arity] Padded(e, n) =>
      read_rec(false, e, state) @@
      (((v, state)) => skip(n, state) @@ (state => k((v, state))))
    | RangedInt({minimum, maximum}) =>
      Atom.ranged_int(~minimum, ~maximum, resume, state, k)
    | RangedFloat({minimum, maximum}) =>
      Atom.ranged_float(~minimum, ~maximum, resume, state, k)
    | [@implicit_arity] String_enum(_, arr) =>
      Atom.string_enum(arr, resume, state, k)
    | [@implicit_arity] Array(max_length, e) =>
      let max_length =
        switch (max_length) {
        | Some(l) => l
        | None => max_int
        };
      read_list(Array_too_long, max_length, e, state) @@
      (((l, state)) => k((Array.of_list(l), state)));
    | [@implicit_arity] List(max_length, e) =>
      let max_length =
        switch (max_length) {
        | Some(l) => l
        | None => max_int
        };
      read_list(List_too_long, max_length, e, state, k);
    | Obj(Req({encoding: e, _})) => read_rec(whole, e, state, k)
    | Obj(Dft({encoding: e, _})) => read_rec(whole, e, state, k)
    | Obj(Opt({kind: `Dynamic, encoding: e, _})) =>
      Atom.bool(resume, state) @@
      (
        ((present, state)) =>
          if (!present) {
            k((None, state));
          } else {
            read_rec(whole, e, state) @@
            (((v, state)) => k((Some(v), state)));
          }
      )
    | Obj(Opt({kind: `Variable, encoding: e, _})) =>
      let size = remaining_bytes(state);
      if (size == 0) {
        k((None, state));
      } else {
        read_rec(whole, e, state) @@ (((v, state)) => k((Some(v), state)));
      };
    | Objs({kind: `Fixed(sz), left, right}) =>
      ignore(check_remaining_bytes(state, sz): option(int));
      ignore(check_allowed_bytes(state, sz): option(int));
      read_rec(false, left, state) @@
      (
        ((left, state)) =>
          read_rec(whole, right, state) @@
          (((right, state)) => k(((left, right), state)))
      );
    | Objs({kind: `Dynamic, left, right}) =>
      read_rec(false, left, state) @@
      (
        ((left, state)) =>
          read_rec(whole, right, state) @@
          (((right, state)) => k(((left, right), state)))
      )
    | Objs({kind: `Variable, left, right}) =>
      read_variable_pair(left, right, state, k)
    | Tup(e) => read_rec(whole, e, state, k)
    | Tups({kind: `Fixed(sz), left, right}) =>
      ignore(check_remaining_bytes(state, sz): option(int));
      ignore(check_allowed_bytes(state, sz): option(int));
      read_rec(false, left, state) @@
      (
        ((left, state)) =>
          read_rec(whole, right, state) @@
          (((right, state)) => k(((left, right), state)))
      );
    | Tups({kind: `Dynamic, left, right}) =>
      read_rec(false, left, state) @@
      (
        ((left, state)) =>
          read_rec(whole, right, state) @@
          (((right, state)) => k(((left, right), state)))
      )
    | Tups({kind: `Variable, left, right}) =>
      read_variable_pair(left, right, state, k)
    | Conv({inj, encoding, _}) =>
      read_rec(whole, encoding, state) @@
      (((v, state)) => k((inj(v), state)))
    | Union({tag_size, cases, _}) =>
      Atom.tag(tag_size, resume, state) @@
      (
        ((ctag, state)) =>
          switch (
            List.find_opt(
              fun
              | Case({tag: Tag(tag), _})
              | Lazy_case({tag: Tag(tag), _}) => tag == ctag
              | Case({tag: Json_only, _})
              | Lazy_case({tag: Json_only, _}) => false,
              cases,
            )
          ) {
          | None => Error(Unexpected_tag(ctag))
          | Some(Case({encoding, inj, _})) =>
            read_rec(whole, encoding, state) @@
            (((v, state)) => k((inj(v), state)))
          | Some(Lazy_case({encoding, inj, _})) =>
            read_rec(whole, Lazy.force(encoding), state) @@
            (((v, state)) => k((inj(v), state)))
          }
      )
    | Dynamic_size({kind, encoding: e}) =>
      Atom.int(kind, resume, state) @@
      (
        ((sz, state)) => {
          let remaining = check_remaining_bytes(state, sz);
          let state = {...state, remaining_bytes: Some(sz)};
          ignore(check_allowed_bytes(state, sz): option(int));
          read_rec(true, e, state) @@
          (
            ((v, state)) =>
              if (state.remaining_bytes != Some(0)) {
                Error(Extra_bytes);
              } else {
                k((v, {...state, remaining_bytes: remaining}));
              }
          );
        }
      )
    | Check_size({limit, encoding: e}) =>
      let old_allowed_bytes = state.allowed_bytes;
      let limit =
        switch (state.allowed_bytes) {
        | None => limit
        | Some(current_limit) => min(current_limit, limit)
        };

      switch (state.remaining_bytes) {
      | Some(remaining) when whole && limit < remaining =>
        raise(Size_limit_exceeded)
      | _ => ()
      };
      let state = {...state, allowed_bytes: Some(limit)};
      read_rec(whole, e, state) @@
      (
        ((v, state)) => {
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

          k((v, {...state, allowed_bytes}));
        }
      );
    | Describe({encoding: e, _}) => read_rec(whole, e, state, k)
    | Splitted({encoding: e, _}) => read_rec(whole, e, state, k)
    | Mu({fix, _}) => read_rec(whole, fix(e), state, k)
    | Delayed(f) => read_rec(whole, f(), state, k)
    };
  }

and remaining_bytes = ({remaining_bytes, _}) =>
  switch (remaining_bytes) {
  | None =>
    /* This function should only be called with a variable encoding,
       for which the `remaining_bytes` should never be `None`. */
    assert(false)
  | Some(len) => len
  }

and read_variable_pair:
  type left right ret.
    (
      Encoding.t(left),
      Encoding.t(right),
      state,
      (((left, right), state)) => status(ret)
    ) =>
    status(ret) =
  (e1, e2, state, k) => {
    let size = remaining_bytes(state);
    switch (Encoding.classify(e1), Encoding.classify(e2)) {
    | (`Dynamic | `Fixed(_), `Variable) =>
      read_rec(false, e1, state) @@
      (
        ((left, state)) =>
          read_rec(true, e2, state) @@
          (((right, state)) => k(((left, right), state)))
      )
    | (`Variable, `Fixed(n)) =>
      if (n > size) {
        Error(Not_enough_data);
      } else {
        let state = {...state, remaining_bytes: Some(size - n)};
        read_rec(true, e1, state) @@
        (
          ((left, state)) => {
            assert(state.remaining_bytes == Some(0));
            let state = {...state, remaining_bytes: Some(n)};
            read_rec(true, e2, state) @@
            (
              ((right, state)) => {
                assert(state.remaining_bytes == Some(0));
                k(((left, right), state));
              }
            );
          }
        );
      }
    | _ => assert(false)
    };
  }

/* Should be rejected by [Encoding.Kind.combine] */
and read_list:
  type a ret.
    (
      read_error,
      int,
      Encoding.t(a),
      state,
      ((list(a), state)) => status(ret)
    ) =>
    status(ret) =
  (error, max_length, e, state, k) => {
    let rec loop = (state, acc, max_length) => {
      let size = remaining_bytes(state);
      if (size == 0) {
        k((List.rev(acc), state));
      } else if (max_length == 0) {
        raise(error);
      } else {
        read_rec(false, e, state) @@
        (((v, state)) => loop(state, [v, ...acc], max_length - 1));
      };
    };

    loop(state, [], max_length);
  };

let read_rec = (e, state, k) =>
  try(read_rec(false, e, state, k)) {
  | Read_error(err) => Error(err)
  };

/** ******************** */;

/** Various entry points */;

let success = ((v, state)) =>
  Success({result: v, size: state.total_read, stream: state.stream});

let read_stream = (~init=Binary_stream.empty, encoding) =>
  switch (Encoding.classify(encoding)) {
  | `Variable =>
    invalid_arg("Data_encoding.Binary.read_stream: variable encoding")
  | `Dynamic
  | `Fixed(_) =>
    /* No hardcoded read limit in a stream. */
    let state = {
      remaining_bytes: None,
      allowed_bytes: None,
      stream: init,
      total_read: 0,
    };

    read_rec(encoding, state, success);
  };
