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

let n_length = value => {
  let bits = Z.numbits(value);
  if (bits == 0) {
    1;
  } else {
    (bits + 6) / 7;
  };
};

let z_length = value => (Z.numbits(value) + 1 + 6) / 7;

let rec length: type x. (Encoding.t(x), x) => int =
  (e, value) =>
    Encoding.(
      switch (e.encoding) {
      /* Fixed */
      | Null => 0
      | Empty => 0
      | Constant(_) => 0
      | Bool => Binary_size.bool
      | Int8 => Binary_size.int8
      | Uint8 => Binary_size.uint8
      | Int16 => Binary_size.int16
      | Uint16 => Binary_size.uint16
      | Int31 => Binary_size.int31
      | Int32 => Binary_size.int32
      | Int64 => Binary_size.int64
      | N => n_length(value)
      | Z => z_length(value)
      | RangedInt({minimum, maximum}) =>
        Binary_size.integer_to_size @@
        Binary_size.range_to_size(~minimum, ~maximum)
      | Float => Binary_size.float
      | RangedFloat(_) => Binary_size.float
      | Bytes(`Fixed(n)) => n
      | String(`Fixed(n)) => n
      | [@implicit_arity] Padded(e, n) => length(e, value) + n
      | [@implicit_arity] String_enum(_, arr) =>
        Binary_size.integer_to_size @@ Binary_size.enum_size(arr)
      | Objs({kind: `Fixed(n), _}) => n
      | Tups({kind: `Fixed(n), _}) => n
      | Union({kind: `Fixed(n), _}) => n
      /* Dynamic */
      | Objs({kind: `Dynamic, left, right}) =>
        let (v1, v2) = value;
        length(left, v1) + length(right, v2);
      | Tups({kind: `Dynamic, left, right}) =>
        let (v1, v2) = value;
        length(left, v1) + length(right, v2);
      | Union({kind: `Dynamic, tag_size, cases}) =>
        let rec length_case = (
          fun
          | [] => raise(Write_error(No_case_matched))
          | [Case({tag: Json_only, _}), ...tl]
          | [Lazy_case({tag: Json_only, _}), ...tl] => length_case(tl)
          | [Case({encoding: e, proj, _}), ...tl] =>
            switch (proj(value)) {
            | None => length_case(tl)
            | Some(value) =>
              Binary_size.tag_size(tag_size) + length(e, value)
            }
          | [Lazy_case({encoding: e, proj, _}), ...tl] => {
              let e = Lazy.force(e);
              switch (proj(value)) {
              | None => length_case(tl)
              | Some(value) =>
                Binary_size.tag_size(tag_size) + length(e, value)
              };
            }
        );

        length_case(cases);
      | Mu({kind: `Dynamic, fix, _}) => length(fix(e), value)
      | Obj(Opt({kind: `Dynamic, encoding: e, _})) =>
        switch (value) {
        | None => 1
        | Some(value) => 1 + length(e, value)
        }
      /* Variable */
      | Ignore => 0
      | Bytes(`Variable) => Bytes.length(value)
      | String(`Variable) => String.length(value)
      | [@implicit_arity] Array(Some(max_length), _e)
          when Array.length(value) > max_length =>
        raise(Write_error(Array_too_long))
      | [@implicit_arity] Array(_, e) =>
        Array.fold_left((acc, v) => length(e, v) + acc, 0, value)
      | [@implicit_arity] List(Some(max_length), _e)
          when List.length(value) > max_length =>
        raise(Write_error(List_too_long))
      | [@implicit_arity] List(_, e) =>
        List.fold_left((acc, v) => length(e, v) + acc, 0, value)
      | Objs({kind: `Variable, left, right}) =>
        let (v1, v2) = value;
        length(left, v1) + length(right, v2);
      | Tups({kind: `Variable, left, right}) =>
        let (v1, v2) = value;
        length(left, v1) + length(right, v2);
      | Obj(Opt({kind: `Variable, encoding: e, _})) =>
        switch (value) {
        | None => 0
        | Some(value) => length(e, value)
        }
      | Union({kind: `Variable, tag_size, cases}) =>
        let rec length_case = (
          fun
          | [] => raise(Write_error(No_case_matched))
          | [Case({tag: Json_only, _}), ...tl]
          | [Lazy_case({tag: Json_only, _}), ...tl] => length_case(tl)
          | [Case({encoding: e, proj, _}), ...tl] =>
            switch (proj(value)) {
            | None => length_case(tl)
            | Some(value) =>
              Binary_size.tag_size(tag_size) + length(e, value)
            }
          | [Lazy_case({encoding: e, proj, _}), ...tl] => {
              let e = Lazy.force(e);
              switch (proj(value)) {
              | None => length_case(tl)
              | Some(value) =>
                Binary_size.tag_size(tag_size) + length(e, value)
              };
            }
        );

        length_case(cases);
      | Mu({kind: `Variable, fix, _}) => length(fix(e), value)
      /* Recursive*/
      | Obj(Req({encoding: e, _})) => length(e, value)
      | Obj(Dft({encoding: e, _})) => length(e, value)
      | Tup(e) => length(e, value)
      | Conv({encoding: e, proj, _}) => length(e, proj(value))
      | Describe({encoding: e, _}) => length(e, value)
      | Splitted({encoding: e, _}) => length(e, value)
      | Dynamic_size({kind, encoding: e}) =>
        let length = length(e, value);
        Binary_size.integer_to_size(kind) + length;
      | Check_size({limit, encoding: e}) =>
        let length = length(e, value);
        if (length > limit) {
          raise(Write_error(Size_limit_exceeded));
        };
        length;
      | Delayed(f) => length(f(), value)
      }
    );

let fixed_length = e =>
  switch (Encoding.classify(e)) {
  | `Fixed(n) => Some(n)
  | `Dynamic
  | `Variable => None
  };

let fixed_length_exn = e =>
  switch (fixed_length(e)) {
  | Some(n) => n
  | None => invalid_arg("Data_encoding.Binary.fixed_length_exn")
  };
