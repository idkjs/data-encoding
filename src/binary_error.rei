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
    use the corresponding module intended for use: {!Data_encoding.Binary}. */;

type read_error =
  | Not_enough_data
  | Extra_bytes
  | No_case_matched
  | Unexpected_tag(int)
  | Invalid_size(int)
  | Invalid_int({
      min: int,
      v: int,
      max: int,
    })
  | Invalid_float({
      min: float,
      v: float,
      max: float,
    })
  | Trailing_zero
  | Size_limit_exceeded
  | List_too_long
  | Array_too_long;

exception Read_error(read_error);

let read_error_encoding: Encoding.t(read_error);

let pp_read_error: (Format.formatter, read_error) => unit;

type write_error =
  | Size_limit_exceeded
  | No_case_matched
  | Invalid_int({
      min: int,
      v: int,
      max: int,
    })
  | Invalid_float({
      min: float,
      v: float,
      max: float,
    })
  | Invalid_bytes_length({
      expected: int,
      found: int,
    })
  | Invalid_string_length({
      expected: int,
      found: int,
    })
  | Invalid_natural
  | List_too_long
  | Array_too_long;

let write_error_encoding: Encoding.t(write_error);

let pp_write_error: (Format.formatter, write_error) => unit;

exception Write_error(write_error);
