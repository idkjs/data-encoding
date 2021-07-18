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

let bool: int;

let int8: int;

let uint8: int;

let char: int;

let int16: int;

let uint16: int;

let uint30: int;

let uint32: int;

let uint64: int;

let int31: int;

let int32: int;

let int64: int;

let float: int;

type tag_size = [ | `Uint8 | `Uint16];

let tag_size: tag_size => int;

type signed_integer = [ | `Int31 | `Int16 | `Int8];

type unsigned_integer = [ | `Uint30 | `Uint16 | `Uint8];

type integer = [ signed_integer | unsigned_integer];

let integer_to_size: [< integer] => int;

let min_int: [< integer] => int;

let max_int: [< integer] => int;

let range_to_size: (~minimum: int, ~maximum: int) => integer;

let unsigned_range_to_size: int => unsigned_integer;

let enum_size: array('a) => [> unsigned_integer];
