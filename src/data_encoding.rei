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

/** Type-safe serialization and deserialization of data structures. */;

/** {1 Data Encoding} */;

/** {2 Overview}

    This module provides type-safe serialization and deserialization of
    data structures. Backends are provided to both /ad hoc/ binary, JSON
    and BSON.

    This works by writing type descriptors by hand, using the provided
    combinators. These combinators can fine-tune the binary
    representation to be compact and efficient, but also provide
    proper field names and meta information. As a result, an API that uses
    those descriptors can be automatically introspected and documented.

    Here is an example encoding for type [(int * string)].

    [let enc = obj2 (req "code" uint16) (req "message" string)]

    In JSON, this encoding maps values of type [int * string] to JSON
    objects with a field [code] whose value is a number and a field
    [message] whose value is a string.

    In binary, this encoding maps to two raw bytes for the [int]
    followed by the size of the string in bytes, and finally the raw
    contents of the string. This binary format is mostly tagless,
    meaning that serialized data cannot be interpreted without the
    encoding that was used for serialization.

    Regarding binary serialization, encodings are classified as either:
    - fixed size (booleans, integers, numbers)
      data is always the same size for that type ;
    - dynamically sized (arbitrary strings and bytes)
      data is of unknown size and requires an explicit length field ;
    - variable size (special case of strings, bytes, and arrays)
      data makes up the remainder of an object of known size,
      thus its size is given by the context, and does not
      have to be serialized.

    JSON operations are delegated to [json-data-encoding]. */;

/** {2 Module structure}

    This [Data_encoding] module provides multiple submodules:
    - [Encoding] contains the necessary types and constructors for making the
    type descriptors.
    - [Json], [Bson], and [Binary] contain functions to serialize and
    deserialize values.

*/;

module Encoding: {
  /** The type descriptors for values of type ['a]. */

  type t('a) = Encoding.t('a);

  type encoding('a) = t('a);

  /** {3 Ground descriptors} */;

  /** Special value [null] in JSON, nothing in binary. */

  let null: encoding(unit);

  /** Empty object (not included in binary, encoded as empty object in JSON). */

  let empty: encoding(unit);

  /** Unit value, omitted in binary.
      Serialized as an empty object in JSON, accepts any object when deserializing. */

  let unit: encoding(unit);

  /** Constant string (data is not included in the binary data). */

  let constant: string => encoding(unit);

  /** Signed 8 bit integer
      (data is encoded as a byte in binary and an integer in JSON). */

  let int8: encoding(int);

  /** Unsigned 8 bit integer
      (data is encoded as a byte in binary and an integer in JSON). */

  let uint8: encoding(int);

  /** Signed 16 bit integer
      (data is encoded as a short in binary and an integer in JSON). */

  let int16: encoding(int);

  /** Unsigned 16 bit integer
      (data is encoded as a short in binary and an integer in JSON). */

  let uint16: encoding(int);

  /** Signed 31 bit integer, which corresponds to type int on 32-bit OCaml systems
      (data is encoded as a 32 bit int in binary and an integer in JSON). */

  let int31: encoding(int);

  /** Signed 32 bit integer
      (data is encoded as a 32-bit int in binary and an integer in JSON). */

  let int32: encoding(int32);

  /** Signed 64 bit integer
      (data is encoded as a 64-bit int in binary and a decimal string in JSON). */

  let int64: encoding(int64);

  /** Integer with bounds in a given range. Both bounds are inclusive.

      @raise Invalid_argument if the bounds are beyond the interval
      [-2^30; 2^30-1]. These bounds are chosen to be compatible with all versions
      of OCaml.
  */

  let ranged_int: (int, int) => encoding(int);

  /** Big number
      In JSON, data is encoded as a decimal string.
      In binary, data is encoded as a variable length sequence of
      bytes, with a running unary size bit: the most significant bit of
      each byte tells is this is the last byte in the sequence (0) or if
      there is more to read (1). The second most significant bit of the
      first byte is reserved for the sign (positive if zero). Binary_size and
      sign bits ignored, data is then the binary representation of the
      absolute value of the number in little-endian order. */

  let z: encoding(Z.t);

  /** Positive big number, see [z]. */

  let n: encoding(Z.t);

  /** Encoding of floating point number
      (encoded as a floating point number in JSON and a double in binary). */

  let float: encoding(float);

  /** Float with bounds in a given range. Both bounds are inclusive */

  let ranged_float: (float, float) => encoding(float);

  /** Encoding of a boolean
      (data is encoded as a byte in binary and a boolean in JSON). */

  let bool: encoding(bool);

  /** Encoding of a string
      - encoded as a byte sequence in binary prefixed by the length
        of the string
      - encoded as a string in JSON. */

  let string: encoding(string);

  /** Encoding of arbitrary bytes
      (encoded via hex in JSON and directly as a sequence byte in binary). */

  let bytes: encoding(Bytes.t);

  /** {3 Descriptor combinators} */;

  /** Combinator to make an optional value
      (represented as a 1-byte tag followed by the data (or nothing) in binary
       and either the raw value or an empty object in JSON). */

  let option: encoding('a) => encoding(option('a));

  /** Combinator to make a {!result} value
      (represented as a 1-byte tag followed by the data of either type in binary,
       and either unwrapped value in JSON (the caller must ensure that both
       encodings do not collide)). */

  let result: (encoding('a), encoding('b)) => encoding(result('a, 'b));

  /** Array combinator.
      - encoded as an array in JSON
      - encoded as the concatenation of all the element in binary
       prefixed its length in bytes

      If [max_length] is passed and the encoding of elements has fixed
      size, a {!check_size} is automatically added for earlier rejection.

      @raise Invalid_argument if the inner encoding is variable. */

  let array: (~max_length: int=?, encoding('a)) => encoding(array('a));

  /** List combinator.
      - encoded as an array in JSON
      - encoded as the concatenation of all the element in binary
       prefixed its length in bytes

      If [max_length] is passed and the encoding of elements has fixed
      size, a {!check_size} is automatically added for earlier rejection.

      @raise Invalid_argument if the inner encoding is also variable. */

  let list: (~max_length: int=?, encoding('a)) => encoding(list('a));

  /** Provide a transformer from one encoding to a different one.

      Used to simplify nested encodings or to change the generic tuples
      built by {!obj1}, {!tup1} and the like into proper records.

      A schema may optionally be provided as documentation of the new encoding. */

  let conv:
    ('a => 'b, 'b => 'a, ~schema: Json_schema.schema=?, encoding('b)) =>
    encoding('a);

  /** Association list.
      An object in JSON, a list of pairs in binary. */

  let assoc: encoding('a) => encoding(list((string, 'a)));

  /** {3 Product descriptors} */;

  /** An enriched encoding to represent a component in a structured
      type, augmenting the encoding with a name and whether it is a
      required or optional. Fields are used to encode OCaml tuples as
      objects in JSON, and as sequences in binary, using combinator
      {!obj1} and the like. */

  type field('a);

  /** Required field. */

  let req:
    (~title: string=?, ~description: string=?, string, encoding('t)) =>
    field('t);

  /** Optional field. Omitted entirely in JSON encoding if None.
      Omitted in binary if the only optional field in a [`Variable]
      encoding, otherwise a 1-byte prefix (`0` or `255`) tells if the
      field is present or not. */

  let opt:
    (~title: string=?, ~description: string=?, string, encoding('t)) =>
    field(option('t));

  /** Optional field of variable length.
      Only one can be present in a given object. */

  let varopt:
    (~title: string=?, ~description: string=?, string, encoding('t)) =>
    field(option('t));

  /** Required field with a default value.
      If the default value is passed, the field is omitted in JSON.
      The value is always serialized in binary. */

  let dft:
    (~title: string=?, ~description: string=?, string, encoding('t), 't) =>
    field('t);

  /** {4 Constructors for objects with N fields} */;

  /** These are serialized to binary by converting each internal
      object to binary and placing them in the order of the original
      object. These are serialized to JSON as a JSON object with the
      field names. An object might only contains one 'variable'
      field, typically the last one. If the encoding of more than one
      field are 'variable', the first ones should be wrapped with
      [dynamic_size].

      @raise Invalid_argument if more than one field is a variable one. */;

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

  /** Create a larger object from the encodings of two smaller ones.
      @raise Invalid_argument if both arguments are not objects  or if both
      tuples contains a variable field.. */

  let merge_objs: (encoding('o1), encoding('o2)) => encoding(('o1, 'o2));

  /** {4 Constructors for tuples with N fields} */;

  /** These are serialized to binary by converting each internal
      object to binary and placing them in the order of the original
      object. These are serialized to JSON as JSON arrays/lists.  Like
      objects, a tuple might only contains one 'variable' field,
      typically the last one. If the encoding of more than one field
      are 'variable', the first ones should be wrapped with
      [dynamic_size].

      @raise Invalid_argument if more than one field is a variable one. */;

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

  /** Create a large tuple encoding from two smaller ones.
      @raise Invalid_argument if both values are not tuples or if both
      tuples contains a variable field. */

  let merge_tups: (encoding('a1), encoding('a2)) => encoding(('a1, 'a2));

  /** {3 Sum descriptors} */;

  /** A partial encoding to represent a case in a variant type.  Hides
      the (existentially bound) type of the parameter to the specific
      case, providing its encoder, and converter functions to and from
      the union type. */

  type case('t);

  type case_tag =
    | Tag(int)
    | Json_only;

  /** Encodes a variant constructor. Takes the encoding for the specific
      parameters, a recognizer function that will extract the parameters
      in case the expected case of the variant is being serialized, and
      a constructor function for deserialization.

      The tag must be less than the tag size of the union in which you use the case.
      An optional tag gives a name to a case and should be used to maintain
      compatibility.

      An optional name for the case can be provided,
      which is used in the binary documentation. */

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

  /** Create a single encoding from a series of cases.

      In JSON, all cases are tried one after the other. The caller must
      check for collisions.

      In binary, a prefix tag is added to discriminate quickly between
      cases. The default is [`Uint8] and you must use a [`Uint16] if you are
      going to have more than 256 cases.

      @raise Invalid_argument if it is given the empty list
      or if there are more cases than can fit in the tag size. */

  let union:
    (~tag_size: [ | `Uint8 | `Uint16]=?, list(case('t))) => encoding('t);

  /** {3 Predicates over descriptors} */;

  /** Is the given encoding serialized as a JSON object? */

  let is_obj: encoding('a) => bool;

  /** Does the given encoding encode a tuple? */

  let is_tup: encoding('a) => bool;

  /** Classify the binary serialization of an encoding as explained in the
      preamble. */

  let classify: encoding('a) => [ | `Fixed(int) | `Dynamic | `Variable];

  /** {3 Specialized descriptors} */;

  /** Encode enumeration via association list
      - represented as a string in JSON and
      - represented as an integer representing the element's position
        in the list in binary. The integer size depends on the list size.*/

  let string_enum: list((string, 'a)) => encoding('a);

  /** Create encodings that produce data of a fixed length when binary encoded.
      See the preamble for an explanation. */

  module Fixed: {
    /** @raise Invalid_argument if the argument is less or equal to zero. */

    let string: int => encoding(string);

    /** @raise Invalid_argument if the argument is less or equal to zero. */

    let bytes: int => encoding(Bytes.t);

    /** [add_padding e n] is a padded version of the encoding [e]. In Binary,
        there are [n] null bytes ([\000]) added after the value encoded by [e].
        In JSON, padding is ignored.

        @raise Invalid_argument if [n <= 0]. */

    let add_padding: (encoding('a), int) => encoding('a);
  };

  /** Create encodings that produce data of a variable length when binary encoded.
      See the preamble for an explanation. */

  module Variable: {
    let string: encoding(string);

    let bytes: encoding(Bytes.t);

    /** @raise Invalid_argument if the encoding argument is variable length
        or may lead to zero-width representation in binary. */

    let array: (~max_length: int=?, encoding('a)) => encoding(array('a));

    /** @raise Invalid_argument if the encoding argument is variable length
        or may lead to zero-width representation in binary. */

    let list: (~max_length: int=?, encoding('a)) => encoding(list('a));
  };

  module Bounded: {
    /** Encoding of a string whose length does not exceed the specified length.
        The size field uses the smallest integer that can accommodate the
        maximum size - e.g., [`Uint8] for very short strings, [`Uint16] for
        longer strings, etc.

        Attempting to construct a string with a length that is too long causes
        an [Invalid_argument] exception. */

    let string: int => encoding(string);

    /** See {!string} above. */

    let bytes: int => encoding(Bytes.t);
  };

  /** Mark an encoding as being of dynamic size.
      Forces the size to be stored alongside content when needed.
      Typically used to combine two variable encodings in a same
      objects or tuple, or to use a variable encoding in an array or a list. */

  let dynamic_size:
    (~kind: [ | `Uint30 | `Uint16 | `Uint8]=?, encoding('a)) => encoding('a);

  /** [check_size size encoding] ensures that the binary encoding
      of a value will not be allowed to exceed [size] bytes. The reader
      and the writer fails otherwise. This function do not modify
      the JSON encoding. */

  let check_size: (int, encoding('a)) => encoding('a);

  /** Recompute the encoding definition each time it is used.
      Useful for dynamically updating the encoding of values of an extensible
      type via a global reference (e.g., exceptions). */

  let delayed: (unit => encoding('a)) => encoding('a);

  /** Define different encodings for JSON and binary serialization. */

  let splitted:
    (~json: encoding('a), ~binary: encoding('a)) => encoding('a);

  /** Combinator for recursive encodings. */

  let mu:
    (
      string,
      ~title: string=?,
      ~description: string=?,
      encoding('a) => encoding('a)
    ) =>
    encoding('a);

  /** {3 Documenting descriptors} */;

  /** Give a name to an encoding and optionally
      add documentation to an encoding. */

  let def:
    (string, ~title: string=?, ~description: string=?, encoding('t)) =>
    encoding('t);

  /** See {!lazy_encoding} below.*/

  type lazy_t('a);

  /** Combinator to have a part of the binary encoding lazily deserialized.
      This is transparent on the JSON side. */

  let lazy_encoding: encoding('a) => encoding(lazy_t('a));

  /** Force the decoding (memoized for later calls), and return the
      value if successful. */

  let force_decode: lazy_t('a) => option('a);

  /** Obtain the bytes without actually deserializing.  Will serialize
      and memoize the result if the value is not the result of a lazy
      deserialization. */

  let force_bytes: lazy_t('a) => Bytes.t;

  /** Make a lazy value from an immediate one. */

  let make_lazy: (encoding('a), 'a) => lazy_t('a);

  /** Apply on structure of lazy value, and combine results */

  let apply_lazy:
    (
      ~fun_value: 'a => 'b,
      ~fun_bytes: Bytes.t => 'b,
      ~fun_combine: ('b, 'b) => 'b,
      lazy_t('a)
    ) =>
    'b;

  /** Create a {!Data_encoding.t} value which records knowledge of
      older versions of a given encoding as long as one can "upgrade"
      from an older version to the next (if upgrade is impossible one
      should consider that the encoding is completely different).

      See the module [Documented_example] in ["./test/versioned.ml"]
      for a tutorial.
  */;
};

include  (module type of Encoding) with type t('a) = Encoding.t('a);

module With_version: {
  /** An encapsulation of consecutive encoding versions. */

  type t(_);

  /** [first_version enc] records that [enc] is the first (known)
      version of the object. */

  let first_version: encoding('a) => t('a);

  /** [next_version enc upgrade prev] constructs a new version from
      the previous version [prev] and an [upgrade] function. */

  let next_version: (encoding('a), 'b => 'a, t('b)) => t('a);

  /** Make an encoding from an encapsulation of versions; the
      argument [~name] is used to prefix the version "tag" in the
      encoding, it should not change from one version to the next. */

  let encoding: (~name: string, t('a)) => encoding('a);
};

module Json: {
  /** In memory JSON data, compatible with [Ezjsonm]. */

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

  /** Encodes raw JSON data (BSON is used for binary). */

  let encoding: Encoding.t(json);

  /** Encodes a JSON schema (BSON encoded for binary). */

  let schema_encoding: Encoding.t(schema);

  /** Create a {!Json_encoding.encoding} from an {!encoding}. */

  let convert: Encoding.t('a) => Json_encoding.encoding('a);

  /** Generate a schema from an {!encoding}. */

  let schema: (~definitions_path: string=?, Encoding.t('a)) => schema;

  /** Construct a JSON object from an encoding. */

  let construct: (Encoding.t('t), 't) => json;

  /** Destruct a JSON object into a value.
      Fail with an exception if the JSON object and encoding do not match.. */

  let destruct: (Encoding.t('t), json) => 't;

  /** JSON Error. */;

  type path = list(path_item)

  /** A set of accessors that point to a location in a JSON object. */

  and path_item = [
    | /** A field in an object. */
      `Field(string)
    | /** An index in an array. */
      `Index(int)
    | /** Any / every field or index. */
      `Star
    | /** The next element after an array. */
      `Next
  ];

  /** Exception raised by destructors, with the location in the original
      JSON structure and the specific error. */

  exception Cannot_destruct((path, exn));

  /** Unexpected kind of data encountered, with the expectation. */

  exception Unexpected(string, string);

  /** Some {!union} couldn't be destructed, with the reasons for each {!case}. */

  exception No_case_matched(list(exn));

  /** Array of unexpected size encountered, with the expectation. */

  exception Bad_array_size(int, int);

  /** Missing field in an object. */

  exception Missing_field(string);

  /** Supernumerary field in an object. */

  exception Unexpected_field(string);

  let print_error:
    (
      ~print_unknown: (Format.formatter, exn) => unit=?,
      Format.formatter,
      exn
    ) =>
    unit;

  /** Helpers for writing encoders. */

  let cannot_destruct: format4('a, Format.formatter, unit, 'b) => 'a;

  let wrap_error: ('a => 'b, 'a) => 'b;

  /** Read a JSON document from a string. */

  let from_string: string => result(json, string);

  /** Write a JSON document to a string. This goes via an intermediate
      buffer and so may be slow on large documents. */

  let to_string: (~newline: bool=?, ~minify: bool=?, json) => string;

  let pp: (Format.formatter, json) => unit;
};

module Bson: {
  type bson = Json_repr_bson.bson;

  type t = bson;

  /** Construct a BSON object from an encoding. */

  let construct: (Encoding.t('t), 't) => bson;

  /** Destruct a BSON object into a value.
      Fail with an exception if the JSON object and encoding do not match.. */

  let destruct: (Encoding.t('t), bson) => 't;
};

module Binary_schema: {
  type t;

  let pp: (Format.formatter, t) => unit;

  let encoding: Encoding.t(t);
};

module Binary: {
  /** All the errors that might be returned while reading a binary value */

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

  let pp_read_error: (Format.formatter, read_error) => unit;

  let read_error_encoding: t(read_error);

  /** All the errors that might be returned while writing a binary value */

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

  let pp_write_error: (Format.formatter, write_error) => unit;

  let write_error_encoding: t(write_error);

  exception Write_error(write_error);

  /** Compute the expected length of the binary representation of a value */

  let length: (Encoding.t('a), 'a) => int;

  /** Returns the size of the binary representation that the given
      encoding might produce, only when the size of the representation
      does not depends of the value itself. */

  let fixed_length: Encoding.t('a) => option(int);

  let fixed_length_exn: Encoding.t('a) => int;

  /** [read enc buf ofs len] tries to reconstruct a value from the
      bytes in [buf] starting at offset [ofs] and reading at most
      [len] bytes. This function also returns the offset of the first
      unread bytes in the [buf]. */

  let read:
    (Encoding.t('a), Bytes.t, int, int) => result((int, 'a), read_error);

  let read_opt: (Encoding.t('a), Bytes.t, int, int) => option((int, 'a));

  let read_exn: (Encoding.t('a), Bytes.t, int, int) => (int, 'a);

  /** Return type for the function [read_stream]. */

  type status('ret) =
    | /** Fully decoded value, together with the total amount of bytes reads,
        and the remaining unread stream. */
      Success({
        result: 'ret,
        size: int,
        stream: Binary_stream.t,
      })
    | /** Partially decoded value.*/
      Await(Bytes.t => status('ret))
    | /** Failure. The stream is garbled and it should be dropped. */
      Error(
        read_error,
      );

  /** Streamed equivalent of [read]. This variant cannot be called on
      variable-size encodings. */

  let read_stream: (~init: Binary_stream.t=?, Encoding.t('a)) => status('a);

  /** [write enc v buf ofs len] writes the binary representation of [v]
      as described by to [enc], in  [buf] starting at the offset [ofs]
      and writing at most [len] bytes. The function returns the offset
      of first unwritten bytes, or returns [None] in case of failure.
      In the latter case, some data might have been written on the buffer. */

  let write:
    (Encoding.t('a), 'a, Bytes.t, int, int) => result(int, write_error);

  let write_opt: (Encoding.t('a), 'a, Bytes.t, int, int) => option(int);

  let write_exn: (Encoding.t('a), 'a, Bytes.t, int, int) => int;

  /** [of_bytes enc buf] is equivalent to [read enc buf 0 (length buf)].
      The function fails if the buffer is not fully read. */

  let of_bytes: (Encoding.t('a), Bytes.t) => result('a, read_error);

  let of_bytes_opt: (Encoding.t('a), Bytes.t) => option('a);

  /** [of_bytes_exn enc buf] is equivalent to [of_bytes], except
      @raise [Read_error] instead of returning [None] in case of error. */

  let of_bytes_exn: (Encoding.t('a), Bytes.t) => 'a;

  /** [to_bytes enc v] is the equivalent of [write env buf 0 len]
      where [buf] is a newly allocated buffer of the expected
      length [len] (see [length env v]).
      The parameter [buffer_size] controls the initial size of [buf]. */

  let to_bytes:
    (~buffer_size: int=?, Encoding.t('a), 'a) => result(Bytes.t, write_error);

  let to_bytes_opt:
    (~buffer_size: int=?, Encoding.t('a), 'a) => option(Bytes.t);

  /** [to_bytes_exn enc v] is equivalent to [to_bytes enc v], except
      @raise [Write_error] instead of returning [None] in case of error. */

  let to_bytes_exn: (~buffer_size: int=?, Encoding.t('a), 'a) => Bytes.t;

  let describe: Encoding.t('a) => Binary_schema.t;
};

type json = Json.t;

let json: Encoding.t(json);

type json_schema = Json.schema;

let json_schema: Encoding.t(json_schema);

type bson = Bson.t;

module Registration: {
  type id = string;

  /** A encoding that has been {!register}ed. It can be retrieved using either
      {!list} or {!find}. */

  type t;

  /** Descriptions and schemas of registered encodings. */

  let binary_schema: t => Binary_schema.t;

  let json_schema: t => Json.schema;

  let description: t => option(string);

  /** Printers for the encodings. */

  let json_pretty_printer: (t, Format.formatter, Json.t) => unit;

  let binary_pretty_printer: (t, Format.formatter, Bytes.t) => unit;

  /** [register ~id encoding] registers the [encoding] with the [id]. It can
      later be found using {!find} and providing the matching [id]. It will
      also appear in the results of {!list}. */

  let register:
    (~pp: (Format.formatter, 'a) => unit=?, Encoding.t('a)) => unit;

  /** [find id] is [Some r] if [register id e] has been called, in which
      case [r] matches [e]. Otherwise, it is [None]. */

  let find: id => option(t);

  /** [list ()] is a list of pairs [(id, r)] where [r] is
      a registered encoding for the [id]. */

  let list: unit => list((id, t));

  /** Conversion functions from/to json to/from bytes. */

  let bytes_of_json: (t, Json.t) => option(Bytes.t);

  let json_of_bytes: (t, Bytes.t) => option(Json.t);
};
