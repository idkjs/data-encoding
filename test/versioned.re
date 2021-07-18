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

/**
   Tests for the {!Data_encoding.With_version} module.
*/;

/** This module is a simple example of use of {!With_version}. */
module Documented_example = {
  /** Here we show how to {i "versionize"} a given random encoding (which
      just happens to be very similar to {!Internal_event.Debug_event}). */;

  /** We are going to provide successive versions of a module
      implementing {!INTENDED_SIGNATURE} (which is similar to a
      simplified {!Internal_event.EVENT_DEFINITION}): */
  module type INTENDED_SIGNATURE = {
    type t;

    let encoding: Data_encoding.t(t);

    let pp: (Format.formatter, t) => unit;
  };

  /** The name, once used with {!With_version.encoding}, appears in
      the serialized values, it has to remain constant across versions: */

  let name = "versioned-documented-example";

  /** The first version has a [(string * string) list] field. */
  module V0 = {
    type t = {
      message: string,
      attachment: list((string, string)),
    };

    /** This is the "naked" (i.e. non-versioned) encoding of version-0: */

    let encoding =
      Data_encoding.(
        conv(
          ({message, attachment}) => (message, attachment),
          ((message, attachment)) => {message, attachment},
          obj2(
            req("message", string),
            req("attach", list(tup2(string, string))),
          ),
        )
      );
  };

  /** The versioned implementation of {!INTENDED_SIGNATURE}: */
  module First_version: INTENDED_SIGNATURE with type t = V0.t = {
    include V0;

    /** The encoding with the version tagging: */

    let encoding =
      Data_encoding.With_version.(
        encoding(~name, first_version(V0.encoding))
      );

    let pp = (ppf, {message, attachment}) => {
      open Format;
      fprintf(ppf, "%s:@ %s@ [", name, message);
      pp_open_box(ppf, 2);
      pp_print_list(
        ~pp_sep=(fmt, ()) => fprintf(fmt, ";@ "),
        (fmt, (k, v)) => fprintf(fmt, "%s: %S", k, v),
        ppf,
        attachment,
      );
      pp_close_box(ppf, ());
      fprintf(ppf, "]");
      ();
    };
  };

  /** In a later version we want the attachment to be any piece of
      Json and not just a key-value list: */
  module V1 = {
    /** Version 1 is very similar to {!Internal_event.Debug_event}: */

    type t = {
      message: string,
      attachment: Data_encoding.Json.t,
    };

    let make = (~attach=`Null, message, ()) => {message, attachment: attach};

    /** Note the "upgrade" function which can make a {!V1.t} from a {!V0.t}: */

    let of_v0 = ({V0.message, attachment}) => {
      message,
      attachment: `O(List.map(((k, v)) => (k, `String(v)), attachment)),
    };

    /** Again we build first a version-free encoding: */

    let encoding =
      Data_encoding.(
        conv(
          ({message, attachment}) => (message, attachment),
          ((message, attachment)) => {message, attachment},
          obj2(req("message", string), req("attachment", json)),
        )
      );
  };

  /** The second version exports {!V1.t} while being able to parse
      (and upgrade from) {!First_version.t} values. */
  module Second_version: INTENDED_SIGNATURE with type t = V1.t = {
    include V1;

    /** Here is the interesting use of {!Data_encoding.With_version}: the
        encoding uses both {!V0.encoding} and {!V1.encoding} and
        provides {!V1.of_v0} as an upgrade function. */

    let encoding =
      Data_encoding.With_version.(
        encoding(
          ~name,
          first_version(V0.encoding) |> next_version(V1.encoding, V1.of_v0),
        )
      );

    let pp = (ppf, {message, attachment}) =>
      Format.(
        fprintf(
          ppf,
          "%s:@ %s@ %a",
          name,
          message,
          Data_encoding.Json.pp,
          attachment,
        )
      );
  };

  /** This test "serializes" successively using
      {!First_version.encoding} and {!Second_version.encoding}, and then
      shows that the former's output can be parsed with the later. */

  let actual_test = () => {
    let v0_thing: First_version.t = (
      {
        V0.message: "The v0 message",
        attachment: [("k1", "v1"), ("k2", "v2")],
      }: First_version.t
    );

    let json_v0 =
      Data_encoding.Json.construct(First_version.encoding, v0_thing);

    let expected_json_v0 =
      `O([
        (
          name ++ ".v0",
          /* -> here we see how the [~name] is used. */
          `O([
            ("message", `String(v0_thing.V0.message)),
            (
              "attach",
              `A(
                List.map(
                  ((k, v)) => `A([`String(k), `String(v)]),
                  v0_thing.V0.attachment,
                ),
              ),
            ),
          ]),
        ),
      ]);

    if (json_v0 != expected_json_v0) {
      Alcotest.failf(
        "Json-v0: %a@ Vs@ %a",
        Data_encoding.Json.pp,
        json_v0,
        Data_encoding.Json.pp,
        expected_json_v0,
      );
    };
    /* Up to here we only used the {!First_version} module. Now the
       same process with {!Second_version}: */
    let v1_thing: Second_version.t = (
      {
        V1.message: "The v1 message",
        attachment: `O([("k1", `String("v1")), ("kn", `Float(42.))]),
      }: Second_version.t
    );

    let json_v1 =
      Data_encoding.Json.construct(Second_version.encoding, v1_thing);

    let expected_json_v1 =
      `O([
        (
          name ++ ".v1",
          `O([
            ("message", `String(v1_thing.V1.message)),
            ("attachment", v1_thing.V1.attachment),
          ]),
        ),
      ]);

    if (json_v1 != expected_json_v1) {
      Alcotest.failf(
        "Json-v1: %a@ Vs@ %a",
        Data_encoding.Json.pp,
        json_v1,
        Data_encoding.Json.pp,
        expected_json_v1,
      );
    };
    /* Now the {b interesting part}, we decode ("destruct") the JSON from
       {!First_version} with {!Second_version}: */
    let v0_decoded_later: Second_version.t = (
      Data_encoding.Json.destruct(Second_version.encoding, json_v0): Second_version.t
    );

    /* And we check that going through JSON is equivalent to just
       calling the upgrade function directly on the {!First_version.t}
       value: */
    let expected_v1 = V1.of_v0(v0_thing);
    if (v0_decoded_later != expected_v1) {
      Alcotest.failf(
        "Parsing v0 with v1: %a@ Vs@ %a",
        Second_version.pp,
        v0_decoded_later,
        Second_version.pp,
        expected_v1,
      );
    };
    ();
  };
};

/** This test builds a few successive versions of encodings and tries
    out parsing/printing with successive encapsulated
    versioned-encodings.

    Check out ["_build/_tests/versioned.001.output"] to see how they look.
*/

let test_n_encapsulated_versions = () => {
  open Data_encoding;
  let name = "test0";
  let version_0 = obj2(req("left", string), req("right", string));
  let versioned_0 =
    With_version.(encoding(~name) @@ first_version(version_0));
  let value_0 = ("v0", "k0");
  let json_0 = Json.construct(versioned_0, value_0);
  Helpers.no_exception(() =>
    let result = Json.destruct(versioned_0, json_0);
    if (result != value_0) {
      Alcotest.failf("value-0");
    };
  );
  module Ex = {
    type v0 = (string, string);

    type t =
      | Hide(Data_encoding.t('a), With_version.t('a), 'a, v0 => 'a): t;
  };
  let make_next =
      ([@implicit_arity] Ex.Hide(enc, versioned, example, from_v0), index) => {
    let new_tag = Printf.sprintf("left-%d", index);
    let version_n = obj2(req(new_tag, string), req("right", enc));
    let upgrade = vn => ("some-random-extra-string", vn);
    let versioned_n =
      With_version.(next_version(version_n, upgrade, versioned));

    let encoding = With_version.(encoding(~name, versioned_n));
    let example_n = ("val4" ++ new_tag, example);
    let json_example_n = Json.construct(encoding, example_n);
    Helpers.no_exception(() =>
      let result = Json.destruct(encoding, json_example_n);
      if (result != example_n) {
        Alcotest.failf("value-%d", index);
      };
    );
    let json_example_p =
      Json.construct(With_version.(encoding(~name, versioned)), example);

    Helpers.no_exception(() =>
      let result = Json.destruct(encoding, json_example_p);
      if (result != upgrade(example)) {
        Alcotest.failf("value-%d-previous-encoding", index);
      };
    );
    let next_upgrade = x => upgrade(from_v0(x));
    Helpers.no_exception(() =>
      let result = Json.destruct(encoding, json_0);
      if (result != next_upgrade(value_0)) {
        Alcotest.failf("value-%d-from-v0-encoding", index);
      };
    );
    Format.eprintf(
      "json_example_%d:@ %a\n%!",
      index,
      Json.pp,
      json_example_n,
    );
    Format.eprintf(
      "json_example_%d-from-v0:@ %a\n%!",
      index,
      Json.pp,
      Json.construct(encoding, next_upgrade(value_0)),
    );
    [@implicit_arity]
    Ex.Hide(version_n, versioned_n, example_n, next_upgrade);
  };

  let Ex.Hide(_) =
    ListLabels.fold_left(
      List.init(10, (+)(1)),
      ~init=
        [@implicit_arity]
        Ex.Hide(
          version_0,
          With_version.(first_version(version_0)),
          value_0,
          x => x,
        ),
      ~f=make_next,
    );

  ();
};

let tests = [
  ("example-test", `Quick, Documented_example.actual_test),
  ("test-encapsulated-versions", `Quick, test_n_encapsulated_versions),
];
