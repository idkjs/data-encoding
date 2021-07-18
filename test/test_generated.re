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

/* NOTE: the current release of Crowbar, v0.1, is quite limited. Several
 * improvements have been made to the dev version which will make it possible to
 * simplify this file and increase coverage.
 * For now, this is a limited test-suite. */

let char = Crowbar.map([Crowbar.uint8], Char.chr);

let string = Crowbar.bytes;

/* The v0.1 of Crowbar doesn't have fixed-size string generation. When we
 * update Crowbar, we can improve this generator. */
let short_string =
  Crowbar.(
    choose([
      const(""),
      map([char], c => String.make(1, c)),
      map(
        [char, char, char, char],
        (c1, c2, c3, c4) => {
          let s = Bytes.make(4, c1);
          Bytes.set(s, 1, c2);
          Bytes.set(s, 2, c3);
          Bytes.set(s, 3, c4);
          Bytes.to_string(s);
        },
      ),
    ])
  );

let short_string1 =
  Crowbar.(
    choose([
      map([char], c => String.make(1, c)),
      map(
        [char, char, char, char],
        (c1, c2, c3, c4) => {
          let s = Bytes.make(4, c1);
          Bytes.set(s, 1, c2);
          Bytes.set(s, 2, c3);
          Bytes.set(s, 3, c4);
          Bytes.to_string(s);
        },
      ),
    ])
  );

let mbytes = Crowbar.map([Crowbar.bytes], Bytes.of_string);

let short_mbytes = Crowbar.map([short_string], Bytes.of_string);

let short_mbytes1 = Crowbar.map([short_string1], Bytes.of_string);

/* We need to hide the type parameter of `Encoding.t` to avoid the generator
 * combinator `choose` from complaining about different types. We use first
 * level modules (for now) to encode existentials.
 *
 * An alternative is used in https://gitlab.com/gasche/fuzz-data-encoding */

module type TESTABLE = {
  type t;

  let v: t;

  let ding: Data_encoding.t(t);

  let pp: Crowbar.printer(t);
};

type testable = (module TESTABLE);

let null: testable = (
  (module
   {
     type t = unit;

     let v = ();

     let ding = Data_encoding.null;

     let pp = (ppf, ()) => Crowbar.pp(ppf, "(null)");
   }): testable
);

let empty: testable = (
  (module
   {
     type t = unit;

     let v = ();

     let ding = Data_encoding.empty;

     let pp = (ppf, ()) => Crowbar.pp(ppf, "(empty)");
   }): testable
);

let unit: testable = (
  (module
   {
     type t = unit;

     let v = ();

     let ding = Data_encoding.unit;

     let pp = (ppf, ()) => Crowbar.pp(ppf, "(unit)");
   }): testable
);

let map_constant = (s: string): testable =>
  (module
   {
     type t = unit;

     let v = ();

     let ding = Data_encoding.constant(s);

     let pp = (ppf, ()) => Crowbar.pp(ppf, "\"%s\"", s);
   });

let map_int8 = (i: int): testable =>
  (module
   {
     type t = int;

     let v = i;

     let ding = Data_encoding.int8;

     let pp = Crowbar.pp_int;
   });

let map_uint8 = (i: int): testable =>
  (module
   {
     type t = int;

     let v = i;

     let ding = Data_encoding.uint8;

     let pp = Crowbar.pp_int;
   });

let map_int16 = (i: int): testable =>
  (module
   {
     type t = int;

     let v = i;

     let ding = Data_encoding.int16;

     let pp = Crowbar.pp_int;
   });

let map_uint16 = (i: int): testable =>
  (module
   {
     type t = int;

     let v = i;

     let ding = Data_encoding.uint16;

     let pp = Crowbar.pp_int;
   });

let map_int32 = (i: int32): testable =>
  (module
   {
     type t = int32;

     let v = i;

     let ding = Data_encoding.int32;

     let pp = Crowbar.pp_int32;
   });

let map_int64 = (i: int64): testable =>
  (module
   {
     type t = int64;

     let v = i;

     let ding = Data_encoding.int64;

     let pp = Crowbar.pp_int64;
   });

let map_range_int = (a, b, c): testable => {
  let (small, middle, big) =
    switch (List.sort(compare, [a, b, c])) {
    | [small, middle, big] =>
      assert(small <= middle);
      assert(middle <= big);
      (small, middle, big);
    | _ => assert(false)
    };

  (module
   {
     type t = int;

     let v = middle;

     let ding = Data_encoding.ranged_int(small, big);

     let pp = (ppf, i) => Crowbar.pp(ppf, "(%d :[%d;%d])", i, small, big);
   });
};

let map_range_float = (a, b, c): testable =>
  if (compare(a, nan) == 0 || compare(b, nan) == 0 || compare(c, nan) == 0) {
    /* copout */
    null;
  } else {
    let (small, middle, big) =
      switch (List.sort(compare, [a, b, c])) {
      | [small, middle, big] =>
        assert(small <= middle);
        assert(middle <= big);
        (small, middle, big);
      | _ => assert(false)
      };

    (module
     {
       type t = float;

       let v = middle;

       let ding = Data_encoding.ranged_float(small, big);

       let pp = (ppf, i) => Crowbar.pp(ppf, "(%f :[%f;%f])", i, small, big);
     });
  };

let map_bool = (b): testable =>
  (module
   {
     type t = bool;

     let v = b;

     let ding = Data_encoding.bool;

     let pp = Crowbar.pp_bool;
   });

let map_string = (s): testable =>
  (module
   {
     type t = string;

     let v = s;

     let ding = Data_encoding.string;

     let pp = Crowbar.pp_string;
   });

let map_bytes = (s): testable =>
  (module
   {
     type t = Bytes.t;

     let v = s;

     let ding = Data_encoding.bytes;

     let pp = (ppf, m) =>
       if (Bytes.length(m) > 40) {
         Crowbar.pp(
           ppf,
           "@[<hv 1>%a … (%d more bytes)@]",
           Hex.pp,
           Hex.of_bytes(Bytes.sub(m, 1, 30)),
           Bytes.length(m),
         );
       } else {
         Hex.pp(ppf, Hex.of_bytes(m));
       };
   });

let map_float = (f): testable =>
  (module
   {
     type t = float;

     let v = f;

     let ding = Data_encoding.float;

     let pp = Crowbar.pp_float;
   });

let map_fixed_string = (s): testable =>
  (module
   {
     type t = string;

     let v = s;

     let ding = Data_encoding.Fixed.string(String.length(s));

     let pp = (ppf, s) => Crowbar.pp(ppf, "\"%s\"", s);
   });

let map_fixed_bytes = (s): testable =>
  (module
   {
     type t = Bytes.t;

     let v = s;

     let ding = Data_encoding.Fixed.bytes(Bytes.length(s));

     let pp = (fmt, x) => Hex.pp(fmt, Hex.of_bytes(x));
   });

let map_variable_string = (s): testable =>
  (module
   {
     type t = string;

     let v = s;

     let ding = Data_encoding.Variable.string;

     let pp = (ppf, s) => Crowbar.pp(ppf, "\"%s\"", s);
   });

let map_variable_bytes = (s): testable =>
  (module
   {
     type t = Bytes.t;

     let v = s;

     let ding = Data_encoding.Variable.bytes;

     let pp = (fmt, x) => Hex.pp(fmt, Hex.of_bytes(x));
   });

/* And now combinators */

let dyn_if_not = ding =>
  switch (Data_encoding.classify(ding)) {
  | `Fixed(_)
  | `Dynamic => ding
  | `Variable => Data_encoding.dynamic_size(ding)
  };

let map_some = (t: testable): testable => {
  module T = (val t);
  (module
   {
     type t = option(T.t);

     let v = Some(T.v);

     let ding =
       try(Data_encoding.option(T.ding)) {
       | Invalid_argument(_) => Crowbar.bad_test()
       };

     let pp = (ppf, o) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>%a@]",
         (fmt, v) =>
           switch (v) {
           | None => Format.fprintf(fmt, "None")
           | Some(v) => Format.fprintf(fmt, "Some(%a)", T.pp, v)
           },
         o,
       );
   });
};

let map_none = (t: testable): testable => {
  module T = (val t);
  (module
   {
     type t = option(T.t);

     let v = None;

     let ding =
       try(Data_encoding.option(T.ding)) {
       | Invalid_argument(_) => Crowbar.bad_test()
       };

     let pp = (ppf, o) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>%a@]",
         (fmt, v) =>
           switch (v) {
           | None => Format.fprintf(fmt, "None")
           | Some(v) => Format.fprintf(fmt, "Some(%a)", T.pp, v)
           },
         o,
       );
   });
};

let map_ok = (t_o: testable, t_e: testable): testable => {
  module T_O = (val t_o);
  module T_E = (val t_e);
  (module
   {
     type t = result(T_O.t, T_E.t);

     let v = Ok(T_O.v);

     let ding = Data_encoding.result(T_O.ding, T_E.ding);

     let pp = (ppf, r) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>%a@]",
         (fmt, r) =>
           switch (r) {
           | Ok(o) => Format.fprintf(fmt, "Ok(%a)", T_O.pp, o)
           | Error(e) => Format.fprintf(fmt, "Error(%a)", T_E.pp, e)
           },
         r,
       );
   });
};

let map_error = (t_o: testable, t_e: testable): testable => {
  module T_O = (val t_o);
  module T_E = (val t_e);
  (module
   {
     type t = result(T_O.t, T_E.t);

     let v = Error(T_E.v);

     let ding = Data_encoding.result(T_O.ding, T_E.ding);

     let pp = (ppf, r) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>%a@]",
         (fmt, r) =>
           switch (r) {
           | Ok(o) => Format.fprintf(fmt, "Ok(%a)", T_O.pp, o)
           | Error(e) => Format.fprintf(fmt, "Error(%a)", T_E.pp, e)
           },
         r,
       );
   });
};

let map_variable_list = (t: testable, ts: list(testable)): testable => {
  module T = (val t);
  (module
   {
     type t = list(T.t);

     let ding = Data_encoding.Variable.list(dyn_if_not(T.ding));

     let v =
       List.fold_left(
         (acc, t: testable) => {
           module T = (val t);
           /* We can get rid of this Obj when we update Crowbar */
           [Obj.magic(T.v), ...acc];
         },
         [],
         ts,
       );

     let pp = Crowbar.pp_list(T.pp);
   });
};

let map_variable_array = (t: testable, ts: array(testable)): testable => {
  module T = (val t);
  (module
   {
     type t = array(T.t);

     let ding = Data_encoding.Variable.array(dyn_if_not(T.ding));

     let v =
       Array.of_list(
         Array.fold_left(
           (acc, t: testable) => {
             module T = (val t);
             [Obj.magic(T.v), ...acc];
           },
           [],
           ts,
         ),
       );

     let pp = (ppf, a) =>
       if (Array.length(a) > 40) {
         Crowbar.pp(
           ppf,
           "@[<hv 1>[|%a … (%d more elements)|]@]",
           Format.pp_print_list(
             ~pp_sep=(ppf, ()) => Format.fprintf(ppf, ";@ "),
             T.pp,
           ),
           Array.to_list(Array.sub(a, 0, 30)),
           Array.length(a),
         );
       } else {
         Crowbar.pp(
           ppf,
           "@[<hv 1>[|%a|]@]",
           Format.pp_print_list(
             ~pp_sep=(ppf, ()) => Format.fprintf(ppf, ";@ "),
             T.pp,
           ),
           Array.to_list(a),
         );
       };
   });
};

let map_dynamic_size = (t: testable): testable => {
  module T = (val t);
  (module
   {
     include T;

     let ding = Data_encoding.dynamic_size(T.ding);
   });
};

let map_tup1 = (t1: testable): testable => {
  module T1 = (val t1);
  (module
   {
     include T1;

     let ding = Data_encoding.tup1(T1.ding);

     let pp = (ppf, v1) => Crowbar.pp(ppf, "@[<hv 1>(%a)@]", T1.pp, v1);
   });
};

let map_tup2 = (t1: testable, t2: testable): testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  (module
   {
     type t = (T1.t, T2.t);

     let ding = Data_encoding.tup2(dyn_if_not(T1.ding), T2.ding);

     let v = (T1.v, T2.v);

     let pp = (ppf, (v1, v2)) =>
       Crowbar.pp(ppf, "@[<hv 1>(%a, %a)@]", T1.pp, v1, T2.pp, v2);
   });
};

let map_tup3 = (t1: testable, t2: testable, t3: testable): testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  (module
   {
     type t = (T1.t, T2.t, T3.t);

     let ding =
       Data_encoding.tup3(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         T3.ding,
       );

     let v = (T1.v, T2.v, T3.v);

     let pp = (ppf, (v1, v2, v3)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
       );
   });
};

let map_tup4 =
    (t1: testable, t2: testable, t3: testable, t4: testable): testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  module T4 = (val t4);
  (module
   {
     type t = (T1.t, T2.t, T3.t, T4.t);

     let ding =
       Data_encoding.tup4(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         dyn_if_not(T3.ding),
         T4.ding,
       );

     let v = (T1.v, T2.v, T3.v, T4.v);

     let pp = (ppf, (v1, v2, v3, v4)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
         T4.pp,
         v4,
       );
   });
};

let map_tup5 =
    (t1: testable, t2: testable, t3: testable, t4: testable, t5: testable)
    : testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  module T4 = (val t4);
  module T5 = (val t5);
  (module
   {
     type t = (T1.t, T2.t, T3.t, T4.t, T5.t);

     let ding =
       Data_encoding.tup5(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         dyn_if_not(T3.ding),
         dyn_if_not(T4.ding),
         T5.ding,
       );

     let v = (T1.v, T2.v, T3.v, T4.v, T5.v);

     let pp = (ppf, (v1, v2, v3, v4, v5)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
         T4.pp,
         v4,
         T5.pp,
         v5,
       );
   });
};

let map_tup6 =
    (
      t1: testable,
      t2: testable,
      t3: testable,
      t4: testable,
      t5: testable,
      t6: testable,
    )
    : testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  module T4 = (val t4);
  module T5 = (val t5);
  module T6 = (val t6);
  (module
   {
     type t = (T1.t, T2.t, T3.t, T4.t, T5.t, T6.t);

     let ding =
       Data_encoding.tup6(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         dyn_if_not(T3.ding),
         dyn_if_not(T4.ding),
         dyn_if_not(T5.ding),
         T6.ding,
       );

     let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v);

     let pp = (ppf, (v1, v2, v3, v4, v5, v6)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a, %a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
         T4.pp,
         v4,
         T5.pp,
         v5,
         T6.pp,
         v6,
       );
   });
};

let map_tup7 =
    (
      t1: testable,
      t2: testable,
      t3: testable,
      t4: testable,
      t5: testable,
      t6: testable,
      t7: testable,
    )
    : testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  module T4 = (val t4);
  module T5 = (val t5);
  module T6 = (val t6);
  module T7 = (val t7);
  (module
   {
     type t = (T1.t, T2.t, T3.t, T4.t, T5.t, T6.t, T7.t);

     let ding =
       Data_encoding.tup7(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         dyn_if_not(T3.ding),
         dyn_if_not(T4.ding),
         dyn_if_not(T5.ding),
         dyn_if_not(T6.ding),
         T7.ding,
       );

     let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v);

     let pp = (ppf, (v1, v2, v3, v4, v5, v6, v7)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
         T4.pp,
         v4,
         T5.pp,
         v5,
         T6.pp,
         v6,
         T7.pp,
         v7,
       );
   });
};

let map_tup8 =
    (
      t1: testable,
      t2: testable,
      t3: testable,
      t4: testable,
      t5: testable,
      t6: testable,
      t7: testable,
      t8: testable,
    )
    : testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  module T4 = (val t4);
  module T5 = (val t5);
  module T6 = (val t6);
  module T7 = (val t7);
  module T8 = (val t8);
  (module
   {
     type t = (T1.t, T2.t, T3.t, T4.t, T5.t, T6.t, T7.t, T8.t);

     let ding =
       Data_encoding.tup8(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         dyn_if_not(T3.ding),
         dyn_if_not(T4.ding),
         dyn_if_not(T5.ding),
         dyn_if_not(T6.ding),
         dyn_if_not(T7.ding),
         T8.ding,
       );

     let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v);

     let pp = (ppf, (v1, v2, v3, v4, v5, v6, v7, v8)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
         T4.pp,
         v4,
         T5.pp,
         v5,
         T6.pp,
         v6,
         T7.pp,
         v7,
         T8.pp,
         v8,
       );
   });
};

let map_tup9 =
    (
      t1: testable,
      t2: testable,
      t3: testable,
      t4: testable,
      t5: testable,
      t6: testable,
      t7: testable,
      t8: testable,
      t9: testable,
    )
    : testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  module T4 = (val t4);
  module T5 = (val t5);
  module T6 = (val t6);
  module T7 = (val t7);
  module T8 = (val t8);
  module T9 = (val t9);
  (module
   {
     type t = (T1.t, T2.t, T3.t, T4.t, T5.t, T6.t, T7.t, T8.t, T9.t);

     let ding =
       Data_encoding.tup9(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         dyn_if_not(T3.ding),
         dyn_if_not(T4.ding),
         dyn_if_not(T5.ding),
         dyn_if_not(T6.ding),
         dyn_if_not(T7.ding),
         dyn_if_not(T8.ding),
         T9.ding,
       );

     let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v, T9.v);

     let pp = (ppf, (v1, v2, v3, v4, v5, v6, v7, v8, v9)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
         T4.pp,
         v4,
         T5.pp,
         v5,
         T6.pp,
         v6,
         T7.pp,
         v7,
         T8.pp,
         v8,
         T9.pp,
         v9,
       );
   });
};

let map_tup10 =
    (
      t1: testable,
      t2: testable,
      t3: testable,
      t4: testable,
      t5: testable,
      t6: testable,
      t7: testable,
      t8: testable,
      t9: testable,
      t10: testable,
    )
    : testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  module T3 = (val t3);
  module T4 = (val t4);
  module T5 = (val t5);
  module T6 = (val t6);
  module T7 = (val t7);
  module T8 = (val t8);
  module T9 = (val t9);
  module T10 = (val t10);
  (module
   {
     type t = (T1.t, T2.t, T3.t, T4.t, T5.t, T6.t, T7.t, T8.t, T9.t, T10.t);

     let ding =
       Data_encoding.tup10(
         dyn_if_not(T1.ding),
         dyn_if_not(T2.ding),
         dyn_if_not(T3.ding),
         dyn_if_not(T4.ding),
         dyn_if_not(T5.ding),
         dyn_if_not(T6.ding),
         dyn_if_not(T7.ding),
         dyn_if_not(T8.ding),
         dyn_if_not(T9.ding),
         T10.ding,
       );

     let v = (T1.v, T2.v, T3.v, T4.v, T5.v, T6.v, T7.v, T8.v, T9.v, T10.v);

     let pp = (ppf, (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)) =>
       Crowbar.pp(
         ppf,
         "@[<hv 1>(%a, %a, %a, %a, %a, %a, %a, %a, %a, %a)@]",
         T1.pp,
         v1,
         T2.pp,
         v2,
         T3.pp,
         v3,
         T4.pp,
         v4,
         T5.pp,
         v5,
         T6.pp,
         v6,
         T7.pp,
         v7,
         T8.pp,
         v8,
         T9.pp,
         v9,
         T10.pp,
         v10,
       );
   });
};

let map_merge_tups = (t1: testable, t2: testable): testable => {
  module T1 = (val t1);
  module T2 = (val t2);
  (module
   {
     type t = (T1.t, T2.t);

     let ding =
       Data_encoding.merge_tups(dyn_if_not(T1.ding), dyn_if_not(T2.ding));

     let v = (T1.v, T2.v);

     let pp = (ppf, (v1, v2)) =>
       Crowbar.pp(ppf, "@[<hv 1>(%a, %a)@]", T1.pp, v1, T2.pp, v2);
   });
};

let testable_printer: Crowbar.printer(testable) = (
  (ppf, t: testable) => {
    module T = (val t);
    T.pp(ppf, T.v);
  }:
    Crowbar.printer(testable)
);

/* helpers to construct values tester values */

/* Generator for testable values */

let tup_gen = (tgen: Crowbar.gen(testable)): Crowbar.gen(testable) =>
  Crowbar.
    /* Stack overflow if there are more levels */
    (
      with_printer(testable_printer) @@
      choose([
        map([tgen], map_tup1),
        map([tgen, tgen], map_tup2),
        map([tgen, tgen, tgen], map_tup3),
        map([tgen, tgen, tgen, tgen], map_tup4),
        map([tgen, tgen, tgen, tgen, tgen], map_tup5),
        map([tgen, tgen, tgen, tgen, tgen, tgen], map_tup6),
      ])
    );

let gen = {
  open Crowbar;
  let g: Crowbar.gen(testable) = (
    fix(g =>
      choose([
        const(null),
        const(empty),
        const(unit),
        map([short_string], map_constant),
        map([int8], map_int8),
        map([uint8], map_uint8),
        map([int32], map_int32),
        map([int64], map_int64),
        map([int8, int8, int8], map_range_int),
        map([float, float, float], map_range_float),
        map([bool], map_bool),
        map([short_string], map_string),
        map([short_mbytes], map_bytes),
        map([float], map_float),
        map([short_string1], map_fixed_string),
        map([short_mbytes1], map_fixed_bytes),
        map([short_string], map_variable_string),
        map([short_mbytes], map_variable_bytes),
        map([g], map_some),
        map([g], map_none),
        map([g], map_dynamic_size),
        map([g], map_tup1),
        map([g, g], map_tup2),
        map([g, g, g], map_tup3),
        map([g, g, g, g], map_tup4),
        map([g, g, g, g, g], map_tup5),
        map([g, g, g, g, g, g], map_tup6),
        map([g, g], (t1, t2) =>
          map_merge_tups(map_tup1(t1), map_tup1(t2))
        ),
        map([g, g, g], (t1, t2, t3) =>
          map_merge_tups(map_tup2(t1, t2), map_tup1(t3))
        ),
        map([g, g, g], (t1, t2, t3) =>
          map_merge_tups(map_tup1(t1), map_tup2(t2, t3))
        ),
      ])
    ):
      Crowbar.gen(testable)
    /* TODO: use newer version of crowbar to get these generators
             map [int16] map_int16;
             map [uint16] map_uint16;
       */
    /* NOTE: the int encoding require ranges to be 30-bit compatible */
    /* NOTE: we cannot use lists/arrays for now. They require the
          data-inside to be homogeneous (e.g., same rangedness of ranged
          numbers) which we cannot guarantee right now. This can be fixed once
          we update Crowbar and get access to the new `dynamic_bind` generator
          combinator.

          map [g; list g] map_variable_list;
          map [g; list g] (fun t ts -> map_variable_array t (Array.of_list ts));
       */
  );

  with_printer(testable_printer, g);
};

/* TODO: The following features are not yet tested
      val string_enum : (string * 'a) list -> 'a encoding
      val delayed : (unit -> 'a encoding) -> 'a encoding
      val json : json encoding
      val json_schema : json_schema encoding
      type 'a field
      val req :
      ?title:string -> ?description:string ->
      string -> 't encoding -> 't field
      val opt :
      ?title:string -> ?description:string ->
      string -> 't encoding -> 't option field
      val varopt :
      ?title:string -> ?description:string ->
      string -> 't encoding -> 't option field
      val dft :
      ?title:string -> ?description:string ->
      string -> 't encoding -> 't -> 't field
      val obj1 : 'f1 field -> 'f1 encoding
      val obj2 : 'f1 field -> 'f2 field -> ('f1 * 'f2) encoding
      val obj3 : 'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding
      val obj4 :
      val obj5 :
      val obj6 :
      val obj7 :
      val obj8 :
      val obj9 :
      val obj10 :
      val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding
      val array : 'a encoding -> 'a array encoding
      val list : 'a encoding -> 'a list encoding
      val assoc : 'a encoding -> (string * 'a) list encoding
      type 't case
      type case_tag = Tag of int | Json_only
      val case : case_tag -> 'a encoding -> ('t -> 'a option) -> ('a -> 't) -> 't case
      val union : ?tag_size:[ `Uint8 | `Uint16 ] -> 't case list -> 't encoding

   */

/* Basic functions for executing tests on a given input */
let roundtrip_json = (pp, ding, v) => {
  let json =
    try(Data_encoding.Json.construct(ding, v)) {
    | Invalid_argument(m) =>
      Crowbar.fail(Format.asprintf("Cannot construct: %a (%s)", pp, v, m))
    };

  let vv =
    try(Data_encoding.Json.destruct(ding, json)) {
    | [@implicit_arity] Data_encoding.Json.Cannot_destruct(_, _) =>
      Crowbar.fail("Cannot destruct")
    };

  Crowbar.check_eq(~pp, v, vv);
};

let roundtrip_binary = (pp, ding, v) => {
  let bin =
    try(Data_encoding.Binary.to_bytes_exn(ding, v)) {
    | Data_encoding.Binary.Write_error(we) =>
      Format.kasprintf(
        Crowbar.fail,
        "Cannot construct: %a (%a)",
        pp,
        v,
        Data_encoding.Binary.pp_write_error,
        we,
      )
    };

  let vv =
    try(Data_encoding.Binary.of_bytes_exn(ding, bin)) {
    | Data_encoding.Binary.Read_error(re) =>
      Format.kasprintf(
        Crowbar.fail,
        "Cannot destruct: %a (%a)",
        pp,
        v,
        Data_encoding.Binary.pp_read_error,
        re,
      )
    };

  Crowbar.check_eq(~pp, v, vv);
};

/* Setting up the actual tests */
let test_testable_json = (testable: testable) => {
  module T = (val testable);
  roundtrip_json(T.pp, T.ding, T.v);
};

let test_testable_binary = (testable: testable) => {
  module T = (val testable);
  roundtrip_binary(T.pp, T.ding, T.v);
};

let () = {
  Crowbar.add_test(~name="binary roundtrips", [gen], test_testable_binary);
  Crowbar.add_test(~name="json roundtrips", [gen], test_testable_json);
  ();
};
