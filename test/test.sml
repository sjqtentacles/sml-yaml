(* test.sml — structural tests for sml-yaml on top of the Harness. *)
structure Tests =
struct
  open Harness

  structure Y = Yaml

  (* Structural equality on Yaml.t, with epsilon comparison for floats so that
     parse/serialize round-trips don't fail on tiny representation drift. *)
  fun feq (a, b) =
    Real.== (a, b) orelse
    (let val d = Real.abs (a - b)
         val m = Real.max (Real.abs a, Real.abs b)
     in d <= 1e~9 orelse d <= m * 1e~9 end)

  fun eq (Y.Null, Y.Null) = true
    | eq (Y.Bool a, Y.Bool b) = a = b
    | eq (Y.Int a, Y.Int b) = a = b
    | eq (Y.Float a, Y.Float b) = feq (a, b)
    | eq (Y.Str a, Y.Str b) = a = b
    | eq (Y.Seq a, Y.Seq b) = eqList (a, b)
    | eq (Y.Map a, Y.Map b) = eqMap (a, b)
    | eq _ = false
  and eqList ([], []) = true
    | eqList (x :: xs, y :: ys) = eq (x, y) andalso eqList (xs, ys)
    | eqList _ = false
  and eqMap ([], []) = true
    | eqMap ((k1, v1) :: xs, (k2, v2) :: ys) =
        k1 = k2 andalso eq (v1, v2) andalso eqMap (xs, ys)
    | eqMap _ = false

  fun toDebugString v =
    case v of
      Y.Null => "Null"
    | Y.Bool b => "Bool " ^ Bool.toString b
    | Y.Int i => "Int " ^ IntInf.toString i
    | Y.Float r => "Float " ^ Real.toString r
    | Y.Str s => "Str \"" ^ s ^ "\""
    | Y.Seq xs => "Seq [" ^ String.concatWith ", " (List.map toDebugString xs) ^ "]"
    | Y.Map kvs =>
        "Map [" ^ String.concatWith ", "
          (List.map (fn (k, v) => "(\"" ^ k ^ "\", " ^ toDebugString v ^ ")") kvs)
          ^ "]"

  (* The Harness only exports `check`, so encode pass/fail in terms of it,
     folding any detail into the test name for readable failure output. *)
  fun pass name = Harness.check name true
  fun fail name detail =
    Harness.check (name ^ (if detail = "" then "" else ": " ^ detail)) false

  (* check that `parse input` yields a value structurally equal to `expected`. *)
  fun checkParse name (input, expected) =
    (let val actual = Y.parse input
     in if eq (actual, expected) then pass name
        else fail name (toDebugString expected ^ " <> " ^ toDebugString actual)
     end)
    handle e => fail name ("raised " ^ exnMessage e)

  fun checkVal name (actual, expected) =
    if eq (actual, expected) then pass name
    else fail name (toDebugString expected ^ " <> " ^ toDebugString actual)

  fun run () =
    let
      (* 1. scalars *)
      val () = section "scalars"
      val () = checkParse "null" ("null", Y.Null)
      val () = checkParse "tilde-null" ("~", Y.Null)
      val () = checkParse "true" ("true", Y.Bool true)
      val () = checkParse "false" ("false", Y.Bool false)
      val () = checkParse "int" ("42", Y.Int 42)
      val () = checkParse "neg-int" ("-7", Y.Int (~7))
      val () = checkParse "float" ("3.14", Y.Float 3.14)
      val () = checkParse "plain-string" ("hello", Y.Str "hello")
      val () = checkParse "double-quoted" ("\"quoted\"", Y.Str "quoted")
      val () = checkParse "single-quoted" ("'single'", Y.Str "single")
      val () = checkParse "dq-escape" ("\"a\\nb\"", Y.Str "a\nb")
      val () = checkParse "multiword-plain" ("hello world", Y.Str "hello world")

      (* 2. block mappings *)
      val () = section "block mappings"
      val () = checkParse "simple-map"
                 ("name: Alice\nage: 30",
                  Y.Map [("name", Y.Str "Alice"), ("age", Y.Int 30)])
      val () = checkParse "map-quoted-val"
                 ("greeting: \"hi there\"",
                  Y.Map [("greeting", Y.Str "hi there")])
      val () = checkParse "map-null-val"
                 ("a:\nb: 2",
                  Y.Map [("a", Y.Null), ("b", Y.Int 2)])

      (* 3. block sequences *)
      val () = section "block sequences"
      val () = checkParse "simple-seq"
                 ("- a\n- b\n- c", Y.Seq [Y.Str "a", Y.Str "b", Y.Str "c"])
      val () = checkParse "int-seq"
                 ("- 1\n- 2\n- 3", Y.Seq [Y.Int 1, Y.Int 2, Y.Int 3])

      (* 4. nested block *)
      val () = section "nested block"
      val () = checkParse "nested"
                 ("person:\n  name: Bob\n  scores:\n    - 1\n    - 2",
                  Y.Map [("person",
                    Y.Map [("name", Y.Str "Bob"),
                           ("scores", Y.Seq [Y.Int 1, Y.Int 2])])])
      val () = checkParse "seq-of-maps"
                 ("- name: a\n  v: 1\n- name: b\n  v: 2",
                  Y.Seq [Y.Map [("name", Y.Str "a"), ("v", Y.Int 1)],
                         Y.Map [("name", Y.Str "b"), ("v", Y.Int 2)]])

      (* 5. flow + comments + multi-doc *)
      val () = section "flow / comments / multi-doc"
      val () = checkParse "flow-map-seq"
                 ("{a: 1, b: [2, 3]}",
                  Y.Map [("a", Y.Int 1), ("b", Y.Seq [Y.Int 2, Y.Int 3])])
      val () = checkParse "flow-seq"
                 ("[1, 2, 3]", Y.Seq [Y.Int 1, Y.Int 2, Y.Int 3])
      val () = checkParse "flow-nested"
                 ("[[1, 2], [3]]",
                  Y.Seq [Y.Seq [Y.Int 1, Y.Int 2], Y.Seq [Y.Int 3]])
      val () = checkParse "empty-flow-seq" ("[]", Y.Seq [])
      val () = checkParse "empty-flow-map" ("{}", Y.Map [])
      val () = checkParse "comment-trailing"
                 ("x: 1 # comment", Y.Map [("x", Y.Int 1)])
      val () = checkParse "comment-full-line"
                 ("# just a comment\ny: 2", Y.Map [("y", Y.Int 2)])
      val () = checkInt "multi-doc-count"
                 (2, List.length (Y.parseAll "---\na: 1\n---\nb: 2"))
      val () = checkInt "multi-doc-leading"
                 (2, List.length (Y.parseAll "a: 1\n---\nb: 2"))
      val () =
        let val docs = Y.parseAll "---\na: 1\n---\nb: 2"
        in checkVal "multi-doc-first" (hd docs, Y.Map [("a", Y.Int 1)]);
           checkVal "multi-doc-second" (hd (tl docs), Y.Map [("b", Y.Int 2)])
        end

      (* 6. roundtrip + errors *)
      val () = section "roundtrip / errors"
      fun roundtrip name v =
        let val s = Y.toString v
            val v2 = Y.parse s
        in if eq (v, v2) then pass name
           else fail name (toDebugString v ^ " ~> " ^ s ^ " ~> " ^ toDebugString v2)
        end handle e => fail name ("raised " ^ exnMessage e ^ " on [" ^ Y.toString v ^ "]")
      val () = roundtrip "rt-null" Y.Null
      val () = roundtrip "rt-bool" (Y.Bool true)
      val () = roundtrip "rt-int" (Y.Int 12345)
      val () = roundtrip "rt-float" (Y.Float 3.14)
      val () = roundtrip "rt-str" (Y.Str "hello world")
      val () = roundtrip "rt-seq" (Y.Seq [Y.Int 1, Y.Str "a", Y.Bool false])
      val () = roundtrip "rt-map"
                 (Y.Map [("name", Y.Str "Alice"), ("age", Y.Int 30),
                         ("active", Y.Bool true)])
      val () = roundtrip "rt-nested"
                 (Y.Map [("person",
                    Y.Map [("name", Y.Str "Bob"),
                           ("scores", Y.Seq [Y.Int 1, Y.Int 2])])])
      val () = roundtrip "rt-empty-seq" (Y.Seq [])
      val () = roundtrip "rt-empty-map" (Y.Map [])
      val () = checkRaises "err-empty-flow-val" (fn () => Y.parse "{a: }")
      val () = checkRaises "err-unterminated-dq" (fn () => Y.parse "\"oops")
      val () = checkRaises "err-unterminated-sq" (fn () => Y.parse "'oops")
      val () = checkRaises "err-unterminated-flow-seq" (fn () => Y.parse "[1, 2")
      val () = checkRaises "err-unterminated-flow-map" (fn () => Y.parse "{a: 1")

      (* 7. toStringIndent *)
      val () = section "toStringIndent"
      val nested =
        Y.Map [("person",
          Y.Map [("name", Y.Str "Bob"),
                 ("scores", Y.Seq [Y.Int 1, Y.Int 2])])]
      val () = checkString "indent 2 == toString"
                 (Y.toString nested, Y.toStringIndent 2 nested)
      val () = checkString "indent 4 layout"
                 ("person:\n    name: Bob\n    scores:\n        - 1\n        - 2\n",
                  Y.toStringIndent 4 nested)
      val () =
        let val v2 = Y.parse (Y.toStringIndent 4 nested)
        in checkVal "indent 4 re-parses" (v2, nested) end

      (* 8. JSON bridge *)
      val () = section "json bridge"
      (* scalar typing carried over verbatim (json holds a real, so it is not
         an equality type: compare via constructor tag + jsonToString). *)
      fun jtag j =
        case j of
          Y.JNull => "null" | Y.JBool _ => "bool" | Y.JInt _ => "int"
        | Y.JFloat _ => "float" | Y.JStr _ => "str"
        | Y.JArr _ => "arr" | Y.JObj _ => "obj"
      val () = checkString "type true"  ("bool", jtag (Y.toJson (Y.Bool true)))
      val () = checkString "type false" ("bool", jtag (Y.toJson (Y.Bool false)))
      val () = checkString "type null"  ("null", jtag (Y.toJson Y.Null))
      val () = checkString "type int"   ("int", jtag (Y.toJson (Y.Int 123)))
      val () = checkString "type float" ("float", jtag (Y.toJson (Y.Float 1.5)))
      val () = checkString "type str"   ("str", jtag (Y.toJson (Y.Str "str")))
      val () = checkString "json true"  ("true", Y.jsonToString (Y.toJson (Y.Bool true)))
      val () = checkString "json false" ("false", Y.jsonToString (Y.toJson (Y.Bool false)))
      val () = checkString "json null"  ("null", Y.jsonToString (Y.toJson Y.Null))
      val () = checkString "json int"   ("123", Y.jsonToString (Y.toJson (Y.Int 123)))
      val () = checkString "json float" ("1.5", Y.jsonToString (Y.toJson (Y.Float 1.5)))
      val () = checkString "json str"   ("\"str\"", Y.jsonToString (Y.toJson (Y.Str "str")))
      (* a known small YAML maps to a known JSON string (no floats: exact bytes) *)
      val doc = Y.parse "name: Alice\nage: 30\nactive: true\ntags:\n  - x\n  - y\nnote: ~"
      val () = checkString "yaml -> json string"
                 ("{\"name\":\"Alice\",\"age\":30,\"active\":true,\"tags\":[\"x\",\"y\"],\"note\":null}",
                  Y.toJsonString doc)
      val () = checkString "negative int json" ("-7", Y.toJsonString (Y.Int (~7)))
      val () = checkString "json string escapes" ("\"a\\nb\"", Y.toJsonString (Y.Str "a\nb"))
      (* round-trip a representative doc YAML -> JSON -> YAML preserving structure *)
      val rep =
        Y.Map [("n", Y.Int 1), ("f", Y.Float 1.5), ("b", Y.Bool false),
               ("z", Y.Null), ("s", Y.Str "hi"),
               ("arr", Y.Seq [Y.Int 1, Y.Str "two", Y.Bool true]),
               ("obj", Y.Map [("k", Y.Str "v")])]
      val () = checkVal "fromJson o toJson = id" (Y.fromJson (Y.toJson rep), rep)
    in
      Harness.run ()
    end
end
