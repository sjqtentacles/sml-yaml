# sml-yaml

Subset YAML 1.2 parser (block + flow, multi-doc) in pure Standard ML.

The value type mirrors [`sml-json`](https://github.com/sjqtentacles/sml-json)'s
`Json.t` (`Null` / `Bool` / `Int` / `Float` / `Str` plus `Seq` / `Map`
containers), so the two libraries interoperate cleanly. Builds and tests pass on
both **MLton** and **Poly/ML**.

## Features

- Scalars: plain, single-quoted (`'...'`), and double-quoted (`"..."` with
  `\n \t \r \\ \" \0` escapes).
- Automatic scalar typing: `null` / `~`, `true` / `false`, integers
  (`IntInf.int`), floats, otherwise a string.
- Block mappings (`key: value`) and block sequences (`- item`), nested by
  indentation (any consistent indent width).
- Flow collections: sequences `[a, b]` and mappings `{k: v}`, arbitrarily
  nested.
- `#` comments to end of line (respecting quotes).
- `---` document separators via `parseAll`.
- A re-parseable serializer (`toString`).

Not supported (by design): anchors/aliases, tags, block scalars (`|` / `>`),
and complex (non-string) keys.

## Installation

```
smlpkg add github.com/sjqtentacles/sml-yaml
smlpkg sync
```

This library declares a dependency on
[`sml-parsec`](https://github.com/sjqtentacles/sml-parsec), which is vendored
under `lib/`.

Add the library's MLB to your own `sources.mlb`:

```
$(SML_LIB)/basis/basis.mlb
lib/github.com/sjqtentacles/sml-yaml/sources.mlb
```

## API

```sml
signature YAML =
sig
  datatype t
    = Null
    | Bool   of bool
    | Int    of IntInf.int
    | Float  of real
    | Str    of string
    | Seq    of t list
    | Map    of (string * t) list

  val parse     : string -> t        (* single document; raises Fail on error *)
  val parseAll  : string -> t list   (* multi-document; split on "---" lines  *)
  val toString  : t -> string        (* re-parseable serializer               *)
  val toStringIndent : int -> t -> string   (* configurable indent step       *)

  (* JSON bridge (self-contained AST; no sml-json dependency). *)
  datatype json
    = JNull | JBool of bool | JInt of IntInf.int | JFloat of real
    | JStr of string | JArr of json list | JObj of (string * json) list
  val toJson       : t -> json
  val fromJson     : json -> t
  val jsonToString : json -> string  (* compact, deterministic JSON text      *)
  val toJsonString : t -> string     (* = jsonToString o toJson               *)
end
structure Yaml :> YAML
```

## Usage

```sml
(* scalars *)
val Yaml.Null      = Yaml.parse "null"      (* also "~" *)
val Yaml.Bool true = Yaml.parse "true"
val Yaml.Int  i    = Yaml.parse "42"
val Yaml.Float f   = Yaml.parse "3.14"
val Yaml.Str  s    = Yaml.parse "'single'"

(* block mapping *)
val Yaml.Map [("name", Yaml.Str "Alice"), ("age", Yaml.Int 30)] =
  Yaml.parse "name: Alice\nage: 30"

(* block sequence *)
val Yaml.Seq [Yaml.Str "a", Yaml.Str "b", Yaml.Str "c"] =
  Yaml.parse "- a\n- b\n- c"

(* nested block style *)
val Yaml.Map [("person",
      Yaml.Map [("name",   Yaml.Str "Bob"),
                ("scores", Yaml.Seq [Yaml.Int 1, Yaml.Int 2])])] =
  Yaml.parse "person:\n  name: Bob\n  scores:\n    - 1\n    - 2"

(* flow style *)
val Yaml.Map [("a", Yaml.Int 1), ("b", Yaml.Seq [Yaml.Int 2, Yaml.Int 3])] =
  Yaml.parse "{a: 1, b: [2, 3]}"

(* trailing comment *)
val Yaml.Map [("x", Yaml.Int 1)] = Yaml.parse "x: 1 # comment"

(* multiple documents *)
val [doc1, doc2] = Yaml.parseAll "---\na: 1\n---\nb: 2"

(* round-trip: parse (toString v) is structurally equal to v *)
val v = Yaml.Map [("name", Yaml.Str "Alice"), ("age", Yaml.Int 30)]
val () = if Yaml.parse (Yaml.toString v) = v then () else raise Fail "oops"
```

### JSON bridge & pretty-printing

`toJson`/`fromJson` map the YAML value AST onto a self-contained JSON AST 1:1
(this library has no `sml-json` dependency). YAML scalar typing is carried over
verbatim: `null`→`JNull`, `true`/`false`→`JBool`, integers→`JInt`,
floats→`JFloat`, strings→`JStr`; sequences→`JArr` (arrays) and mappings→`JObj`
(objects, key order preserved). `jsonToString` emits compact JSON with no
insignificant whitespace (integer/string output is byte-identical across
compilers). `toStringIndent n` is `toString` with `n` spaces per nesting level.

```sml
val doc = Yaml.parse "name: Alice\nage: 30\nactive: true\ntags:\n  - x\n  - y\nnote: ~"
Yaml.toJsonString doc
  (* {"name":"Alice","age":30,"active":true,"tags":["x","y"],"note":null} *)

Yaml.toStringIndent 4 (Yaml.parse "a:\n  b: 1")   (* 4-space indentation *)

(* JSON -> YAML AST -> JSON round-trips structurally *)
val same = Yaml.fromJson (Yaml.toJson doc)
```

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
make all-tests  # both
```

## License

MIT
