(* yaml.sig

   A subset YAML 1.2 parser and serializer for Standard ML. The value type
   `t` deliberately mirrors sml-json's `Json.t` so the two libraries
   interoperate cleanly (Null/Bool/Int/Float/Str plus Seq/Map containers).

   Supported: plain / single-quoted / double-quoted scalars; block mappings
   (`key: value`); block sequences (`- item`); flow sequences (`[a, b]`); flow
   mappings (`{k: v}`); `#` comments to end of line; `---` document separators;
   and scalar auto-detection (null / true / false / int / float / else string).

   NOT supported: anchors/aliases, tags, block scalars (`|` / `>`), and complex
   (non-string) keys. Indentation defines nesting for block style; any
   consistent indent width is accepted. *)

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

  (* Parse a single YAML document. Raises `Fail` on a parse error. *)
  val parse     : string -> t

  (* Parse a multi-document stream, split on lines that are exactly `---`. *)
  val parseAll  : string -> t list

  (* Serialize back to YAML text. The result re-parses (via `parse`) to a
     structurally equal value. *)
  val toString  : t -> string

  (* Like `toString`, but with a configurable indentation step (number of
     spaces per nesting level). `toStringIndent 2` equals `toString`. *)
  val toStringIndent : int -> t -> string

  (* ---- JSON bridge -------------------------------------------------- *)

  (* A self-contained JSON value AST (this repo does not depend on sml-json).
     The bridge maps YAML scalars/containers onto JSON 1:1:
       Null  <-> JNull          Bool  <-> JBool
       Int   <-> JInt           Float <-> JFloat
       Str   <-> JStr           Seq   <-> JArr (arrays)
       Map   <-> JObj (objects, key order preserved).
     YAML scalar typing (null / true / false / integer / float / string) is
     therefore carried over verbatim. *)
  datatype json
    = JNull
    | JBool  of bool
    | JInt   of IntInf.int
    | JFloat of real
    | JStr   of string
    | JArr   of json list
    | JObj   of (string * json) list

  val toJson   : t -> json
  val fromJson : json -> t

  (* Compact, deterministic JSON text (no insignificant whitespace). Integer
     and string output is byte-identical across compilers; floats use the
     Basis `Real.toString` formatting. *)
  val jsonToString : json -> string
  (* Convenience: `jsonToString o toJson`. *)
  val toJsonString : t -> string
end
