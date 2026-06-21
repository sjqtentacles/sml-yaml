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
end
