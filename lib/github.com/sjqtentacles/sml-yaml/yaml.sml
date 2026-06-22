structure Yaml :> YAML =
struct
  datatype t
    = Null
    | Bool   of bool
    | Int    of IntInf.int
    | Float  of real
    | Str    of string
    | Seq    of t list
    | Map    of (string * t) list

  fun err msg = raise Fail ("YAML parse error: " ^ msg)

  (* ---- small string helpers ---------------------------------------- *)

  fun isWs c = c = #" " orelse c = #"\t"
  fun isSpace c = isWs c orelse c = #"\n" orelse c = #"\r"

  fun ltrim s =
    Substring.string (Substring.dropl isWs (Substring.full s))
  fun rtrim s =
    Substring.string (Substring.dropr isWs (Substring.full s))
  fun trim s = ltrim (rtrim s)

  (* Strip an unquoted trailing `#` comment. A `#` only starts a comment when
     at the start of the scalar or preceded by whitespace. Quotes are tracked
     so `#` inside a quoted scalar is preserved. *)
  fun stripComment s =
    let
      val n = String.size s
      fun go (i, prevWs, q) =
        if i >= n then n
        else
          let val c = String.sub (s, i) in
            case q of
              SOME qc =>
                if c = qc then go (i + 1, false, NONE)
                else go (i + 1, false, q)
            | NONE =>
                if c = #"#" andalso prevWs then i
                else if c = #"\"" orelse c = #"'" then go (i + 1, false, SOME c)
                else go (i + 1, isWs c, NONE)
          end
    in
      String.substring (s, 0, go (0, true, NONE))
    end

  (* ---- scalar interpretation --------------------------------------- *)

  fun parseDoubleQuoted s =
    (* s includes surrounding double quotes *)
    let
      val n = String.size s
      fun go (i, acc) =
        if i >= n then err "unterminated double-quoted string"
        else
          let val c = String.sub (s, i) in
            if c = #"\"" then (String.implode (List.rev acc), i + 1)
            else if c = #"\\" then
              if i + 1 >= n then err "dangling escape in double-quoted string"
              else
                let val e = String.sub (s, i + 1)
                    val r =
                      case e of
                        #"n" => #"\n" | #"t" => #"\t" | #"r" => #"\r"
                      | #"\\" => #"\\" | #"\"" => #"\"" | #"0" => #"\000"
                      | #"/" => #"/" | _ => e
                in go (i + 2, r :: acc) end
            else go (i + 1, c :: acc)
          end
    in
      if n < 1 orelse String.sub (s, 0) <> #"\"" then err "expected double quote"
      else
        let val (str, next) = go (1, [])
        in if next <> n then err "trailing data after double-quoted string"
           else str
        end
    end

  fun parseSingleQuoted s =
    (* single quotes; '' is an escaped quote *)
    let
      val n = String.size s
      fun go (i, acc) =
        if i >= n then err "unterminated single-quoted string"
        else
          let val c = String.sub (s, i) in
            if c = #"'" then
              if i + 1 < n andalso String.sub (s, i + 1) = #"'"
              then go (i + 2, #"'" :: acc)
              else (String.implode (List.rev acc), i + 1)
            else go (i + 1, c :: acc)
          end
    in
      if n < 1 orelse String.sub (s, 0) <> #"'" then err "expected single quote"
      else
        let val (str, next) = go (1, [])
        in if next <> n then err "trailing data after single-quoted string"
           else str
        end
    end

  fun looksLikeInt s =
    let val ss = if String.size s > 0 andalso
                    (String.sub (s, 0) = #"-" orelse String.sub (s, 0) = #"+")
                 then String.extract (s, 1, NONE) else s
    in ss <> "" andalso CharVector.all Char.isDigit ss end

  fun looksLikeFloat s =
    let
      val hasDot = CharVector.exists (fn c => c = #".") s
      val hasE = CharVector.exists (fn c => c = #"e" orelse c = #"E") s
      val ok = CharVector.all
                 (fn c => Char.isDigit c orelse c = #"." orelse c = #"-"
                          orelse c = #"+" orelse c = #"e" orelse c = #"E") s
      val hasDigit = CharVector.exists Char.isDigit s
    in ok andalso hasDigit andalso (hasDot orelse hasE) end

  (* Interpret a plain (unquoted, comment-stripped, trimmed) scalar. *)
  fun scalarOfPlain raw =
    let val s = trim raw in
      if s = "" then Null
      else if s = "null" orelse s = "Null" orelse s = "NULL" orelse s = "~"
        then Null
      else if s = "true" orelse s = "True" orelse s = "TRUE" then Bool true
      else if s = "false" orelse s = "False" orelse s = "FALSE" then Bool false
      else if looksLikeInt s then
        (case IntInf.fromString s of SOME i => Int i | NONE => Str s)
      else if looksLikeFloat s then
        (case Real.fromString s of SOME r => Float r | NONE => Str s)
      else Str s
    end

  (* Interpret a scalar token that may be quoted. `s` is already trimmed. *)
  fun scalarOfToken s =
    if s = "" then Null
    else
      let val c0 = String.sub (s, 0) in
        if c0 = #"\"" then Str (parseDoubleQuoted s)
        else if c0 = #"'" then Str (parseSingleQuoted s)
        else scalarOfPlain s
      end

  (* ---- flow style: [a, b]  and  {k: v} ------------------------------ *)
  (* A char-index recursive descent over the whole string `s`. Each `flowX`
     returns (value, nextIndex). *)

  fun skipWsAt (s, i) =
    let val n = String.size s
        fun go i = if i < n andalso isSpace (String.sub (s, i)) then go (i + 1) else i
    in go i end

  (* Read a flow scalar / key token up to one of the stop chars (at top level
     of the current flow container), honoring quotes. Returns (token, next). *)
  fun readFlowToken (s, i, stops) =
    let
      val n = String.size s
      fun isStop c = List.exists (fn x => x = c) stops
      fun go (j, acc, depthSq, depthBr) =
        if j >= n then (String.implode (List.rev acc), j)
        else
          let val c = String.sub (s, j) in
            if (c = #"\"" orelse c = #"'") then
              let val (raw, next) = readQuotedRaw (s, j)
              in go (next, List.rev (String.explode raw) @ acc, depthSq, depthBr) end
            else if depthSq = 0 andalso depthBr = 0 andalso isStop c then
              (String.implode (List.rev acc), j)
            else
              let
                val (dSq, dBr) =
                  case c of
                    #"[" => (depthSq + 1, depthBr)
                  | #"]" => (depthSq - 1, depthBr)
                  | #"{" => (depthSq, depthBr + 1)
                  | #"}" => (depthSq, depthBr - 1)
                  | _ => (depthSq, depthBr)
              in go (j + 1, c :: acc, dSq, dBr) end
          end
    in go (i, [], 0, 0) end

  (* Copy a quoted region verbatim (including the surrounding quotes) so the
     scalar interpreter can later decode escapes. Raises on no close. *)
  and readQuotedRaw (s, i) =
    let
      val n = String.size s
      val q = String.sub (s, i)
      fun go (j, acc) =
        if j >= n then err "unterminated quoted string in flow"
        else
          let val c = String.sub (s, j) in
            if q = #"'" andalso c = #"'" then
              if j + 1 < n andalso String.sub (s, j + 1) = #"'"
              then go (j + 2, #"'" :: #"'" :: acc)
              else (String.implode (List.rev (c :: acc)), j + 1)
            else if q = #"\"" andalso c = #"\\" then
              if j + 1 >= n then err "dangling escape in flow string"
              else go (j + 2, String.sub (s, j + 1) :: c :: acc)
            else if c = q then (String.implode (List.rev (c :: acc)), j + 1)
            else go (j + 1, c :: acc)
          end
    in go (i + 1, [String.sub (s, i)]) end

  fun flowValue (s, i) =
    let val i = skipWsAt (s, i)
        val n = String.size s
    in
      if i >= n then err "unexpected end of flow value"
      else
        case String.sub (s, i) of
          #"[" => flowSeq (s, i + 1)
        | #"{" => flowMap (s, i + 1)
        | _ =>
            let val (tok, next) = readFlowToken (s, i, [#",", #"]", #"}"])
                val tk = trim tok
                val () = if tk = "" then err "empty value in flow collection" else ()
                val v = scalarOfToken tk
            in (v, next) end
    end

  and flowSeq (s, i) =
    let
      val n = String.size s
      fun loop (i, acc) =
        let val i = skipWsAt (s, i) in
          if i >= n then err "unterminated flow sequence"
          else if String.sub (s, i) = #"]" then (Seq (List.rev acc), i + 1)
          else
            let val (v, j) = flowValue (s, i)
                val j = skipWsAt (s, j)
            in
              if j >= n then err "unterminated flow sequence"
              else
                case String.sub (s, j) of
                  #"," => loop (j + 1, v :: acc)
                | #"]" => (Seq (List.rev (v :: acc)), j + 1)
                | _ => err "expected ',' or ']' in flow sequence"
            end
        end
    in
      (* allow an immediate close for [] *)
      let val k = skipWsAt (s, i) in
        if k < n andalso String.sub (s, k) = #"]" then (Seq [], k + 1)
        else loop (i, [])
      end
    end

  and flowMap (s, i) =
    let
      val n = String.size s
      fun loop (i, acc) =
        let val i = skipWsAt (s, i) in
          if i >= n then err "unterminated flow mapping"
          else if String.sub (s, i) = #"}" then (Map (List.rev acc), i + 1)
          else
            let
              val (ktok, jk) = readFlowToken (s, i, [#":"])
              val key = flowKeyString (trim ktok)
              val jk = skipWsAt (s, jk)
              val () = if jk < n andalso String.sub (s, jk) = #":" then ()
                       else err "expected ':' in flow mapping"
              val (v, jv) = flowValue (s, jk + 1)
              val jv = skipWsAt (s, jv)
            in
              if jv >= n then err "unterminated flow mapping"
              else
                case String.sub (s, jv) of
                  #"," => loop (jv + 1, (key, v) :: acc)
                | #"}" => (Map (List.rev ((key, v) :: acc)), jv + 1)
                | _ => err "expected ',' or '}' in flow mapping"
            end
        end
    in
      let val k = skipWsAt (s, i) in
        if k < n andalso String.sub (s, k) = #"}" then (Map [], k + 1)
        else loop (i, [])
      end
    end

  (* A flow/mapping key as a string; decode quotes but keep scalars textual. *)
  and flowKeyString s =
    if s = "" then err "empty key in flow mapping"
    else
      let val c0 = String.sub (s, 0) in
        if c0 = #"\"" then parseDoubleQuoted s
        else if c0 = #"'" then parseSingleQuoted s
        else s
      end

  (* Parse a whole string as a flow value, requiring all input consumed. *)
  fun parseFlow s =
    let val (v, j) = flowValue (s, 0)
        val j = skipWsAt (s, j)
    in if j <> String.size s then err "trailing data after flow value" else v end

  (* Interpret an inline value string (right of `key:`, or a `- ` item, or a
     whole single-line document): flow collection, quoted, or plain scalar. *)
  fun inlineValue raw =
    let val s = trim raw in
      if s = "" then Null
      else
        let val c0 = String.sub (s, 0) in
          if c0 = #"[" orelse c0 = #"{" then parseFlow s
          else scalarOfToken s
        end
    end

  (* ---- block style: indentation-driven recursive descent ------------ *)

  (* Indentation (count of leading spaces; tabs treated as a single space) and
     the remaining content of a raw source line. Blank / comment-only lines
     are filtered out before block parsing. *)
  type line = { indent : int, text : string }

  fun countIndent s =
    let val n = String.size s
        fun go i = if i < n andalso String.sub (s, i) = #" " then go (i + 1) else i
    in go 0 end

  fun splitLines s = String.fields (fn c => c = #"\n") s

  (* Drop trailing CR, strip comments, compute indent; keep only meaningful. *)
  fun toLines src =
    let
      fun clean raw =
        let val raw = if String.size raw > 0
                         andalso String.sub (raw, String.size raw - 1) = #"\r"
                      then String.substring (raw, 0, String.size raw - 1)
                      else raw
            val noComment = stripComment raw
            val indent = countIndent noComment
            val text = trim noComment
        in { indent = indent, text = text } end
      val all = List.map clean (splitLines src)
    in List.filter (fn {text, ...} => text <> "") all end

  fun isSeqLine (ln : line) =
    let val t = #text ln in
      t = "-" orelse (String.size t >= 2 andalso String.sub (t, 0) = #"-"
                      andalso String.sub (t, 1) = #" ")
    end

  (* Find the `:` that separates a block mapping key from its value, ignoring
     colons inside quotes/flow collections. Returns SOME idx or NONE. *)
  fun findMapColon t =
    let
      val n = String.size t
      fun go (i, depth, q) =
        if i >= n then NONE
        else
          let val c = String.sub (t, i) in
            case q of
              SOME qc => if c = qc then go (i + 1, depth, NONE)
                         else go (i + 1, depth, q)
            | NONE =>
                if c = #"\"" orelse c = #"'" then go (i + 1, depth, SOME c)
                else if c = #"[" orelse c = #"{" then go (i + 1, depth + 1, q)
                else if c = #"]" orelse c = #"}" then go (i + 1, depth - 1, q)
                else if c = #":" andalso depth = 0
                        andalso (i + 1 >= n orelse isWs (String.sub (t, i + 1)))
                then SOME i
                else go (i + 1, depth, q)
          end
    in go (0, 0, NONE) end

  (* Parse a block of lines, all logically at >= `minIndent`. Returns the
     value formed by the lines whose indent equals the block's base indent. *)
  fun parseBlock (lines : line list) : t =
    case lines of
      [] => Null
    | (first :: _) =>
        let val base = #indent first in
          if isSeqLine first then parseBlockSeq (base, lines)
          else parseBlockMap (base, lines)
        end

  (* Group lines into the first item at `base` plus its (more-indented) body,
     returning (bodyLines, rest). *)
  and takeChildren (base, lines) =
    let
      fun go (acc, []) = (List.rev acc, [])
        | go (acc, (ln :: rest)) =
            if #indent ln > base then go (ln :: acc, rest)
            else (List.rev acc, ln :: rest)
    in go ([], lines) end

  and parseBlockSeq (base, lines) =
    let
      fun loop (acc, []) = Seq (List.rev acc)
        | loop (acc, (ln :: rest)) =
            if #indent ln <> base then err "inconsistent indentation in sequence"
            else if not (isSeqLine ln) then err "expected sequence item '-'"
            else
              let
                val t = #text ln
                val itemText = if t = "-" then "" else trim (String.extract (t, 2, NONE))
                val (children, rest') = takeChildren (base, rest)
              in
                if itemText = "" then
                  (* value is the nested block on following lines *)
                  let val v = if null children then Null else parseBlock children
                  in loop (v :: acc, rest') end
                else if String.size itemText > 0
                        andalso (String.sub (itemText, 0) = #"[")
                  then loop (inlineValue itemText :: acc, joinBack (children, rest'))
                else if String.size itemText > 0
                        andalso (String.sub (itemText, 0) = #"{")
                  then loop (inlineValue itemText :: acc, joinBack (children, rest'))
                else
                  (* item may itself begin a mapping/seq inline: "- key: v" *)
                  (case findMapColon itemText of
                     SOME _ =>
                       (* re-create a virtual block: the inline part plus the
                          children, all re-based at a deeper indent *)
                       let
                         val virtualIndent = base + 2
                         val head = { indent = virtualIndent, text = itemText }
                         val v = parseBlock (head :: children)
                       in loop (v :: acc, rest') end
                   | NONE =>
                       if null children
                       then loop (inlineValue itemText :: acc, rest')
                       else err "unexpected indented block after scalar item")
              end
      and joinBack (children, rest) =
        (* scalar/flow items take no children; push them back as siblings *)
        children @ rest
    in loop ([], lines) end

  and parseBlockMap (base, lines) =
    let
      fun loop (acc, []) = Map (List.rev acc)
        | loop (acc, (ln :: rest)) =
            if #indent ln <> base then err "inconsistent indentation in mapping"
            else if isSeqLine ln then err "unexpected sequence item in mapping"
            else
              let
                val t = #text ln
              in
                case findMapColon t of
                  NONE => err ("expected 'key: value' but got: " ^ t)
                | SOME ci =>
                    let
                      val keyRaw = trim (String.substring (t, 0, ci))
                      val key = flowKeyString keyRaw
                      val valRaw =
                        if ci + 1 >= String.size t then ""
                        else trim (String.extract (t, ci + 1, NONE))
                      val (children, rest') = takeChildren (base, rest)
                    in
                      if valRaw <> "" then
                        (* inline value; children (if any) would be illegal here
                           unless valRaw is empty, so treat children as siblings
                           only when there are none *)
                        if null children then
                          loop ((key, inlineValue valRaw) :: acc, rest')
                        else err "unexpected indented block after inline value"
                      else
                        let val v = if null children then Null
                                    else parseBlock children
                        in loop ((key, v) :: acc, rest') end
                    end
              end
    in loop ([], lines) end

  (* ---- document-level driver --------------------------------------- *)

  fun parseDoc src =
    let val lines = toLines src in
      case lines of
        [] => Null
      | [ln] =>
          let val t = #text ln in
            if isSeqLine ln then parseBlock lines
            else (case findMapColon t of
                    SOME _ => parseBlock lines
                  | NONE => inlineValue t)
          end
      | _ => parseBlock lines
    end

  fun parse src = parseDoc src

  (* Split on lines that are exactly `---` (after trimming). *)
  fun parseAll src =
    let
      val rawLines = splitLines src
      fun isSep raw =
        let val l = trim (if String.size raw > 0
                             andalso String.sub (raw, String.size raw - 1) = #"\r"
                          then String.substring (raw, 0, String.size raw - 1)
                          else raw)
        in l = "---" end
      fun group ([], cur, acc) = List.rev (List.rev cur :: acc)
        | group (l :: ls, cur, acc) =
            if isSep l then group (ls, [], List.rev cur :: acc)
            else group (ls, l :: cur, acc)
      val groups = group (rawLines, [], [])
      val texts = List.map (String.concatWith "\n") groups
      val docs = List.filter (fn s => trim s <> "") texts
    in
      case docs of [] => [parse ""] | _ => List.map parse docs
    end

  (* ---- serialization ----------------------------------------------- *)

  fun needsQuote s =
    s = "" orelse s = "null" orelse s = "true" orelse s = "false" orelse s = "~"
    orelse looksLikeInt s orelse looksLikeFloat s
    orelse CharVector.exists
             (fn c => c = #":" orelse c = #"#" orelse c = #"[" orelse c = #"]"
                      orelse c = #"{" orelse c = #"}" orelse c = #","
                      orelse c = #"\"" orelse c = #"'" orelse c = #"\n"
                      orelse c = #"\t") s
    orelse (String.sub (s, 0) = #"-")
    orelse isWs (String.sub (s, 0))
    orelse isWs (String.sub (s, String.size s - 1))

  fun quoteStr s =
    let
      fun esc c =
        case c of
          #"\"" => "\\\"" | #"\\" => "\\\\" | #"\n" => "\\n"
        | #"\t" => "\\t" | #"\r" => "\\r" | _ => String.str c
    in "\"" ^ String.concat (List.map esc (String.explode s)) ^ "\"" end

  fun scalarToString v =
    case v of
      Null => "null"
    | Bool true => "true"
    | Bool false => "false"
    | Int i => IntInf.toString i
    | Float r => Real.toString r
    | Str s => if needsQuote s then quoteStr s else s
    | _ => raise Fail "scalarToString: not a scalar"

  fun isScalar (Seq _) = false
    | isScalar (Map _) = false
    | isScalar _ = true

  fun isEmptyColl (Seq []) = true
    | isEmptyColl (Map []) = true
    | isEmptyColl _ = false

  fun indentStr n = CharVector.tabulate (n, fn _ => #" ")

  fun scalarOrEmpty (Seq []) = "[]"
    | scalarOrEmpty (Map []) = "{}"
    | scalarOrEmpty v = scalarToString v

  fun emit (v, ind) =
    case v of
      Seq [] => indentStr ind ^ "[]\n"
    | Map [] => indentStr ind ^ "{}\n"
    | Seq xs => String.concat (List.map (fn x => emitSeqItem (x, ind)) xs)
    | Map kvs => String.concat (List.map (fn kv => emitMapEntry (kv, ind)) kvs)
    | _ => indentStr ind ^ scalarToString v ^ "\n"

  and emitSeqItem (x, ind) =
    if isScalar x orelse isEmptyColl x then
      indentStr ind ^ "- " ^ scalarOrEmpty x ^ "\n"
    else
      indentStr ind ^ "-\n" ^ emit (x, ind + 2)

  and emitMapEntry ((k, v), ind) =
    let val key = if needsQuote k then quoteStr k else k in
      if isScalar v orelse isEmptyColl v then
        indentStr ind ^ key ^ ": " ^ scalarOrEmpty v ^ "\n"
      else
        indentStr ind ^ key ^ ":\n" ^ emit (v, ind + 2)
    end

  fun toString v =
    case v of
      Seq [] => "[]"
    | Map [] => "{}"
    | Seq _ => rtrim (emit (v, 0))
    | Map _ => rtrim (emit (v, 0))
    | _ => scalarToString v

  (* Indentation-step-configurable emitter (mirrors `emit` but uses `step`
     spaces per level instead of a fixed 2). *)
  fun emitN (step, v, ind) =
    case v of
      Seq [] => indentStr ind ^ "[]\n"
    | Map [] => indentStr ind ^ "{}\n"
    | Seq xs => String.concat (List.map (fn x => emitSeqItemN (step, x, ind)) xs)
    | Map kvs => String.concat (List.map (fn kv => emitMapEntryN (step, kv, ind)) kvs)
    | _ => indentStr ind ^ scalarToString v ^ "\n"

  and emitSeqItemN (step, x, ind) =
    if isScalar x orelse isEmptyColl x then
      indentStr ind ^ "- " ^ scalarOrEmpty x ^ "\n"
    else
      indentStr ind ^ "-\n" ^ emitN (step, x, ind + step)

  and emitMapEntryN (step, (k, v), ind) =
    let val key = if needsQuote k then quoteStr k else k in
      if isScalar v orelse isEmptyColl v then
        indentStr ind ^ key ^ ": " ^ scalarOrEmpty v ^ "\n"
      else
        indentStr ind ^ key ^ ":\n" ^ emitN (step, v, ind + step)
    end

  fun toStringIndent step v =
    case v of
      Seq [] => "[]"
    | Map [] => "{}"
    | Seq _ => rtrim (emitN (step, v, 0))
    | Map _ => rtrim (emitN (step, v, 0))
    | _ => scalarToString v

  (* ---- JSON bridge -------------------------------------------------- *)

  datatype json
    = JNull
    | JBool  of bool
    | JInt   of IntInf.int
    | JFloat of real
    | JStr   of string
    | JArr   of json list
    | JObj   of (string * json) list

  fun toJson v =
    case v of
      Null    => JNull
    | Bool b  => JBool b
    | Int i   => JInt i
    | Float r => JFloat r
    | Str s   => JStr s
    | Seq xs  => JArr (List.map toJson xs)
    | Map kvs => JObj (List.map (fn (k, v) => (k, toJson v)) kvs)

  fun fromJson j =
    case j of
      JNull    => Null
    | JBool b  => Bool b
    | JInt i   => Int i
    | JFloat r => Float r
    | JStr s   => Str s
    | JArr xs  => Seq (List.map fromJson xs)
    | JObj kvs => Map (List.map (fn (k, v) => (k, fromJson v)) kvs)

  (* SML prints a leading "~" for negative numbers; JSON requires "-". *)
  fun fixSign s =
    if String.size s > 0 andalso String.sub (s, 0) = #"~"
    then "-" ^ String.extract (s, 1, NONE) else s

  fun jsonQuote s =
    let
      fun esc c =
        case c of
          #"\"" => "\\\""
        | #"\\" => "\\\\"
        | #"\n" => "\\n"
        | #"\t" => "\\t"
        | #"\r" => "\\r"
        | #"\b" => "\\b"
        | #"\f" => "\\f"
        | _ =>
            if Char.ord c < 0x20 then
              let val h = Int.fmt StringCvt.HEX (Char.ord c)
              in "\\u" ^ StringCvt.padLeft #"0" 4 h end
            else String.str c
    in "\"" ^ String.concat (List.map esc (String.explode s)) ^ "\"" end

  fun jsonToString j =
    case j of
      JNull    => "null"
    | JBool b  => if b then "true" else "false"
    | JInt i   => fixSign (IntInf.toString i)
    | JFloat r => fixSign (Real.toString r)
    | JStr s   => jsonQuote s
    | JArr xs  => "[" ^ String.concatWith "," (List.map jsonToString xs) ^ "]"
    | JObj kvs =>
        "{" ^ String.concatWith ","
                (List.map (fn (k, v) => jsonQuote k ^ ":" ^ jsonToString v) kvs)
        ^ "}"

  fun toJsonString v = jsonToString (toJson v)
end
