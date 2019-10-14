open Unicode.UString

module CharSet = Set.Make (Char)
module StringSet = Set.Make (Utf8String)
module StringMap = Map.Make (Utf8String)
module TerminalSet = Set.Make (Grammar.Terminal)
module TerminalMap = Map.Make (Grammar.Terminal)
module TerminalOptMap = ParseTable.TerminalOptMap
module Route = ParseTable.Route

module RuleSet = Set.Make (struct type t = Grammar.rule let compare = compare end)

let constructor name =
  let buffer = Bytes.of_string name in
  let c = Char.code (Bytes.get buffer 0) in
  if c >= 0x61 && c <= 0x7a then
    Bytes.set buffer 0 (Char.chr (c - 0x20));
  Bytes.to_string buffer

let generate_non_terminal_type nt fmt =
  match nt with
  | Grammar.Primitive Grammar.Int ->
    Format.fprintf fmt "(int Span.located)"
  | Grammar.Primitive Grammar.Ident ->
    Format.fprintf fmt "(string Span.located)"
  | Grammar.Ref r ->
    let def = Option.get r.definition in
    Format.fprintf fmt "(%s Span.located)" def.Grammar.name
  | Grammar.Iterated (r, _) ->
    let def = Option.get r.definition in
    Format.fprintf fmt "(%s Span.located list Span.located)" def.Grammar.name
  | Grammar.Optional r ->
    let def = Option.get r.definition in
    Format.fprintf fmt "(%s Span.located option)" def.Grammar.name

let generate_definition_declaration is_rec def fmt =
  let name = def.Grammar.name in
  if is_rec then
    Format.fprintf fmt "and %s = \n" name
  else
    Format.fprintf fmt "type %s = \n" name;
  List.iter (
    function (rule, _) ->
      let args = Grammar.rule_args rule in
      if args = [] then
        Format.fprintf fmt "| %s\n" (fst rule.constructor)
      else begin
        let rec generate_args args fmt =
          match args with
          | [] -> ()
          | a::[] ->
            Format.fprintf fmt "%t" (generate_non_terminal_type a)
          | a::args ->
            Format.fprintf fmt "%t * %t" (generate_non_terminal_type a) (generate_args args)
        in
        Format.fprintf fmt "| %s of %t\n" (fst rule.constructor) (generate_args args)
      end
  ) def.rules;
  Format.fprintf fmt "\n"

let generate_terminal t fmt =
  match t with
  | Grammar.Terminal.Keyword name ->
    Format.fprintf fmt "Lexer.Keyword Lexer.Keyword.%s" (constructor name)
  | Grammar.Terminal.Begin c ->
    Format.fprintf fmt "Lexer.Begin '%c'" c
  | Grammar.Terminal.End c ->
    Format.fprintf fmt "Lexer.End '%c'" c
  | Grammar.Terminal.Operator op ->
    Format.fprintf fmt "Lexer.Operator \"%s\"" op
  | Grammar.Terminal.Primitive _ ->
    failwith "primitive terminal should not be generated."

let generate_definition_function_name def fmt =
  let name = def.Grammar.name in
  Format.fprintf fmt "parse_nt_%s" name

let generate_non_terminal_function_name nt fmt =
  match nt with
  | Grammar.Primitive Int ->
    Format.fprintf fmt "parse_int"
  | Grammar.Primitive Ident ->
    Format.fprintf fmt "parse_ident"
  | Grammar.Ref r ->
    let def = Option.get r.definition in
    generate_definition_function_name def fmt
  | Grammar.Iterated (r, sep) ->
    let def = Option.get r.definition in
    Format.fprintf fmt "parse_iter_nt_%s %s" sep def.Grammar.name
  | Grammar.Optional r ->
    let def = Option.get r.definition in
    Format.fprintf fmt "parse_opt_nt_%s" def.Grammar.name

let generate_definition_parser table def fmt =
  let name = def.Grammar.name in
  Format.fprintf fmt "and %t span lexer = \n" (generate_definition_function_name def);
  Format.fprintf fmt "match lexer () with \n";
  let nt_table = Hashtbl.find table name in
  TerminalOptMap.iter (
    fun terminal_opt route ->
      match route with
      | Route.Rule ((rule, _), _) ->
        begin match terminal_opt with
          | Some terminal ->
            Format.fprintf fmt "| Seq.Cons ((%t, _), _) -> \n" (generate_terminal terminal);
            let rec generate_tokens arg_count tokens =
              match tokens with
              | [] ->
                if arg_count = 0 then
                  Format.fprintf fmt "(Ast.%s, span), lexer\n" (fst rule.constructor)
                else begin
                  let rec print_args n fmt =
                    if n = 0 then
                      Format.fprintf fmt "arg0"
                    else begin
                      Format.fprintf fmt "%t, arg%d" (print_args (n - 1)) n
                    end
                  in
                  Format.fprintf fmt "(Ast.%s (%t), span), lexer\n" (fst rule.constructor) (print_args (arg_count - 1))
                end
              | (Grammar.Terminal terminal, _)::tokens' ->
                Format.fprintf fmt "begin match consume span lexer with \n";
                Format.fprintf fmt "| (%t, _), span, lexer -> \n" (generate_terminal terminal);
                generate_tokens arg_count tokens';
                Format.fprintf fmt "| (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))\n";
                (* Format.fprintf fmt "| None, span -> raise (Error (UnexpectedEOF, span))\n"; *)
                Format.fprintf fmt "end\n"
              | (Grammar.NonTerminal nt, _)::tokens' ->
                let i = arg_count in
                Format.fprintf fmt "let arg%d, lexer = %t (Span.next span) lexer in \n" i (generate_non_terminal_function_name nt);
                Format.fprintf fmt "let span = Span.union span (snd arg%d) in \n" i;
                generate_tokens (arg_count + 1) tokens'
            in
            generate_tokens 0 rule.tokens
          | None -> ()
        end
      | _ -> ()
  ) nt_table;
  begin match TerminalOptMap.find_first_opt Option.is_none nt_table with
    | Some (_, route) ->
      begin match route with
        | Route.Rule ((_, _), _) ->
          Format.fprintf fmt "| _ -> %s\n" (constructor name)
        | _ -> failwith "No route. This is a bug."
      end
    | None ->
      Format.fprintf fmt "| Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))\n";
      Format.fprintf fmt "| Seq.Nil -> raise (Error (UnexpectedEOF, span))\n"
  end;
  Format.fprintf fmt "@."

let generate_non_terminal_parser table nt fmt =
  match nt with
  | Grammar.Primitive Grammar.Int ->
    Format.fprintf fmt "and parse_int span lexer =
match lexer () with
| Seq.Cons ((Int i, token_span), lexer) -> (i, token_span), lexer
| Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
| Seq.Nil -> raise (Error (UnexpectedEOF, span))

"
  | Grammar.Primitive Grammar.Ident ->
    Format.fprintf fmt "and parse_ident span lexer =
match lexer () with
| Seq.Cons ((Ident name, token_span), lexer) -> (name, token_span), lexer
| Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
| Seq.Nil -> raise (Error (UnexpectedEOF, span))

"
  | Grammar.Ref r ->
    generate_definition_parser table (Option.get r.definition) fmt
  | _ ->
    failwith "TODO"

module TokenKind = struct
  type t =
    | Keyword
    | Begin
    | End
    | Operator
    | Primitive of Grammar.primitive

  let compare = compare
end

module TokenKindSet = Set.Make (TokenKind)

let terminal_kinds g =
  Grammar.fold_terminals (
    fun t set ->
      let kind = match t with
        | Grammar.Terminal.Keyword _ -> TokenKind.Keyword
        | Grammar.Terminal.Begin _ -> TokenKind.Begin
        | Grammar.Terminal.End _ -> TokenKind.End
        | Grammar.Terminal.Operator _ -> TokenKind.Operator
        | Grammar.Terminal.Primitive p -> TokenKind.Primitive p
      in
      TokenKindSet.add kind set
  ) g TokenKindSet.empty

let terminal_keywords g =
  Grammar.fold_terminals (
    fun t set ->
      match t with
      | Grammar.Terminal.Keyword name -> StringSet.add name set
      | _ -> set
  ) g StringSet.empty

let terminal_operators g =
  Grammar.fold_terminals (
    fun t set ->
      match t with
      | Grammar.Terminal.Operator name -> StringSet.add name set
      | _ -> set
  ) g StringSet.empty

let terminal_delimiters g =
  Grammar.fold_terminals (
    fun t set ->
      match t with
      | Grammar.Terminal.Begin d -> CharSet.add d set
      | Grammar.Terminal.End d -> CharSet.add d set
      | _ -> set
  ) g CharSet.empty

let generate_lexer_token_type g out =
  let kinds = terminal_kinds g in
  Format.fprintf out "type token = \n";
  TokenKindSet.iter (
    function
    | TokenKind.Keyword -> Format.fprintf out "| Keyword of Keyword.t \n"
    | TokenKind.Begin -> Format.fprintf out "| Begin of char \n"
    | TokenKind.End -> Format.fprintf out "| End of char \n"
    | TokenKind.Operator -> Format.fprintf out "| Operator of Utf8String.t \n"
    | TokenKind.Primitive Grammar.Int -> Format.fprintf out "| Int of int \n"
    | TokenKind.Primitive Grammar.Ident -> Format.fprintf out "| Ident of Utf8String.t \n"
  ) kinds;
  Format.fprintf out "\n";
  kinds

let generate_lexer_keyword_type g out =
  let keywords = terminal_keywords g in
  if StringSet.is_empty keywords then () else begin
    Format.fprintf out "type t = \n";
    StringSet.iter (
      function name ->
        Format.fprintf out "| %s \n" (constructor name)
    ) keywords
  end

let generate_lexer_int fmt =
  Format.fprintf fmt "let int_opt str =
match int_of_string_opt str with
| Some i -> Some (Int i)
| None -> None

"

let generate_lexer_delimiters g fmt =
  let delimiters = terminal_delimiters g in
  if CharSet.is_empty delimiters then
    Format.fprintf fmt "let delimiter_opt _ = None\n\n"
  else begin
    Format.fprintf fmt "let delimiter_opt str =
match str with\n";
    CharSet.iter (
      function
      | '(' -> Format.fprintf fmt "  | \"(\" -> Some (Begin '(')\n"
      | '[' -> Format.fprintf fmt "  | \"[\" -> Some (Begin '[')\n"
      | '{' -> Format.fprintf fmt "  | \"{\" -> Some (Begin '{')\n"
      | ')' -> Format.fprintf fmt "  | \")\" -> Some (End ')')\n"
      | ']' -> Format.fprintf fmt "  | \"]\" -> Some (End ']')\n"
      | '}' -> Format.fprintf fmt "  | \"}\" -> Some (End '}')\n"
      | _ -> failwith "invalid delimiter"
    ) delimiters;
    Format.fprintf fmt " | _ -> None

"
  end

let generate_lexer_keywords g fmt =
  let keywords = terminal_keywords g in
  if StringSet.is_empty keywords then
    Format.fprintf fmt "let keyword_opt _ = None\n\n"
  else begin
    Format.fprintf fmt "let keyword_opt str =
match str with\n";
    StringSet.iter (
      function name ->
        Format.fprintf fmt "  | \"%s\" -> Some (Keyword Keyword.%s)\n" name (constructor name)
    ) keywords;
    Format.fprintf fmt " | _ -> None

"
  end

let generate_lexer_operators g fmt =
  let operators = terminal_operators g in
  if StringSet.is_empty operators then
    Format.fprintf fmt "let operator_opt _ = None\n\n"
  else begin
    Format.fprintf fmt "let operator_opt str =
match str with\n";
    StringSet.iter (
      function name ->
        Format.fprintf fmt "  | \"%s\" -> Some (Operator \"%s\")\n" name name
    ) operators;
    Format.fprintf fmt " | _ -> None

"
  end

let generate_lexer_ident fmt =
  Format.fprintf fmt "let ident_opt str =
let res, _ = Utf8String.fold_left (
    fun accu c ->
      match accu with
      | false, _ -> false, true
      | true, not_first ->
        if UChar.is_alphabetic c || (not_first && UChar.is_numeric c) then
          (true, true)
        else
          (false, true)
  ) (true, false) str
in
if res then Some (Ident str) else None

"

let generate_lexer_errors fmt =
  Format.fprintf fmt "type error = UnknownToken of Utf8String.t

exception Error of error CodeMap.Span.located

"

let generate_lexer_interface g fmt =
  Format.fprintf fmt "open Unicode\n";
  Format.fprintf fmt "open UString\n\n";
  Format.fprintf fmt "module Keyword : sig\n  %tend\n\n" (generate_lexer_keyword_type g);
  ignore (generate_lexer_token_type g fmt);
  generate_lexer_errors fmt;
  Format.fprintf fmt "type t = token CodeMap.Span.located Seq.t\n\n";
  Format.fprintf fmt "val create : UChar.t Seq.t -> t\n"

let generate_ast g fmt =
  Format.fprintf fmt "open CodeMap\n\n";
  ignore (Grammar.fold (
      fun (def, _) is_rec ->
        generate_definition_declaration is_rec def fmt;
        true
    ) g false)

let generate_lexer g fmt =
  Format.fprintf fmt "open CodeMap\n";
  Format.fprintf fmt "open Unicode\n";
  Format.fprintf fmt "open UString\n\n";
  Format.fprintf fmt "module Keyword = struct\n  %tend\n\n" (generate_lexer_keyword_type g);
  let kinds = generate_lexer_token_type g fmt in
  generate_lexer_errors fmt;
  Format.fprintf fmt "type t = token CodeMap.Span.located Seq.t\n\n";

  generate_lexer_delimiters g fmt;
  generate_lexer_keywords g fmt;
  generate_lexer_operators g fmt;

  if TokenKindSet.mem (TokenKind.Primitive Grammar.Int) kinds then
    generate_lexer_int fmt
  else
    Format.fprintf fmt "let int_opt _ = None\n\n";

  if TokenKindSet.mem (TokenKind.Primitive Grammar.Ident) kinds then
    generate_lexer_ident fmt
  else
    Format.fprintf fmt "let ident_opt _ = None\n\n";

  Format.fprintf fmt "let token_of_buffer span str =
  match delimiter_opt str with
  | Some token -> token
  | None ->
    begin match int_opt str with
    | Some token -> token
    | None ->
      begin match keyword_opt str with
        | Some token -> token
        | None ->
          begin match ident_opt str with
            | Some token -> token
            | None ->
              begin match operator_opt str with
                | Some token -> token
                | None ->
                  raise (Error (UnknownToken str, span))
              end
          end
      end
  end

";
  Format.fprintf fmt "let consume span chars =
  begin match chars () with
    | Seq.Nil -> (span, Seq.Nil)
    | Seq.Cons (c, chars) ->
      (* Add [c] to the [span]. *)
      let span = Span.push c span in
      (span, Seq.Cons (c, chars))
  end

let create input =
  let rec next span chars () =
    begin match consume span chars with
      | _, Seq.Nil -> Seq.Nil
      | span, Seq.Cons (c, chars) ->
        if UChar.is_whitespace c || UChar.is_control c then
            next (Span.next span) chars ()
        else
            read_token span (c, chars)
    end
  and read_token span (c, chars) =
    let return span chars buffer =
      let token = token_of_buffer span buffer in
      Seq.Cons (Span.located span token, next (Span.next span) chars)
    in
    let rec read span chars buffer =
      match consume span chars with
      | _, Seq.Nil -> return span chars buffer
      | span', Seq.Cons (c, chars') ->
        if UChar.is_whitespace c || UChar.is_control c then
          return span chars buffer
        else
          read span' chars' (Utf8String.push c buffer)
    in
    read span chars (Utf8String.push c \"\")
  in
  next Span.default input
"

let generate_parser_errors fmt =
  Format.fprintf fmt "type error =
  | UnexpectedToken of Lexer.token
  | UnexpectedEOF

exception Error of error CodeMap.Span.located

"

let generate_parser g fmt =
  let table = ParseTable.of_grammar g in
  Format.fprintf fmt "open CodeMap\n\n";
  generate_parser_errors fmt;
  Format.fprintf fmt "let rec consume span lexer =
    match lexer () with
    | Seq.Nil -> raise (Error (UnexpectedEOF, span))
    | Seq.Cons ((token, span'), lexer') ->
      (token, span'), (Span.union span span'), lexer'

";
  Grammar.iter_non_terminals (
    function nt ->
      generate_non_terminal_parser table nt fmt
  ) g;
  Grammar.iter (
    function (def, _) ->
      let name = def.Grammar.name in
      Format.fprintf fmt "let parse_%s lexer =
  let res, _ = %t Span.default lexer in
  res

" name (generate_definition_function_name def)
  ) g

let generate_parser_interface g fmt =
  generate_parser_errors fmt;
  Grammar.iter (
    function (def, _) ->
      let name = def.Grammar.name in
      Format.fprintf fmt "val parse_%s : Lexer.t -> Ast.%s CodeMap.Span.located\n" name name
  ) g
