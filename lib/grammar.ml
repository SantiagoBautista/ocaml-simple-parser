open CodeMap
open Unicode.UString

module StringMap = Map.Make (Utf8String)

type error =
  | MultipleDefinitions of Utf8String.t * Span.t
  | UndefinedNonTerminal of Utf8String.t

exception Error of error Span.located

module Terminal = struct
  type t =
    | Keyword of Utf8String.t

  let compare = compare

  let print t out =
    match t with
    | Keyword name ->
      Format.fprintf out "%s" name
end

type token =
  | Terminal of Terminal.t
  | NonTerminal of non_terminal

and non_terminal =
  | Ref of non_terminal_ref
  | Iterated of non_terminal_ref * Utf8String.t
  | Optional of non_terminal_ref

and non_terminal_ref = {
  span: Span.t;
  mutable definition: definition option
}

and rule = token Span.located list

and definition = {
  name: Utf8String.t;
  rules: rule Span.located list
}

module NonTerminal = struct
  type t = non_terminal

  let print t fmt =
    match t with
    | Ref nt ->
      Format.fprintf fmt "<%s>" (Option.get nt.definition).name
    | Iterated (nt, sep) ->
      Format.fprintf fmt "<%s*%s>" (Option.get nt.definition).name sep
    | Optional nt ->
      Format.fprintf fmt "<%s?>" (Option.get nt.definition).name

  let compare = compare
end

module NonTerminalSet = Set.Make (NonTerminal)

type t = (definition Span.located) StringMap.t

let token_of_ast table (token, span) =
  match token with
  | Ast.Terminal name -> Terminal (Terminal.Keyword name), span
  | Ast.NonTerminal name ->
    begin match Hashtbl.find_opt table name with
      | Some nt -> NonTerminal (Ref nt), span
      | None ->
        raise (Error (UndefinedNonTerminal name, span))
    end

let rule_of_ast table ((ast, span) : Ast.rule Span.located) =
  List.map (token_of_ast table) ast, span

let definition_of_ast table (ast, span) =
  {
    name = fst ast.Ast.name;
    rules = List.map (rule_of_ast table) ast.Ast.rules
  }, span

let of_ast ast =
  let table = Hashtbl.create (List.length ast) in
  (* Register non-terminals. *)
  List.iter (
    function (def_ast, span) ->
      let name = fst def_ast.Ast.name in
      begin match Hashtbl.find_opt table name with
        | Some nt -> raise (Error (MultipleDefinitions (name, nt.span), span))
        | None ->
          Hashtbl.replace table name { span = span; definition = None }
      end
  ) ast;
  (* Compile non-terminals. *)
  List.iter (
    function (def_ast, span) ->
      let name = fst def_ast.Ast.name in
      let nt = Hashtbl.find table name in
      let def, _ = definition_of_ast table (def_ast, span) in
      nt.definition <- Some def
  ) ast;
  (* Building the grammar. *)
  Hashtbl.fold (
    fun name nt g ->
      StringMap.add name (Option.get nt.definition, nt.span) g
  ) table StringMap.empty

let fold f t accu =
  StringMap.fold (fun _ def accu -> f def accu) t accu

let iter f t =
  fold (fun def () -> f def) t ()

let non_terminals t =
  StringMap.fold (fun _ (def, span) set ->
      let nt = Ref { span = span; definition = Some def } in
      let set = NonTerminalSet.add nt set in
      List.fold_right (fun (rule, _) set ->
          List.fold_right (
            fun (token, _) set ->
              match token with
              | Terminal _ -> set
              | NonTerminal nt ->
                NonTerminalSet.add nt set
          ) rule set
        ) def.rules set
    ) t NonTerminalSet.empty

let fold_non_terminals f t accu =
  NonTerminalSet.fold f (non_terminals t) accu

let iter_non_terminals f t =
  fold_non_terminals (fun nt () -> f nt) t ()

let print_token token fmt =
  match token with
  | Terminal terminal ->
    Terminal.print terminal fmt
  | NonTerminal nt ->
    NonTerminal.print nt fmt

let print_rule rule fmt =
  List.iter (
    function (token, _) ->
      Format.fprintf fmt "%t " (print_token token)
  ) rule

let print_definition def fmt =
  Format.fprintf fmt "<%s> ::= \n" def.name;
  List.iter (
    function (rule, _) ->
      Format.fprintf fmt "\t%t\n" (print_rule rule)
  ) def.rules

let print_non_terminal =
  NonTerminal.print

let print g fmt =
  StringMap.iter (
    fun _ (definition, _) ->
      print_definition definition fmt
  ) g
