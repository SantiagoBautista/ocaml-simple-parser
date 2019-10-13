open CodeMap
open Unicode.UString

module StringMap = Map.Make (Utf8String)
module TerminalSet = Set.Make (Grammar.Terminal)
module TerminalMap = Map.Make (Grammar.Terminal)
module TerminalOptMap = Map.Make (struct type t = Grammar.Terminal.t option let compare = compare end)

module RuleSet = Set.Make (struct type t = Grammar.rule let compare = compare end)

module Route = struct
  type t =
    | Rule of (Grammar.rule Span.located) * rule_route
    | None
    | Nil

  and rule_route = t list
end

type error =
  | Ambiguity of Grammar.non_terminal * Grammar.Terminal.t option * Route.t * Route.t
  | RuleAmbiguity of Grammar.definition * Grammar.rule * Grammar.Terminal.t option * Route.rule_route * Route.rule_route
  | RuleInnerAmbiguity of Grammar.definition * Grammar.rule * int * Span.t * Grammar.Terminal.t * Route.t * int * Span.t
  | IterationAmbiguity of Grammar.definition * Grammar.rule * Grammar.non_terminal_ref * Utf8String.t * Route.t

exception Error of error Span.located

module ParseTable = struct
  type t = (Utf8String.t, Route.t TerminalOptMap.t) Hashtbl.t

  let rec compute_first_terminals_of_non_terminal_def visited_rules nt (def, span) =
    let add terminal_opt route set =
      begin match TerminalOptMap.find_opt terminal_opt set with
        | Some route' ->
          raise (Error (Ambiguity (nt, terminal_opt, route', route), span))
        | None ->
          TerminalOptMap.add terminal_opt route set
      end
    in
    List.fold_right (
      fun (rule, span) set ->
        if RuleSet.mem rule visited_rules then
          set
        else begin
          let rule_terminals = compute_first_terminals_of_rule visited_rules def (rule, span) in
          TerminalOptMap.fold (
            fun terminal_opt rule_route set ->
              let route = Route.Rule ((rule, span), rule_route) in
              add terminal_opt route set
          ) rule_terminals set
        end
    ) def.Grammar.rules TerminalOptMap.empty

  and compute_first_terminals_of_non_terminal visited_rules (nt, span) =
    let add terminal_opt route set =
      begin match TerminalOptMap.find_opt terminal_opt set with
        | Some route' ->
          raise (Error (Ambiguity (nt, terminal_opt, route', route), span))
        | None ->
          TerminalOptMap.add terminal_opt route set
      end
    in
    begin match nt with
      | Grammar.Ref rnt ->
        let def = Option.get rnt.definition in
        compute_first_terminals_of_non_terminal_def visited_rules nt (def, span)
      | Grammar.Iterated (nt, sep) ->
        let set = compute_first_terminals_of_non_terminal visited_rules (Grammar.Ref nt, span) in
        let set = match TerminalOptMap.find_opt None set with
          | Some route ->
            add (Some (Grammar.Terminal.Keyword sep)) route set
          | None -> set
        in
        add None Route.Nil set
      | Grammar.Optional nt ->
        add None Route.None (compute_first_terminals_of_non_terminal visited_rules (Grammar.Ref nt, span))
    end

  and compute_first_terminals_of_rule visited_rules def (rule, span) =
    let visited_rules = RuleSet.add rule visited_rules in
    let add rule_route terminal_opt set =
      let rule_route = List.rev rule_route in
      begin match TerminalOptMap.find_opt terminal_opt set with
        | Some rule_route' ->
          raise (Error (RuleAmbiguity (def, rule, terminal_opt, rule_route, rule_route'), span))
        | None ->
          TerminalOptMap.add terminal_opt rule_route set
      end
    in
    let rec aux rule_route tokens set =
      match tokens with
      | [] -> add rule_route None set
      | (Grammar.Terminal terminal, _)::_ -> add rule_route (Some terminal) set
      | (Grammar.NonTerminal nt, span)::tokens' ->
        let firsts = compute_first_terminals_of_non_terminal visited_rules (nt, span) in
        TerminalOptMap.fold (
          fun terminal_opt route set ->
            match terminal_opt with
            | Some terminal -> add (route::rule_route) (Some terminal) set
            | None -> aux (route::rule_route) tokens' set
        ) firsts set
    in
    aux [] rule TerminalOptMap.empty

  let check_rule_ambiguities table def (rule, rule_span) =
    let rec first_terminals_of table nt =
      let add terminal_opt route set =
        TerminalOptMap.add terminal_opt route set
      in
      begin match nt with
        | Grammar.Ref nt ->
          let def = Option.get nt.definition in
          Hashtbl.find table def.Grammar.name
        | Grammar.Iterated (nt, sep) ->
          let set = first_terminals_of table (Grammar.Ref nt) in
          let set = match TerminalOptMap.find_opt None set with
            | Some route ->
              add (Some (Grammar.Terminal.Keyword sep)) route set
            | None -> set
          in
          add None Route.Nil set
        | Grammar.Optional nt ->
          add None Route.None (first_terminals_of table (Grammar.Ref nt))
      end
    in
    let rec check i tokens =
      match tokens with
      | [] -> ()
      | (Grammar.Terminal _, _)::tokens' ->
        check (i + 1) tokens'
      | (Grammar.NonTerminal nt, span)::tokens' ->
        let rec check_no_collision i span terminal route offset tokens =
          match tokens with
          | [] -> ()
          | (Grammar.Terminal terminal', span')::_ ->
            if terminal = terminal' then
              raise (Error (RuleInnerAmbiguity (def, rule, i, span, terminal, route, offset, span'), rule_span))
          | (Grammar.NonTerminal nt, span')::tokens' ->
            let terminals = first_terminals_of table nt in
            begin match TerminalOptMap.find_opt (Some terminal) terminals with
              | Some route ->
                raise (Error (RuleInnerAmbiguity (def, rule, i, span, terminal, route, offset, span'), rule_span))
              | None ->
                begin match TerminalOptMap.find_opt None terminals with
                  | Some route ->
                    check_no_collision i span terminal route (offset + 1) tokens'
                  | None -> ()
                end
            end
        in
        begin match nt with
          | Iterated (nt, sep) ->
            let def = Option.get nt.definition in
            let terminals = Hashtbl.find table def.Grammar.name in
            begin match TerminalOptMap.find_opt (Some (Grammar.Terminal.Keyword sep)) terminals with
              | Some route ->
                raise (Error (IterationAmbiguity (def, rule, nt, sep, route), span))
              | None -> ()
            end
          | _ -> ()
        end;
        let terminals = first_terminals_of table nt in
        if TerminalOptMap.mem None terminals then
          TerminalOptMap.iter (
            fun terminal_opt route ->
              match terminal_opt with
              | None -> ()
              | Some terminal ->
                check_no_collision i span terminal route 1 tokens'
          ) terminals;
        check (i + 1) tokens'
    in
    check 0 rule

  let of_grammar g =
    let table = Hashtbl.create 8 in
    Grammar.iter (
      function (def, def_span) ->
        let nt = Grammar.Ref {
            span = def_span;
            definition = Some def
          }
        in
        let nt_table = compute_first_terminals_of_non_terminal RuleSet.empty (nt, def_span) in
        Hashtbl.add table def.name nt_table
    ) g;
    Grammar.iter (
      function (def, _) ->
        List.iter (check_rule_ambiguities table def) def.Grammar.rules
    ) g;
    table
end

let generate_lexer _ =
  failwith "TODO"

let constructor name =
    let buffer = Bytes.of_string name in
    let c = Char.code (Bytes.get buffer 0) in
    if c >= 0x61 && c <= 0x7a then
        Bytes.set buffer 0 (Char.chr (c - 0x20));
    Bytes.to_string buffer

let generate_terminal t fmt =
  match t with
  | Grammar.Terminal.Keyword name ->
    Format.fprintf fmt "Token.Keyword %s" (constructor name)
  | Grammar.Terminal.Begin c ->
    Format.fprintf fmt "Token.Begin '%c'" c
  | Grammar.Terminal.End c ->
    Format.fprintf fmt "Token.End '%c'" c
  | Grammar.Terminal.Operator op ->
    Format.fprintf fmt "Token.Operator \"%s\"" op

let generate_definition_parser table def fmt =
  let name = def.Grammar.name in
  Format.fprintf fmt "let parse_%s pos lexer = \n" name;
  Format.fprintf fmt "let span = Span.of_position pos in \n";
  Format.fprintf fmt "match lexer () with \n";
  let nt_table = Hashtbl.find table name in
  TerminalOptMap.iter (
    fun terminal_opt route ->
      match route with
      | Route.Rule ((rule, _), _) ->
        begin match terminal_opt with
        | Some terminal ->
          Format.fprintf fmt "| Seq.Cons ((%t, _), _) -> \n" (generate_terminal terminal);
          let rec generate_rule rule =
            match rule with
            | [] ->
              Format.fprintf fmt "%s ()\n" (constructor name)
            | (Grammar.Terminal terminal, _)::rule' ->
              Format.fprintf fmt "begin match consume span lexer with \n";
              Format.fprintf fmt "| Some (%t, lexer), span -> \n" (generate_terminal terminal);
              generate_rule rule';
              Format.fprintf fmt "| Some (token, _), span -> raise (Error (UnexpectedToken token, span))\n";
              Format.fprintf fmt "| None, span -> raise (Error (UnexpectedEOF, span))\n";
              Format.fprintf fmt "end\n"
            | (Grammar.NonTerminal _, _)::rule' ->
              Format.fprintf fmt "let _ = print_thing in \n";
              generate_rule rule'
          in
          generate_rule rule
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
      Format.fprintf fmt "| Seq.Cons ((token, span), _) -> raise (Error (UnexpectedToken token, span))\n";
      Format.fprintf fmt "| Seq.Nil -> raise (Error (UnexpectedEOF, span))\n"
  end;
  Format.fprintf fmt "@."

let generate_non_terminal_parser table nt fmt =
  match nt with
  | Grammar.Ref r ->
    generate_definition_parser table (Option.get r.definition) fmt
  | _ ->
    failwith "TODO"

let generate_parser g out =
  let table = ParseTable.of_grammar g in
  Grammar.iter_non_terminals (function nt -> generate_non_terminal_parser table nt out) g

let print_error e fmt =
  match e with
  | Ambiguity (nt, None, _, _) ->
    Format.fprintf fmt "multiple ways to nullify `%t`" (Grammar.print_non_terminal nt)
  | Ambiguity (nt, Some terminal, _, _) ->
    Format.fprintf fmt "multiple ways to start `%t` with `%t`" (Grammar.print_non_terminal nt) (Grammar.Terminal.print terminal)
  | RuleAmbiguity (_, rule, None, _, _) ->
    Format.fprintf fmt "the following rule have multiple ways to be empty:\n%t" (Grammar.print_rule rule)
  | RuleAmbiguity (_, rule, Some terminal, _, _) ->
    Format.fprintf fmt "the following rule have multiple ways to start with %t:\n%t" (Grammar.Terminal.print terminal) (Grammar.print_rule rule)
  | RuleInnerAmbiguity (_, rule, _, _, _, _, _, _) ->
    Format.fprintf fmt "the following rule is ambiguous:\n%t" (Grammar.print_rule rule)
  | IterationAmbiguity (_, rule, _, _, _) ->
    Format.fprintf fmt "the following rule is ambiguous:\n%t" (Grammar.print_rule rule)
