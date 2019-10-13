open CodeMap

module SeqThen = struct
  type ('a, 'b) node =
    | Nil of 'b
    | Cons of 'a * ('a, 'b) t

  and ('a, 'b) t = unit -> ('a, 'b) node

  let rec fold_left f accu t =
    match t () with
    | Nil b -> accu, b
    | Cons (a, t') -> fold_left f (f accu a) t'
end

type error =
  | UnexpectedEOF
  | Unexpected of Lexer.token * (Lexer.token_kind list)

exception Error of error Span.located

let consume span lexer =
  match lexer () with
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))
  | Seq.Cons ((token, span'), lexer') ->
    (token, span'), (Span.union span span'), lexer'

let tokens lexer : (Ast.token Span.located, Lexer.t) SeqThen.t =
  let rec next lexer () =
    match lexer () with
    | Seq.Nil
    | Seq.Cons ((Lexer.Token.RuleSeparator, _), _)
    | Seq.Cons ((Lexer.Token.DefinitionSeparator, _), _) ->
      SeqThen.Nil lexer
    | Seq.Cons ((token, span), lexer') ->
      begin match token with
        | Lexer.Token.Terminal name -> SeqThen.Cons ((Ast.Terminal name, span), next lexer')
        | Lexer.Token.NonTerminal name -> SeqThen.Cons ((Ast.NonTerminal name, span), next lexer')
        | _ -> raise (Error (Unexpected (token, [Lexer.TokenKind.Terminal; Lexer.TokenKind.NonTerminal]), span))
      end
  in
  next lexer

let rules lexer =
  let rec next lexer () =
    match lexer () with
    | Seq.Nil -> SeqThen.Nil lexer
    | Seq.Cons ((Lexer.Token.DefinitionSeparator, _), _) ->
      SeqThen.Nil lexer
    | Seq.Cons ((Lexer.Token.RuleSeparator, span), lexer') -> (* Empty rule *)
      SeqThen.Cons (([], span), next lexer')
    | Seq.Cons ((token, span), lexer') ->
      let first_token = match token with
        | Lexer.Token.Terminal name -> Ast.Terminal name
        | Lexer.Token.NonTerminal name -> Ast.Terminal name
        | _ -> raise (Error (Unexpected (token, [Lexer.TokenKind.Terminal; Lexer.TokenKind.NonTerminal]), span))
      in
      let (tokens, span), lexer' = SeqThen.fold_left
          (fun (tokens, span) (t, s) -> (t, s)::tokens, Span.union s span)
          ([first_token, span], span)
          (tokens lexer')
      in
      SeqThen.Cons ((List.rev tokens, span), next lexer')
  in
  next lexer

let definitions lexer =
  let rec next lexer () =
    match lexer () with
    | Seq.Nil -> SeqThen.Nil lexer
    | Seq.Cons ((Lexer.Token.NonTerminal name, name_span), lexer') -> (* Parse the non-terminal name. *)
      (* Consume the required definition separator `::=`. *)
      let (token, token_span), span, lexer' = consume name_span lexer' in
      begin match token with
        | Lexer.Token.Definition ->
          let (rules, span), lexer' = SeqThen.fold_left
              (
                fun (rules, span) (r, s) ->
                  if r = []
                  then rules, span
                  else (r, s)::rules, Span.union s span
              )
              ([], span)
              (rules lexer')
          in
          (* Parse the optional end `;`. *)
          let lexer', span =
            match lexer' () with
            | Seq.Nil -> lexer', span
            | Seq.Cons ((Lexer.Token.DefinitionSeparator, span'), lexer') ->
              lexer', Span.union span' span
            | Seq.Cons ((unexpected, span'), _) -> (* this should never happen tho. *)
              raise (Error (Unexpected (unexpected, [Lexer.TokenKind.DefinitionSeparator]), span'))
          in
          (* Generate the definition. *)
          let definition =
            {
              Ast.name = name, name_span;
              rules = List.rev rules
            }
          in
          SeqThen.Cons ((definition, span), next lexer')
        | _ -> raise (Error (Unexpected (token, [Lexer.TokenKind.Definition]), token_span))
      end
    | Seq.Cons ((unexpected, span), _) ->
      raise (Error (Unexpected (unexpected, [Lexer.TokenKind.NonTerminal]), span))
  in
  next lexer

let parse lexer =
  let g, _ = SeqThen.fold_left
      (fun g def -> def::g)
      []
      (definitions lexer)
  in
  g
