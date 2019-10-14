open CodeMap
open Unicode.UString

(** Token. *)
type token =
  | Terminal of Utf8String.t
  | NonTerminal of Utf8String.t

(** Non-terminal rule. *)
type rule = {
  constructor: Utf8String.t Span.located;
  tokens: token Span.located list
}

(** Non-terminal definition. *)
type definition = {
  name: Utf8String.t Span.located;
  rules: rule Span.located list
}

(** Grammar definition. *)
type grammar = definition Span.located list

let print_token token fmt =
  match token with
  | Terminal name -> Format.fprintf fmt "%s" name
  | NonTerminal name -> Format.fprintf fmt "<%s>" name

let print_rule rule fmt =
  Format.fprintf fmt "| %s " (fst rule.constructor);
  List.iter (
    function (token, _) ->
      Format.fprintf fmt "%t " (print_token token)
  ) rule.tokens

let print_definition def fmt =
  Format.fprintf fmt "<%s> ::= \n" (fst def.name);
  List.iter (
    function (rule, _) ->
      Format.fprintf fmt "\t%t\n" (print_rule rule)
  ) def.rules

let print g fmt =
List.iter (
  function (definition, _) ->
    print_definition definition fmt
) g
