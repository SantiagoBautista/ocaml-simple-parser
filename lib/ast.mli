open CodeMap
open Unicode.UString

(** Token. *)
type token =
  | Terminal of Utf8String.t
  | NonTerminal of Utf8String.t

(** Non-terminal rule. *)
type rule = token Span.located list

(** Non-terminal definition. *)
type definition = {
  name: Utf8String.t Span.located;
  rules: rule Span.located list
}

(** Grammar definition. *)
type grammar = definition Span.located list

val print : grammar -> Format.formatter -> unit

val print_definition : definition -> Format.formatter -> unit

val print_rule : rule -> Format.formatter -> unit

val print_token : token -> Format.formatter -> unit
