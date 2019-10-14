open CodeMap
open Unicode.UString

type error =
  | MultipleDefinitions of Utf8String.t * Span.t
  | UndefinedNonTerminal of Utf8String.t

exception Error of error Span.located

module Terminal : sig
  type t =
    | Keyword of Utf8String.t
    | Begin of char
    | End of char
    | Operator of Utf8String.t

  val compare : t -> t -> int

  val print : t -> Format.formatter -> unit
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

and rule = {
  constructor: Utf8String.t Span.located;
  tokens: token Span.located list
}

and definition = {
  name: Utf8String.t;
  rules: rule Span.located list
}

type t

val of_ast : Ast.grammar -> t

val fold : (definition Span.located -> 'a -> 'a) -> t -> 'a -> 'a

val iter : (definition Span.located -> unit) -> t -> unit

val fold_non_terminals : (non_terminal -> 'a -> 'a) -> t -> 'a -> 'a

val iter_non_terminals : (non_terminal -> unit) -> t -> unit

val print : t -> Format.formatter -> unit

val print_definition : definition -> Format.formatter -> unit

val print_non_terminal : non_terminal -> Format.formatter -> unit

val print_rule : rule -> Format.formatter -> unit

val print_token : token -> Format.formatter -> unit
