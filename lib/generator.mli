open CodeMap
open Unicode.UString

module ParseTable : sig
  type t

  val of_grammar : Grammar.t -> t
end

module Route : sig
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

val generate_lexer : Format.formatter -> unit
(** Write the lexer code to the given output channel. *)

val generate_parser : Grammar.t -> Format.formatter -> unit
(** Write the parser code to the given output channel. *)

val print_error : error -> Format.formatter -> unit
