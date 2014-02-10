
module type SYMBOL =
  sig
    type t
  end

module type TOKEN =
  sig
    type t
    type kind
    val kind : t -> kind
    val position : t -> Lexing.position
  end

module type PARSER =
  sig
    type token
    type 'a t
    val run : 'a t -> token list -> 'a
  end

module Parser (Token : TOKEN) (State : SYMBOL) : PARSER

