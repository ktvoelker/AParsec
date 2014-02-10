
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
    type 'a t
    type token
    type token_kind
    type error = {
      position : Lexing.position option;
      message  : string option;
      expected : token_kind option;
      actual   : token option;
    }
    exception Error of error
    val run : 'a t -> token list -> 'a
  end

module Parser (Token : TOKEN) (State : SYMBOL) : PARSER

