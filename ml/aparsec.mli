
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
    type symbol
    type error = {
      position : Lexing.position option;
      message  : string option;
      expected : token_kind option;
      actual   : token option;
    }
    exception Error of error
    val run : 'a t -> token list -> 'a
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
    val pure : 'a -> 'a t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <* ) : 'a t -> 'b t -> 'a t
    val ( *> ) : 'a t -> 'b t -> 'b t
    val empty : 'a t
    val ( <|> ) : 'a t -> 'a t -> 'a t
    val some : 'a t -> 'a list t
    val many : 'a t -> 'a list t
    val eof : unit t
    val token : token_kind -> token t
    val ptry : 'a t -> 'a t
    val label : symbol -> 'a t -> 'a t
    val position : Lexing.position t
  end

module Parser (Token : TOKEN) (State : SYMBOL) : PARSER

