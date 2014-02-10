
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
    type _ t
    type token
    val run : 'a t -> token list -> 'a
  end

module Parser (Tok : TOKEN) (Sym : SYMBOL) : PARSER =
  struct
    type token = Tok.t
    type 'a t =
      | End    : unit t
      | Const  : 'a -> 'a t
      | Token  : Tok.kind -> Tok.t t
      | Skip   : 'a t * 'b t -> 'b t
      | App    : ('a -> 'b) t * 'a t -> 'b t
      | Try    : 'a t -> 'a t
      | Repeat : 'a t -> ('a list) t
      | Fail   : string option -> 'a t
      | Choice : 'a t * 'a t -> 'a t
      | Label  : Sym.t * 'a t -> 'a t
      | GetPos : Lexing.position t
    let run : type a. a t -> token list -> a = fun p ts -> match p with
      | End -> ()
      | Const x -> x
  end

