
module type FUNCTOR = sig
  type _ t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

module type APPLICATIVE = sig
  include FUNCTOR
  val pure : 'a -> 'a t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( <* ) : 'a t -> 'b t -> 'a t
  val ( *> ) : 'a t -> 'b t -> 'b t
end

module type ALTERNATIVE = sig
  include APPLICATIVE
  val empty : 'a t
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val some : 'a t -> 'a list t
  val many : 'a t -> 'a list t
end

