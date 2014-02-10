
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

module Parser (Tok : TOKEN) (Sym : SYMBOL) : PARSER =
  struct
    type token = Tok.t
    type token_kind = Tok.kind
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
    type error = {
      position : Lexing.position option;
      message  : string option;
      expected : token_kind option;
      actual   : token option;
    }
    let default_error =
      {
        position = None;
        message  = None;
        expected = None;
        actual   = None;
      }
    let error_of_stream = function
      | [] -> default_error
      | (_, t) :: _ ->
        {
          default_error with
          position = Some (Tok.position t);
          actual   = Some t;
        }
    exception Error of error
    let range a b =
      let rec go tl a b =
        if a <= b
        then go (b :: tl) a (b - 1)
        else tl
      in go [] a b
    let rec run : type a. a t -> token list -> a = fun p ts ->
      let stream = ref (List.combine (range 1 (List.length ts)) ts)
      in let fail exp msg =
        let base = error_of_stream !stream
        in raise (Error {
          base with
          message = Some msg;
          expected = exp;
        })
      in let rec go : type a. a t -> a = function
        | End ->
          begin match !stream with
            | [] -> ()
            | t :: _ -> fail None "Unexpected token"
          end
        | Const x -> x
        | Token exp ->
          begin match !stream with
            | [] -> fail (Some exp) "Unexpected end-of-input"
            | (_, t) :: ts ->
              if Tok.kind t = exp
              then (stream := ts; t)
              else fail (Some exp) "Unexpected token"
          end
        | Skip (p1, p2) -> let _ = go p1 in go p2
        | App (pf, pa) -> let f = go pf in f (go pa)
        | Try p ->
          let ts = !stream
          in begin try go p with Error _ as err -> stream := ts; raise err end
        | Repeat p as rp -> begin try go p :: go rp with Error _ -> [] end
        (* TODO preserve message *)
        | Fail None -> fail None "Unknown error"
        | Fail (Some msg) -> fail None msg
        | Choice (p1, p2) ->
          begin
            if !stream = []
            then try go p1 with Error _ -> go p2
            else
              let orig_index = fst (List.hd !stream)
              in
                try
                  go p1
                with Error _ as err ->
                  let
                    stream = !stream
                  in
                    if stream != [] && fst (List.hd stream) = orig_index
                    then go p2
                    else raise err
          end
        | Label (_, p) -> go p
        | GetPos ->
          begin match !stream with
            | [] -> Lexing.dummy_pos
            | (_, t) :: _ -> Tok.position t
          end
      in go p
  end

