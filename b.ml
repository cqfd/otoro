open Core.Std

type t = S of string
       | I of int
       | L of t list
       | D of (string * t) list

(* Parser combinator helper module *)
module Parser = struct
  type 'a t = string -> ('a * string) option

  let return (x : 'a) : 'a t = fun input -> Some (x, input)

  let (>>=) (t : 'a t) (f : 'a -> 'b t) : 'b t = fun input ->
      match t input with
      | None -> None
      | Some (x, input') -> f x input'

  let (>>) (t : 'a t) (t' : 'b t) : 'b t = t >>= fun _ -> t'

  let (>>|) (t : 'a t) (f : 'a -> 'b) : 'b t =
    t >>= fun x -> return (f x)

  let (||) (t : 'a t) (t' : 'a t) : 'a t = fun input ->
    match t input with
    | None -> t' input
    | Some (x, input') -> Some (x, input')

  let rec many_star (t : 'a t) : 'a list t =
    many_plus t || return []
  and many_plus (t : 'a t) : 'a list t =
    t >>= fun x ->
    many_star t >>= fun xs ->
    return (x::xs)

  let failure : 'a t = fun input -> None

  let item : char t = fun input ->
    if String.is_empty input then
      failure input
    else
      let rest = String.drop_prefix input 1 in
      Some (input.[0], rest)

  let sat p : char t =
    item >>= fun i -> if p i then return i else failure

  let char c : char t = sat (Char.equal c)

  let digit = sat Char.is_digit

  let int : int t =
    many_plus digit
    >>| fun digits -> Int.of_string (String.of_char_list digits)

  let take n : string t = fun input ->
    if String.length input < n then
      None
    else
      Some (String.prefix input n, String.drop_prefix input n)
end

(* A parser that recognizes bencoded strings *)
let s : t Parser.t = Parser.(
  int >>= fun n ->
  char ':' >>
  take n >>| fun result -> S result
)

(* A parser that recognizes bencoded ints *)
let i : t Parser.t = Parser.(
  char 'i' >>
  int >>= fun n -> 
  char 'e' >> 
  return (I n)
)

(* A parser that recognizes bencoded lists *)
let rec l : t Parser.t = fun input -> Parser.(
  char 'l' >> 
  many_star decode >>= fun bs -> 
  char 'e' >> 
  return (L bs)
) input

(* A parser that recognizes bencoded dictionaries *)
and d : t Parser.t = fun input -> Parser.(
  let kv_pair =
    s >>= function 
    | (S k) ->
      decode >>= fun v ->
      return (k, v)
    | _ -> assert false
  in
  char 'd' >> 
  many_star kv_pair >>= fun kvs ->
  char 'e' >>
  return (D kvs)
) input

(* Finally, a parser that recognizes any kind of bencoded value *)
and decode : t Parser.t = fun input -> Parser.(s || i || l || d) input

(* Sanity checks *)
let () =
  assert (decode "4:spam" = Some (S "spam", ""));
  assert (decode "1:spam" = Some (S "s", "pam"));
  assert (decode "i123e" = Some (I 123, ""));
  assert (
    decode "l4:spami123e3:fooe" = Some (
      L [S "spam"; I 123; S "foo"], ""
    )
  );
  assert (
    decode "d3:cow3:moo4:spam4:eggse" = Some (
      D [("cow", S "moo"); ("spam", S "eggs")], ""
    )
  );
  assert (
    decode "d4:spaml1:a1:bee" = Some (
      D [("spam", L [S "a"; S "b"])], ""
    )
  );
