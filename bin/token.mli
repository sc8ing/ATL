type t = 
  | LPar
  | RPar
  | Plus
  | Minus
  | Term of char (* elementary term *) (* any way to enfore capitalization in type? *)
  | STIndicator
  | ConcInd (* conclusion indicator *)

val toString : t -> string
val toStrings : t list -> string
