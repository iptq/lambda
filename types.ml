type var = string

type term =
  | Assign of string * term
  | TmVar of var
  | TmAbs of var * term
  | TmApp of term * term

type context = (string * term) list

