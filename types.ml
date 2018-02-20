type var = string

type term =
  | TmVar of var
  | TmAbs of var * term
  | TmApp of term * term

type input =
  | Term of term
  | Assign of string * term

type context = (string * term) list

