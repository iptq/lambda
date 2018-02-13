type term =
  | TmVar of char
  | TmAbs of char * term
  | TmApp of term * term

type binding = NameBind

type context = (char * binding) list

