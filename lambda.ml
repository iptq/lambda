open Lexer
open Lexing
open Parser
open Types

let rec string_of_term (t:term) =
  match t with
  | TmAbs(x, t1) ->
      "(\\" ^ (String.make 1 x) ^ "." ^ (string_of_term t1) ^ ")"
  | TmApp(t1, t2) ->
      "(" ^ (string_of_term t1) ^ (string_of_term t2) ^ ")"
  | TmVar(x) ->
      String.make 1 x

let _ =
  try
    let rec loop ctx =
      print_string "> "; flush stdout;
      let lexbuf = Lexing.from_channel stdin in
      let result = Parser.main Lexer.token lexbuf in
        print_endline (string_of_term result); flush stdout;
        loop ctx in
    loop []
  with Lexer.Eof ->
    print_endline "error";
    exit 0
