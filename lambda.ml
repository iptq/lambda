open Lexer
open Lexing
open Parser
open Types

exception EvaluationComplete

let rec string_of_term (t:term) =
  match t with
  | TmAbs(x, t1) ->
      "(\\" ^ x ^ "." ^ (string_of_term t1) ^ ")"
  | TmApp(t1, t2) ->
      "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
  | TmVar(x) -> x

(*
let termshift d t =
  let rec walk c t =
    match t with
    | TmVar(x) -> if 

let termsubsttop (s:term) (t:term) =
  termshift (-1) (termsubst 0 (termshift 1 s) t)

let rec isval ctx t =
  match t with
  | TmAbs(_, _, _) -> true
  | _ -> false

let rec helper ctx t =
  match t with
  | TmApp(TmAbs(x, t1), v2) when isval ctx v2 -> termsubsttop v2 t1
  | _ -> EvaluationComplete
*)

let assign ctx (name, t) : context =
  (name, t)::ctx

let rec lookup ctx name : term =
  match ctx with
  | [] -> raise (Failure ("unbound variable " ^ name))
  | (n, t)::tail ->
      if n = name then t else lookup tail name

let rec eval (ctx, t) =
  let rec helper (ctx, t) =
    match t with
    | Assign(name, t') ->
        let (ctx', r) = helper(ctx, t') in
          (assign ctx (name, r), r)
    | TmAbs(name, t') ->
        let (ctx', v) = helper (ctx, t') in
        (assign ctx (name, v), v)
    | TmVar(name) ->
        (ctx, lookup ctx name)
    | _ -> raise EvaluationComplete
  in try
    let (ctx', t') = helper (ctx, t) in
    eval (ctx', t')
  with EvaluationComplete -> (ctx, t)

let _ =
  try
    let rec loop ctx =
      print_string "> "; flush stdout;
      let lexbuf = Lexing.from_channel stdin in
      let t = Parser.main Lexer.token lexbuf in
      let (ctx', r) = eval (ctx, t) in
        print_endline (string_of_term r); flush stdout;
        loop ctx' in
    loop []
  with Lexer.Eof ->
    print_endline "^D";
    exit 0
