open Lexer
open Lexing
open Parser
open Types

exception UnboundVariable of string
exception EvaluationComplete

(*
((\a.(\b.((a a) b))) (\a.(\b.a))) (\a.(\b.a))
((\a.(\b.((a b) a))) (\a.(\b.a))) (\a.(\b.b))
 *)

let rec string_of_term (t: Types.term) =
  match t with
  | TmAbs(x, t1) ->
      "(\\" ^ x ^ "." ^ (string_of_term t1) ^ ")"
  | TmApp(t1, t2) ->
      "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"
  | TmVar(x) -> x

let rec string_of_term_in_ctx ctx tm =
  match ctx with
  | [] -> (string_of_term tm)
  | (n, t)::r -> if t = tm then n else (string_of_term_in_ctx r tm)

let rec string_of_ctx (c: Types.context) =
  match c with
  | [] -> ""
  | (n, t)::r -> (n ^ ": " ^ (string_of_term t) ^ "\n" ^ (string_of_ctx r))

let assign ctx (name, t) : Types.context =
  (name, t)::ctx

let remove ctx name =
  List.filter (fun (a, b) -> a != name) ctx

let rec lookup ctx name : Types.term =
  match ctx with
  | [] -> raise (UnboundVariable name)
  | (n, t)::tail ->
      if n = name then t else lookup tail name

let rec subst name tm repl =
  match tm with
  | TmVar(x) -> if x = name then repl else TmVar(x)
  | TmAbs(n, t) -> if n = name then TmAbs(n, t) else TmAbs(n, subst name t repl)
  | TmApp(a, b) -> TmApp(subst name a repl, subst name b repl)

let rec try_subst name tm repl =
  match tm with
  | TmVar(x) -> if x = name then (true, repl) else (false, TmVar(x))
  | TmAbs(n, t) -> if n = name then (false, TmAbs(n, t)) else (let (s, t') = try_subst name t repl in (s, TmAbs(n, t')))
  | TmApp(a, b) -> (let ((sa, a'), (sb, b')) = (try_subst name a repl, try_subst name b repl) in (sa || sb, TmApp(a', b')))

let rec eval (ctx, t) =
  let rec helper (ctx, t) d =
    (*print_endline ((String.init (d * 2) (fun _ -> ' ')) ^ "helper" ^ (string_of_term t));
    print_endline (string_of_ctx ctx);*)
    match t with
    | TmApp(TmApp(_, _) as a, b) ->
        let (_, a') = helper (ctx, a) (d + 1) in
        helper (ctx, TmApp(a', b)) (d + 1)
    | TmApp(TmAbs(n, t), r) ->
        (* let ctx' = assign ctx (n, r) in
        helper (ctx', t) (d + 1) *)
        let (s, t') = try_subst n t r in
        (*print_endline ("try_subst('" ^ n ^ "', " ^ (string_of_term t) ^ ", " ^ (string_of_term r) ^ ") = " ^ (if s then "true" else "false"));*)
        if s then helper (ctx, t') (d + 1) else (ctx, t')
    | TmApp(TmVar(n), b) ->
        let (_, a) = helper (ctx, lookup ctx n) (d + 1) in
        helper (ctx, TmApp(a, b)) (d + 1)
    | TmVar(n) -> (ctx, lookup ctx n)
    | TmAbs(_, TmVar(_)) as t -> (ctx, t)
    | TmAbs(n, _) as t ->
        let r = TmVar(n) in
        let (s, t') = try_subst n t r in
        (*print_endline ("try_subst('" ^ n ^ "', " ^ (string_of_term t) ^ ", " ^ (string_of_term r) ^ ") = " ^ (if s then "true" else "false"));*)
        if s then helper (ctx, t') (d + 1) else (ctx, t')
  in try
    let (ctx', t') = helper (ctx, t) 0 in
    if t = t' then raise EvaluationComplete else
    eval (ctx', t')
  with
  | UnboundVariable v -> raise (Failure ("unbound variable '" ^ v ^ "'"))
  | EvaluationComplete -> (ctx, t)

let _ =
  let rec loop ctx =
    try
      print_string "> "; flush stdout;
      let lexbuf = Lexing.from_channel stdin in
      let x = Parser.main Lexer.token lexbuf in
      match x with
      | Types.Term t ->
          let (ctx', r) = eval (ctx, t) in
            print_endline (string_of_term_in_ctx ctx r); flush stdout;
            loop ctx'
      | Types.Assign (n, t) ->
          let (ctx', r) = eval (ctx, t) in
            loop (assign ctx' (n, r))
    with
    | Lexer.Eof ->
        loop ctx
    | End_of_file ->
        print_endline "^D";
        exit 0
  in loop []

