{
  open Parser
  exception Eof
}

rule token = parse
  | ' ' | '\t' { token lexbuf }
  | '\n' { EOL }
  | '\\' { Lambda }
  | '.' { Dot }
  | ['a'-'z'] as c | ['A'-'Z'] as c { Ident(c) }
  | eof { raise Eof }
