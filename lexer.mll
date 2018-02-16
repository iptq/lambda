{
  open Parser
  exception Eof
}

rule token = parse
  | ' ' | '\t' { token lexbuf }
  | '\n' { EOL }
  | '=' { Equal }
  | '\\' { Lambda }
  | '(' { LParen }
  | ')' { RParen }
  | '.' { Dot }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as s { Ident(s) }
  | eof { raise Eof }
