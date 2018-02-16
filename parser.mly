%{
  open Types
%}

%token EOL
%token Dot
%token Equal
%token LParen
%token RParen
%token <string> Ident
%token Lambda

%start main
%type <Types.term> main
%%

main:
  | assign EOL { $1 }
  | expr EOL { $1 }
;
assign:
  | Ident Equal expr { Types.Assign($1, $3) }
;
expr:
  | var { Types.TmVar($1) }
  | Lambda var Dot expr { Types.TmAbs($2, $4) }
  | LParen expr expr RParen { Types.TmApp ($2, $3) }
;
var:
  Ident { $1 }
;

