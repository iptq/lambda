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
%type <Types.input> main
%%

main:
  | assign EOL { $1 }
  | expr EOL { Types.Term($1) }
;
assign:
  | Ident Equal expr { Types.Assign($1, $3) }
;
expr:
  | var { Types.TmVar($1) }
  | LParen expr RParen { $2 }
  | Lambda var Dot expr { Types.TmAbs($2, $4) }
  | expr expr { Types.TmApp($1, $2) }
;
var:
  Ident { $1 }
;

