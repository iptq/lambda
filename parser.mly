%{
  open Types
%}

%token EOL
%token Dot
%token <char> Ident
%token Lambda

%start main
%type <Types.term> main
%%

main:
  expr EOL { $1 }
;
expr:
  | var { Types.TmVar($1) }
  | Lambda var Dot expr { Types.TmAbs($2, $4) }
  | expr expr { Types.TmApp ($1, $2) }
;
var:
  Ident { $1 }
;

