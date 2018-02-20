%{
  open Types
%}

%token EOL
%token Dot
%token Equal
%token LParen
%token RParen
%token Let
%token <string> Ident
%token Lambda

%right prec_Abs
%left prec_App

%start main
%type <Types.input> main

%%

main:
  | assign EOL { $1 }
  | expr EOL { Types.Term($1) }
;
assign:
  | Let Ident Equal expr { Types.Assign($2, $4) }
;
expr:
  | Ident { Types.TmVar($1) }
  | LParen expr RParen { $2 }
  | Lambda Ident Dot expr %prec prec_Abs { Types.TmAbs($2, $4) }
  | expr expr %prec prec_App { Types.TmApp($1, $2) }
;

