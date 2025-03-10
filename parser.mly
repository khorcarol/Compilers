/* File parser.mly */

%{

let get_loc = Parsing.symbol_start_pos 

%}

%token <int> INT
%token<string> IDENT
%token ADD SUB MUL DIV SEMICOLON
%token LPAREN RPAREN
%token BEGIN END
%token IF THEN ELSE BOOL INTTYPE TRUE FALSE LET LETFUN FN IN END ARROW  COLON EQUAL
%token EOF
%left ADD SUB        /* lowest precedence */
%left MUL DIV  EQUAL ARROW        /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%nonassoc INT  IDENT TRUE FALSE LPAREN/* highest precedence */
  

%start main
%type <Past.type_expr> texpr
%type <Past.expr> simple_expr 
%type <Past.expr> expr1 
%type <Past.expr> expr 
%type <Past.expr list> exprlist
%type <Past.expr> main

%%
main:
	expr EOF                { $1 }
;
simple_expr:
| INT                                { Past.Integer (get_loc(), $1) }
| TRUE								 {Past.Bool (get_loc(), true)}
| IDENT                              { Past.Var (get_loc(), $1) }
| FALSE								 {Past.Bool (get_loc(), false)}
| LPAREN expr RPAREN                 { $2 }

expr1:
| simple_expr                        { $1 }
| expr1 ADD expr1                      { Past.Op(get_loc(), $1, Past.ADD, $3) }
| expr1 SUB expr1                      { Past.Op(get_loc(), $1, Past.SUB, $3) }
| expr1 MUL expr1                      { Past.Op(get_loc(), $1, Past.MUL, $3) }
| expr1 DIV expr1                      { Past.Op(get_loc(), $1, Past.DIV, $3) }
| expr1 simple_expr                  { Past.App (get_loc(), $1, $2) } 

expr:
| expr1                             { $1 }
| FN IDENT COLON texpr ARROW expr 	{ Past.Lambda(get_loc(), ($2, $4, $6)) }
| LETFUN IDENT LPAREN IDENT COLON texpr RPAREN COLON texpr EQUAL expr IN expr END
                                     { Past.LetFun (get_loc(), $2, ($4, $6, $11), $9, $13) }
| LET IDENT COLON texpr EQUAL expr IN expr END { Past.Let (get_loc(), $2, $4, $6, $8) }
| BEGIN exprlist END                 { Past.Seq(get_loc(), $2) }
| IF expr THEN expr ELSE expr 		 { Past.If(get_loc(), $2, $4, $6)}

exprlist:
|   expr                             { [$1] }
|   expr  SEMICOLON exprlist         { $1 :: $3  }


texpr: 
| BOOL                               { Past.TEbool  }
| INTTYPE                            { Past.TEint  }
| texpr ARROW texpr                  { Past.TEarrow ($1, $3)}
| LPAREN texpr RPAREN                { $2 }
