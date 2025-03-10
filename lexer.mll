(* File lexer.mll *)
{
  open Parser
  open Lexing 

(* next_line copied from  Ch. 16 of "Real Workd Ocaml" *) 
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let newline = ('\010' | "\013\010" )
let ident_reg_exp = ['A'-'Z' 'a'-'z']+ ['0'-'9' 'A'-'Z' 'a'-'z' '_' '\'']* 
let int_reg_exp = ['0'-'9']+

	rule token = parse
	  | [' ' '\t']     { token lexbuf }     (* skip blanks *)
	| '('            { LPAREN }
	  | ')'            { RPAREN }
	  	| ":" { COLON }	
	  | ';'	           { SEMICOLON }
	  | '+'            { ADD }
	  | '-'            { SUB }
	  | '*'            { MUL }
	  | '/'            { DIV }

		| "->" { ARROW }		
	  | "="            { EQUAL }
	  | "begin"        { BEGIN }
	  | "end"          { END }
	  | "if"			{IF}
	  | "then"			{THEN}
	  | "else"			{ELSE}
	| "let fun" { LETFUN}
		| "let val" { LET}
		| "fn" { FN }
		| "in" { IN }
		| "end" { END }
	  | "true"			{TRUE}
	  | "false" 		{FALSE}
	    | "bool" { BOOL }
  | "int" { INTTYPE }
	  | eof            { EOF }  
	  | int_reg_exp { INT (int_of_string (Lexing.lexeme lexbuf)) }
	    | ident_reg_exp { IDENT (Lexing.lexeme lexbuf) }
	  | "(*" { comment lexbuf; token lexbuf }
	  | newline { next_line lexbuf; token lexbuf } 
	  | eof { EOF }
	  | _ { Errors.complain ("Lexer : Illegal character " ^ (Char.escaped(Lexing.lexeme_char lexbuf 0)))
}

and comment = parse
  | "*)" { () }
  | newline { next_line lexbuf; comment lexbuf }
  | "(*" {comment lexbuf; comment lexbuf }
  | _ { comment lexbuf } 