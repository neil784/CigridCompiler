{
  open Parser
  exception Error of char
}

let whitespace = ['\t' ' ' '\r']
let line_comment = "//" [^ '\n']*
let pre_processing_directive = "#" [^ '\n']*
let new_line = ['\n']
let start_multiline_comment = "/*"
let end_multiline_comment = "*/"

let letter = ['A'-'Z'] | ['a'-'z']
let digit = ['0'-'9']
let non_digit = '_' | letter
let ident = non_digit (digit | non_digit)*

let non_escape_chars = [^ '\\' '\n' '\t' '\'' '\"']
let escape_sequence = '\\' ('\\' | '\'' | '\"' | 'n' | 't')
let valid_string = '\"' (non_escape_chars | escape_sequence)* '\"'
let valid_char = '\'' (non_escape_chars | escape_sequence) '\''

let decimal_integer = '0' | ['1'-'9'] ['0'-'9']*
let hexadecimal_integer = '0' ['x' 'X'] (['0'-'9' 'a'-'f' 'A'-'F'])+

rule token = parse
    | whitespace | line_comment | pre_processing_directive
        { token lexbuf }
    | new_line
        { Lexing.new_line lexbuf; token lexbuf }
    | start_multiline_comment
        { handle_multiline_comment lexbuf }
    | ident as str
        { match str with
            | "break"  -> BREAK
            | "char"   -> CHAR
            | "delete" -> DELETE
            | "else"   -> ELSE
            | "extern" -> EXTERN
            | "for"    -> FOR
            | "if"     -> IF
            | "int"    -> INT
            | "new"    -> NEW
            | "return" -> RETURN
            | "struct" -> STRUCT
            | "void"   -> VOID
            | "while"  -> WHILE
            | s        -> IDENT(s)
        }
    | (decimal_integer | hexadecimal_integer) as n
        { INTEGER_CONSTANT(int_of_string n) }
    | valid_char as c
        { match c with
            | "\'\\n\'" -> CHAR_CONSTANT('\n')
            | "\'\\t\'" -> CHAR_CONSTANT('\t')
            | "\\\\" -> CHAR_CONSTANT('\\')
            | "\'\\\'\'" -> CHAR_CONSTANT('\'')
            | "\'\\\"\'" -> CHAR_CONSTANT('\"')
            | s -> CHAR_CONSTANT(String.get s 1)
        }
    | valid_string as str
        { STRING_CONSTANT(str) }
    | '!'
        { NOT }
    | '~'
        { BITWISE_NOT }
    | '+' 
        { ADD }
    | '-'
        { SUB }
    | '*'
        { MUL }
    | '/'
        { DIV }
    | '%'
        { MOD }
    | '<'
        { LESS_THAN }
    | '>'
        { GREATER_THAN }
    | "<="
        { LESS_THAN_EQUAL }
    | ">="
        { GREATER_THAN_EQUAL }
    | "=="
        { EQUAL }
    | "!="
        { NOT_EQUAL }
    | '&'
        { BITWISE_AND }
    | '|'
        { BITWISE_OR }
    | "&&"
        { LOGICAL_AND }
    | "||"
        { LOGICAL_OR }
    | "<<"
        { SHIFT_LEFT }
    | ">>"
        { SHIFT_RIGHT }
    | '='
        { ASSIGN }
    | '.'
        { DOT }
    | ','
        { COMMA }
    | ';'
        { SEMICOLON }
    | '('
        { LPAREN }
    | ')'
        { RPAREN }
    | '{'
        { LBRACE }
    | '}'
        { RBRACE }
    | '['
        { LBRACKET }
    | ']'
        { RBRACKET }
    | "++"
        { INCREMENT }
    | "--"
        { DECREMENT }
    | eof
        { EOF }
    | _ as c
        { raise (Error c) }

and handle_multiline_comment = parse
    | end_multiline_comment
        { token lexbuf }
    | new_line
        { Lexing.new_line lexbuf; handle_multiline_comment lexbuf }
    | eof
        { exit 1 }
    | _
        { handle_multiline_comment lexbuf }
