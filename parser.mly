%{ open Ast %}

// Value Tokens
%token <string> IDENT
%token <int>    INTEGER_CONSTANT
%token <char>   CHAR_CONSTANT
%token <string> STRING_CONSTANT

// Keywords
%token BREAK  "break"
%token CHAR   "char"
%token DELETE "delete"
%token ELSE   "else"
%token EXTERN "extern"
%token FOR    "for"
%token IF     "if"
%token INT    "int"
%token NEW    "new"
%token RETURN "return"
%token STRUCT "struct"
%token VOID   "void"
%token WHILE  "while"

// Unops
%token NOT         "!"
%token BITWISE_NOT "~"

// Binops
%token ADD                "+"
%token SUB                "-"
%token MUL                "*"
%token DIV                "/"
%token MOD                "%"
%token LESS_THAN          "<"
%token GREATER_THAN       ">"
%token LESS_THAN_EQUAL    "<="
%token GREATER_THAN_EQUAL ">="
%token EQUAL              "=="
%token NOT_EQUAL          "!="
%token BITWISE_AND        "&"
%token BITWISE_OR         "|"
%token LOGICAL_AND        "&&"
%token LOGICAL_OR         "||"
%token SHIFT_LEFT         "<<"
%token SHIFT_RIGHT        ">>"

// Misc
%token ASSIGN    "="
%token DOT       "."
%token COMMA     ","
%token SEMICOLON ";"
%token LPAREN    "("
%token RPAREN    ")"
%token LBRACE    "{"
%token RBRACE    "}"
%token LBRACKET  "["
%token RBRACKET  "]"
%token INCREMENT "++"
%token DECREMENT "--"
%token EOF

// Precedence Rules
%left "||"
%left "&&"
%left "|"
%left "&"
%left "==" "!="
%left "<" ">" "<=" ">="
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%right "!" "~"
%nonassoc NEGATE

%start program
%type <program> program

%%

ty:
    | "void"
        { TVoid }
    | "int"
        { TInt }
    | "char"
        { TChar }
    | r = IDENT
        { TIdent(r) }
    | t = ty "*"
        { TPoint(t) }

expr:
    | r = IDENT
        { EVar(r) }
    | i = INTEGER_CONSTANT
        { EInt(i) }
    | c = CHAR_CONSTANT
        { EChar(c) }
    | r = STRING_CONSTANT
        { EString(r) }
    | e1 = expr "+" e2 = expr
        { EBinOp(BopAdd, e1, e2) }
    | e1 = expr "-" e2 = expr
        { EBinOp(BopSub, e1, e2) }
    | e1 = expr "*" e2 = expr
        { EBinOp(BopMul, e1, e2) }
    | e1 = expr "/" e2 = expr
        { EBinOp(BopDiv, e1, e2) }
    | e1 = expr "%" e2 = expr
        { EBinOp(BopMod, e1, e2) }
    | e1 = expr "<" e2 = expr
        { EBinOp(BopLessThan, e1, e2) }
    | e1 = expr ">" e2 = expr
        { EBinOp(BopGreaterThan, e1, e2) }
    | e1 = expr "<=" e2 = expr
        { EBinOp(BopLessThanEqual, e1, e2) }
    | e1 = expr ">=" e2 = expr
        { EBinOp(BopGreaterThanEqual, e1, e2) }
    | e1 = expr "==" e2 = expr
        { EBinOp(BopEqual, e1, e2) }
    | e1 = expr "!=" e2 = expr
        { EBinOp(BopNotEqual, e1, e2) }
    | e1 = expr "&" e2 = expr
        { EBinOp(BopBitwiseAnd, e1, e2) }
    | e1 = expr "|" e2 = expr
        { EBinOp(BopBitwiseOr, e1, e2) }
    | e1 = expr "&&" e2 = expr
        { EBinOp(BopLogicalAnd, e1, e2) }
    | e1 = expr "||" e2 = expr
        { EBinOp(BopLogicalOr, e1, e2) }
    | e1 = expr "<<" e2 = expr
        { EBinOp(BopShiftLeft, e1, e2) }
    | e1 = expr ">>" e2 = expr
        { EBinOp(BopShiftRight, e1, e2) }
    | "!" e = expr
        { EUnOp(UopNot, e) }
    | "~" e = expr
        { EUnOp(UopBitwiseNot, e) }
    | "-" e = expr %prec NEGATE
        { EUnOp(UopNegate, e) }
    | r = IDENT "(" el = expr_list ")"
        { ECall(r, el) }
    | "new" t = ty "[" e = expr "]"
        { ENew(t, e) }
    | r1 = IDENT "[" e = expr "]" "." r2 = IDENT
        { EArrayAccess(r1, e, r2) }
    | r = IDENT "[" e = expr "]"
        { EArrayAccess(r, e, "") }    
    | "(" e = expr ")"
        { e }

expr_list:
    | /* empty */
        { [] }
    | e = expr
        { [e] }
    | el = expr_list "," e = expr
        { el @ [e] }

stmt:
    | v = varassign ";"
        { v }
    | "{" sl = stmt_list "}"
        { SScope(sl) }
    | "if" "(" e = expr ")" s1 = stmt "else" s2 = stmt
        { SIf(e, s1, s2) }
    | "if" "(" e = expr ")" s = stmt
        { SIf(e, s, SEmpty) }
    | "while" "(" e = expr ")" s = stmt
        { SWhile(e, s) }
    | "break" ";"
        { SBreak }
    | "return" e = expr ";"
        { SReturn(e) }
    | "return" ";"
        { SReturn(EEmpty) }
    | "delete" "[" "]" r = IDENT ";"
        { SDelete(r) }
    | "for" "(" v = varassign ";" e = expr ";" a = assign ")" s = stmt
        { SScope([v; SWhile(e, SScope([s; a]))]) }

stmt_list:
    | /* empty */
        { [] }
    | s = stmt
        { [s] }
    | sl = stmt_list s = stmt
        { sl @ [s] }

assign:
    | r = IDENT "(" el = expr_list ")"
        { SExpr(ECall(r, el)) }
    | r = IDENT "=" e = expr
        { SVarAssign(r, e) }
    | r = IDENT "++"
        { SVarAssign(r, EBinOp(BopAdd, EVar(r), EInt(1))) }
    | r = IDENT "--"
        { SVarAssign(r, EBinOp(BopSub, EVar(r), EInt(1))) }
    | r1 = IDENT "[" e1 = expr "]" "." r2 = IDENT "=" e2 = expr
        { SArrayAssign(r1, e1, r2, e2) }
    | r1 = IDENT "[" e = expr "]" "." r2 = IDENT "++"
        { SArrayAssign(r1, e, r2, EBinOp(BopAdd, EArrayAccess(r1, e, r2), EInt(1))) }
    | r1 = IDENT "[" e = expr "]" "." r2 = IDENT "--"
        { SArrayAssign(r1, e, r2, EBinOp(BopSub, EArrayAccess(r1, e, r2), EInt(1))) }
    | r = IDENT "[" e1 = expr "]" "=" e2 = expr
        { SArrayAssign(r, e1, "", e2) }
    | r = IDENT "[" e = expr "]" "++"
        { SArrayAssign(r, e, "", EBinOp(BopAdd, EArrayAccess(r, e, ""), EInt(1))) }
    | r = IDENT "[" e = expr "]" "--"
        { SArrayAssign(r, e, "", EBinOp(BopSub, EArrayAccess(r, e, ""), EInt(1))) }

varassign:
    | t = ty r = IDENT "=" e = expr
        { SVarDef(t, r, e) }
    | a = assign
        { a }

params:
    | /* empty */
        { [] }
    | t = ty r = IDENT
        { [PParam(t, r)] }
    | p = params "," t = ty r = IDENT
        { p @ [PParam(t, r)] }

struct_attributes:
    | /* empty */
        { [] }
    | t = ty r = IDENT ";"
        { [SParam(t, r)] }
    | sa = struct_attributes t = ty r = IDENT ";"
        { sa @ [SParam(t, r)] }

global:
    | t = ty r = IDENT "(" p = params ")" "{" sl = stmt_list "}"
        { GFuncDef(t, r, p, SScope(sl)) }
    | "extern" t = ty r = IDENT "(" p = params ")" ";"
        { GFuncDecl(t, r, p) }
    | t = ty r = IDENT "=" e = expr ";"
        { GVarDef(t, r, e) }
    | "extern" t = ty r = IDENT ";"
        { GVarDecl(t, r) }
    | "struct" r = IDENT "{" sa = struct_attributes "}" ";"
        { GStruct(r, sa) }

global_list:
    | /* empty */
        { [] }
    | g = global
        { [g] }
    | gl = global_list g = global
        { gl @ [g] }

program:
    | gl = global_list EOF
        { PProgram(gl) }
