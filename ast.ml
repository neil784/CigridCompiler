open Printf

(* AST Node Types *)
type uop = UopNot | UopBitwiseNot | UopNegate

type bop = BopAdd | BopSub | BopMul | BopDiv | BopMod | BopLessThan | BopGreaterThan | BopLessThanEqual | BopGreaterThanEqual | BopEqual | BopNotEqual | BopBitwiseAnd | BopBitwiseOr | BopLogicalAnd | BopLogicalOr | BopShiftRight | BopShiftLeft

type ty =
  | TVoid
  | TInt
  | TChar
  | TIdent of string
  | TPoint of ty

type expr =
  | EVar         of string
  | EInt         of int
  | EChar        of char
  | EString      of string
  | EBinOp       of bop * expr * expr
  | EUnOp        of uop * expr
  | ECall        of string * expr list
  | ENew         of ty * expr
  | EArrayAccess of string * expr * string
  | EEmpty

type stmt =
  | SExpr        of expr
  | SVarDef      of ty * string * expr
  | SVarAssign   of string * expr
  | SArrayAssign of string * expr * string * expr
  | SScope       of stmt list
  | SIf          of expr * stmt * stmt
  | SWhile       of expr * stmt
  | SBreak
  | SReturn      of expr
  | SDelete      of string
  | SEmpty

type params =
  | PParam of ty * string

type struct_attributes =
  | SParam of ty * string

type global =
  | GFuncDef  of ty * string * params list * stmt
  | GFuncDecl of ty * string * params list
  | GVarDef   of ty * string * expr
  | GVarDecl  of ty * string
  | GStruct   of string * struct_attributes list

type program =
  | PProgram of global list

(* Pretty printing AST *)
let pprint_ident = function
  | "" -> ""
  | s  -> "\"" ^ s ^ "\""

let pprint_uop = function
  | UopNot        -> "!"
  | UopBitwiseNot -> "~"
  | UopNegate     -> "-"

let pprint_bop = function
  | BopAdd              -> "+"
  | BopSub              -> "-"
  | BopMul              -> "*"
  | BopDiv              -> "/"
  | BopMod              -> "%"
  | BopLessThan         -> "<"
  | BopGreaterThan      -> ">"
  | BopLessThanEqual    -> "<="
  | BopGreaterThanEqual -> ">="
  | BopEqual            -> "=="
  | BopNotEqual         -> "!="
  | BopBitwiseAnd       -> "&"
  | BopBitwiseOr        -> "|"
  | BopLogicalAnd       -> "&&"
  | BopLogicalOr        -> "||"
  | BopShiftLeft        -> "<<"
  | BopShiftRight       -> ">>"

let rec pprint_ty = function
  | TVoid     -> "TVoid"
  | TInt      -> "TInt"
  | TChar     -> "TChar"
  | TIdent(r) -> "TIdent(" ^ pprint_ident r ^ ")"
  | TPoint(t) -> "TPoint(" ^ pprint_ty t ^ ")"

let pprint_char = function
  | '\n' -> "EChar(\'\\n\')"
  | '\t' -> "EChar(\'\\t\')"
  | '\\' -> "EChar(\'\\\\\')"
  | '\'' -> "EChar(\'\\\'\')"
  | '\"' -> "EChar(\'\\\"\')"
  | c    -> sprintf "EChar(\'%c\')" c

let rec pprint_expr = function
  | EVar(r)                 -> "EVar(" ^ pprint_ident r ^ ")"
  | EInt(i)                 -> "EInt(" ^ string_of_int i ^ ")"
  | EChar(c)                -> pprint_char c
  | EString(r)              -> "EString(" ^ r ^ ")"
  | EBinOp(bop, e1, e2)     -> "EBinOp(" ^ pprint_bop bop ^ ", " ^ pprint_expr e1 ^ ", " ^ pprint_expr e2 ^ ")"
  | EUnOp(uop, e)           -> "EUnOp(" ^ pprint_uop uop ^ ", " ^ pprint_expr e ^ ")"
  | ECall(r, el)            -> "ECall(" ^ pprint_ident r ^ "," ^ "{" ^ pprint_expr_list el ^ "})"
  | ENew(t, e)              -> "ENew(" ^ pprint_ty t ^ ", " ^ pprint_expr e ^ ")"
  | EArrayAccess(r1, e, r2) -> "EArrayAccess(" ^ pprint_ident r1 ^ ", " ^ pprint_expr e ^ ", " ^ pprint_ident r2 ^ ")"
  | EEmpty                  -> ""
and pprint_expr_list expr_list =
  match expr_list with
  | []        -> ""
  | expr_list -> String.concat " " (List.map pprint_expr expr_list)

let rec pprint_stmt = function
  | SExpr(e)                     -> "SExpr(" ^ pprint_expr e ^ ")"
  | SVarDef(t, r, e)             -> "SVarDef(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", " ^ pprint_expr e ^ ")"
  | SVarAssign(r, e)             -> "SVarAssign(" ^ pprint_ident r ^ ", " ^ pprint_expr e ^ ")"
  | SArrayAssign(r1, e1, r2, e2) -> "SArrayAssign(" ^ pprint_ident r1 ^ ", " ^ pprint_expr e1 ^ ", " ^ pprint_ident r2 ^ ", " ^ pprint_expr e2 ^ ")"
  | SScope(sl)                   -> "SScope({\n" ^ pprint_stmt_list sl ^ "\n})"
  | SIf(e, s1, s2)               -> "SIf(" ^ pprint_expr e ^ ", " ^ pprint_stmt s1 ^ ", " ^ pprint_stmt s2 ^ ")"
  | SWhile(e, s)                 -> "SWhile(" ^ pprint_expr e ^ ", " ^ pprint_stmt s ^ ")"
  | SBreak                       -> "SBreak"
  | SReturn(e)                   -> "SReturn(" ^ pprint_expr e ^ ")"
  | SDelete(r)                   -> "SDelete(" ^ pprint_ident r ^ ")"
  | SEmpty                       -> ""
and pprint_stmt_list stmt_list =
  match stmt_list with
  | []        -> ""
  | stmt_list -> String.concat "\n" (List.map pprint_stmt stmt_list)

let pprint_params = function
  | []          -> "{}"
  | params_list -> "{" ^ String.concat " " (List.map (function PParam(t, r) -> Printf.sprintf "(%s,%s)" (pprint_ty t) (pprint_ident r)) params_list) ^ "}"

let pprint_struct_attributes = function
  | []                -> "{}"
  | struct_attributes -> "{\n" ^ String.concat "\n" (List.map (function SParam(t, r) -> Printf.sprintf "(%s, %s)" (pprint_ty t) (pprint_ident r)) struct_attributes) ^ "\n}"

let pprint_global = function
  | GFuncDef(t, r, p, s)  -> "GFuncDef(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", " ^ pprint_params p ^ ", " ^ pprint_stmt s ^ ")"
  | GFuncDecl(t, r, p)    -> "GFuncDecl(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", " ^ pprint_params p ^ ")"
  | GVarDef(t, r, e)      -> "GVarDef(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ", " ^ pprint_expr e ^ ")"
  | GVarDecl(t, r)        -> "GVarDecl(" ^ pprint_ty t ^ ", " ^ pprint_ident r ^ ")"
  | GStruct(r, sa)        -> "GStruct(" ^ pprint_ident r ^ ", " ^ pprint_struct_attributes sa ^ ")"

let pprint_global_list global_list =
  match global_list with
  | []          -> ""
  | global_list -> String.concat "\n\n" (List.map pprint_global global_list)

let pprint_program = function
  | PProgram(gl) -> pprint_global_list gl

(* Populating (variable identifier -> memory address) hash table from AST *)
let stmt_ht hash_table = function
  | SVarDef(t, r, e) -> Hashtbl.add hash_table r ("qword [rsp + " ^ string_of_int (Hashtbl.length hash_table * 8) ^ "]")
  | _ -> ()

let rec stmt_list_ht hash_table = function
  | s::sl -> stmt_ht hash_table s; stmt_list_ht hash_table sl
  | _ -> ()

let global_ht hash_table = function
  | GFuncDef(t, r, p, SScope(sl)) -> stmt_list_ht hash_table sl
  | _ -> ()

let rec global_list_ht hash_table = function
  | g::gl -> global_ht hash_table g; global_list_ht hash_table gl
  | _ -> ()

let program_ht hash_table = function
  | PProgram(gl) -> global_list_ht hash_table gl

(* Generating ASM from AST *)
let rec expr_asm r hash_table = function
  | EInt(i)                -> "\n\tadd\t" ^ Hashtbl.find hash_table r ^ ", " ^ string_of_int i
  | EVar(s)                -> if r = s then ("\n\tadd\t" ^ Hashtbl.find hash_table r ^ ", r10") else ("\n\tmov\tr11, " ^ Hashtbl.find hash_table s ^ "\n\tadd\t" ^ Hashtbl.find hash_table r ^ ", r11")
  | EBinOp(BopAdd, e1, e2) -> expr_asm r hash_table e1 ^ expr_asm r hash_table e2
  | _                      -> ""

let stmt_asm hash_table = function
  | SVarDef(t, r, e) -> "mov\t" ^ Hashtbl.find hash_table r ^ ", 0" ^ expr_asm r hash_table e
  | SVarAssign(r, e) -> "mov\tr10, " ^ Hashtbl.find hash_table r ^ "\n\tmov\t" ^ Hashtbl.find hash_table r ^ ", 0" ^ expr_asm r hash_table e
  | SReturn(EVar(r)) -> "mov\trax, " ^ Hashtbl.find hash_table r ^ "\n\tadd\trsp, " ^ string_of_int (Hashtbl.length hash_table * 8) ^ "\n\tret"
  | SReturn(EInt(i)) -> "mov\trax, " ^ string_of_int i ^ "\n\tadd\trsp, " ^ string_of_int (Hashtbl.length hash_table * 8) ^ "\n\tret"
  | SReturn(EEmpty)  -> "add\trsp, " ^ string_of_int (Hashtbl.length hash_table * 8) ^ "\n\tret"
  | _                -> ""

let stmt_list_asm hash_table stmt_list =
  match stmt_list with
  | [] -> ""
  | stmt_list -> "\t" ^ String.concat "\n\t" (List.map (stmt_asm hash_table) stmt_list)

let global_asm hash_table = function
  | GFuncDef(t, r, p, SScope(sl)) -> r ^ ":\n\tsub\trsp, " ^ string_of_int (Hashtbl.length hash_table * 8) ^ "\n" ^ stmt_list_asm hash_table sl
  | _ -> ""

let global_list_asm hash_table global_list =
  match global_list with
  | [] -> ""
  | global_list -> String.concat "\n\n" (List.map (global_asm hash_table) global_list)

let program_asm hash_table = function
  | PProgram(gl) -> "\tglobal\tmain\n\n\tsection\t.text\n\n" ^ global_list_asm hash_table gl
