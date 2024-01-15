open Printf
open Ast

let print_incorrect_usage () =
  fprintf stderr "%s" ("Usage: ./cigrid <flags> <filename>\n" ^
                      "  --pretty-print     Pretty prints the Abstract Syntax Tree\n" ^
                      "  --line-error       Prints line number to stderr if lexing/parsing error occurs\n" ^
                      "  --asm              Generates nasm x86 assembly from AST and prints to stdout\n"); 
  exit 1

let does_flag_exist flag =
  let rec helper i =
    if i = Array.length Sys.argv - 1 then
      false
    else if Sys.argv.(i) = flag then
      true
    else
      helper (i + 1)
  in
  helper 1

let cigrid_to_ast () =
  let filename = Sys.argv.(Array.length Sys.argv - 1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let should_pretty_print = does_flag_exist "--pretty-print" in
  let should_show_line_error = does_flag_exist "--line-error" in
  let should_generate_asm = does_flag_exist "--asm" in
  let ast =
    try
      Parser.program Lexer.token lexbuf
    with
    | Lexer.Error(c) -> if should_show_line_error then (fprintf stderr "%d\n" lexbuf.lex_curr_p.pos_lnum; exit 1) else (exit 1)
    | Parser.Error -> if should_show_line_error then (fprintf stderr "%d\n" lexbuf.lex_curr_p.pos_lnum; exit 1) else (exit 1)
  in
  if should_pretty_print then (Printf.printf "%s\n" (pprint_program ast));
  if should_generate_asm then (
    let hash_table = Hashtbl.create 10 in
    program_ht hash_table ast;
    Printf.printf "%s\n" (program_asm hash_table ast)
  )

let check_flags () =
  let is_valid = function
    | "--pretty-print" -> true
    | "--line-error" -> true
    | "--asm" -> true
    | _ -> false
  in
  let rec helper i =
    if i = Array.length Sys.argv - 1 then
      true
    else if not (is_valid Sys.argv.(i)) then
      false
    else
      helper (i + 1)
  in
  helper 1

let () =
  if (Array.length Sys.argv < 2 || not (check_flags ())) then (
    print_incorrect_usage ()
  ) else (
    cigrid_to_ast ()
  )
