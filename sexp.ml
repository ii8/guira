
type sexp = Atom of string | List of sexp list

type state =
  | In_start
  | In_list of sexp list
  | In_atom of char list * sexp list
  | In_string of char list * sexp list
  | In_comment

let atom cs =
  let len = List.length cs in
  let res = Bytes.create len in
  let rec imp i = function
    | [] -> Bytes.to_string res
    | c :: cs -> Bytes.set res i c; imp (i - 1) cs in
  Atom (imp (len - 1) cs)

let parse next =
  let rec r state = match next () with
    | None -> failwith "unexpected eof"
    | Some c -> match state with
      | In_start -> begin match c with
        | ' ' | '\n' | '\t' -> r state
        | ';' -> ignore (r In_comment); r state
        | '(' -> r (In_list [])
        | _ -> failwith ("expressions must start with '('")
      end
      | In_list es -> begin match c with
        | ' ' | '\n' | '\t' -> r state
        | ';' -> ignore (r In_comment); r state
        | '(' -> r (In_list (r (In_list []) :: es))
        | ')' -> List (List.rev es)
        | '"' -> r (In_string ([], es))
        | _ -> r (In_atom ([c], es))
      end
      | In_atom (s, es) -> begin match c with
        | ' ' | '\n' | '\t' -> r (In_list (atom s :: es))
        | ';' -> ignore (r In_comment); r (In_list (atom s :: es))
        | '(' -> r (In_list (r (In_list []) :: atom s :: es))
        | ')' -> List (List.rev (atom s :: es))
        | _ -> r (In_atom (c::s, es))
      end
      | In_string (s, es) -> begin match c with
        | '"' -> r (In_list (atom s :: es))
        | _ -> r (In_string (c::s, es))
      end
      | In_comment -> begin match c with
        | '\n' -> Atom ""
        | _ -> r In_comment
      end in
  r In_start

let prerr_sexp exp =
  let rec p = function
    | Atom s -> s
    | List e -> "(" ^ (String.concat " " (List.map p e)) ^ ")" in
  prerr_endline(p exp)

let parse_stdin () =
  let next () = try Some (input_char stdin) with End_of_file -> None in
  parse next

let parse_string s =
  let l = String.length s in
  let i = ref (-1) in
  parse begin fun () ->
    i := !i + 1;
    if l > !i
      then Some (String.get s !i)
      else None
  end

(* Tests *)

let test_parse_string () =
  List.for_all (fun a -> a) [
    parse_string "(day)" = List [ Atom "day" ];
    parse_string "(month jul (day (nth 2)))" =
      List [
        Atom "month";
        Atom "jul";
        List [Atom "day"; List [Atom "nth"; Atom "2"] ] ];
  ]

let tests = [
    "Sexp.parse_string", test_parse_string;
  ]
