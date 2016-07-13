type sexp = Atom of string | List of sexp list

val prerr_sexp : sexp -> unit

val parse_stdin : unit -> sexp
val parse_string : string -> sexp

val tests : (string * (unit -> bool)) list
