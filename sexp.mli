type sexp = Atom of string | List of sexp list

val parse : unit -> sexp
val prerr : sexp -> unit
