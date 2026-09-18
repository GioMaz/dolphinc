open Semant

let compile_prog_from_ast p =
    let _ = typecheck_prog p in
    None
