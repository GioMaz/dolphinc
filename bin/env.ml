module Err = Errors
module Sym = Symbol
module SymMap = Symbol.Table
module TAst = TypedAst


exception Unimplemented

type environment = {
  errs : Err.error list ref;
  vartyps : TAst.rettyp SymMap.t;
  funtyps : (TAst.typ list * TAst.rettyp) SymMap.t;
}

let empty = {
  errs = ref [];
  vartyps = SymMap.empty;
  funtyps = SymMap.empty;
}

(* create an initial environment with the given functions defined *)
let make_env function_types = {
  errs = ref [];
  vartyps = SymMap.empty; 
  funtyps = List.fold_left
    (fun map (sym, typ) -> SymMap.add sym typ map)
    SymMap.empty
    function_types;
}

let insert_err env err = env.errs := err :: !(env.errs)

(* insert a local declaration into the environment *)
let insert_local_decl env sym typ = 
  let rettyp = TAst.RetTyp typ in
  { env with vartyps = SymMap.add sym rettyp env.vartyps }

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym =
  match SymMap.find_opt sym env.vartyps with
  | Some typ -> Some ([], typ)
  | None ->
      match SymMap.find_opt sym env.funtyps with
      | Some typ -> Some typ
      | None -> None
