module Err = Errors
module Sym = Symbol
module TAst = TypedAst

exception Unimplemented

type varOrFun =
| Var of TAst.typ
| Fun of TAst.funtype

type environment = {
  errs : Err.error list ref;
  typs : varOrFun Sym.Table.t;
}

let empty = {
  errs = ref [];
  typs = Sym.Table.empty;
}

(* create an initial environment with the given functions defined *)
let make_env function_types =
      let emp = Sym.Table.empty in
      let env =
        List.fold_left (fun env (fsym, ftp)
          -> Sym.Table.add fsym (Fun ftp) env) emp function_types
      in {
        typs= env;
        errs = ref []
      }

let insert_err env err = env.errs := err :: !(env.errs)

(* insert a local declaration into the environment *)
let insert_local_decl env sym typ = 
  { env with typs = Sym.Table.add sym typ env.typs }

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym = Sym.Table.find_opt sym env.typs
