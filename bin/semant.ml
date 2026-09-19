module TAst = TypedAst
module Sym = Symbol
exception Unimplemented (* your code should eventually compile without this exception *)

let typecheck_typ = function
| Ast.Int -> TAst.Int
| Ast.Bool -> TAst.Bool

let tbinop_of_binop = function
  | Ast.Plus  -> TAst.Plus
  | Ast.Minus -> TAst.Minus
  | Ast.Mul   -> TAst.Mul
  | Ast.Div   -> TAst.Div
  | Ast.Rem   -> TAst.Rem
  | Ast.Lt    -> TAst.Lt
  | Ast.Le    -> TAst.Le
  | Ast.Gt    -> TAst.Gt
  | Ast.Ge    -> TAst.Ge
  | Ast.Lor   -> TAst.Lor
  | Ast.Land  -> TAst.Land
  | Ast.Eq    -> TAst.Eq
  | Ast.NEq   -> TAst.NEq 

let tunop_of_unop = function
  | Ast.Neg   -> TAst.Neg
  | Ast.Lnot  -> TAst.Lnot

let tident_of_ident = function
  | Ast.Ident { name } -> TAst.Ident { sym = Sym.symbol name }

let join typ1 typ2 typ =
  if typ1 = TAst.ErrorType || typ2 = TAst.ErrorType then
    TAst.ErrorType
  else
    typ

(* should return a pair of a typed expression and its inferred type. you can/should use typecheck_expr inside infertype_expr. *)
let rec infertype_expr env expr =
  match expr with
  | Ast.Integer { int } -> (TAst.Integer {int = int}, TAst.Int)
  | Ast.Boolean { bool } -> (TAst.Boolean {bool = bool}, TAst.Bool)

  | Ast.BinOp { left; op; right } -> (
      (* TODO: consider returning TAst.TypeError in case of type error *)
      match op with
      | Ast.Plus | Ast.Minus | Ast.Mul | Ast.Div | Ast.Rem ->
          let texpr1, typ1 = typecheck_expr env left TAst.Int in
          let texpr2, typ2 = typecheck_expr env right TAst.Int in
          let typ = join typ1 typ2 TAst.Int in (
            TAst.BinOp {left = texpr1; op = (tbinop_of_binop op); right = texpr2; tp = typ},
            typ
          )
      | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
          let texpr1, typ1 = typecheck_expr env left TAst.Int in
          let texpr2, typ2 = typecheck_expr env right TAst.Int in
          let typ = join typ1 typ2 TAst.Bool in (
            TAst.BinOp {left = texpr1; op = (tbinop_of_binop op); right = texpr2; tp = typ},
            typ
          )
      | Ast.Lor | Ast.Land ->
          let texpr1, typ1 = typecheck_expr env left TAst.Bool in
          let texpr2, typ2 = typecheck_expr env right TAst.Bool in
          let typ = join typ1 typ2 TAst.Bool in (
            TAst.BinOp {left = texpr1; op = (tbinop_of_binop op); right = texpr2; tp = typ},
            typ
          )
      | Ast.Eq | Ast.NEq ->
          let texpr1, typ1 = infertype_expr env left in
          let texpr2, typ2 = typecheck_expr env right typ1 in 
          let typ = join typ1 typ2 TAst.Bool in (
            TAst.BinOp {left = texpr1; op = (tbinop_of_binop op); right = texpr2; tp = typ},
            typ
          )
    )

    | Ast.UnOp { op; operand } -> (
        match op with
        | Ast.Neg ->
            let texpr, typ = typecheck_expr env operand TAst.Int in (
              TAst.UnOp {op = (tunop_of_unop op); operand = texpr; tp = typ},
              typ
            )
        | Ast.Lnot ->
            let texpr, typ = typecheck_expr env operand TAst.Bool in (
              TAst.UnOp {op = (tunop_of_unop op); operand = texpr; tp = typ},
              typ
            )
      )

    | Ast.Lval lvl -> 
        let tlvl, typ = infertype_lval env lvl in (TAst.Lval tlvl, typ)

    | Ast.Assignment { lvl; rhs } ->
        let tlvl, typ1 = infertype_lval env lvl in
        let trhs, typ2 = typecheck_expr env rhs typ1 in (
          TAst.Assignment {lvl = tlvl; rhs = trhs; tp = typ2},
          typ2
        )

    | Ast.Call { fname; args } -> raise Unimplemented

and infertype_lval env lvl =
  match lvl with
  | _ -> raise Unimplemented

(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr env expr typ =
  let texpr, intyp = infertype_expr env expr in

  if intyp = TAst.ErrorType || typ <> intyp then
    (* TODO: report error in env *)
    (texpr, TAst.ErrorType)
  else
    (texpr, intyp)

(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)
let rec typecheck_statement env stm =
  match stm with
  | Ast.VarDeclStm { name; tp; body } -> (
      match tp with
      | Some tp -> let _tbody, _typ = typecheck_expr env body TAst.Bool in (* TODO *)
          raise Unimplemented
      | None -> let tbody, typ = infertype_expr env body in (
          TAst.VarDeclStm { name = tident_of_ident name; tp = typ; body = tbody },
          TAst.Bool
        )
    )
  | Ast.ExprStm { expr } -> raise Unimplemented
  | Ast.IfThenElseStm { cond; thbr; elbro } -> raise Unimplemented
  | Ast.CompoundStm { stms } -> raise Unimplemented
  | Ast.ReturnStm { ret } -> raise Unimplemented

(* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq env stms = raise Unimplemented

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment = raise Unimplemented

(* let rec typecheck_binop env left op right = *)
(*   let (texpr1, typ1) = typecheck_expr env left in *)
(*   let (texpr2, typ2) = typecheck_expr env right in *)
(*   match typ1, typ2 with *)
(*   | TAst.ErrorType, _ | _, TAst.ErrorType -> *)
(*       (TAst.BinOp {left = texpr1; op = TAst.Plus; right = texpr2; tp = TAst.ErrorType}, TAst.ErrorType) *)
(*   | TAst.Int, TAst.Int -> raise Unimplemented *)
(*   | _, _ -> raise Unimplemented *)
(**)
(* and typecheck_expr env = function *)
(*   | Ast.Integer { int } -> (TAst.Integer {int = int}, TAst.Int) *)
(*   | Ast.Boolean { bool } -> (TAst.Boolean {bool = bool}, TAst.Bool) *)
(*   | Ast.BinOp { left; op; right } -> ( *)
(*       raise Unimplemented *)
(*     ) *)
(*   | _ -> raise Unimplemented *)

(* should check that the program (sequence of statements) ends in a return statement and make sure that all statements are valid as described in the assignment. Should use typecheck_statement_seq. *)
let typecheck_prog prg = raise Unimplemented
