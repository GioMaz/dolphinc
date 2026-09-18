open Ast

let example_1 = [
    VarDeclStm { name = Ident { name = "a" } ; tp = None; body = Integer { int = 32L } };
    ReturnStm { ret = Lval (Var (Ident {name = "a"}))}
]
