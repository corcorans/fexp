(*

   Name:        Stephen Corcoran
   Class:       CS 3520, Fall 2012
   Assignment:  MP7

*)


let rec value = function
   | Cont(x) -> x
   | Add(x, y) -> value(x) + value(y)
   | Subt(x, y) -> value(x) - value(y)
   | Mult(x, y) -> value(x) * value(y)
   | Div(x, y) -> value(x) / value(y)
   | If(x, y, z) -> if value(x) == 0 then value(y)
                  else value(z)

type Expr =
     Const of int
   | Add   of Expr * Expr
   | Subt  of Expr * Expr
   | Mult  of Expr * Expr
   | Div   of Expr * Expr
   | If    of Expr * Expr * Expr


let rec code = function
   | Const(x)   -> [PUSH(x)]
   | Add(l,  r) -> code(l) @ code(r) @ [ADD]
   | Subt(l, r) -> code(l) @ code(r) @ [SUB]
   | Mult(l, r) -> code(l) @ code(r) @ [MUL]
   | Div(l,  r) -> code(l) @ code(r) @ [DIV]
   | If(test, truepart, falsepart) -> 
       let tp = code(truepart)
       let fp = code(falsepart)
       in code(test) @ [JIZ(List.length(tp) + 1)]
          @ tp @ [JMP(List.length(fp))] @ fp

let adjacent_filter = function