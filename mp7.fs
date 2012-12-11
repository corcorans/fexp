(*

Name: Stephen Corcoran
Class: CS 3520, Fall 2012
Assignment: MP7

*)

// Expression decleration.
type Expr =
     Const of int
   | Add of Expr * Expr
   | Subt of Expr * Expr
   | Mult of Expr * Expr
   | Div of Expr * Expr
   | If of Expr * Expr * Expr

// Determines the value that are passed in recursively.
let rec value = function
     Const(x)    -> x
   | Add(x, y)   -> value(x) + value(y)
   | Subt(x, y)  -> value(x) - value(y)
   | Mult(x, y)  -> value(x) * value(y)
   | Div(x, y)   -> value(x) / value(y)
   | If(x, y, z) -> if value(x) = 0 then value(z)
                    else value(y)

// Instruction decleration.
type Instr = 
   | PUSH of int
   | ADD
   | SUB
   | MUL
   | DIV
   | JIZ of int
   | JMP of int
   | POP

// Determines the code that is passed in recursively.
let rec code = function
   | Const(x)   -> [PUSH(x)]
   | Add(l, r)  -> code(l) @ code(r) @ [ADD]
   | Subt(l, r) -> code(l) @ code(r) @ [SUB]
   | Mult(l, r) -> code(l) @ code(r) @ [MUL]
   | Div(l, r)  -> code(l) @ code(r) @ [DIV]
   | If(test, truepart, falsepart) ->
       let tp = code(truepart)
       let fp = code(falsepart)
       in code(test) @ [JIZ(List.length(tp) + 1)]
          @ tp @ [JMP(List.length(fp))] @ fp

// Returns a sublist of the items in which the first predicate returns
// true and the following after that is true.
let rec adjacent_filter predicate1 predicate2 items =
    match items with
    | []    -> []
    | X::[] -> []
    | x::xs when predicate1(x) && predicate2(List.head(xs)) 
            -> x::(adjacent_filter predicate1 predicate2 xs)
    | x::xs -> adjacent_filter predicate1 predicate2 xs

// Returns true for ADD otherwise false.
let is_add = function
    | ADD -> true
    | _   -> false 

// Returns true for SUB otherwise false.
let is_sub = function
    | SUB -> true
    | _   -> false    

// Returns true for DIV otherwise false.    
let is_div = function
    | DIV -> true
    | _   -> false

// Returns true for MUL otherwise false.    
let is_mul = function
    | MUL -> true
    | _   -> false

// Returns true for JIZ otherwise false.
let is_jiz = function
    | JIZ _ -> true
    | _     -> false

// Counts the number of times either ADD, SUB, MUL or DIV appears before JIZ.    
let complex_tests = function
    | []  -> 0
    | record -> 
         let add = List.length(adjacent_filter is_add is_jiz record)
         let sub = List.length(adjacent_filter is_sub is_jiz record)
         let div = List.length(adjacent_filter is_div is_jiz record)
         let mul = List.length(adjacent_filter is_mul is_jiz record)
         in int(add + sub + div + mul)