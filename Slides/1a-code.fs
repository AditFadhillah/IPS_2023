module CodeLec1a

type Value = IntVal  of int
           | BoolVal of bool

type Exp = Constant of Value
         | Plus   of Exp * Exp
         | Minus  of Exp * Exp
         | Times  of Exp * Exp
         | Divide of Exp * Exp
         | Let    of (string * Exp * Exp)
         | Var    of string
         | If     of Exp * Exp * Exp
         | Less   of Exp * Exp
         | Equal  of Exp * Exp
         | And    of Exp * Exp
         | Or     of Exp * Exp

type SymTab<'a> = SymTab of (string * 'a) list

let empty () = SymTab []
let bind n i (SymTab stab) = SymTab ((n,i)::stab)
let rec lookup n tab = match tab with
                        | SymTab [] -> None
                        | SymTab ((n1,i1)::remtab) ->
                           if n = n1 then Some i1
                           else lookup n (SymTab remtab)

exception MyError of string

let rec eval vtable e =
  match e with
    | Constant v -> v
    | Plus (e1, e2) ->
      match (eval vtable e1, eval vtable e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 + n2)
        | _ -> raise (MyError "Operands to + are not ints")
    | Minus (e1, e2) ->
      match (eval vtable e1, eval vtable e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 - n2)
        | _ -> raise (MyError "Operands to - are not ints")
    | Times (e1, e2) ->
      match (eval vtable e1, eval vtable e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
        | _ -> raise (MyError "Operands to * are not ints")
    | Divide (e1, e2) ->
      match (eval vtable e1, eval vtable e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 / n2)
        | _ -> raise (MyError "Operands to / are not ints")
    | Var v ->
      match lookup v vtable with
        | None -> raise (MyError ("Unknown variable "+v))
        | Some n -> n
    | Let (v, e1, e2) ->
      let n1 = eval vtable e1
      let vtable1 = bind v n1 vtable
      eval vtable1 e2
    | Equal (e1, e2) ->
      match (eval vtable e1, eval vtable e2) with
        | (IntVal  n1, IntVal n2)  -> BoolVal (n1 = n2)
        | (BoolVal b1, BoolVal b2) -> BoolVal (b1 = b2)
        | _ -> raise (MyError "Operands to = not same type")
    | Less (e1, e2) ->
      match (eval vtable e1, eval vtable e2) with
        | (IntVal n1, IntVal n2) -> BoolVal (n1 < n2)
        | _ -> raise (MyError "Operands to < not integers")
    | If (cond, truebranch, falsebranch) ->
      match eval vtable cond with
        | BoolVal true  -> eval vtable truebranch
        | BoolVal false -> eval vtable falsebranch
        | _ -> raise (MyError "If condition not a bool")
    | And (e1, e2) ->
      match eval vtable e1 with
        | BoolVal true  -> eval vtable e2
        | BoolVal false -> BoolVal false
        | _ -> raise (MyError "And operand not a bool")
    | Or (e1, e2) ->
      match eval vtable e1 with
        | BoolVal true  -> BoolVal true
        | BoolVal false -> eval vtable e2
        | _ -> raise (MyError "Or  operand not a bool")

let ex1 = eval (empty ()) (Plus (Constant (IntVal 2),
                                 Constant (IntVal 3)))

let ex2 = eval (empty ()) (If (Constant (BoolVal true),
                               Constant (IntVal 2),
                               Divide (Constant (IntVal 2),
                                       Constant (IntVal 0))))
