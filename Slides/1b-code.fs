Fsummodule CodeLec1b

type Value = IntVal  of int
           | BoolVal of bool

type Exp = Constant of Value
         | Plus   of Exp * Exp
         | Minus  of Exp * Exp
         | Times  of Exp * Exp
         | Divide of Exp * Exp
         | Let    of string * Exp * Exp
         | Var    of string
         | If     of Exp * Exp * Exp
         | Less   of Exp * Exp
         | Equal  of Exp * Exp
         | And    of Exp * Exp
         | Or     of Exp * Exp
         | Call   of string * Exp list

type Type  = Int | Bool
type Param = Type * string

type FunDec = FunDec of Type * string * Param list * Exp

type Prog = FunDec list


type SymTab<'a> = SymTab of (string * 'a) list

// empty : unit -> SymTab<'a>
let empty () = SymTab []

// bind : string -> 'a -> SymTab<'a> -> SymTab<'a>
let bind n i (SymTab stab) = SymTab ((n,i)::stab)

// lookup : string -> SymTab<'a> -> 'a option
let rec lookup n tab = match tab with
                        | SymTab [] -> None
                        | SymTab ((n1,i1)::remtab) ->
                           if n = n1 then Some i1
                           else lookup n (SymTab remtab)

type VarTab = SymTab<Value>
type FunTab = SymTab<FunDec>

exception MyError of string

// bindParams : Param list -> Value list -> VarTab
let rec bindParams params vals =
   match (params, vals) with
     | ([], []) -> empty()  // SymTab.empty()
     | ((par_type, par_name)::rparams, v::rvs) ->
       let vtable = bindParams rparams rvs
       match (par_type, v) with
        | (Int, IntVal  _) -> bind par_name v vtable
        | (Bool,BoolVal _) -> bind par_name v vtable
        | _ -> raise (MyError "Parameter type mismatch")
     | _ -> raise (MyError "Parameter count mismatch")

// eval : VarTab -> FunTab -> Exp -> Value
let rec eval vtable ftable e =
  match e with
    | Constant v -> v
    | Plus (e1, e2) ->
      match (eval vtable ftable e1, eval vtable ftable e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 + n2)
        | _ -> raise (MyError "Operands to + are not ints")
    | Minus (e1, e2) ->
      match (eval vtable ftable e1, eval vtable ftable e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 - n2)
        | _ -> raise (MyError "Operands to - are not ints")
    | Times (e1, e2) ->
      match (eval vtable ftable e1, eval vtable ftable e2) with
        | (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
        | _ -> raise (MyError "Operands to * are not ints")
    | Equal (e1, e2) ->
      match (eval vtable ftable e1, eval vtable ftable e2) with
        | (IntVal  n1, IntVal n2)  -> BoolVal (n1 = n2)
        | (BoolVal b1, BoolVal b2) -> BoolVal (b1 = b2)
        | _ -> raise (MyError "Operands to = not same type")
    // ...
    | Var x ->
      match lookup x vtable with
        | None -> raise (MyError ("Unknown variable " + x))
        | Some v -> v
    | Let (x, e1, e2) ->
      let v1 = eval vtable ftable e1
      let vtable1 = bind x v1 vtable
      eval vtable1 ftable e2
    | If (cond, truebranch, falsebranch) ->
      match eval vtable ftable cond with
        | BoolVal true  -> eval vtable ftable truebranch
        | BoolVal false -> eval vtable ftable falsebranch
        | _ -> raise (MyError "If condition not a bool")

    | Call (fname, args) ->
      let vals = List.map(fun arg -> eval vtable ftable arg) args
      match lookup fname ftable with
       | None -> raise (MyError ("Unknown function "+fname))
       | Some fundec -> callFun ftable fundec vals

// callFun : FunTab -> FunDec -> Value list -> Value
and callFun ftable fundec act_arg_vals =
  match fundec with
    | FunDec (rettype, fname, form_params, body) ->
        let vtable = bindParams form_params act_arg_vals
        let result = eval vtable ftable body
        match (result, rettype) with
          | (IntVal n, Int) -> IntVal n
          | (BoolVal b, Bool) -> BoolVal b
          | _ -> raise (MyError "Return value mismatch")

// makeFunTable : FunDec list -> FunTab
let rec makeFunTable fundecs =
  match fundecs with
    | [] -> empty ()  // SymTab.empty
    | (fundec::funs) ->
        let ftable = makeFunTable funs
        match fundec with
          | FunDec (rettype, fname, params, body) ->
              bind fname fundec ftable  // SymTab.bind

// evalProg : Prog -> Value
let evalProg prog =
  let ftable = makeFunTable prog
  match lookup "main" ftable with
    | None -> raise (MyError "No main function defined")
    | Some fundec -> callFun ftable fundec []

// Example
let fact_10_prog =
  [FunDec (Int, "main", [],
           Call ("fact", [Constant (IntVal 10)]));
   FunDec (Int, "fact", [(Int, "x")],
           If (Equal (Var "x", Constant (IntVal 0)),
               Constant (IntVal 1),
               Times (Var "x",
                      Call ("fact",
                            [Minus (Var "x",
                                    Constant (IntVal 1))]))))
          ]

let ex = evalProg fact_10_prog
