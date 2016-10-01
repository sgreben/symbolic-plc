namespace FsPlcVm

module SMT =
    open Microsoft.Z3
    let Z3ContextOptions = System.Collections.Generic.Dictionary<string, string>()
    let ctx = new Context(Z3ContextOptions)
    let solver = ctx.MkSolver()

    module Solver = 
        let print_assertions () = 
            printfn "%A" solver.Assertions
        let add_assertion (phi:BoolExpr) = 
            solver.Assert(phi)
        let push () = 
            solver.Push ()
        let clear () = solver.Reset ()
        let push_add phi = 
            push ()
            ignore (add_assertion phi)
    
        let pop (k : uint32) = 
            solver.Pop(k)
    
        exception Solver_bug
        let get_model () =
            solver.Model
        let check () = 
            match solver.Check() with
            | Status.SATISFIABLE -> Some true
            | Status.UNSATISFIABLE -> Some false
            | Status.UNKNOWN -> None
            | _ -> raise Solver_bug

        let check_bool b =
            push_add b
            let result = check ()
            pop 1u
            result
    let IntSort = ctx.IntSort

    let formulaSimplifier = ctx.MkTactic("simplify")
    let simplify (f:BoolExpr) = 
        let g = ctx.MkGoal(false,false,false)
        g.Assert(f)
        let and_fs (fs:BoolExpr[]) = if fs.Length = 1 then fs.[0] else ctx.MkAnd fs
        let fs = formulaSimplifier.Apply(g).Subgoals |> Array.map (fun g -> and_fs g.Formulas)
        and_fs fs

    module Var = 
        let inline mk_int (name:string) = ctx.MkIntConst(name)
        let inline mk_real (name:string) = ctx.MkRealConst(name)
        let inline mk_bool (name:string) = ctx.MkBoolConst(name)

    module Const = 
        let inline mk_int(i : int64) = ctx.MkInt(i)
        let inline mk_uint(i : uint64) = ctx.MkInt(i)
        let inline mk_real(x : double) = ctx.MkReal(x.ToString())
        let mk_true = ctx.MkBool(true)
        let mk_false = ctx.MkBool(false)
        let inline mk_time millis = mk_int millis
        let inline mk_bool b = if b then mk_true else mk_false
        let mk_int_two = mk_int 2L

    module Rel = 
        let inline _and x y = ctx.MkAnd(x, y)
        let inline _or x y = ctx.MkOr(x, y)
        let inline _not x = ctx.MkNot x
        let inline _xor x y = ctx.MkXor(x,y)
        let inline le x y = ctx.MkLe(x,y)
        let inline ge x y = ctx.MkGe(x,y)
        let inline lt x y = ctx.MkLt(x,y)
        let inline gt x y = ctx.MkGt(x,y)
        let inline eq a b = ctx.MkEq(a :> Expr, b :> Expr)
        let inline neq a b = ctx.MkDistinct(a,b)

    module Op = 
        let inline as_int (e:ArithExpr) = match e with :? IntExpr as e -> e
        let inline as_real (e:ArithExpr) = match e with :? RealExpr as e -> e
        let inline add x y = ctx.MkAdd(x,y)
        let inline add_int (x:IntExpr) (y:IntExpr) = add x y |> as_int
        let inline add_real (x:RealExpr) (y:RealExpr) = add x y |> as_real
        let inline sub x y = ctx.MkSub(x,y)
        let inline sub_int (x:IntExpr) (y:IntExpr) = sub x y |> as_int
        let inline sub_real (x:RealExpr) (y:RealExpr) = sub x y |> as_real
        let inline mul x y = ctx.MkMul(x,y)
        let inline mul_int (x:IntExpr) (y:IntExpr) = mul x y |> as_int
        let inline mul_real (x:RealExpr) (y:RealExpr) = mul x y |> as_real
        let inline div x y = ctx.MkDiv(x,y)
        let inline div_int (x:IntExpr) (y:IntExpr) = div x y |> as_int
        let inline div_real (x:RealExpr) (y:RealExpr) = div x y |> as_real
        let inline modulus x y = ctx.MkMod(x,y)

