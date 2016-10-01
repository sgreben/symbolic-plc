namespace FsPlcVm

module IL = FsPlcModel.IL
open Microsoft.Z3

module Semantics = 
    open Representation
    exception Not_supported
    let int_value _ : Representation.Array_index = 0
    let index_array a i = 
        match i with
        | _ -> raise Not_supported
    let modify_array a i v = 
        match i with 
        | _ -> raise Not_supported
    let to_sym = function
        | INT i -> SYM_INT (SMT.Const.mk_int i)
        | UINT i -> SYM_UINT (SMT.Const.mk_uint i)
        | REAL f -> SYM_REAL (SMT.Const.mk_real f)
        | TIME t -> SYM_TIME (SMT.Const.mk_time t)
        | STRING s -> raise Not_supported
        | BOOL b -> SYM_BOOL (SMT.Const.mk_bool b)
        | SYM s -> s
    let dummy_int = VBasic (INT 0L)
    exception Not_implemented
    let is_boolean_one v = raise Not_implemented
    let is_boolean_zero v = raise Not_implemented
    exception Value_type_error
    let complement = function
        | INT x -> INT (-x)
        | UINT x -> UINT ( ~~~ x)
        | REAL x -> REAL (-x)
        | BOOL x -> BOOL (not x)
        | SYM (SYM_BOOL x) -> SYM (SYM_BOOL (SMT.Rel._not x))
        | SYM _ -> raise Not_supported
        | _ -> raise Value_type_error
    
    exception Solver_failed
    type Check_sat_result = Valid | Sat of Symbolic_bool | Unsat
    let inline check_sat_sym b = 
        SMT.Solver.push_add b
        match SMT.Solver.check () with
        | Some true -> SMT.Solver.pop 1u; Sat b
        | Some false -> SMT.Solver.pop 1u; Unsat
        | _ -> raise Solver_failed
    let inline check_sat b = 
        match b with
        | BOOL true -> Valid
        | BOOL false -> Unsat
        | SYM (SYM_BOOL b) -> check_sat_sym b
        | _ -> raise Value_type_error
    module Symop = SMT.Op
    let inline add_sym x y =
        match x, y with
        | SYM_INT x, SYM_INT y -> SYM_INT (Symop.add_int x y)
        | SYM_TIME x, SYM_TIME y -> SYM_TIME (Symop.add_int x y)
        | SYM_UINT x, SYM_UINT y -> SYM_UINT (Symop.add_int x y)
        | SYM_REAL x, SYM_REAL y -> SYM_REAL (Symop.add_real x y)
        | _ -> raise Value_type_error
    let inline sub_sym x y =
        match x, y with
        | SYM_INT x, SYM_INT y -> SYM_INT (Symop.sub_int x y)
        | SYM_TIME x, SYM_TIME y -> SYM_TIME (Symop.sub_int x y)
        | SYM_UINT x, SYM_UINT y -> SYM_INT (Symop.sub_int x y)
        | SYM_REAL x, SYM_REAL y -> SYM_REAL (Symop.sub_real x y)
        | _ -> raise Value_type_error
    let inline div_sym x y =
        match x, y with
        | SYM_INT x, SYM_INT y -> SYM_INT (Symop.div_int x y)
        | SYM_UINT x, SYM_UINT y -> SYM_UINT (Symop.div_int x y)
        | SYM_REAL x, SYM_REAL y -> SYM_REAL (Symop.div_real x y)
        | _ -> raise Value_type_error
    let inline mul_sym x y =
        match x, y with
        | SYM_INT x, SYM_INT y -> SYM_INT (Symop.mul_int x y)
        | SYM_UINT x, SYM_UINT y -> SYM_UINT (Symop.mul_int x y)
        | SYM_REAL x, SYM_REAL y -> SYM_REAL (Symop.mul_real x y)
        | _ -> raise Value_type_error
    let inline modulus_sym x y =
        match x, y with
        | SYM_INT x, SYM_INT y -> SYM_INT (Symop.modulus x y)
        | SYM_UINT x, SYM_UINT y -> SYM_UINT (Symop.modulus x y)
        | _ -> raise Value_type_error
    let inline add x y = 
        match x,y with
        | INT x, INT y -> INT (x+y)
        | UINT x, UINT y -> UINT (x+y)
        | REAL x, REAL y -> REAL (x+y)
        | SYM x, SYM y -> SYM (add_sym x y)
        | SYM x, y -> SYM (add_sym x (to_sym y))
        | y, SYM x -> SYM (add_sym (to_sym y) x)
        | _ -> raise Value_type_error
    let inline sub x y = 
        match x,y with
        | INT x, INT y -> INT (x-y)
        | UINT x, UINT y -> UINT (x-y)
        | REAL x, REAL y -> REAL (x-y)
        | SYM x, SYM y -> SYM (sub_sym x y)
        | SYM x, y -> SYM (sub_sym x (to_sym y))
        | y, SYM x -> SYM (sub_sym (to_sym y) x)
        | _ -> raise Value_type_error
    let inline div x y = 
        match x,y with
        | INT x, INT y -> INT (x/y)
        | UINT x, UINT y -> UINT (x/y)
        | REAL x, REAL y -> REAL (x/y)
        | SYM x, SYM y -> SYM (div_sym x y)
        | SYM x, y -> SYM (div_sym x (to_sym y))
        | y, SYM x -> SYM (div_sym (to_sym y) x)
        | _ -> raise Value_type_error
    let inline mul x y = 
        match x,y with
        | INT x, INT y -> INT (x*y)
        | UINT x, UINT y -> UINT (x*y)
        | REAL x, REAL y -> REAL (x*y)
        | SYM x, SYM y -> SYM (mul_sym x y)
        | SYM x, y -> SYM (mul_sym x (to_sym y))
        | y, SYM x -> SYM (mul_sym (to_sym y) x)
        | _ -> raise Value_type_error
    let inline modulus x y =
        match x,y with
        | INT x, INT y -> INT (x % y)
        | UINT x, UINT y -> UINT (x % y)
        | SYM x, SYM y -> SYM (modulus_sym x y)
        | SYM x, y -> SYM (modulus_sym x (to_sym y))
        | y, SYM x -> SYM (modulus_sym (to_sym y) x)
        | _ -> raise Value_type_error
    let inline arith_sym v =
        match v with
        | SYM_INT x -> Some(x :> ArithExpr)
        | SYM_REAL x -> Some(x :> ArithExpr)
        | SYM_UINT x -> Some(x :> ArithExpr)
        | SYM_TIME x -> Some(x :> ArithExpr)
        | _ -> None
    module Symrel = SMT.Rel
    let inline cmp_sym op x y = 
        match arith_sym x, arith_sym y with
        | Some x, Some y -> SYM_BOOL (op x y)
        | _ -> raise Value_type_error
    let inline le_sym x y = cmp_sym Symrel.le x y
    let inline lt_sym x y = cmp_sym Symrel.lt x y
    let inline ge_sym x y = cmp_sym Symrel.ge x y
    let inline gt_sym x y = cmp_sym Symrel.gt x y
    let inline eq_sym x y = 
        match x, y with
        | SYM_BOOL b, SYM_BOOL b' ->  SYM_BOOL (Symrel.eq b b')
        | _ -> cmp_sym Symrel.eq x y
    let inline neq_sym x y = cmp_sym Symrel.neq x y
    let dummy_bool = BOOL true
    let inline dummy_logic _ _ = dummy_bool
    let inline _and v v' = 
        match v, v' with
        | BOOL x, BOOL y -> BOOL (x && y)
        | SYM (SYM_BOOL x), BOOL true
        | BOOL true, SYM (SYM_BOOL x) ->  SYM (SYM_BOOL x)
        | SYM (SYM_BOOL x), BOOL false
        | BOOL false, SYM (SYM_BOOL x) ->  BOOL false
        | SYM (SYM_BOOL x), SYM (SYM_BOOL y) -> SYM (SYM_BOOL (SMT.Rel._and x y))
        | _ -> raise Not_implemented
    let inline _or v v' =  
        match v, v' with
        | BOOL x, BOOL y -> BOOL (x || y)
        | SYM (SYM_BOOL x), BOOL true
        | BOOL true, SYM (SYM_BOOL x) ->  BOOL true
        | SYM (SYM_BOOL x), BOOL false
        | BOOL false, SYM (SYM_BOOL x) ->  SYM (SYM_BOOL x)
        | SYM (SYM_BOOL x), SYM (SYM_BOOL y) -> SYM (SYM_BOOL (SMT.Rel._or x y))
        | _ -> raise Not_implemented
    let inline _xor v v' =  
        match v, v' with
        | BOOL x, BOOL y -> BOOL (x <> y)
        | SYM (SYM_BOOL x), SYM (SYM_BOOL y) -> SYM (SYM_BOOL (SMT.Rel._xor x y))
        | _ -> raise Not_implemented
    let inline cmpop f x y = BOOL (f x y)
    let inline lt x y = 
        match x,y with
        | INT x, INT y -> cmpop (<) x y
        | UINT x, UINT y -> cmpop (<) x y
        | REAL x, REAL y -> cmpop (<) x y
        | BOOL x, BOOL y -> cmpop (<) x y
        | SYM x, SYM y -> SYM (lt_sym x y)
        | SYM x , y -> SYM (lt_sym x  (to_sym y))
        | x, SYM y -> SYM (lt_sym (to_sym x) y)
        | _ -> raise Value_type_error

    let inline gt x y = 
        match x,y with
        | INT x, INT y -> cmpop (>) x y
        | UINT x, UINT y -> cmpop (>) x y
        | REAL x, REAL y -> cmpop (>) x y
        | BOOL x, BOOL y -> cmpop (>) x y
        | SYM x, SYM y -> SYM (gt_sym x y)
        | SYM x, y -> SYM (gt_sym x  (to_sym y))
        | x, SYM y -> SYM (gt_sym (to_sym x) y)
        | _ -> raise Value_type_error
    let inline ge x y = 
        match x,y with
        | INT x, INT y -> cmpop (>=) x y
        | UINT x, UINT y -> cmpop (>=) x y
        | REAL x, REAL y -> cmpop (>=) x y
        | BOOL x, BOOL y -> cmpop (>=) x y
        | SYM x, SYM y -> SYM (ge_sym x y)
        | SYM x , y -> SYM (ge_sym x  (to_sym y))
        | x, SYM y -> SYM (ge_sym (to_sym x) y)
        | _ -> raise Value_type_error
    let inline le x y =
        match x,y with
        | INT x, INT y -> cmpop (<=) x y
        | UINT x, UINT y -> cmpop (<=) x y
        | REAL x, REAL y -> cmpop (<=) x y
        | BOOL x, BOOL y -> cmpop (<=) x y
        | SYM x, SYM y -> SYM (le_sym x y)
        | SYM x , y -> SYM (le_sym x  (to_sym y))
        | x, SYM y -> SYM (le_sym (to_sym x) y)
        | _ -> raise Value_type_error
    let inline eq x y = 
        match x,y with
        | SYM x, SYM y -> SYM (eq_sym x y)
        | SYM x , y -> SYM (eq_sym x  (to_sym y))
        | x, SYM y -> SYM (eq_sym (to_sym x) y)
        | x,y -> BOOL (x=y)
    let inline neq x y = 
        match x,y with
        | SYM x, SYM y -> SYM (neq_sym x y)
        | SYM x , y -> SYM (neq_sym x  (to_sym y))
        | x, SYM y -> SYM (neq_sym (to_sym x) y)
        | x,y -> BOOL (x<>y)

    module Builtin =
        module Unop =
            let trunc v  = dummy_int
            let abs = dummy_int
            let sqrt = dummy_int
            let ln = dummy_int
            let log = dummy_int
            let sin = dummy_int
            let cos = dummy_int
            let tan = dummy_int
            let asin = dummy_int
            let acos = dummy_int
            let atan = dummy_int
        module Extop =
            let add _ _ = dummy_int
            let mul _ _ = dummy_int
            let mux _ _ = dummy_int
            let min _ _ = dummy_int
            let max _ _ = dummy_int
            let gt _ _ =  dummy_bool
            let lt _ _ =  dummy_bool
            let ge _ _ =  dummy_bool
            let le _ _ =  dummy_bool
            let eq _ _ =  dummy_bool
            let neq _ _ = dummy_bool
