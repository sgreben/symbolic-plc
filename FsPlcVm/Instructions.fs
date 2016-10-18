namespace FsPlcVm

module IL = FsPlcModel.IL

/// Implementation of LLIL instructions
module Instructions = 
    open IL
    
    module Vmp = Primitives
    module Vms = Semantics
    
    open States
    open Representation
    open Language
    
    type Successors = 
        /// Two guarded successors
        | S2G of State * Symbolic_bool * State * Symbolic_bool
        /// Two successors
        | S2 of State * State
        /// One successor
        | S1 of State
        /// One successor, passed
        | S1P of State
        /// One successor, failed
        | S1F of State
        /// One guarded successor
        | S1G of State * Symbolic_bool
        /// No successors
        | S0
    
    exception Soundness_bug
    
    exception Infeasible
    
    /// Conditional modifier
    let inline mod_c f s = 
        match Vmp.cr_is_sat s, Vmp.not_cr_is_sat s with
        | Vms.Sat b, Vms.Sat b' -> S2G(f s, b, s, b')
        | Vms.Sat b, Vms.Unsat -> S1G(f s, b)
        | Vms.Sat b, Vms.Valid -> raise Infeasible
        | Vms.Unsat, Vms.Sat b -> S1G(s, b)
        | Vms.Unsat, Vms.Unsat -> raise Soundness_bug
        | Vms.Unsat, Vms.Valid -> S1 s
        | Vms.Valid, Vms.Sat b -> raise Infeasible
        | Vms.Valid, Vms.Unsat -> S1(f s)
        | Vms.Valid, Vms.Valid -> raise Soundness_bug
    
    /// Conditional modifier (negated)
    let inline mod_cn f s = 
        match Vmp.not_cr_is_sat s, Vmp.cr_is_sat s with
        | Vms.Sat b, Vms.Sat b' -> S2G(s, b', f s, b)
        | Vms.Sat b, Vms.Unsat -> S1(f s)
        | Vms.Sat _, Vms.Valid -> raise Infeasible
        | Vms.Unsat, Vms.Sat b -> S1G(s, b)
        | Vms.Unsat, Vms.Unsat -> raise Soundness_bug
        | Vms.Unsat, Vms.Valid -> S1 s
        | Vms.Valid, Vms.Sat _ -> raise Infeasible
        | Vms.Valid, Vms.Unsat -> S1(f s)
        | Vms.Valid, Vms.Valid -> raise Soundness_bug
    
    /// Nullary instructions
    module Nullop = 
        /// Bitwise complement
        let inline _not s = Vmp.set_cr_value_basic (Vmp.cr_value_basic s |> Vms.complement) s
        
        /// Logical negation
        let inline neg s = _not s
        
        /// Load current symbolic timestamp and set fresh timestamp
        let inline ld_current_time s = 
            let fresh_id = sprintf "time_%d" (Vmp.ic s)
            let fresh_time = SMT.Var.mk_int fresh_id
            let lbound = SMT.Rel.le (Vmp.time s) fresh_time
            
            let bounds = 
                match s.task_current with
                | Some(interval, tid) -> 
                    let current = fresh_time
                    let ubound_value = SMT.Const.mk_int (interval * (int64) (Vmp.task_count tid s))
                    let ubound = SMT.Rel.lt current ubound_value
                    SMT.Rel._and lbound ubound
                | None -> lbound
            
            let s = Vmp.set_time fresh_time s
            S1G(Vmp.set_cr_value_basic (SYM(SYM_TIME(Vmp.time s))) s, bounds)
        
        let inline ld_current_time_nochange s = S1(Vmp.set_cr_value_basic (SYM(SYM_TIME(Vmp.time s))) s)
        
        let inline step op s = 
            match op with
            | LD_CURRENT_TIME -> ld_current_time s
            | LD_CURRENT_TIME_NOCHANGE -> ld_current_time_nochange s
            | NOT -> S1(_not s)
            | NEG -> S1(neg s)
    
    module Loadop = 
        let inline mod_n f s = Vmp.negate_cr (f s)
        let inline load_ref v s = Vmp.read v s |> fun c -> Vmp.set_cr c s
        let inline load_refn v s = mod_n (load_ref v) s
        let inline load_imm v s = Vmp.set_cr (Value v) s
        let inline load_immn v s = mod_n (load_imm v) s
        
        let inline load_field f s = 
            match Vmp.cr_value s with
            | VStruct _struct -> Vmp.set_cr (Vmp.read _struct.[f] s) s
            | _ -> raise Vmp.Type_error
        
        let inline load_fieldn f s = mod_n (load_field f) s
        
        let inline load_index_imm i s = 
            match Vmp.cr_value s with
            | VArray_boxed _array -> Vmp.set_cr (Vmp.read _array.[i] s) s
            | _ -> raise Vmp.Type_error
        
        let inline load_index_immn i s = mod_n (load_index_imm i) s
        
        let inline load_index_ref v s = 
            match Vmp.read v s, Vmp.cr s with
            | Value(VBasic i), Value(VArray_boxed a) -> Vmp.set_cr (Semantics.index_array a i) s
            | _ -> raise Vmp.Type_error
        
        let inline load_index_refn v s = mod_n (load_index_ref v) s
        
        let inline step_ref op r s = 
            match op with
            | LD -> load_ref r s
            | LDN -> load_refn r s
        
        let inline step_imm op v s = 
            match op with
            | LD -> load_imm v s
            | LDN -> load_immn v s
        
        let inline step_field op f s = 
            match op with
            | LD -> load_field f s
            | LDN -> load_fieldn f s
        
        let inline step_index_imm op i s = 
            match op with
            | LD -> load_index_imm i s
            | LDN -> load_index_immn i s
        
        let inline step_index_ref op ri s = 
            let r = Vmp.aux_ref ri s
            match op with
            | LD -> load_index_ref r s
            | LDN -> load_index_refn r s
    
    module Storeop = 
        let inline mod_n f s = Vmp.set_cr (Vmp.cr s) (f (Vmp.negate_cr s))
        let inline store_imm r s = Vmp.write r (Vmp.cr s) s
        let inline store_immn r s = mod_n (store_imm r) s
        
        let inline store_ref r s = 
            match Vmp.read r s with
            | Reference r -> store_imm r s
            | _ -> raise Vmp.Type_error
        
        let inline store_refn r s = mod_n (store_ref r) s
        
        let inline _store_field rv f v s = 
            match rv with
            | Value(VStruct _struct) -> Vmp.write _struct.[f] v s
            | _ -> raise Vmp.Type_error
        
        let inline store_field_imm rv f s = _store_field rv f (Vmp.cr s) s
        let inline store_field_immn rv f s = mod_n (store_field_imm rv f) s
        let inline store_field_ref r f s = store_field_imm (Vmp.read r s) f s
        let inline store_field_refn r f s = mod_n (store_field_ref r f) s
        let inline _store_index (_array : Array_repr_boxed) i v s = Vmp.write _array.[Semantics.int_value i] v s
        let inline store_index_imm r i s = _store_index r i (Vmp.cr s) s
        let inline store_index_immn r i s = mod_n (store_index_imm r i) s
        let inline store_index_ref r i s = store_index_imm (Vmp.read_boxed_array r s) i s
        let inline store_index_refn r i s = mod_n (store_index_ref r i) s
        let inline s_imm r s = Vmp.write r (Vmp.cr_value |> Semantics.is_boolean_one) s
        let inline r_imm r s = Vmp.write r (Vmp.cr_value |> Semantics.is_boolean_zero) s
        let inline s_ref r s = s_imm (Vmp.read_ref r s) s
        let inline r_ref r s = r_imm (Vmp.read_ref r s) s
        let inline s_index_imm r i s = _store_index r i (Vmp.cr_value |> Semantics.is_boolean_one) s
        let inline s_index_ref r i s = 
            _store_index (Vmp.read_boxed_array r s) i (Vmp.cr_value |> Semantics.is_boolean_one) s
        let inline s_field_imm r f s = _store_field r f (Vmp.cr_value |> Semantics.is_boolean_one) s
        let inline r_field_imm r f s = _store_field r f (Vmp.cr_value |> Semantics.is_boolean_zero) s
        let inline s_field_ref r f s = _store_field (Vmp.read r s) f (Vmp.cr_value |> Semantics.is_boolean_one) s
        let inline r_field_ref r f s = _store_field (Vmp.read r s) f (Vmp.cr_value |> Semantics.is_boolean_zero) s
        let inline r_index_imm r i s = _store_index r i (Vmp.cr_value |> Semantics.is_boolean_zero) s
        let inline r_index_ref r i s = 
            _store_index (Vmp.read_boxed_array r s) i (Vmp.cr_value |> Semantics.is_boolean_zero) s
        
        let inline step_ref op r s = 
            match op with
            | ST -> store_ref r s
            | STN -> store_refn r s
            | S -> s_ref r s
            | R -> r_ref r s
        
        let inline step_imm op r s = 
            match op with
            | ST -> store_imm r s
            | STN -> store_immn r s
            | S -> s_imm r s
            | R -> r_imm r s
        
        let inline step_aux op ri s = step_imm op (Vmp.aux_ref ri s) s
        
        let inline step_field_imm op f i s = 
            let r = Vmp.aux i s
            match op with
            | ST -> store_field_imm r f s
            | STN -> store_field_immn r f s
            | S -> s_field_imm r f s
            | R -> r_field_imm r f s
        
        let inline step_field_ref op f i s = 
            let r = Vmp.aux_ref i s
            match op with
            | ST -> store_field_ref r f s
            | STN -> store_field_refn r f s
            | S -> s_field_ref r f s
            | R -> r_field_ref r f s
        
        let inline step_index_imm op i ri s = 
            let r = Vmp.aux_value_array ri s
            match op with
            | ST -> store_index_imm r i s
            | STN -> store_index_immn r i s
            | S -> s_index_imm r i s
            | R -> r_index_imm r i s
        
        let inline step_index_ref op i ri s = 
            let r = Vmp.aux_ref ri s
            match op with
            | ST -> store_index_ref r i s
            | STN -> store_index_refn r i s
            | S -> s_index_ref r i s
            | R -> r_index_ref r i s
        
        let inline step_index_aux op ri rj s = 
            let r = Vmp.aux_value_array ri s
            let i = Vmp.aux_value rj s
            match op with
            | ST -> store_index_imm r i s
            | STN -> store_index_immn r i s
            | S -> s_index_imm r i s
            | R -> r_index_imm r i s
        
        let inline mov r r' s = Vmp.mov r r' s
    
    module Jmpop = 
        let inline jmp pc' s = Vmp.set_pc pc' s
        let inline jmpc pc' s = mod_c (jmp pc') (Vmp.inc_pc s)
        let inline jmpcn pc' s = mod_cn (jmp pc') (Vmp.inc_pc s)
        let inline jmp_ref r offset s = jmp (offset + (Vmp.read_code_ptr r s)) s
        
        let inline step op pc s = 
            match op with
            | JMP -> S1(jmp pc s)
            | JMPC -> jmpc pc s
            | JMPCN -> jmpcn pc s
        
        let inline step_rel op offset s = 
            let pc = offset + Vmp.pc s
            match op with
            | JMP -> S1(jmp pc s)
            | JMPC -> jmpc pc s
            | JMPCN -> jmpcn pc s
    
    module Callop = 
        let inline cal pc s = Vmp.set_pc pc (Vmp.push_pc s)
        let inline cal_ref r s = cal (Vmp.read r s |> Vmp.ensure_ptr) s
        let inline cal_imm pc s = cal pc s
        let inline calc_ref r s = mod_c (cal_ref r) s
        let inline calcn_ref r s = mod_cn (cal_ref r) s
        let inline calc_imm pc s = mod_c (cal_imm pc) s
        let inline calcn_imm pc s = mod_cn (cal_imm pc) s
        
        let inline step_imm op pc _this s = 
            match op with
            | CAL -> S1(cal_imm pc s)
            | CALC -> calc_imm pc s
            | CALCN -> calcn_imm pc s
        
        let inline step_aux op ri _this s = 
            let pc = Vmp.aux_code_ptr ri s
            match op with
            | CAL -> S1(cal_imm pc s)
            | CALC -> calc_imm pc s
            | CALCN -> calcn_imm pc s
        
        let inline step_ref op r _this s = 
            match op with
            | CAL -> S1(cal_ref r s)
            | CALC -> calc_ref r s
            | CALCN -> calcn_ref r s
    
    module Aluop = 
        let inline _prim f s = Vmp.set_cr_value_basic (Vmp.cr_value_basic s |> f) s
        
        module Arithop = 
            let inline flip f x y = f y x
            let inline add v = _prim (flip Vms.add v)
            let inline sub v = _prim (flip Vms.sub v)
            let inline div v = _prim (flip Vms.div v)
            let inline mul v = _prim (flip Vms.mul v)
            let inline modulus v = _prim (flip Vms.modulus v)
            
            let inline step v op s = 
                match op with
                | ADD -> add v s
                | SUB -> sub v s
                | MUL -> mul v s
                | DIV -> div v s
                | MOD -> modulus v s
            
            let inline step_ref r op s = step (Vmp.read_basic_value r s) op s
            let inline step_push op = Vmp.push_op (ABINOP_POP op)
            let inline step_aux ri op s = step (Vmp.aux_basic_value ri s) op s
        
        module Logicop = 
            let inline _prim f s = Vmp.set_cr_value_basic (Vmp.cr_value_basic s |> f) s
            let inline mod_n f s = f s |> Vmp.negate_cr
            let inline _and v = _prim (Vms._and v)
            let inline _andn v = mod_n (_and v)
            let inline _or v = _prim (Vms._or v)
            let inline _orn v = mod_n (_or v)
            let inline _xor v = _prim (Vms._xor v)
            let inline _xorn v = mod_n (_xor v)
            
            let inline step v op s = 
                match op with
                | AND -> _and v s
                | OR -> _or v s
                | XOR -> _xor v s
                | ANDN -> _andn v s
                | ORN -> _orn v s
                | XORN -> _xor v s
            
            let inline step_ref r op s = step (Vmp.read_basic_value r s) op s
            let inline step_push op = Vmp.push_op (LBINOP_POP op)
        
        module Cmpop = 
            let inline _prim f s = Vmp.set_cr_value_basic (Vmp.cr_value_basic s |> f) s
            let inline flip f x y = f y x
            let inline gt v = _prim (flip Vms.gt v)
            let inline lt v = _prim (flip Vms.lt v)
            let inline ge v = _prim (flip Vms.ge v)
            let inline le v = _prim (flip Vms.le v)
            let inline eq v = _prim (flip Vms.eq v)
            let inline neq v = _prim (flip Vms.neq v)
            
            let inline step v op s = 
                match op with
                | GT -> gt v s
                | LT -> lt v s
                | GE -> ge v s
                | LE -> le v s
                | EQ -> eq v s
                | NE -> neq v s
            
            let inline step_push op = Vmp.push_op (CMPOP_POP op)
            let inline step_imm v op s = step v op s
            let inline step_ref r op s = step (Vmp.read_basic_value r s) op s
        
        let inline pop s = 
            let (v', op), s = Vmp.pop_op s
            let v = Vmp.cr_value_basic s
            let s = Vmp.set_cr_value v' s
            match op with
            | LBINOP_POP op -> Logicop.step v op s
            | ABINOP_POP op -> Arithop.step v op s
            | CMPOP_POP op -> Cmpop.step v op s
    
    module Funop = 
        let inline funop pc mem s = Callop.cal pc s
        
        module Builtin = 
            module Extop = 
                let inline add vs s = 
                    vs
                    |> List.fold Vms.add (Vmp.cr_value_basic s)
                    |> fun v -> Vmp.set_cr_value_basic v s

                let inline mul vs s = 
                    vs
                    |> List.fold Vms.mul (Vmp.cr_value_basic s)
                    |> fun v -> Vmp.set_cr_value_basic v s

                let inline eq vs s =
                    let cr = Vmp.cr_value_basic s
                    vs
                    |> List.fold (fun all_eq v -> Vms._and all_eq (Vms.eq v cr)) (BOOL true)
                    |> fun v -> Vmp.set_cr_value_basic v s

                let inline ge vs s =
                    let cr = Vmp.cr_value_basic s
                    vs
                    |> List.fold (fun (all_geq, last) v -> (Vms._and all_geq (Vms.ge last v), v)) (BOOL true, cr)
                    |> fun (v, _) -> Vmp.set_cr_value_basic v s

                let inline gt vs s =
                    let cr = Vmp.cr_value_basic s
                    vs
                    |> List.fold (fun (all_gt, last) v -> (Vms._and all_gt (Vms.gt last v), v)) (BOOL true, cr)
                    |> fun (v, _) -> Vmp.set_cr_value_basic v s

                let inline le vs s =
                    let cr = Vmp.cr_value_basic s
                    vs
                    |> List.fold (fun (all_le, last) v -> (Vms._and all_le (Vms.le last v), v)) (BOOL true, cr)
                    |> fun (v, _) -> Vmp.set_cr_value_basic v s

                let inline lt vs s =
                    let cr = Vmp.cr_value_basic s
                    vs
                    |> List.fold (fun (all_lt, last) v -> (Vms._and all_lt (Vms.lt last v), v)) (BOOL true, cr)
                    |> fun (v, _) -> Vmp.set_cr_value_basic v s

                let inline max vs s =
                    vs
                    |> List.fold Vms.Builtin.Extop.max (Vmp.cr_value_basic s)
                    |> fun v -> Vmp.set_cr_value_basic v s

                let inline min vs s =
                    vs
                    |> List.fold Vms.Builtin.Extop.min (Vmp.cr_value_basic s)
                    |> fun v -> Vmp.set_cr_value_basic v s

                let inline mux vs s = 
                    Vmp.set_cr_value_basic (Vms.Builtin.Extop.mux (Vmp.cr_value_basic s) vs) s

                let inline values vs s = 
                    vs 
                    |> List.map (fun r -> Vmp.aux r s) 
                    |> List.map (function 
                        | Value(VBasic v) -> v
                        | Reference r -> Vmp.read_basic_value r s
                        | _ -> raise Vmp.Type_error)
                
                let inline step op vs s = 
                    let vs = values vs s
                    match op with
                    | ADD_EXT -> add vs s
                    | MUL_EXT -> mul vs s
                    | EQ_EXT -> eq vs s
                    | GE_EXT -> ge vs s
                    | GT_EXT -> gt vs s
                    | LE_EXT -> le vs s
                    | LT_EXT -> lt vs s
                    | MAX -> max vs s
                    | MIN -> min vs s
                    | MUX -> mux vs s
    
    module Retop = 
        let inline ret s = Vmp.ret s
        let inline retc s = mod_c ret s
        let inline retcn s = mod_cn ret s
        
        let inline step op s = 
            match op with
            | RET -> S1(ret s)
            | RETC -> retc s
            | RETCN -> retcn s
    
    module Osop = 
        let inline is_task_ready interval tid s = 
            let current = Vmp.time s
            let lbound_value = SMT.Const.mk_int (interval * (int64) (Vmp.task_count tid s))
            let ubound_value = SMT.Const.mk_int (interval * (int64) (1 + Vmp.task_count tid s))
            let lbound = SMT.Rel.ge current lbound_value
            let ubound = SMT.Rel.lt current ubound_value
            S1G(Vmp.set_cr_value_basic (SYM(SYM_BOOL lbound)) s, ubound)
        
        let inline end_task interval tid s = 
            let current = Vmp.time s
            let ubound_value = SMT.Const.mk_int (interval * (int64) (Vmp.task_count tid s))
            let ubound = SMT.Rel.lt current ubound_value
            S1G(Vmp.unset_task_current s, ubound)
        
        let inline begin_task i tid s = Vmp.set_task_current (i, tid) s |> Vmp.inc_task_count tid
        
        let inline fresh_sensor_var t ptr s = 
            let fresh_id = sprintf "sensor_%d_%d" ptr (Vmp.cycle s)
            match t with
            | TINT -> SYM_INT(SMT.Var.mk_int fresh_id)
            | TTIME -> SYM_TIME(SMT.Var.mk_int fresh_id)
            | TUINT -> SYM_UINT(SMT.Var.mk_int fresh_id)
            | TREAL -> SYM_REAL(SMT.Var.mk_real fresh_id)
            | TBOOL -> SYM_BOOL(SMT.Var.mk_bool fresh_id)
        
        let inline read_input r t s = 
            let v = fresh_sensor_var t r s
            Vmp.write r (Value(VBasic(SYM v))) s
        
        let inline write_output r s = s
        let inline next_cycle s = Vmp.inc_cycle s
        
        let inline step op s = 
            match op with
            | READ_INPUT(t, r) -> S1(read_input r t s)
            | WRITE_OUTPUT r -> S1(write_output r s)
            | NEXT_CYCLE -> S1(next_cycle s)
            | IS_TASK_READY(i, tid) -> (is_task_ready i tid s)
            | END_TASK(i, tid) -> (end_task i tid s)
            | BEGIN_TASK(i, tid) -> S1(begin_task i tid s)
    
    module Monitorop = 
        let inline log_state s = 
            printfn "<<<MONITOR>>>%A" s
            s
        
        let inline _assume s = 
            match Vmp.cr_is_sat s with
            | Vms.Unsat -> S0
            | Vms.Sat b -> S1G(s, b)
            | Vms.Valid -> S1 s
        
        let inline step op s = 
            match op with
            | LOG_STATE -> S1(log_state s)
            | PASS -> S1P s
            | FAIL -> S1F s
            | ASSUME -> _assume s
    
    exception Not_supported
    
    let inline step_op op s = 
        match op with
        | NULLOP op -> Nullop.step op s
        | LOADOP_REF(op, r) -> S1(Loadop.step_ref op r s)
        | LOADOP_IMM(op, v) -> S1(Loadop.step_imm op v s)
        | LOADOP_FIELD(op, f) -> S1(Loadop.step_field op f s)
        | LOADOP_INDEX_IMM(op, i) -> S1(Loadop.step_index_imm op i s)
        | LOADOP_INDEX_REF(op, ri) -> S1(Loadop.step_index_ref op ri s)
        | STOREOP_REF(op, r) -> S1(Storeop.step_ref op r s)
        | STOREOP_AUX(op, ri) -> S1(Storeop.step_aux op ri s)
        | STOREOP_IMM(op, r) -> S1(Storeop.step_imm op r s)
        | STOREOP_FIELD_IMM(op, f, ri) -> S1(Storeop.step_field_imm op f ri s)
        | STOREOP_FIELD_REF(op, f, ri) -> S1(Storeop.step_field_ref op f ri s)
        | STOREOP_INDEX_IMM(op, i, ri) -> S1(Storeop.step_index_imm op i ri s)
        | STOREOP_INDEX_REF(op, i, ri) -> S1(Storeop.step_index_ref op i ri s)
        | STOREOP_INDEX_AUX(op, ri, rj) -> S1(Storeop.step_index_aux op ri rj s)
        | MOV(r, r') -> S1(Storeop.mov r r' s)
        | JMPOP(op, pc) -> Jmpop.step op pc s
        | JMPOP_REL(op, offset) -> Jmpop.step_rel op offset s
        | JMP_REF(r, offset) -> S1(Jmpop.jmp_ref r offset s)
        | NONDET pc -> S2(Jmpop.jmp pc s, s)
        | ABINOP_REF(op, r) -> S1(Aluop.Arithop.step_ref r op s)
        | ABINOP_IMM(op, v) -> S1(Aluop.Arithop.step v op s)
        | ABINOP_PUSH op -> S1(Aluop.Arithop.step_push op s)
        | ABINOP_AUX(op, ri) -> S1(Aluop.Arithop.step_aux ri op s)
        | LBINOP_REF(op, r) -> S1(Aluop.Logicop.step_ref r op s)
        | LBINOP_IMM(op, v) -> S1(Aluop.Logicop.step (BOOL v) op s)
        | LBINOP_PUSH op -> S1(Aluop.Logicop.step_push op s)
        | CMPOP_IMM(op, v) -> S1(Aluop.Cmpop.step_imm v op s)
        | CMPOP_REF(op, r) -> S1(Aluop.Cmpop.step_ref r op s)
        | CMPOP_PUSH op -> S1(Aluop.Cmpop.step_push op s)
        | FUN(pc, mem) -> S1(Funop.funop pc mem s)
        | FUN_BUILTIN_EXTOP(op, vs) -> S1(Funop.Builtin.Extop.step op vs s)
        | FUN_BUILTIN_BINOP(op, v) -> raise Not_supported
        | FUN_BUILTIN_TERNOP(op, v, v') -> raise Not_supported
        | FUN_BUILTIN_UNOP op -> raise Not_supported
        | CALLOP_IMM(op, pc, _this) -> Callop.step_imm op pc _this s
        | CALLOP_REF(op, r, _this) -> Callop.step_ref op r _this s
        | CALLOP_AUX(op, ri, _this) -> Callop.step_aux op ri _this s
        | RETOP op -> Retop.step op s
        | MOV_CR_AUX ri -> S1(Vmp.mov_cr_aux ri s)
        | MOV_AUX_CR ri -> S1(Vmp.mov_aux_cr ri s)
        | ST_AUX(ri, c) -> S1(Vmp.st_aux ri c s)
        | POP -> S1(Aluop.pop s)
        | LD_THIS -> S1(Vmp.ld_this s)
        | NOP -> S1 s
        | OSOP op -> Osop.step op s
        | MONITOROP op -> Monitorop.step op s
    
    let step_pc op s = 
        let s = Vmp.inc_ic s
        match op with
        | JMPOP _ -> s
        | JMPOP_REL _ -> s
        | JMP_REF _ -> s
        | _ -> Vmp.inc_pc s
    
    let step op s = step_op op (step_pc op s)
