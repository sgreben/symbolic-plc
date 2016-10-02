namespace FsPlcCompiler.Stage1

open FsPlcModel.IL
open FsPlcModel

module Compile = 
    open Language
    open FsPlcCompiler.Stage0.Access
    open FsPlcVm.Language
    open FsPlcVm.Representation
    
    exception Not_supported
    
    type Memory_env = 
        { map : Memmap.Project_memmap
          reg : Register }
    
    module Expression = 
        open StExpressions
        open FsPlcCompiler.Stage0.Language.Expression
        
        let empty_env map = 
            { map = map
              reg = Reg 0 }
        
        let ralloc env = 
            match env.reg with
            | Reg i -> Reg(i + 1), { env with reg = Reg(i + 1) }
        
        let rfree env = 
            match env.reg with
            | Reg i -> { env with reg = Reg(i - 1) }
        
        let rec compile_unop env = 
            function 
            | (Negation, e') -> 
                let e' = compile env e'
                e' @ [ OP(NULLOP NEG) ]
            | (Complement, e') -> 
                let e' = compile env e'
                e' @ [ OP(NULLOP NOT) ]
        
        and compile_binop env = 
            function 
            | (Mul, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(ABINOP_PUSH MUL) ] @ e' @ [ OP POP ]
            | (Div, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(ABINOP_PUSH DIV) ] @ e' @ [ OP POP ]
            | (Mod, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(ABINOP_PUSH MOD) ] @ e' @ [ OP POP ]
            | (Add, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(ABINOP_PUSH ADD) ] @ e' @ [ OP POP ]
            | (Sub, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(ABINOP_PUSH SUB) ] @ e' @ [ OP POP ]
            | (And, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(LBINOP_PUSH AND) ] @ e' @ [ OP POP ]
            | (Or, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(LBINOP_PUSH OR) ] @ e' @ [ OP POP ]
            | (Xor, e, e') -> 
                let e = compile env e
                let e' = compile env e'
                e @ [ OP(LBINOP_PUSH XOR) ] @ e' @ [ OP POP ]
            | (Exp, e, e') -> 
                compile env (Function(Builtin(BINOP EXPT), 
                                      [ Anonymous e
                                        Anonymous e' ]))
        
        and compile_cmp env (op, e, e') = 
            let e = compile env e
            let e' = compile env e'
            
            let compile_cmp_operator = 
                function 
                | Lt -> CMPOP_PUSH LT
                | Gt -> CMPOP_PUSH GT
                | Leq -> CMPOP_PUSH LE
                | Geq -> CMPOP_PUSH GE
                | Eq -> CMPOP_PUSH EQ
                | NotEq -> CMPOP_PUSH NE
            e @ [ OP(compile_cmp_operator op) ] @ e' @ [ OP POP ]
        
        and compile_function_call env (f, parms) = 
            match f with
            | Builtin f -> []
            | Project_function _ -> 
                let var_struct = Memmap.fun_vars_struct env.map f
                
                let _, env, assign_params = 
                    parms |> List.fold (fun (i, env, is) (Anonymous e) -> 
                                 let e = compile env e
                                 let field = Memmap.fun_arg_anon env.map i f
                                 (i + 1), env, is @ e @ [ OP(STOREOP_IMM(ST, field)) ]) (0, env, [])
                assign_params @ [ SFUN(f, var_struct) ]
        
        and compile_literal env = 
            function 
            | Values.BASIC(Values.BOOL b) -> VBasic(BOOL b)
            | Values.BASIC(Values.INT i) -> VBasic(INT((int64) i))
            | Values.BASIC(Values.BYTE i) -> VBasic(UINT((uint64) i))
            | Values.ARRAY _ -> VArray_boxed(Map.empty)
            | Values.STRUCT _ -> VStruct(Map.empty)
            | Values.ENUM s -> VBasic(INT 0L)
            | _ -> raise Not_supported
        
        and compile_structure_access env = 
            function 
            | [] -> []
            | Field(t, id) :: sa -> 
                OP(LOADOP_FIELD(LD, Memmap.field_idx env.map id t)) :: compile_structure_access env sa
            | Index i :: sa -> OP(LOADOP_INDEX_IMM(LD, (int) i)) :: compile_structure_access env sa
            | IndexVar v :: sa -> 
                let r1, env = ralloc env
                let r2, env = ralloc env
                let ld_v = compile_variable_read env v
                
                let env = 
                    env
                    |> rfree
                    |> rfree
                [ OP(MOV_CR_AUX r1) ] @ ld_v @ [ OP(MOV_AUX_CR r2)
                                                 OP(MOV_CR_AUX r1)
                                                 OP(LOADOP_INDEX_REF(LD, r2)) ]
                                               @ compile_structure_access env sa
        
        and compile_variable_read env = 
            function 
            | Local_instance(pt, sa) -> [ OP(LD_THIS) ] @ compile_structure_access env sa
            | Global_instance(mvp, sa) -> 
                [ OP(LOADOP_REF(LD, Memmap.var_global env.map mvp)) ] @ compile_structure_access env sa
        
        and compile env e = 
            match e with
            | Literal v -> [ OP(LOADOP_IMM(LD, compile_literal env v)) ]
            | Variable v -> compile_variable_read env v
            | Unop(u, e) -> compile_unop env (u, e)
            | Binop(b, e, e') -> compile_binop env (b, e, e')
            | Cmp(c, e, e') -> compile_cmp env (c, e, e')
            | Function(f, parms) -> compile_function_call env (f, parms)
    
    module Statement = 
        open FsPlcModel
        open FsPlcModel.StExpressions
        open FsPlcModel.StStatements
        open FsPlcCompiler.Stage0.Language.Expression
        open FsPlcCompiler.Stage0.Language.Statement
        open Language
        
        type Environment = 
            { source : StatementAstSource
              mem_env : Memory_env
              fresh_label : int64
              loop_exit : Label_name list }
        
        let empty_env mem = 
            { source = 
                  { line = 0L
                    column = 0L }
              fresh_label = 0L
              mem_env = mem
              loop_exit = [] }
        
        let ralloc env = 
            let r, mem_env = Expression.ralloc env.mem_env
            r, { env with mem_env = mem_env }
        
        let rfree env = 
            let mem_env = Expression.rfree env.mem_env
            { env with mem_env = mem_env }
        
        let fresh_label env prefix = 
            sprintf "%d_%s" (env.fresh_label) prefix, { env with fresh_label = env.fresh_label + 1L }
        let push_loop_exit env label = { env with loop_exit = label :: env.loop_exit }
        
        exception Operator_stack_underflow
        
        let pop_loop_exit env = 
            match env.loop_exit with
            | label :: loop_exit' -> label, { env with loop_exit = loop_exit' }
            | _ -> raise Operator_stack_underflow
        
        let compile_many compile env = 
            List.fold (fun (is, env) b -> 
                let is', env = compile env b
                (is @ is', env)) ([], env)
        
        let expression env e = Expression.compile env.mem_env e, env
        
        let rec compile_block env b = 
            compile_many 
                (fun env (loc, b) -> compile { env with source = loc } b |> fun (is, env) -> [ BLOCK(loc, is) ], env) 
                env b
        
        and compile_case r exitLabel env = 
            function 
            | CaseRange(lo, hi), b -> 
                let endCaseLabel, env = fresh_label env "end_case_range"
                let b, env = compile_block env b
                List.concat [ [ OP(MOV_CR_AUX r)
                                OP(CMPOP_IMM(GE, INT lo))
                                SJMP(JMPCN, endCaseLabel)
                                OP(MOV_CR_AUX r)
                                OP(CMPOP_IMM(LE, INT hi))
                                SJMP(JMPCN, endCaseLabel) ]
                              b
                              [ SJMP(JMPCN, exitLabel)
                                LABEL endCaseLabel ] ], env
            | CaseValues values, b -> 
                let bodyLabel, env = fresh_label env "case_values_body"
                let endCaseLabel, env = fresh_label env "end_case_values"
                let b, env = compile_block env b
                
                let test_values, env = 
                    values |> List.fold (fun (is, env) v -> 
                                  is @ [ OP(MOV_CR_AUX r)
                                         OP(CMPOP_IMM(EQ, INT v))
                                         (SJMP(JMPC, bodyLabel)) ], env) ([], env)
                List.concat [ test_values
                              [ (SJMP(JMP, endCaseLabel))
                                LABEL bodyLabel ]
                              b
                              [ (SJMP(JMP, exitLabel))
                                LABEL endCaseLabel ] ], env
        
        and compile_cases env (e, cases, case_else) = 
            let exitLabel, env = fresh_label env "end_case"
            let r, mem_env = Expression.ralloc env.mem_env
            let env = { env with mem_env = mem_env }
            let cases, env = compile_many (compile_case r exitLabel) env cases
            let case_else, env = compile_block env case_else
            let e, env = expression env e
            let env = { env with mem_env = Expression.rfree env.mem_env }
            List.concat [ e
                          [ OP(MOV_AUX_CR r) ]
                          cases
                          case_else
                          [ LABEL exitLabel ] ], env
        
        and compile_if_then_else env (_if, _then, _else) = 
            let elseLabel, env = fresh_label env "else"
            let exitLabel, env = fresh_label env "end_if"
            let _then, env = compile_block env _then
            let _else, env = compile_block env _else
            let _if, env = expression env _if
            List.concat [ _if
                          [ (SJMP(JMPCN, elseLabel)) ]
                          _then
                          [ LABEL elseLabel
                            (SJMP(JMP, exitLabel)) ]
                          _else
                          [ LABEL exitLabel ] ], env
        
        and compile_do_while env (cond, body) = 
            let loopLabel, env = fresh_label env "do_while"
            let exitLabel, env = fresh_label env "end_do_while"
            let env = push_loop_exit env exitLabel
            let body, env = compile_block env body
            let cond, env = expression env cond
            let _, env = pop_loop_exit env
            List.concat [ [ LABEL loopLabel ]
                          cond
                          [ (SJMP(JMPCN, exitLabel)) ]
                          body
                          [ (SJMP(JMP, loopLabel))
                            LABEL exitLabel ] ], env
        
        and compile_repeat_until env (cond, body) = 
            let loopLabel, env = fresh_label env "repeat_until"
            let exitLabel, env = fresh_label env "end_repeat_until"
            let env = push_loop_exit env exitLabel
            let body, env = compile_block env body
            let cond, env = expression env cond
            let _, env = pop_loop_exit env
            List.concat [ [ LABEL loopLabel ]
                          body
                          cond
                          [ (SJMP(JMPCN, loopLabel)) ]
                          [ LABEL exitLabel ] ], env
        
        and compile_for env (v, init, bound, delta, b) = // [], env
            let increasingLabel, env = fresh_label env "for_loop"
            let decreasingLabel, env = fresh_label env "for_decreasing"
            let exitLabel, env = fresh_label env "end_for"
            let env = push_loop_exit env exitLabel
            let v' = Expression.compile_variable_read env.mem_env v
            let body, env = compile_block env b
            let bound, env = expression env bound
            let init, env = compile env (Assignment(v, init))
            let increment, env = compile env (Assignment(v, Binop(Add, Variable v, delta)))
            let delta, env = expression env delta
            let _, env = pop_loop_exit env
            List.concat [ init
                          delta
                          [ OP (CMPOP_IMM(LT, INT 0L))
                            (SJMP(JMPC, decreasingLabel))
                            /// FOR (low) TO (high) DO (increment)
                            LABEL increasingLabel ]
                          v'
                          [ OP (CMPOP_PUSH GT) ]
                          bound
                          [ OP POP
                            (SJMP(JMPC, exitLabel)) ]
                          body
                          increment
                          [ SJMP(JMP, increasingLabel)
                            /// FOR (high) TO (low) DO (decrement)
                            LABEL decreasingLabel ]
                          v'
                          [ OP (CMPOP_PUSH LT) ]
                          bound
                          [ OP POP
                            (SJMP(JMPC, exitLabel)) ]
                          body
                          increment
                          [ (SJMP(JMP, decreasingLabel))
                            LABEL exitLabel ] ], env

        and compile_structure_assignment env rhs_reg sa = 
            match sa with
            | [ Field(pt, f) ] -> 
                let r, env = ralloc env
                [ OP(MOV_AUX_CR r)
                  OP(MOV_CR_AUX rhs_reg)
                  OP(STOREOP_FIELD_IMM(ST, Memmap.field_idx env.mem_env.map f pt, r)) ]
            | [ Index i ] -> 
                let r, env = ralloc env
                [ OP(MOV_AUX_CR r)
                  OP(MOV_CR_AUX rhs_reg)
                  OP(STOREOP_INDEX_IMM(ST, i, r)) ]
            | [ IndexVar v ] -> 
                let r0, env = ralloc env
                let r1, env = ralloc env
                let ld_var = Expression.compile_variable_read env.mem_env v
                let env = rfree env
                let env = rfree env
                [ OP(MOV_CR_AUX r0) ] @ ld_var @ [ OP(MOV_AUX_CR r1)
                                                   OP(STOREOP_INDEX_AUX(ST, r0, r1)) ]
            | sa -> 
                let sa_rev = List.rev sa
                let sa_last = List.head sa_rev
                let sa_up_to_last = List.rev (List.tail sa_rev)
                let r, env = ralloc env
                let ld_var = Expression.compile_structure_access env.mem_env sa_up_to_last
                let env = rfree env
                ld_var @ compile_structure_assignment env rhs_reg [ sa_last ]
        
        and compile_assignment env (v, rhs) = 
            let rhs, env = expression env rhs
            match v with
            | Global_instance(mvp, []) -> rhs @ [ OP(STOREOP_IMM(ST, Memmap.var_global env.mem_env.map mvp)) ], env
            | Global_instance(mvp, sa) -> 
                let r, env = ralloc env
                let env = rfree env
                List.concat [ rhs
                              [ OP(MOV_AUX_CR r)
                                OP(LOADOP_REF(LD, Memmap.var_global env.mem_env.map mvp)) ]
                              compile_structure_assignment env r sa ], env
            | Local_instance(pt, sa) -> 
                let r, env = ralloc env
                List.concat [ rhs
                              [ OP(MOV_AUX_CR r)
                                OP LD_THIS ]
                              compile_structure_assignment env r sa ], (rfree env)
        
        and compile_exit env = 
            let exitLabel, _ = pop_loop_exit env
            [ (SJMP(JMP, exitLabel)) ], env
        
        and compile_return env = [ OP(RETOP RET) ], env
        
        and compile_fb_call env (pt, fb, parms) = 
            let r_this, env = ralloc env
            let r_var_struct, env = ralloc env
            
            let env = 
                env
                |> rfree
                |> rfree
            
            let set_params = 
                parms |> List.mapi (fun i (Anonymous e) -> 
                             let ld_value = Expression.compile env.mem_env e
                             let f = Memmap.pou_arg_anon env.mem_env.map i pt
                             let r, env = ralloc env
                             ld_value @ [ OP(STOREOP_FIELD_IMM(ST, f, r_var_struct)) ])
            
            let ld_fb = Expression.compile_variable_read env.mem_env fb
            let set_params = set_params |> List.concat
            ld_fb @ [ OP(MOV_AUX_CR r_var_struct) ] @ set_params @ [ SCALL(pt, r_var_struct) ], env
        
        and compile env s = 
            match s with
            | Assignment(v, e) -> compile_assignment env (v, e)
            | If(i, t, e) -> compile_if_then_else env (i, t, e)
            | DoWhile(c, b) -> compile_do_while env (c, b)
            | RepeatUntil(c, b) -> compile_repeat_until env (c, b)
            | For(v, init, bound, Some delta, b) -> compile_for env (v, init, bound, delta, b)
            | For(v, init, bound, None, b) -> 
                compile_for env (v, init, bound, Literal(Values.BASIC(Values.BYTE 1uy)), b)
            | Case(e, cases, case_else) -> compile_cases env (e, cases, case_else)
            | Exit -> compile_exit env
            | Return -> compile_return env
            | Call_fb(pt, fb, parms) -> compile_fb_call env (pt, fb, parms)
            | Call_fun(ft, parms) -> Expression.compile_function_call env.mem_env (ft, parms), env
    
    module Pou = 
        open Project
        open Language
        open Pous
        
        let compile_pou_body env = function 
            | FsPlcCompiler.Stage0.Project.ST b -> 
                let is, env = Statement.compile_block env b
                is, env
        
        let compile code_offset mem_env (pou : FsPlcCompiler.Stage0.Project.Pou) = 
            let bodies, env = 
                match pou.typ with
                | FUNCTION -> 
                    let ret_mem = Memmap.fun_ret mem_env.map pou.name
                    
                    let bodies, env = 
                        pou.bodies |> List.fold (fun (bs, env) b -> 
                                          let is, env = compile_pou_body env b
                                          (is :: bs, env)) ([], Statement.empty_env mem_env)
                    [ [ OP(LOADOP_REF(LD, ret_mem))
                        OP(RETOP RET) ] ]
                    @ bodies, env
                | _ -> 
                    let bodies, env = 
                        pou.bodies |> List.fold (fun (bs, env) b -> 
                                          let is, env = compile_pou_body env b
                                          (is :: bs, env)) ([], Statement.empty_env mem_env)
                    [ OP(RETOP RET) ] :: bodies, env
            
            let code_offset' = 
                code_offset + (bodies
                               |> List.map Language.code_length
                               |> List.sum)
            
            { name = pou.name
              code_pointer = code_offset
              vars = pou.vars
              typ = pou.typ
              returnType = pou.returnType
              bodies = List.rev bodies }, code_offset'
    
    module Project = 
        open FsPlcCompiler
        open Project
        
        let compile_project_types code_offset mem_env (p : Stage0.Project.Project_types) = 
            let pous, code_offset = 
                p.pous |> List.fold (fun (pous, code_offset) pou -> 
                              let pou, code_offset = Pou.compile code_offset mem_env pou
                              pou :: pous, code_offset) ([], code_offset)
            { dataTypes = p.dataTypes
              pous = List.rev pous }, code_offset
        
        let compile_tasks memmap (p : Stage0.Project.Project) = 
            p.tasks |> List.map (fun task -> 
                           let pous = 
                               task.pous |> List.map (fun (instance, typ) -> 
                                                { data = Memmap.pou_instance_mem memmap instance
                                                  typ = typ })
                           { name = task.name
                             priority = task.priority
                             interval = task.interval
                             pous = pous })
        
        let compile memmap (p : Stage0.Project.Project) = 
            let env = Expression.empty_env memmap
            let code_offset = 0
            let types, code_offset = compile_project_types code_offset env p.types
            
            let libraries, code_offset = 
                p.libraries |> List.fold (fun (libraries, code_offset) (id, pt) -> 
                                   let types, code_offset = compile_project_types code_offset env pt
                                   ((id, types) :: libraries, code_offset)) ([], code_offset)
            
            let tasks = compile_tasks memmap p
            { types = types
              libraries = List.rev libraries
              tasks = tasks }
