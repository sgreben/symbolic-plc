namespace FsPlcVm

module IL = FsPlcModel.IL

open Microsoft.Z3

module Monitor_compiler = 
    open Representation
    
    type Value_constraint = Cell * IL.Cmp_op * Cell
    
    type State = 
        | S of int
    
    type Variable = 
        | V of int
    
    type Time_constraint = 
        | Diff_lt of Variable * int
        | Diff_le of Variable * int
        | Diff_ge of Variable * int
        | Diff_gt of Variable * int
    
    type Time_constraint_spec = 
        | Time_le of int
        | Time_ge of int
    
    let tc_of_spec t = 
        function 
        | Time_le x -> Diff_le(t, x)
        | Time_ge x -> Diff_ge(t, x)
    
    type Formula = 
        | Constraint of Value_constraint * Formula
        | Within of Time_constraint_spec * Formula
        | Delay of Time_constraint_spec * Formula
        | Seq of Formula * Formula
        | Check of Value_constraint
        | Pass
        | Empty
    
    type Atom = 
        | Time_constraint of Time_constraint
        | Value_constraint of Value_constraint
    
    type Constraint = Atom list
    
    type Fta_instruction = 
        | ASSUME of Constraint
        | ASSERT of Constraint
        | PASS
        | NEXT
        | SET_STATE of State
        | NONDET of State
        | LD_TIME of Variable
        | ST_MOMENT of int
        | JMP of State
        | STATE of State
    
    type Compiled_formula = 
        { initial_state : State
          instructions : Fta_instruction list }
    
    type Compile_env = 
        { max_var : Variable
          max_state : State
          max_moment : int }
    
    let fresh_state env = 
        match env.max_state with
        | S i -> S(i + 1), { env with max_state = S(i + 1) }
    
    let fresh_var env = 
        match env.max_var with
        | V i -> V(i + 1), { env with max_var = V(i + 1) }
    
    let next_moment env = env.max_moment, { env with max_moment = env.max_moment + 1 }
    
    let empty_env = 
        { max_state = S 1
          max_var = V 0
          max_moment = 0 }
    
    let rec compile env on_finish C A = 
        match A with
        | Pass -> 
            let s_i, env = fresh_state env
            { initial_state = s_i
              instructions = 
                  [ STATE s_i
                    ASSUME C
                    PASS
                    SET_STATE on_finish
                    NEXT ] }, env
        | Check assertion -> 
            let s_i, env = fresh_state env
            { initial_state = s_i
              instructions = 
                  [ STATE s_i
                    ASSUME C
                    ASSERT [ Value_constraint assertion ]
                    ST_MOMENT env.max_moment
                    JMP on_finish ] }, env
        | Empty -> 
            let s_i, env = fresh_state env
            { initial_state = s_i
              instructions = 
                  [ STATE s_i
                    ASSUME C
                    ST_MOMENT env.max_moment
                    JMP on_finish ] }, env
        | Seq(A, B) -> 
            let s_i, env = fresh_state env
            let s_temp, env = fresh_state env
            let s_f, env = fresh_state env
            let A, env = compile env s_temp C A
            let B, env = compile env s_f C B
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C ]
                                A.instructions
                                [ STATE s_temp ]
                                B.instructions
                                [ STATE s_f
                                  JMP on_finish ] ] }, env
        | Constraint(phi, A) -> compile env on_finish ((Value_constraint phi) :: C) A
        | Delay(Time_le x, A) -> 
            let s_i, env = fresh_state env
            let s_wait, env = fresh_state env
            let s_go, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let moment, env = next_moment env
            let A, env = compile env s_f C A
            let t_phi = Time_constraint(tc_of_spec t (Time_le x))
            let t_phi_not = Time_constraint(Diff_gt(t, x))
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  ST_MOMENT moment
                                  LD_TIME t
                                  STATE s_wait
                                  NONDET s_go
                                  ASSUME(t_phi :: C)
                                  NEXT
                                  STATE s_go
                                  ASSUME(t_phi :: C)
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  ASSUME C
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
        | Delay(Time_ge x, A) -> 
            let s_i, env = fresh_state env
            let s_wait, env = fresh_state env
            let s_go, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let moment, env = next_moment env
            let A, env = compile env s_f C A
            let t_phi = Time_constraint(tc_of_spec t (Time_ge x))
            let t_phi_not = Time_constraint(Diff_lt(t, x))
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  ST_MOMENT moment
                                  LD_TIME t
                                  STATE s_wait
                                  NONDET s_go
                                  ASSUME(t_phi_not :: C)
                                  NEXT
                                  STATE s_go
                                  ASSUME(t_phi :: C)
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  ASSUME C
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
        | Within(Time_le x, A) -> 
            let s_i, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let t_phi = Time_constraint(tc_of_spec t (Time_le x))
            let A, env = compile env s_f (t_phi :: C) A
            let start_moment, env = next_moment env
            let end_moment, env = next_moment env
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  LD_TIME t
                                  ST_MOMENT start_moment
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  ASSUME(t_phi :: C)
                                  ST_MOMENT end_moment
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
        | Within(Time_ge x, A) -> 
            let s_i, env = fresh_state env
            let s_wait, env = fresh_state env
            let s_go, env = fresh_state env
            let s_f, env = fresh_state env
            let t, env = fresh_var env
            let t_phi = Time_constraint(tc_of_spec t (Time_ge x))
            let t_phi_not = Time_constraint(Diff_lt(t, x))
            let A, env = compile env s_f (t_phi :: C) A
            let moment, env = next_moment env
            { initial_state = s_i
              instructions = 
                  List.concat [ [ STATE s_i
                                  ASSUME C
                                  LD_TIME t
                                  JMP A.initial_state ]
                                A.instructions
                                [ STATE s_f
                                  NONDET s_wait
                                  ASSUME(t_phi :: C)
                                  ST_MOMENT moment
                                  JMP on_finish
                                  STATE s_wait
                                  ASSUME(t_phi_not :: C)
                                  SET_STATE on_finish
                                  JMP on_finish ] ] }, env
    
    let compile' A = compile empty_env (S 0) [] A
    
    open Language
    open IL
    
    type Instruction = 
        | STATE of State
        | OP of Operator
        | LD_STATE of State
        | JMP_STATE of State
        | JMP_STATE_NONDET of State
    
    exception Only_basic_values_are_supported_in_constraints

    let compile_constraint m = 
        function 
        | Time_constraint(Diff_le(t, x)) -> 
            [ NULLOP LD_CURRENT_TIME_NOCHANGE
              ABINOP_IMM(SUB, TIME((int64) x))
              CMPOP_REF(LE, m t) ]
        | Time_constraint(Diff_lt(t, x)) -> 
            [ NULLOP LD_CURRENT_TIME_NOCHANGE
              ABINOP_IMM(SUB, TIME((int64) x))
              CMPOP_REF(LT, m t) ]
        | Time_constraint(Diff_gt(t, x)) -> 
            [ NULLOP LD_CURRENT_TIME_NOCHANGE
              ABINOP_IMM(SUB, TIME((int64) x))
              CMPOP_REF(GT, m t) ]
        | Time_constraint(Diff_ge(t, x)) -> 
            [ NULLOP LD_CURRENT_TIME_NOCHANGE
              ABINOP_IMM(SUB, TIME((int64) x))
              CMPOP_REF(GE, m t) ]
        | Value_constraint(a, op, b) -> 
            let load_a = 
                match a with
                | Reference r -> LOADOP_REF(LD, r)
                | Value v -> LOADOP_IMM(LD, v)
            
            let compare_to_b = 
                match b with
                | Reference r -> CMPOP_REF(op, r)
                | Value(VBasic v) -> CMPOP_IMM(op, v)
                | Value _ -> raise Only_basic_values_are_supported_in_constraints
            
            [ load_a; compare_to_b ]
    
    let compile_assume m = 
        function 
        | [] -> []
        | c :: cs -> 
            let rec loop = 
                function 
                | [] -> []
                | [ c ] -> compile_constraint m c
                | c :: cs -> 
                    List.concat [ compile_constraint m c
                                  [ LBINOP_PUSH AND ]
                                  loop cs
                                  [ POP ] ]
            loop (c :: cs)
    
    let compile_instruction s_mem moment_mem m = 
        function 
        | Fta_instruction.PASS -> [ OP(MONITOROP PASS) ]
        | Fta_instruction.ST_MOMENT i -> 
            [ OP(LOADOP_IMM(LD, VBasic(UINT((uint64) i))))
              OP(STOREOP_IMM(ST, moment_mem)) ]
        | NEXT -> [ JMP_STATE(State.S 0) ]
        | Fta_instruction.ASSUME [] -> []
        | Fta_instruction.ASSUME C -> 
            List.concat [ compile_assume m C |> List.map OP
                          [ OP(MONITOROP ASSUME) ] ]
        | Fta_instruction.ASSERT [] -> []
        | Fta_instruction.ASSERT C -> 
            List.concat [ compile_assume m C |> List.map OP
                          [ OP(JMPOP_REL(JMPC, 2))
                            OP(MONITOROP FAIL) ] ]
        | Fta_instruction.NONDET s -> [ JMP_STATE_NONDET s ]
        | LD_TIME t -> 
            [ OP(NULLOP LD_CURRENT_TIME)
              OP(STOREOP_IMM(ST, m t)) ]
        | Fta_instruction.JMP s -> [ JMP_STATE s ]
        | Fta_instruction.SET_STATE s -> 
            [ LD_STATE s
              OP(STOREOP_IMM(ST, s_mem)) ]
        | Fta_instruction.STATE s -> 
            [ STATE s
              OP(STOREOP_IMM(ST, s_mem)) ]
    
    type Compiled = 
        { memory : Map<Memory_address, Cell>
          state_mem : Memory_address
          moment_mem : Memory_address
          code : Operator [] }
    
    let to_vm (env : Compile_env) A mem_offset = 
        let state_memory = mem_offset
        let moment_memory = mem_offset + 1
        let timer_memory (V t) = t + mem_offset + 1
        let instructions = A.instructions |> List.collect (compile_instruction state_memory moment_memory timer_memory)
        let instructions = [ OP(JMP_REF(state_memory, 0)) ] @ instructions @ [ STATE(State.S 0) ]
        
        let state_pc = 
            instructions
            |> List.fold (fun (i, m) -> 
                   function 
                   | STATE s -> (i + 1, Map.add s i m)
                   | _ -> (i + 1, m)) (0, Map.empty)
            |> snd
        
        let instructions = 
            instructions |> List.map (function 
                                | STATE(State.S 0) -> RETOP RET
                                | STATE s | LD_STATE s -> LOADOP_IMM(LD, VCode_ptr state_pc.[s])
                                | JMP_STATE s -> JMPOP(JMP, state_pc.[s])
                                | JMP_STATE_NONDET s -> NONDET state_pc.[s]
                                | OP op -> op)
        
        let memory = 
            seq { 1..timer_memory env.max_var } 
            |> Seq.fold (fun m i -> Map.add (mem_offset + i) (Value(VBasic(TIME 0L))) m) Map.empty
        let memory = Map.add mem_offset (Value(VCode_ptr 1)) memory
        { memory = memory
          state_mem = state_memory
          moment_mem = moment_memory
          code = Array.ofList instructions }
