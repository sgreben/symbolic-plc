namespace FsPlcSpec

/// Compiles FTAs (module Fta) to LLIL assembly
module FtaCompiler = 
    module IL = FsPlcModel.IL
    open FsPlcVm
    open Representation
    open Spec
    open Fta
    
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
        | Fta.Instruction.PASS -> [ OP(MONITOROP PASS) ]
        | Fta.Instruction.ST_MOMENT i -> 
            [ OP(LOADOP_IMM(LD, VBasic(UINT((uint64) i))))
              OP(STOREOP_IMM(ST, moment_mem)) ]
        | NEXT -> [ JMP_STATE(State.S 0) ]
        | Fta.Instruction.ASSUME [] -> []
        | Fta.Instruction.ASSUME C -> 
            List.concat [ compile_assume m C |> List.map OP
                          [ OP(MONITOROP ASSUME) ] ]
        | Fta.Instruction.ASSERT [] -> []
        | Fta.Instruction.ASSERT C -> 
            List.concat [ compile_assume m C |> List.map OP
                          [ OP(JMPOP_REL(JMPC, 2))
                            OP(MONITOROP FAIL) ] ]
        | Fta.Instruction.NONDET s -> [ JMP_STATE_NONDET s ]
        | LD_TIME t -> 
            [ OP(NULLOP LD_CURRENT_TIME)
              OP(STOREOP_IMM(ST, m t)) ]
        | Fta.Instruction.JMP s -> [ JMP_STATE s ]
        | Fta.Instruction.SET_STATE s -> 
            [ LD_STATE s
              OP(STOREOP_IMM(ST, s_mem)) ]
        | Fta.Instruction.STATE s -> 
            [ STATE s
              OP(STOREOP_IMM(ST, s_mem)) ]
    
    type Compiled = 
        { memory : Map<Memory_address, Cell>
          state_mem : Memory_address
          moment_mem : Memory_address
          code : Operator [] }
    
    let to_vm (env : SpecCompiler.Compile_env) A mem_offset = 
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
