namespace FsPlcAnalyzer

open Analysis
open FsPlcVm
open Representation
open SMT

module Testcase = 
    type Env = 
        { location : Code_pointer -> Runtime.Location
          monitor_state : Memory_address
          spec_moment : Memory_address
          memory : Memory_address list
          sensors : Memory_address list
          code : Language.Operator [] }
    
    let env location monitor_state spec_moment memory sensors code = 
        { location = location
          monitor_state = monitor_state
          spec_moment = spec_moment
          memory = memory
          sensors = sensors
          code = code }
    
    type Testcase_step = 
        { location : Runtime.Location
          instr : Language.Operator
          task : Task_id option
          monitor_state : Code_pointer
          spec_moment : Basic_value
          memory : Map<Memory_address, Basic_value>
          time : Symbolic_time
          cr : Cell
          cycle : int }
    
    type Cycle_step = 
        { cycle : int
          sensors : Map<Memory_address, Basic_value>
          step_index : int }
    
    type Constraint_step = 
        { step_index : int
          expr : Symbolic_bool }
    
    type Testcase = 
        { system : Testcase_step array
          cycles : Cycle_step array
          constraints : Constraint_step array }
    
    let capture_memory rs state = 
        rs
        |> Seq.map (fun r -> r, Primitives.read_basic_value r state)
        |> Map.ofSeq
    
    let system_step (e : Env) step_index (state : States.State) = 
        { location = e.location state.pc
          instr = e.code.[state.pc]
          task = 
              match state.task_current with
              | Some(_, tid) -> Some tid
              | None -> None
          monitor_state = Primitives.read_code_ptr e.monitor_state state
          spec_moment = Primitives.read_basic_value e.spec_moment state
          memory = capture_memory e.memory state
          cycle = state.cycle
          cr = state.cr
          time = state.time }
    
    let system_steps e snapshots = Array.mapi (system_step e) snapshots
    
    let constraint_steps item = 
        item.path.ToArray() |> Array.map (fun step -> 
                                   { step_index = (int) step.ic
                                     expr = step.cond })
    
    let cycle_steps (e : Env) (item : Worklist_item) (snapshots : States.State []) = 
        let max_cycle = item.vm_state.cycle
        let steps = ref []
        
        let rec loop c i = 
            if i >= snapshots.Length then ()
            else 
                let c' = snapshots.[i].cycle
                if c' = c then 
                    steps := { cycle = c'
                               step_index = i
                               sensors = capture_memory e.sensors snapshots.[i] }
                             :: !steps
                    loop (c' + 1) (i + 1)
                else loop c (i + 1)
        loop 0 0
        !steps
        |> List.rev
        |> Array.ofList
    
    let build e item = 
        let snapshots = 
            item.snapshots
            |> List.rev
            |> Array.ofSeq
        
        let system = system_steps e snapshots
        { system = system
          cycles = cycle_steps e item snapshots
          constraints = constraint_steps item }
    
    let print_testcase t = 
        let n_steps = t.system.Length
        let i_cycles = ref 0
        let i_constraints = ref 0
        for i = 0 to n_steps - 1 do
            let cycle = 
                if !i_cycles < t.cycles.Length && t.cycles.[!i_cycles].step_index = i then t.cycles.[!i_cycles].sensors
                else Map.empty
            
            let constr = 
                if !i_constraints < t.constraints.Length && t.constraints.[!i_constraints].step_index = i then 
                    Some(t.constraints.[!i_constraints].expr)
                else None
            
            let row = 
                sprintf "%04d %02d %20A %10A %20A %30A %30A %A %A %A" i t.system.[i].cycle t.system.[i].location 
                    t.system.[i].time t.system.[i].task t.system.[i].instr t.system.[i].cr t.system.[i].memory cycle 
                    constr
            if !i_constraints < t.constraints.Length && t.constraints.[!i_constraints].step_index = i then 
                i_constraints := !i_constraints + 1
            if !i_cycles < t.cycles.Length && t.cycles.[!i_cycles].step_index = i then i_cycles := !i_cycles + 1
            printfn "%s" row
    
    exception Unsat
    
    let print_concrete_sensors (e : Env) (t : Testcase) = 
        match SMT.Solver.check() with
        | Some true -> 
            let mdl = SMT.Solver.get_model()
            for i = 0 to t.cycles.Length - 1 do
                let mdl = 
                    t.cycles.[i].sensors 
                    |> Map.map (fun k v -> 
                           let v = 
                               match v with
                               | SYM(SYM_REAL v) -> mdl.Eval(v)
                               | SYM(SYM_TIME v) -> mdl.Eval(v)
                               | SYM(SYM_UINT v) -> mdl.Eval(v)
                               | SYM(SYM_INT v) -> mdl.Eval(v)
                               | SYM(SYM_BOOL v) -> mdl.Eval(v)
                               | INT i -> SMT.Const.mk_int i :> Microsoft.Z3.Expr
                           t.cycles.[i].step_index, mdl.Eval(t.system.[t.cycles.[i].step_index].time), v)
                
                let cycle = sprintf "(cycle=%d) %A" i mdl
                printfn "%s" cycle
        | _ -> raise Unsat
    
    let print_concrete_value (t : Testcase) r = 
        let mdl = SMT.Solver.get_model()
        for i = 0 to t.cycles.Length - 1 do
            match t.system.[t.cycles.[i].step_index].memory.[r] with
            | SYM(SYM_BOOL b) -> printfn "%A" (mdl.Eval(b, true))
            | v -> printfn "%A" v
