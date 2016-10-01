namespace FsPlcVm

module IL = FsPlcModel.IL

open Microsoft.Z3

module Analysis = 
    open Representation
    
    type Step = 
        { id : uint64
          ic : uint64
          cond : Symbolic_bool }
    
    type Path = ResizeArray<Step>
    
    type Backtrack = 
        { pop : int
          push : int }
    
    let inline backtrack' (_from : Path) (_to : Path) = 
        let i_from = ref 0
        let i_to = ref 0
        let i_same = ref -1
        for i = 0 to min (_from.Count - 1) (_to.Count - 1) do
            if _from.[i].id = _to.[i].id then i_same := i
        if !i_same < 0 then 
            { pop = _from.Count
              push = _to.Count }
        else 
            { pop = _from.Count - !i_same - 1
              push = _to.Count - !i_same - 1 }
    
    let inline backtrack pop push (_from : Path) (_to : Path) = 
        match backtrack' _from _to with
        | { pop = 0; push = 0 } -> ()
        | bt -> 
            pop bt.pop
            for i = _to.Count - bt.push to _to.Count - 1 do
                push _to.[i]
    
    type Worklist_item = 
        { path : Path
          snapshots : States.State list
          vm_state : States.State }
    
    let initial vm_state = 
        { path = ResizeArray()
          snapshots = []
          vm_state = vm_state }
    
    let inline set_vm_state v s = { s with vm_state = v }
    
    let inline extend_path next_id cond s = 
        let path' = ResizeArray(s.path)
        path'.Add({ id = next_id()
                    cond = cond
                    ic = (Primitives.ic s.vm_state) })
        { s with path = path' }
    
    let inline extend_path_nobranch next_id cond s = 
        if s.path.Count = 0 then extend_path next_id cond s
        else 
            let path' = ResizeArray(s.path)
            let step = path'.[path'.Count - 1]
            path'.[path'.Count - 1] <- { step with cond = SMT.Rel._and step.cond cond }
            { s with path = path' }
    
    type Stop_reason = 
        | Empty_schedule
        | Budget_exhausted
    
    type Analysis_status = 
        | Continue
        | Stopped of Stop_reason
    
    let inline pop k = SMT.Solver.pop ((uint32) k)
    let inline push step = SMT.Solver.push_add step.cond
    let inline successors state (code : Language.Program) = Instructions.step code.[state.vm_state.pc] state.vm_state
    
    let straightline k state code = 
        let rec loop i state = 
            if i = k then Instructions.S1 state.vm_state
            else 
                match successors state code with
                | Instructions.S1 succ -> 
                    state
                    |> set_vm_state succ
                    |> loop (i + 1)
                | succ -> succ
        loop 0 state
    
    let _fresh_id = ref 0UL
    
    let next_id() = 
        _fresh_id := !_fresh_id + 1UL
        !_fresh_id
    
    open Instructions
    
    let previous_path = ref (ResizeArray<Step>())
    
    let inline stateful_backtrack path = 
        backtrack pop push !previous_path path
        previous_path := path
    
    let inline snapshot s = { s with snapshots = s.vm_state :: s.snapshots }
    
    exception Pass of Worklist_item
    
    exception Fail of Worklist_item
    
    let inline step (code : Language.Program) schedule deschedule () = 
        match deschedule() with
        | Some item -> 
            stateful_backtrack item.path
            let rec loop item = 
                previous_path := item.path
                let item = snapshot item
                //printfn "%s" (sprintf "%05d> cy:%02d %A pc:%03d \t%A" item.vm_state.instruction_count item.vm_state.cycle item.vm_state.time  item.vm_state.pc code.[item.vm_state.pc])
                //printfn "%A" item.vm_state.cr
                match successors item code with
                | S1P s -> raise (Pass item)
                | S1F s -> raise (Fail item)
                | S1 succ -> 
                    item
                    |> set_vm_state succ
                    |> loop
                | S1G(succ, b) -> 
                    SMT.Solver.push_add b
                    item
                    |> set_vm_state succ
                    |> extend_path next_id b
                    |> loop
                | S2(left, right) -> 
                    item
                    |> set_vm_state right
                    |> schedule
                    item
                    |> set_vm_state left
                    |> schedule
                    Continue
                | S2G(left, b, right, b') -> 
                    item
                    |> set_vm_state left
                    |> extend_path next_id b
                    |> schedule
                    item
                    |> set_vm_state right
                    |> extend_path next_id b'
                    |> schedule
                    Continue
                | S0 -> 
                    Continue
            loop item
        | None -> Stopped Empty_schedule
    
    let run (code : Language.Program) schedule deschedule = 
        let rec loop() = 
            match step code schedule deschedule () with
            | Stopped reason -> reason
            | Continue -> loop()
        loop()
    
    module Scheduler = 
        type Schedule = C5.IPriorityQueue<Worklist_item>
        
        module Bfs = 
            let empty() = 
                C5.IntervalHeap<Worklist_item>
                    ({ new System.Collections.Generic.IComparer<Worklist_item> with
                           member this.Compare(left, right) = left.path.Count.CompareTo(right.path.Count) })
            
            let inline schedule (Q : Schedule) s = ignore (Q.Add(s))
            
            let inline deschedule (Q : Schedule) () = 
                if Q.IsEmpty then None
                else Some(Q.DeleteMin())
    
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
              expr : BoolExpr }
        
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
                    if !i_cycles < t.cycles.Length && t.cycles.[!i_cycles].step_index = i then 
                        t.cycles.[!i_cycles].sensors
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
                                   | INT i -> SMT.Const.mk_int i :> Expr
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
