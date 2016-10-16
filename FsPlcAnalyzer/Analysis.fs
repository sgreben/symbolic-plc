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
                | S0 -> Continue
            loop item
        | None -> Stopped Empty_schedule
    
    let run (code : Language.Program) schedule deschedule = 
        let rec loop() = 
            match step code schedule deschedule () with
            | Stopped reason -> reason
            | Continue -> loop()
        loop()
