namespace FsPlcVm

open Microsoft.Z3

module Runtime = 
    open FsPlcModel.IL
    open Representation
    open Language
    
    type Task_pou = 
        { entry : Code_pointer
          data : Memory_address }
    
    type Task = 
        { id : Task_id
          pou : Task_pou list
          priority : int
          interval : System_time }
    
    type Location = 
        | LOC_USER of Code_pointer
        | LOC_MONITOR of Code_pointer
        | LOC_OS of Code_pointer
    
    type Runtime = 
        { code : Operator []
          os_entry : Code_pointer
          location : Code_pointer -> Location
          tasks : Task list }
    
    let make (code : Operator []) tasks monitor inputs = 
        let tasks = List.sortBy (fun t -> t.priority) tasks
        
        let read_inputs = 
            inputs
            |> Seq.map (READ_INPUT >> OSOP)
            |> Array.ofSeq
        
        let monitor_offset = code.Length
        let monitor = Language.relocate monitor_offset monitor
        
        let call_mains = 
            tasks
            |> Seq.collect (fun task -> 
                   [ NULLOP LD_CURRENT_TIME_NOCHANGE
                     OSOP(IS_TASK_READY(task.interval, task.id))
                     JMPOP_REL(JMPCN, 6)
                     OSOP(BEGIN_TASK(task.interval, task.id)) ]
                   @ List.collect (fun pou -> 
                         [ ST_AUX(Reg 0, Reference pou.data) // "this" struct of task POU
                           CALLOP_IMM(CAL, pou.entry, Reg 0) ]) task.pou
                     @ [ NULLOP LD_CURRENT_TIME
                         OSOP(END_TASK(task.interval, task.id)) ])
            |> Array.ofSeq
        
        let os_entry = code.Length + monitor.Length
        
        let wait_for_next_task = 
            let rec loop = 
                function 
                | [] -> []
                | [ t ] -> [ OSOP(IS_TASK_READY(t.interval, t.id)) ]
                | t :: ts -> 
                    [ OSOP(IS_TASK_READY(t.interval, t.id))
                      LBINOP_PUSH OR ]
                    @ loop ts @ [ POP ]
            loop tasks @ [ MONITOROP ASSUME ] |> Array.ofList
        
        let runtime_image = 
            Array.concat [ code
                           monitor
                           read_inputs
                           call_mains
                           [| ST_AUX(Reg 0, Reference 0)
                              CALLOP_IMM(CAL, monitor_offset, Reg 0)
                              OSOP NEXT_CYCLE
                              NULLOP LD_CURRENT_TIME |]
                           wait_for_next_task
                           [| JMPOP(JMP, os_entry) |] ]
        
        let location pc = 
            if pc < code.Length then LOC_USER pc
            else if pc < code.Length + monitor.Length then LOC_MONITOR pc
            else LOC_OS pc
        
        { code = runtime_image
          tasks = tasks
          location = location
          os_entry = os_entry }
