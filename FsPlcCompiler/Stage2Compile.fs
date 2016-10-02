namespace FsPlcCompiler.Stage2

open FsPlcCompiler
open FsPlcModel.IL
open FsPlcModel

// Eliminates symbolic labels and fills in code locations for pous and functions
module Compile = 
    module Statement = 
        open Stage1.Language
        open FsPlcVm.Language
        
        let rec compile code_map pt = 
            function 
            | LABEL l -> [ NOP ]
            | SJMP(op, l) -> [ JMPOP(op, Code_map.pou_label_offset code_map l pt) ]
            | SCALL(pt, r) -> [ CALLOP_IMM(CAL, Code_map.pou_offset code_map pt, r) ]
            | SFUN(ft, offset) -> [ FUN(Code_map.fun_offset code_map ft, offset) ]
            | OP op -> [ op ]
            | BLOCK(_, b) -> compile_block code_map pt b
        
        and compile_block code_map pt = List.collect (compile code_map pt)
    
    module Pou = 
        open Project
        
        let compile code_map (pou : Stage1.Project.Pou) = 
            pou.bodies |> List.collect (Statement.compile_block code_map pou.name)
    
    module Project = 
        open Project
        open FsPlcVm.Runtime
        
        let compile_task code_map id (task : Stage1.Project.Task) = 
            { id = id
              interval = task.interval
              priority = task.priority
              pou = 
                  task.pous |> List.map (fun (pou : Stage1.Project.Task_pou) -> 
                                   { entry = Code_map.pou_offset code_map pou.typ
                                     data = pou.data }) }
        
        let compile_tasks code_map (p : Stage1.Project.Project) = p.tasks |> List.mapi (compile_task code_map)
        
        let compile code_map (p : Stage1.Project.Project) : Project.Project = 
            let user_pous = p.types.pous |> List.collect (Pou.compile code_map)
            let library_pous = 
                p.libraries |> List.collect (fun (id, pt) -> pt.pous |> List.collect (Pou.compile code_map))
            let code = user_pous @ library_pous |> Array.ofList
            let tasks = compile_tasks code_map p
            { code = code
              tasks = tasks }
