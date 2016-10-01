namespace FsPlcCompiler.Stage2

open FsPlcCompiler
open FsPlcModel.IL
open FsPlcModel

module Language = 
    type Statement = FsPlcVm.Language.Operator

module Project = 
    open FsPlcVm.Representation
    open FsPlcCompiler.Stage0.Access
    
    type Program = Language.Statement array
    
    type Task_pou = FsPlcVm.Runtime.Task_pou
    
    type Task = FsPlcVm.Runtime.Task
    
    type Project = 
        { tasks : Task list
          code : Program }

module Code_map = 
    open FsPlcVm.Representation
    open Stage1.Language
    open FsPlcCompiler.Stage0.Access
    
    type Pou_code_map = 
        { labels : Map<Label_name, Code_pointer>
          code_pointer : Code_pointer }
    
    type Module_code_map = Map<Common.Identifier, Pou_code_map>
    
    type Project_code_map = 
        { user : Module_code_map
          libraries : Map<Common.Identifier, Module_code_map> }
    
    let rec block_code_map map offset = 
        function 
        | [] -> map, offset
        | (LABEL l) :: is -> block_code_map (Map.add l offset map) (offset + 1) is
        | (BLOCK(_, b)) :: is -> 
            let map, offset = block_code_map map offset b
            block_code_map map (offset + 1) is
        | (OP _) :: is | (SJMP _) :: is | (SCALL _) :: is | (SFUN _) :: is -> block_code_map map (offset + 1) is
    
    let pou_code_map (pou : Stage1.Project.Pou) = 
        let code_pointer = pou.code_pointer
        let labels, _ = 
            pou.bodies |> List.fold (fun (map, offset) b -> block_code_map map offset b) (Map.empty, code_pointer)
        { code_pointer = code_pointer
          labels = labels }
    
    let project_types_code_map (pt : Stage1.Project.Project_types) = 
        pt.pous |> List.fold (fun code_map pou -> 
                       let (_, id) = pou.name
                       Map.add id (pou_code_map pou) code_map) Map.empty
    
    let project_code_map (p : Stage1.Project.Project) = 
        let user = p.types |> project_types_code_map
        
        let libraries = 
            p.libraries
            |> List.map (fun (id, pt) -> id, project_types_code_map pt)
            |> Map.ofList
        { user = user
          libraries = libraries }
    
    let pou_label_offset code_map l = 
        function 
        | (User, id) -> code_map.user.[id].labels.[l]
        | (Library lib, id) -> code_map.libraries.[lib].[id].labels.[l]
    
    let pou_offset code_map = 
        function 
        | (User, id) -> code_map.user.[id].code_pointer
        | (Library lib, id) -> code_map.libraries.[lib].[id].code_pointer
    
    let fun_offset code_map = 
        function 
        | Project_function(User, id) -> pou_offset code_map (User, id)
        | Project_function(Library lib, id) -> pou_offset code_map (Library lib, id)

//module Code_map_rev =
//    open FsPlcVm.Representation
//    open FsPlcCompiler.Stage0.Access
//    type Code_location = Pou_type_path * StatementAstSource
//    type Code_map_rev = Map<Code_pointer,Code_location>
//    let pou_code_map (p:Stage1.Project.Pou)
// Just eliminate symbolic labels & fill in code locations for pous & functions
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
