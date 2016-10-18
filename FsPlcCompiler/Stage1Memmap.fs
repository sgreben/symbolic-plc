namespace FsPlcCompiler.Stage1

open FsPlcModel.IL
open FsPlcModel

module Memmap = 
    open FsPlcModel
    open Declarations
    open FsPlcVm.Language
    open FsPlcVm.Representation
    open FsPlcCompiler
    
    module Scope = FsPlcCompiler.Stage0.Scope
    
    open FsPlcCompiler.Stage0.Access
    
    type Mem_env = 
        { memory : Map<Memory_address, Cell>
          offset : Memory_address }
    
    let alloc env = env.offset, { env with offset = env.offset + 1 }
    let allocn n env = env.offset, { env with offset = env.offset + n }
    let set_mem r v env = { env with memory = Map.add r v env.memory }
    
    let rec allocate_simple_value (scope : Scope.Project_scope) mem t = 
        match t with
        | Types.BASIC Types.BOOL -> 
            let offset, mem = alloc mem
            let mem = set_mem offset (Value(VBasic(BOOL false))) mem
            offset, mem
        | Types.BASIC Types.LINT | Types.BASIC Types.INT -> 
            let offset, mem = alloc mem
            let mem = set_mem offset (Value(VBasic(INT 0L))) mem
            offset, mem
        | Types.BASIC Types.UINT -> 
            let offset, mem = alloc mem
            let mem = set_mem offset (Value(VBasic(UINT 0UL))) mem
            offset, mem
        | Types.STRUCT s -> 
            let offset, mem = alloc mem
            
            let _, fields, mem = 
                List.fold (fun (i, fields, mem) (id, t) -> 
                    let t = Stage0.Resolve.resolve_project_type scope.types t |> Option.get
                    match t with
                    | Simple_type(_, t) -> 
                        let offset, mem = allocate_simple_value scope mem t
                        (i + 1, Map.add i offset fields, mem)) (0, Map.empty, mem) s
            
            let mem = set_mem offset (Value(VStruct fields)) mem
            offset, mem
    
    type Pou_type_memmap = 
        { field_index : Map<Common.Identifier, Struct_field>
          vars_in : Struct_field [] }
    
    let pou_type_memmap (pou : Scope.Pou_scope) = 
        let field_index = 
            pou.vars_list
            |> Seq.mapi (fun i d -> d.id, i)
            |> Map.ofSeq
        
        let vars_in = 
            pou.vars_in
            |> Seq.map (fun d -> field_index.[d.id])
            |> Array.ofSeq
        
        { field_index = field_index
          vars_in = vars_in }
    
    type Fun_type_memmap = 
        { vars_struct : Memory_address
          vars_in : Common.Identifier []
          var_memory : Map<Common.Identifier, Memory_address>
          field_index : Map<Common.Identifier, Struct_field>
          ret_memory : Memory_address }
    
    let fun_type_memmap (scope : Scope.Project_scope) mem (pou : Scope.Pou_scope) = 
        let field_index = 
            pou.vars_list
            |> Seq.mapi (fun i d -> d.id, i)
            |> Map.ofSeq
        
        let vars_in = 
            pou.vars_in
            |> Seq.map (fun d -> d.id)
            |> Array.ofSeq
        
        let vars_struct_offset, mem = alloc mem
        
        let var_memory, mem = 
            pou.vars_list |> Seq.fold (fun (var_memory, mem) decl -> 
                                 let t = 
                                     Stage0.Resolve.resolve_project_type scope.types decl.typ
                                     |> Option.get
                                     |> Stage0.Resolve.get_simple_type
                                 
                                 let offset, mem = allocate_simple_value scope mem t
                                 (Map.add decl.id offset var_memory, mem)) (Map.empty, mem)
        
        let vars_struct = 
            var_memory
            |> Map.toSeq
            |> Seq.map (fun (id, r) -> field_index.[id], r)
            |> Map.ofSeq
        
        let mem = set_mem vars_struct_offset (Value(VStruct(vars_struct))) mem
        let offset, mem = alloc mem
        let mem = set_mem offset (Value(VBasic(STRING("return value of " + pou.ast.name)))) mem
        { vars_in = vars_in
          field_index = field_index
          vars_struct = vars_struct_offset
          var_memory = var_memory
          ret_memory = offset }, mem
    
    type Module_types_memmap = 
        { struct_field : Map<Common.Identifier, Map<Common.Identifier, Struct_field>>
          fb_memory : Map<Common.Identifier, Pou_type_memmap>
          fun_memory : Map<Common.Identifier, Fun_type_memmap> }
    
    let module_types_memmap scope mem (mt : Scope.Module_types_scope) = 
        let struct_field = 
            mt.simple |> Map.map (fun _ dt -> 
                             match dt.typ with
                             | Types.STRUCT s -> 
                                 s
                                 |> List.mapi (fun i (id, _) -> id, i)
                                 |> Map.ofList
                             | _ -> Map.empty)
        
        let pou_memory_fb = mt.pouFunctionBlock |> Map.map (fun _ -> pou_type_memmap)
        let pou_memory_prg = mt.pouProgram |> Map.map (fun _ -> pou_type_memmap)
        
        let fb_memory = 
            Map(Seq.concat [ pou_memory_fb |> Map.toSeq
                             pou_memory_prg |> Map.toSeq ])
        
        let fun_memory, mem = 
            mt.pouFunction
            |> Map.toSeq
            |> Seq.fold (fun (fun_memory, mem) (id, f) -> 
                   let f, mem = fun_type_memmap scope mem f
                   (Map.add id f fun_memory, mem)) (Map.empty, mem)
        
        { struct_field = struct_field
          fb_memory = fb_memory
          fun_memory = fun_memory }, mem
    
    type Project_types_memmap = 
        { library : Map<Common.Identifier, Module_types_memmap>
          user : Module_types_memmap }
    
    let project_types_memmap scope mem (pt : Scope.Project_types_scope) = 
        let user, mem = module_types_memmap scope mem pt.user
        
        let library, mem = 
            pt.library
            |> Map.toSeq
            |> Seq.fold (fun (library, mem) (id, lib) -> 
                   let lib, mem = module_types_memmap scope mem lib
                   (Map.add id lib library, mem)) (Map.empty, mem)
        { user = user
          library = library }, mem
    
    let lookup_pou_type_memmap (types_memmap : Project_types_memmap) = 
        function 
        | (User, id) -> types_memmap.user.fb_memory.[id]
        | (Library lib, id) -> types_memmap.library.[lib].fb_memory.[id]
    
    let rec allocate_pou (types_memmap : Project_types_memmap) (scope : Scope.Project_scope) mem t = 
        let memmap = lookup_pou_type_memmap types_memmap t
        let vars = Stage0.Lookup.project_pou_type scope.types t |> Option.get
        let offset, mem = alloc mem
        
        let vars_map, mem = 
            vars
            |> Map.toSeq
            |> Seq.fold (fun (vars_map, mem) (id, d) -> 
                   let t = Stage0.Resolve.resolve_project_type scope.types d.typ |> Option.get
                   let offset, mem = allocate_value types_memmap scope mem t
                   let i = memmap.field_index.[id]
                   (Map.add i offset vars_map, mem)) (Map.empty, mem)
        
        let mem = set_mem offset (Value(VStruct vars_map)) mem
        offset, mem
    
    and allocate_value (types_memmap : Project_types_memmap) (scope : Scope.Project_scope) mem = 
        function 
        | Pou_type t -> 
            let ast = Stage0.Lookup.project_pou_ast scope.types t |> Option.get
            match ast.typ with
            | FsPlcModel.Pous.FUNCTION -> 
                let offset, mem = alloc mem
                let mem = set_mem offset (Reference 0) mem
                offset, mem
            | _ -> allocate_pou types_memmap scope mem t
        | Simple_type(_, t) -> 
            match t with
            | Types.BASIC Types.BOOL -> 
                let offset, mem = alloc mem
                let mem = set_mem offset (Value(VBasic(BOOL false))) mem
                offset, mem
            | Types.BASIC Types.LINT | Types.BASIC Types.INT -> 
                let offset, mem = alloc mem
                let mem = set_mem offset (Value(VBasic(INT 0L))) mem
                offset, mem
            | Types.BASIC Types.UINT | Types.BASIC Types.BYTE -> 
                let offset, mem = alloc mem
                let mem = set_mem offset (Value(VBasic(UINT 0UL))) mem
                offset, mem
            | Types.STRUCT s -> 
                let offset, mem = alloc mem
                
                let _, fields, mem = 
                    List.fold (fun (i, fields, mem) (id, t) -> 
                        let t = Stage0.Resolve.resolve_project_type scope.types t |> Option.get
                        let offset, mem = allocate_value types_memmap scope mem t
                        (i + 1, Map.add i offset fields, mem)) (0, Map.empty, mem) s
                
                let mem = set_mem offset (Value(VStruct fields)) mem
                offset, mem
    
    type Gvl_memmap = Map<Common.Identifier, Memory_address>
    
    let gvl_memmap types_memmap (scope : Scope.Project_scope) mem (gvl : Scope.Unit_scope) = 
        Map.toSeq gvl |> Seq.fold (fun (vars, mem) (id, decl) -> 
                             let t = Stage0.Resolve.resolve_project_type scope.types decl.typ |> Option.get
                             let offset, mem = allocate_value types_memmap scope mem t
                             Map.add id offset vars, mem) (Map.empty, mem)
    
    type Module_values_memmap = 
        { gvl : Map<Common.Identifier, Gvl_memmap>
          pouInstance : Map<Common.Identifier, Memory_address> }
    
    let module_values_memmap types_memmap scope mem (mv : Scope.Module_values_scope) = 
        let gvl_map, mem = 
            mv.gvl
            |> Map.toSeq
            |> Seq.fold (fun (gvl_map, mem) (id, gvl) -> 
                   let gvl, mem = gvl_memmap types_memmap scope mem gvl
                   (Map.add id gvl gvl_map, mem)) (Map.empty, mem)
        
        let pouInstance, mem = 
            mv.pouInstance
            |> Map.toSeq
            |> Seq.fold (fun (pou_map, mem) (id, tref) -> 
                   let t = Types.TYPE_REFERENCE tref
                   let t = Stage0.Resolve.resolve_project_type scope.types t |> Option.get
                   match t with
                   | Pou_type t -> 
                       let offset, mem = allocate_pou types_memmap scope mem t
                       (Map.add id offset pou_map, mem)) (Map.empty, mem)
        
        { gvl = gvl_map
          pouInstance = pouInstance }, mem
    
    type Project_values_memmap = 
        { library : Map<Common.Identifier, Module_values_memmap>
          user : Module_values_memmap }
    
    let project_values_memmap types_memmap scope mem (pv : Scope.Project_values_scope) = 
        let user, mem = module_values_memmap types_memmap scope mem pv.user
        
        let library, mem = 
            pv.library
            |> Map.toSeq
            |> Seq.fold (fun (library, mem) (id, lib) -> 
                   let lib, mem = module_values_memmap types_memmap scope mem lib
                   (Map.add id lib library, mem)) (Map.empty, mem)
        { user = user
          library = library }, mem
    
    type Project_memmap = 
        { values : Project_values_memmap
          types : Project_types_memmap }
    
    let project_memmap (p : Scope.Project_scope) = 
        let mem = 
            { memory = Map.empty
              offset = 0 }
        
        let types, mem = project_types_memmap p mem p.types
        let values, mem = project_values_memmap types p mem p.values
        { values = values
          types = types }, mem
    
    let fun_vars_struct scope = 
        function 
        | (User, id) -> scope.types.user.fun_memory.[id].vars_struct
        | (Library lib, id) -> scope.types.library.[lib].fun_memory.[id].vars_struct
    
    let fun_arg_named scope fid = 
        function 
        | (User, id) -> scope.types.user.fun_memory.[id].var_memory.[fid]
        | (Library lib, id) -> scope.types.library.[lib].fun_memory.[id].var_memory.[fid]
    
    let fun_arg_anon scope i = 
        function 
        | (User, id) -> 
            scope.types.user.fun_memory.[id].var_memory.[scope.types.user.fun_memory.[id].vars_in.[i]]
        | (Library lib, id) -> 
            scope.types.user.fun_memory.[id].var_memory.[scope.types.library.[lib].fun_memory.[id].vars_in.[i]]
    
    let fun_ret scope = 
        function 
        | (User, id) -> scope.types.user.fun_memory.[id].ret_memory
        | (Library lib, id) -> scope.types.library.[lib].fun_memory.[id].ret_memory
    
    let pou_field_idx scope fid = 
        function 
        | (User, id) -> 
            try 
                scope.types.user.fb_memory.[id].field_index.[fid]
            with _ -> scope.types.user.fun_memory.[id].field_index.[fid]
        | (Library lib, id) -> 
            try 
                scope.types.library.[lib].fb_memory.[id].field_index.[fid]
            with _ -> scope.types.library.[lib].fun_memory.[id].field_index.[fid]
    
    let struct_field_idx scope fid = function 
        | Types.STRUCT s -> List.findIndex (fun (id, _) -> id = fid) s
    
    let field_idx scope fid = 
        function 
        | Pou_type pt -> pou_field_idx scope fid pt
        | Simple_type(_, t) -> struct_field_idx scope fid t
    
    let var_global scope = 
        function 
        | (User, Gvl(gvl_id, var_id)) -> scope.values.user.gvl.[gvl_id].[var_id]
        | (User, Pou_instance id) -> scope.values.user.pouInstance.[id]
        | (Library lib, Gvl(gvl_id, var_id)) -> scope.values.library.[lib].gvl.[gvl_id].[var_id]
        | (Library lib, Pou_instance id) -> scope.values.library.[lib].pouInstance.[id]
    
    let pou_arg_anon scope i = 
        function 
        | (User, id) -> scope.types.user.fb_memory.[id].vars_in.[i]
        | (Library lib, id) -> scope.types.library.[lib].fb_memory.[id].vars_in.[i]
    
    let pou_arg_named scope fid = 
        function 
        | (User, id) -> scope.types.user.fb_memory.[id].field_index.[fid]
        | (Library lib, id) -> scope.types.library.[lib].fb_memory.[id].field_index.[fid]
    
    let pou_instance_mem scope = 
        function 
        | (User, id) -> scope.values.user.pouInstance.[id]
        | (Library lib, id) -> scope.values.library.[id].pouInstance.[id]
