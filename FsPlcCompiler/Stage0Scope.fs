namespace FsPlcCompiler.Stage0

/// Type and value environments built from the AST
module Scope = 
    open FsPlcModel
    open Declarations
    open StExpressions
    
    type Unit_scope = Map<Common.Identifier, Declaration>
    
    /// A POU type environment
    type Pou_scope = 
        { vars : Unit_scope
          vars_list : Declaration list
          vars_in : Declaration list
          ast : Pous.PouAst }
    
    /// Module value environment
    type Module_values_scope = 
        { gvl : Map<Common.Identifier, Unit_scope>
          pouInstance : Map<Common.Identifier, Common.Qualified_identifier> }
    
    /// Module type environment
    type Module_types_scope = 
        { simple : Map<Common.Identifier, Projects.Data_type>
          pouFunctionBlock : Map<Common.Identifier, Pou_scope>
          pouProgram : Map<Common.Identifier, Pou_scope>
          pouFunction : Map<Common.Identifier, Pou_scope> }
    
    /// Project type environment
    type Project_types_scope = 
        { library : Map<Common.Identifier, Module_types_scope>
          user : Module_types_scope }
    
    /// Project value environment
    type Project_values_scope = 
        { library : Map<Common.Identifier, Module_values_scope>
          user : Module_values_scope }
    
    /// Project type and value environments
    type Project_scope = 
        { values : Project_values_scope
          types : Project_types_scope }
    
    let module_values_scope_empty : Module_values_scope = 
        { gvl = Map.empty
          pouInstance = Map.empty }
    
    let module_types_scope_empty : Module_types_scope = 
        { simple = Map.empty
          pouFunctionBlock = Map.empty
          pouProgram = Map.empty
          pouFunction = Map.empty }
    
    let project_types_scope_empty : Project_types_scope = 
        { library = Map.empty
          user = module_types_scope_empty }
    
    let project_values_scope_empty : Project_values_scope = 
        { library = Map.empty
          user = module_values_scope_empty }
    
    let project_scope_empty : Project_scope = 
        { values = project_values_scope_empty
          types = project_types_scope_empty }
    
    /// Construction of project type and value environments
    module Build = 
        open FsPlcModel
        open Declarations
        open Projects
        open Pous
        
        let declaration_block_scope (db : Declaration_block) = 
            db
            |> Seq.map (fun d -> d.id, d)
            |> Map.ofSeq
        
        let add_function_return_value (p : Pous.PouAst) = 
            match p.typ with
            | FUNCTION -> 
                let ret_typ = p.iface.Value.returnType.Value
                let ret_decl = Declarations.var_output p.name ret_typ
                { p with iface = Some { p.iface.Value with vars = p.iface.Value.vars @ [ ret_decl ] } }
            | _ -> p
        
        let pou_types_scope (p : Pous.PouAst) = 
            match p.iface with
            | None -> Map.empty
            | Some iface -> declaration_block_scope iface.vars
        
        let vars_in (p : Pous.PouAst) = 
            match p.iface with
            | None -> []
            | Some iface -> iface.vars |> List.filter (fun d -> Declarations.is_var_input d.kind)
        
        let vars_list (p : Pous.PouAst) = 
            match p.iface with
            | None -> []
            | Some iface -> iface.vars
        
        let module_types_scope (p : ProjectTypesAst) : Module_types_scope = 
            let filterPouType t = Seq.filter (fun (pou : PouAst) -> pou.typ = t)
            
            let pouTypeScope (pou : PouAst) = 
                let pou = add_function_return_value pou
                (pou.name, 
                 { vars = pou_types_scope pou
                   vars_in = vars_in pou
                   vars_list = vars_list pou
                   ast = pou })
            { simple = 
                  p.dataTypes
                  |> Seq.map (fun dt -> dt.name, dt)
                  |> Map.ofSeq
              pouFunctionBlock = 
                  filterPouType FUNCTION_BLOCK p.pous
                  |> Seq.map pouTypeScope
                  |> Map.ofSeq
              pouFunction = 
                  filterPouType FUNCTION p.pous
                  |> Seq.map pouTypeScope
                  |> Map.ofSeq
              pouProgram = 
                  filterPouType PROGRAM p.pous
                  |> Seq.map pouTypeScope
                  |> Map.ofSeq }
        
        let project_types_scope (p : ProjectAst) : Project_types_scope = 
            { user = module_types_scope p.types
              library = Seq.map (fun (id, pt) -> id, module_types_scope pt) p.libraries |> Map.ofSeq }
        
        let ensure_named_gvl = 
            function 
            | Some n -> n
            | None -> "__GLOBAL__"
        
        let module_values_scope (r : Resource) : Module_values_scope = 
            { gvl = 
                  r.vars
                  |> Seq.map (fun db -> ensure_named_gvl db.name, declaration_block_scope db.vars)
                  |> Map.ofSeq
              pouInstance = r.pouInstances |> Map.ofSeq }
        
        let project_values_scope (p : ProjectAst) : Project_values_scope = 
            let c1 = p.configurations.Head
            let r1 = c1.resources.Head
            { library = Map.empty
              user = module_values_scope r1 }
        
        let project_scope (ast : ProjectAst) : Project_scope = 
            { types = project_types_scope ast
              values = project_values_scope ast }
