namespace FsPlcCompiler.Stage0

module Lookup = 
    open Access
    open Scope
    
    exception Library_not_found of string
    
    /// FIXME: dup
    let (<|>) l r = 
        match l with
        | Some _ -> l
        | None -> r()
    
    let module_pou_ast (m : Module_types_scope) id = 
        m.pouFunction.TryFind id <|> (fun () -> m.pouFunctionBlock.TryFind id) <|> (fun () -> m.pouProgram.TryFind id) 
        |> Option.bind (fun pou_scope -> Some pou_scope.ast)
    
    let project_pou_ast (p : Project_types_scope) (ap : Access_project, id) = 
        match ap with
        | User -> module_pou_ast p.user id
        | Library lib -> p.library.TryFind lib |> Option.bind (fun s -> module_pou_ast s id)

    let module_pou_type (m : Module_types_scope) id = 
        m.pouFunction.TryFind id <|> (fun () -> m.pouFunctionBlock.TryFind id) <|> (fun () -> m.pouProgram.TryFind id) 
        |> Option.bind (fun pou_scope -> Some pou_scope.vars)
    
    let project_pou_type (p:Project_types_scope) (ap:Access_project, id) = 
        match ap with
        | User -> module_pou_type p.user id
        | Library lib -> p.library.TryFind lib |> Option.bind (fun s -> module_pou_type s id)
    
    let module_simple_type (m : Module_types_scope) id = m.simple.TryFind id
    
    let project_simple_type (p : Project_types_scope) (ap : Access_project, id) = 
        match ap with
        | User -> module_simple_type p.user id
        | Library lib -> p.library.TryFind lib |> Option.bind (fun s -> module_simple_type s id)
    
    let project_module_types (p : Project_scope) (ap : Access_project) = 
        match ap with
        | User -> p.types.user
        | Library lib -> 
            match p.types.library.TryFind lib with
            | Some lib -> lib
            | None -> raise (Library_not_found lib)
