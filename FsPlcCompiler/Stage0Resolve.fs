namespace FsPlcCompiler.Stage0

(* 
 Problem: 
    The compiler encounters an identifier "MyVar.X". 
    Which piece of data in the project does this identifier refer to?
 Solution: 
    Given a data structure representing a hierarchy of definition scopes (Project_scope),
    compute an unambiguous description (Value_path/Type_path) of the access path to the 
    given piece of data. 
    
    If "MyVar" is a global variable in the GVL "MyGvl":
    > Global_instance ((User, Gvl("MyGvl","MyVar")), [ Field "X" ])
    If "MyVar" is a POU variable in the globally-declared POU instance "MyPou":
    > Global_instance ((User, Pou_instance("MyPou"), [ Field "MyVar"; Field "X" ])
    If "MyVar" is a local variable of a pou "ThePou" defined in a library "PouLib":
    > Local_instance ((Library "PouLib","ThePou"),[ Field "MyVar"; Field "X" ])
*)

///  Scope resolution for types and values
module Resolve = 
    open FsPlcModel
    open Declarations
    open Scope
    open Access
    open StExpressions
    
    exception Invalid_type_reference of Common.Qualified_identifier
    
    exception Invalid_structure_access of Generalized_type * Access_structure
    
    type Project_types_scope_with_focus = Project_types_scope * Access_project
    
    type Project_scope_with_focus = Project_scope * Access_project
    
    let project_value_access (prj_val_scope : Project_values_scope) = 
        function 
        | User -> Some prj_val_scope.user
        | Library id -> prj_val_scope.library.TryFind id
    
    let project_type_access (prj_types_scope : Project_types_scope) = 
        function 
        | User -> Some prj_types_scope.user
        | Library id -> prj_types_scope.library.TryFind id
    
    let pou_field_access id (p:Project_types_scope) (pou_type_path : Pou_type_path) = 
        Lookup.project_pou_type p pou_type_path 
        |> Option.bind (fun pou -> pou.TryFind id) 
        |> Option.bind (fun d -> Some d.typ)

    let struct_field_access id (s : Types.Struct) = 
        List.tryFind (fun (id', _) -> id' = id) s |> Option.bind (fun (_, t) -> Some t)
    let array_index_access id ((_, t) : Types.Array) = t
    
    (* type scope resolution stack      Example
       (desc. precedence)
       ------------------------         ----------------
       user code (project)              MyDerivedType
       library (qualified)              Visu_Elems.VisuElemsDerivedType
       library (unqualified)            VisuElemsDerivedType *)

    let module_type (scope : Project_types_scope_with_focus) id = 
        let (prj_scope, pa) = scope
        let mtns = (project_type_access prj_scope pa).Value
        let read_alias = Option.bind (fun (dt : Projects.Data_type) -> Some dt.typ)
        let with_type_access_path t = Option.bind (fun v -> Some(t, v))
        let as_simple_type = with_type_access_path (Some(pa, id)) >> Option.bind (Simple_type >> Some)
        let scope_only = Option.bind (fun pou -> Some pou.vars)
        let as_pou_type = with_type_access_path (pa, id) >> Option.bind (fun (t,_) -> Some (Pou_type t))
        (mtns.simple.TryFind id
         |> read_alias
         |> as_simple_type)
        <|> fun () -> 
            (mtns.pouFunctionBlock.TryFind id |> scope_only |> as_pou_type) 
            <|> fun () -> 
                (mtns.pouProgram.TryFind id |> scope_only |> as_pou_type) <|> fun () -> (mtns.pouFunction.TryFind id |> scope_only |> as_pou_type)
    
    let library_type_unqualified (prj_scope : Project_types_scope) id = 
        prj_scope.library |> Seq.tryPick (fun e -> module_type (prj_scope, Library e.Key) id)
    let library_type_qualified (prj_scope : Project_types_scope) id id' = 
        prj_scope.library.TryFind id |> Option.bind (fun _ -> module_type (prj_scope, Library id) id')
    
    let resolve_type (scope : Project_types_scope_with_focus) t = 
        let (prj_scope, pa) = scope
        match t with
        | Types.TYPE_REFERENCE tr -> 
            match tr with
            | [ id ] -> 
                match module_type (prj_scope, pa) id with
                | Some t -> Some t
                | None -> library_type_unqualified prj_scope id
            | [ id; id' ] -> library_type_qualified prj_scope id id'
            | _ -> raise (Invalid_type_reference tr)
        | t -> Some(Simple_type(None, t))
    let resolve_project_type scope t =
        resolve_type (scope,User) t <|> 
        fun () -> scope.library |> Map.toSeq |> Seq.tryPick (fun (lib,_) -> resolve_type (scope,Library lib) t)

    let get_simple_type = function
        | Simple_type (_,t) -> t
    
    let resolve_generalized_type scope t = 
        match t with
        | Pou_type _ -> Some t
        | Simple_type(_, t) -> resolve_type scope t
    
    let rec structure_access' (scope : Project_types_scope_with_focus) (s : Generalized_type, sa : Access_structure) = 
        let resolve = resolve_type scope
        match s, sa with
        | Pou_type(pou_type_path), Field (_,id) ->  pou_field_access id (fst scope) pou_type_path |> Option.bind resolve
        | Simple_type(_, Types.STRUCT s), Field (_,id) -> struct_field_access id s |> Option.bind resolve
        | Simple_type(_, Types.ARRAY(_, t)), Index _ -> resolve t
        | Simple_type(_, Types.TYPE_REFERENCE tr), sa -> 
            resolve (Types.TYPE_REFERENCE tr) |> Option.bind (fun t -> structure_access' scope (t, sa))
        | t, sa -> raise (Invalid_structure_access(t, sa))
    
    let structure_access (scope : Project_types_scope_with_focus) (sa : Access_structure) (s : Generalized_type, value_path) = 
        let (prj_scope, pa) = scope
        try 
            let s = structure_access' (prj_scope, pa) (s, sa)
            let value_path = value_path_structure_access value_path sa
            Option.map (fun s -> s,value_path) s
        with Access.Invalid_structure_access -> None
    
    (* value scope resolution stack     Example
       (desc. precedence)
       ------------------------         ----------------
       builtin                          ADD
       pou local vars                   My_PouInputVar
       module gvl (qualified)           My_GVL.My_Global_Var
       module gvl (unqualified)         My_Global_Var
       module pou instance              MainPou
       module function                  MY_FUNCTION
       library gvl (qualified)          Visu_Elems.GVL_Const.RED
       library function (qualified)     Visu_Elems.LIBRARY_FUNCTION
       library gvl (unqualified)        GVL_Const.RED
       library function (unqualified)   LIBRARY_FUNCTION *)

    let builtin_map = Map.ofList [ "ADD", IL.EXTOP IL.ADD_EXT
                                   "MUL", IL.EXTOP IL.MUL_EXT
                                   "MIN", IL.EXTOP IL.MIN
                                   "MAX", IL.EXTOP IL.MAX ]
    let builtin_function id = 
        builtin_map.TryFind id |> Option.bind (fun builtin -> 
            Some(Builtin_function_type builtin, Function(Builtin builtin)))

    // pou local vars
    let pou_local (prj_scope : Project_scope, pou_type_path : Pou_type_path) id = 
        let (pa, _) = pou_type_path
        Lookup.project_pou_ast prj_scope.types pou_type_path 
        |> Option.bind (fun pou -> 
            pou.iface.Value.vars
            |> List.tryFind (fun d -> d.id = id)
            |> Option.bind (fun d -> Some (Simple_type(None, d.typ), Local_instance ((pa, pou.name), [ Field (Pou_type pou_type_path, id) ]))))
    
    // module gvl (qualified)
    let module_gvl_qualified (scope : Project_scope_with_focus) id id' = 
        let (prj_scope, pa) = scope
        let mvns = (project_value_access prj_scope.values pa).Value
        let tap = (pa, id)
        mvns.gvl.TryFind id
        |> Option.bind (fun d -> d.TryFind id')
        |> Option.bind (fun d -> Some(Simple_type(None, d.typ), Global_instance((pa, Gvl(id, id')), [])))
    
    // module gvl (unqualified)
    let module_gvl_unqualified (scope : Project_scope_with_focus) id = 
        let (prj_scope, pa) = scope
        let mvns = (project_value_access prj_scope.values pa).Value
        match mvns.gvl |> Seq.tryPick (fun e -> 
                              e.Value
                              |> Map.tryFind id
                              |> Option.bind (fun d -> Some(e.Key, d))) with
        | Some(gvlid, d) -> Some(Simple_type(None, d.typ), Global_instance((pa, Gvl(gvlid, id)), []))
        | None -> None
    
    // module pou instance
    let module_pou_instance (scope : Project_scope_with_focus) id = 
        let (prj_scope, pa) = scope
        let mvns = (project_value_access prj_scope.values pa).Value
        mvns.pouInstance.TryFind id
        |> Option.bind (Types.TYPE_REFERENCE >> resolve_type (prj_scope.types, pa))
        |> Option.bind (function 
               | Pou_type(tap, pou) -> Some(Pou_type(tap, pou), Global_instance((pa, Pou_instance id), []))
               | _ -> None)
    
    // module function
    let module_function (scope : Project_scope_with_focus) id = 
        let (prj_scope, pa) = scope
        let mtns = (project_type_access prj_scope.types pa).Value
        mtns.pouFunction.TryFind id |> Option.bind (fun f -> Some(Pou_type((pa, id)), Function((Project_function(pa, id)))))
    
    // library gvl (qualified)
    let library_gvl_qualified (prj_scope : Project_scope) id id' = 
        prj_scope.values.library.TryFind id |> Option.bind (fun d -> module_gvl_unqualified (prj_scope, Library id) id')
    // library gvl (unqualified)
    let library_gvl_unqualified (prj_scope : Project_scope) id = 
        prj_scope.values.library |> Seq.tryPick (fun e -> module_gvl_unqualified (prj_scope, Library e.Key) id)
    // library function (qualified)
    let library_function_qualified (prj_scope : Project_scope) id id' = 
        prj_scope.values.library.TryFind id |> Option.bind (fun d -> module_function (prj_scope, Library id) id')
    // library function (unqualified)
    let library_function_unqualified (prj_scope : Project_scope) id = 
        prj_scope.values.library |> Seq.tryPick (fun e -> module_function (prj_scope, Library e.Key) id)
    
    let resolve_variable (prj_scope : Project_scope, pou_type_path : Pou_type_path) (v : VariableAst) = 
        let pa, mta = pou_type_path
        let prj_scope_focus = prj_scope, pa
        let prj_types_scope_focus = prj_scope.types, pa
        let field_access id = Option.bind (fun (t,v) -> structure_access prj_types_scope_focus (Field (t,id)) (t,v))
        let index_access idx = Option.bind (structure_access prj_types_scope_focus (Index idx))
        
        let rec loop = 
            function 
            | AstDirect id -> 
                pou_local (prj_scope, pou_type_path) id <|> fun () -> 
                builtin_function id <|> fun () ->
                module_gvl_unqualified prj_scope_focus id <|> fun () -> 
                module_pou_instance prj_scope_focus id  <|> fun () -> 
                module_function prj_scope_focus id <|> fun () -> 
                library_gvl_unqualified prj_scope id  <|> fun () -> 
                library_function_unqualified prj_scope id
            | AstDot(AstDirect id, id') -> 
                (pou_local (prj_scope, pou_type_path) id |> field_access id') <|> fun () -> 
                (module_gvl_qualified prj_scope_focus id id' |> field_access id') <|> fun () -> 
                library_function_qualified prj_scope id id' <|> fun () -> 
                loop (AstDirect id) |> field_access id'
            | AstDot(v, id) -> loop v |> field_access id
        /// FIXME: resolve array accesses
            //| AstIndex(v, idx) -> loop v |> index_access idx
        loop v
        