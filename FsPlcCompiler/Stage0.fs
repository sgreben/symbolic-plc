namespace FsPlcCompiler.Stage0

module Typecheck = 
    open Language.Expression
    open Scope
    open Access

module Project =         
    open FsPlcModel.Pous
    type Pou_body = ST of Language.Statement.Block
    type Pou = {
        name : Access.Pou_type_path
        typ : PouType
        returnType : FsPlcModel.Types.Type option
        vars : FsPlcModel.Declarations.Declaration_block
        bodies : Pou_body list
    }
    open FsPlcModel.Projects
    type Project_types = { 
        dataTypes : Data_type list
        pous : Pou list 
    }
     type Task = {
        name:FsPlcModel.Common.Identifier
        interval : int64
        priority : int
        pous : (Access.Pou_instance_path*Access.Pou_type_path) list
    }
    type Project = {
        types : Project_types
        libraries : (FsPlcModel.Common.Identifier * Project_types) list
        tasks : Task list
    }

module Compile = 
    module Expression = 
        open FsPlcModel.StExpressions
        open Language.Expression
        open Scope
        open Access
        
        exception Undefined_variable of VariableAst
        
        exception Undefined_function of VariableAst
        
        exception Not_a_function of VariableAst
        
        let compile_variable (prj_scope : Project_scope, pou_type_path : Pou_type_path) (v : VariableAst) = 
            match Resolve.resolve_variable (prj_scope, pou_type_path) v with
            | Some(t, v) -> t,v
            | None -> raise (Undefined_variable v)
        
        let compile_literal _ l = l
        
        let rec compile_function_arg scope = 
            function 
            | AstAnonymous e -> Anonymous(compile scope e)
            | AstNamed(id, e) -> Named(id, compile scope e)
            | AstNamedOutput(id, v) -> NamedOutput(id, compile_variable scope v |> snd)
            | AstNamedOutputNot(id, v) -> NamedOutputNot(id, compile_variable scope v |> snd)
        
        and compile_function_args scope = List.map (compile_function_arg scope)
        
        and compile_function_call scope (v, args) = 
            match Resolve.resolve_variable scope v with
            | Some(_, Access.Function f) -> Expression_body.Function(f, compile_function_args scope args)
            | Some(_, _) -> raise (Not_a_function v)
            | None -> raise (Undefined_function v)
        
        and compile_unop scope (unop, e) = Unop(unop, compile scope e)
        
        and compile_binop scope (binop, e, e') = Binop(binop, compile scope e, compile scope e')
        
        and compile_cmp scope (cmpop, e, e') = Cmp(cmpop, compile scope e, compile scope e')
        
        and compile scope = 
            function 
            | AstLiteral l -> Literal(compile_literal scope l)
            | AstVariable v -> Variable(compile_variable scope v |> snd)
            | AstFunction(f, args) -> compile_function_call scope (f, args)
            | AstUnop(unop, e) -> compile_unop scope (unop, e)
            | AstBinop(binop, e, e') -> compile_binop scope (binop, e, e')
            | AstCmp(cmpop, e, e') -> compile_cmp scope (cmpop, e, e')
    
    module Statement = 
        open FsPlcModel.StStatements
        open FsPlcModel.StExpressions
        open Language.Statement
        open Scope
        open Access
        
        let compile_assignment scope (v, e) = 
            Assignment(Expression.compile_variable scope v |> snd, Expression.compile scope e)

        exception Not_a_function_block
        let compile_call scope (v, args) = 
            let t,v = Expression.compile_variable scope v
            let args = Expression.compile_function_args scope args
            match v with 
            | Function fun_type ->
                Call_fun(fun_type,args)
            | _ -> match t with
                   | Pou_type pou_type -> Call_fb(pou_type,v,args)
                   | _ -> raise Not_a_function_block
        
        let rec compile_if scope (_if, _then, _else) = 
            If(Expression.compile scope _if, compile_block scope _then, compile_block scope _else)
        
        and compile_cases scope = List.map (fun (case, b) -> case, compile_block scope b)
        
        and compile_case scope (e, cases, _default) = 
            Case(Expression.compile scope e, compile_cases scope cases, compile_block scope _default)
        
        and compile_dowhile scope (_while, _do) = DoWhile(Expression.compile scope _while, compile_block scope _do)
        
        and compile_repeatuntil scope (_until, _repeat) = 
            RepeatUntil(Expression.compile scope _until, compile_block scope _repeat)
        
        and compile_block scope = List.map (fun (src, stmt) -> (src, compile scope stmt))
        
        and compile_for scope (v, _init, _to, _by, _do) = 
            let e = Expression.compile scope
            For(Expression.compile_variable scope v |> snd, e _init, e _to, Option.map e _by, compile_block scope _do)
        
        and compile scope = 
            function 
            | AstAssignment(v, e) -> compile_assignment scope (v, e)
            | AstCall(v, args) -> compile_call scope (v, args)
            | AstIf(c, t, e) -> compile_if scope (c, t, e)
            | AstCase(e, cs, d) -> compile_case scope (e, cs, d)
            | AstDoWhile(c, b) -> compile_dowhile scope (c, b)
            | AstRepeatUntil(c, b) -> compile_repeatuntil scope (c, b)
            | AstFor(v, i, t, d, b) -> compile_for scope (v, i, t, d, b)
            | AstExit -> Exit
            | AstReturn -> Return

    module Pou =
        open FsPlcModel.Pous
        open Project
        let compile_body scope = function
        | AstST body -> ST (Statement.compile_block scope body)

        let add_function_return_value (p:PouAst) =
            match p.typ with
            | FUNCTION ->
                let ret_typ = p.iface.Value.returnType.Value
                let ret_decl = FsPlcModel.Declarations.var_output p.name ret_typ
                [ret_decl]
            | _ -> []
        
        let compile scope (pou : PouAst) = 
            let (_, name) = scope
            let returnType, vars = 
                match pou.iface with
                | None -> None, []
                | Some iface -> iface.returnType, iface.vars
            let bodies = List.map (compile_body scope) pou.bodies
            let ret_decl = add_function_return_value pou
            { name = name
              vars = vars@ret_decl
              typ = pou.typ
              returnType = returnType
              bodies = List.rev bodies }

            
    module Project =
        open FsPlcModel.Projects
        open Project
            
        let compile_pou scope access_project (pou:FsPlcModel.Pous.PouAst) =
            let scope = (scope,(access_project,pou.name))
            Pou.compile scope pou
        
        let compile_project_types scope access_project (p : ProjectTypesAst) = 
            { dataTypes = p.dataTypes
              pous = p.pous |> List.map (compile_pou scope access_project)}
        let compile_tasks scope (p:ProjectAst) =
            let configuration = p.configurations.Head
            let resource = configuration.resources.Head
            resource.tasks |> List.map (fun task -> 
                let task_prio = FsPlcModel.Tasks.priority task |> Option.get
                let task_interval = match FsPlcModel.Tasks.interval task with
                    | Some (FsPlcModel.Tasks.Fixed t) -> t
                let task_pous :  (Access.Pou_instance_path*Access.Pou_type_path) list = task.pou |> List.map (function
                    | id,[pou_id] -> (Access.User,id),(Access.User,pou_id)
                    | id,[lib;pou_id] -> (Access.User,id),(Access.Library lib,pou_id))
                { name = task.name
                  interval = (int64) task_interval
                  priority = task_prio
                  pous = task_pous })
            
        let compile (p : ProjectAst) = 
            let scope = Scope.Build.project_scope p
            let libraries = p.libraries |> List.map (fun (id, pt) -> (id, compile_project_types scope (Access.Library id) pt))
            let tasks = compile_tasks scope p
            { types = compile_project_types scope Access.User p.types
              libraries = libraries
              tasks = tasks }