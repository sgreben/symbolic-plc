namespace FsPlcModel

module Declarations = 
    type Kind = 
        | VAR
        | VAR_INPUT
        | VAR_OUTPUT
        | VAR_IN_OUT
        | VAR_EXTERNAL
        | VAR_GLOBAL
        | VAR_ACCESS
        | VAR_TEMP
    
    let is_var_input = 
        function 
        | VAR_INPUT -> true
        | VAR_IN_OUT -> true
        | _ -> false
    
    let is_var_output = 
        function 
        | VAR_OUTPUT -> true
        | VAR_IN_OUT -> true
        | _ -> false
    
    type Attribute = 
        | RETAIN
        | NON_RETAIN
        | CONSTANT
        | R_EDGE
        | F_EDGE
        | READ_ONLY
        | READ_WRITE
    
    type Declaration = 
        { kind : Kind
          attr : Attribute list
          id : Common.Identifier
          typ : Types.Type
          ivalue : Values.Value option }
    
    type Declaration_block = Declaration list
    
    type Declaration_block_named = 
        { name : Common.Identifier option
          vars : Declaration_block }
    
    type Declarations = 
        { var : Declaration_block
          var_input : Declaration_block
          var_output : Declaration_block
          var_in_out : Declaration_block
          var_external : Declaration_block
          var_global : Declaration_block
          var_access : Declaration_block
          var_temp : Declaration_block }
    
    let declaration_block_empty = []
    
    let declaration_block_named_empty = 
        { name = None
          vars = declaration_block_empty }
    
    let declarations_empty = 
        { var = declaration_block_empty
          var_input = declaration_block_empty
          var_output = declaration_block_empty
          var_in_out = declaration_block_empty
          var_external = declaration_block_empty
          var_global = declaration_block_empty
          var_access = declaration_block_empty
          var_temp = declaration_block_empty }
    
    let declarations = 
        List.fold (fun ds d -> 
            match d.kind with
            | VAR -> { ds with var = d :: ds.var }
            | VAR_INPUT -> { ds with var_input = d :: ds.var_input }
            | VAR_OUTPUT -> { ds with var_output = d :: ds.var_output }
            | VAR_IN_OUT -> { ds with var_in_out = d :: ds.var_in_out }
            | VAR_EXTERNAL -> { ds with var_external = d :: ds.var_external }
            | VAR_GLOBAL -> { ds with var_global = d :: ds.var_global }
            | VAR_ACCESS -> { ds with var_access = d :: ds.var_access }
            | VAR_TEMP -> { ds with var_temp = d :: ds.var_temp }) declarations_empty
    
    let var id typ = 
        { kind = VAR
          attr = []
          id = id
          typ = typ
          ivalue = None }
    
    let var_local = var
    let var_temp id typ = { var id typ with kind = VAR_TEMP }
    let var_input id typ = { var id typ with kind = VAR_INPUT }
    let var_output id typ = { var id typ with kind = VAR_OUTPUT }
    let var_in_out id typ = { var id typ with kind = VAR_IN_OUT }
    let var_global id typ = { var id typ with kind = VAR_GLOBAL }
