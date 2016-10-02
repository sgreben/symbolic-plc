namespace FsPlcCompiler.Stage1

open FsPlcModel.IL
open FsPlcModel

module Language = 
    open FsPlcCompiler
    
    module Access = Stage0.Access
    module Repr = FsPlcVm.Representation
    module Vm = FsPlcVm.Language
    
    type Label_name = string
    
    type Operator = Vm.Operator
    
    type Statement = 
        | LABEL of Label_name
        | SJMP of Jump_op * Label_name
        | SCALL of Access.Pou_type_path * Repr.Register
        | SFUN of Access.Function_type_path * Repr.Memory_address
        | OP of Operator
        | BLOCK of Block
    
    and Block = StatementAstSource * Statement list
    
    let code_length is = 
        let rec code_length' = 
            function 
            | BLOCK(_, b) -> List.map code_length' b |> List.sum
            | _ -> 1
        List.map code_length' is |> List.sum