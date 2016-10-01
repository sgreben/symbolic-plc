namespace FsPlcModel

module Pous = 
    type PouType = 
        | FUNCTION
        | FUNCTION_BLOCK
        | PROGRAM
    
    type PouInterface = 
        { returnType : Types.Type option
          vars : Declarations.Declaration_block }
    
    type BodyAst = 
        | AstST of StStatements.BlockAst
        (*| AstIL of IlStatements.Program<Common.Identifier,StExpressions.ExpressionAst,StExpressions.VariableAst>*)
    
    type PouActionAst = 
        { name : Common.Identifier
          body : BodyAst list }
    
    type PouTransitionAst = 
        { name : Common.Identifier
          body : BodyAst list }
    
    type PouAst = 
        { name : Common.Identifier
          typ : PouType
          iface : PouInterface option
          actions : PouActionAst list
          transitions : PouTransitionAst list
          bodies : BodyAst list }