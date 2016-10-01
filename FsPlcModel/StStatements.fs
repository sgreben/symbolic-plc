namespace FsPlcModel

module StStatements = 
    open StExpressions
    
    type Case = 
        | CaseValues of int64 list
        | CaseRange of int64 * int64
    
    type StatementAst = 
        | AstAssignment of VariableAst * ExpressionAst
        | AstCall of VariableAst * AstFunctionParameter list
        | AstIf of ExpressionAst * (*then*) BlockAst * (*else*) BlockAst
        | AstCase of ExpressionAst * (Case * BlockAst) list * (*default*) BlockAst
        | AstDoWhile of ExpressionAst * BlockAst
        | AstRepeatUntil of ExpressionAst * BlockAst
        | AstFor of VariableAst * (*:=*) ExpressionAst * (*to*) ExpressionAst * (*by*) (ExpressionAst option) * BlockAst
        | AstExit
        | AstReturn
    
    and BlockAst = (StatementAstSource*StatementAst) list
   