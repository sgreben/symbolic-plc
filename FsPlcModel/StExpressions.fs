namespace FsPlcModel

module StExpressions = 
    type Unop = 
        | Negation
        | Complement
    
    type Binop = 
        | Exp
        | Mul
        | Div
        | Mod
        | Add
        | Sub
        | And
        | Or
        | Xor
    
    type Compop = 
        | Lt
        | Gt
        | Leq
        | Geq
        | Eq
        | NotEq
    
    type VariableAst = 
        | AstDirect of Common.Identifier
        | AstDot of VariableAst * Common.Identifier
        | AstIndex of VariableAst * IndexAst
    
    and IndexAst = ExpressionAst * ExpressionAst list
    
    and ExpressionAst = 
        | AstLiteral of Values.Value
        | AstVariable of VariableAst
        | AstFunction of VariableAst * AstFunctionParameter list
        | AstUnop of Unop * ExpressionAst
        | AstBinop of Binop * ExpressionAst * ExpressionAst
        | AstCmp of Compop * ExpressionAst * ExpressionAst
    
    and AstFunctionParameter = 
        | AstAnonymous of ExpressionAst
        | AstNamed of Common.Identifier * ExpressionAst
        | AstNamedOutput of Common.Identifier * VariableAst
        | AstNamedOutputNot of Common.Identifier * VariableAst