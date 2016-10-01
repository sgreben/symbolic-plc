namespace FsPlcCompiler.Stage0

module Language = 
    module Expression = 
        open FsPlcModel
        
        type Unop = StExpressions.Unop
        
        type Binop = StExpressions.Binop
        
        type Compop = StExpressions.Compop
        
        type Variable = Access.Value_path
        
        and Expression_body = 
            | Literal of Values.Value
            | Variable of Variable
            | Function of Access.Function_type_path * Function_arg list
            | Unop of Unop * Expression
            | Binop of Binop * Expression * Expression
            | Cmp of Compop * Expression * Expression

        and Expression = Expression_body
        
        and Function_arg = 
            | Anonymous of Expression
            | Named of Common.Identifier * Expression
            | NamedOutput of Common.Identifier * Variable
            | NamedOutputNot of Common.Identifier * Variable
    
    module Statement = 
        open Expression
        
        type Case = FsPlcModel.StStatements.Case
        
        type Statement = 
            | Assignment of Variable * Expression
            | Call_fb of Access.Pou_type_path * Variable * Function_arg list
            | Call_fun of Access.Function_type_path * Function_arg list
            | If of Expression * (*then*) Block * (*else*) Block
            | Case of Expression * (Case * Block) list * (*default*) Block
            | DoWhile of Expression * Block
            | RepeatUntil of Expression * Block
            | For of Variable * (*:=*) Expression * (*to*) Expression * (*by*) Expression option * Block
            | Exit
            | Return
        
        and Block = (FsPlcModel.StatementAstSource * Statement) list

