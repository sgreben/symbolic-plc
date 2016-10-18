namespace FsPlcParser
open FsPlcModel

module StExpressionsParser = 
    open Common.Parsers
    open FParsec
    open FParsec.Primitives
    open StExpressions
    
    let index, indexRef = createParserForwardedToRef<IndexAst, unit>()
    
    let variable : Parser<VariableAst, unit> = 
        let rec variable' v = 
            choice [ dot >>. ident >>= fun i -> variable' (AstDot(v, i))
                     brackets index >>= fun idx -> variable' (AstIndex(v, idx))
                     preturn v ]
        ident .>> ws |>> AstDirect
        >>= variable'
        .>> notFollowedBy (pstring "#")
        <!> "variable"
    
    let expression, expressionRef = createParserForwardedToRef<ExpressionAst, unit>()
    let named_input_param = ident .>> ws .>> skipString ":=" .>> ws .>>. expression
    let named_output_param = ident .>> ws .>> skipString "=>" .>> ws .>>. variable
    
    let named_param = 
        choice [ attempt named_input_param |>> AstNamed
                 keyword "not" >>. named_output_param |>> AstNamedOutputNot
                 named_output_param |>> AstNamedOutput ]
    
    let function_param = 
        choice [ attempt named_param
                 expression |>> AstAnonymous ]
    
    let function_params = sepBy function_param (comma >>. ws)
    let function_call = variable .>>. parens function_params <!> "function call"
    let literal = ValuesParser.anyValue <!> "literal"
    
    let atom = 
        choice [ attempt function_call |>> AstFunction
                 attempt variable |>> AstVariable
                 literal |>> AstLiteral ]
        <!> "atom"
    
    let aexpOpp = new OperatorPrecedenceParser<ExpressionAst, unit, unit>()
    let aexp = aexpOpp.ExpressionParser <!> "aexp"
    
    do indexRef := sepBy1 aexp (comma >>. ws) |>> fun xs -> (xs.Head, xs.Tail)
    aexpOpp.TermParser <- choice [ atom <!> "aexp.atom"
                                   parens aexp <!> "aexp.parens" ]
    aexpOpp.AddOperator(InfixOperator("-", ws, 6, Associativity.Left, fun x y -> AstBinop(Sub, x, y)))
    aexpOpp.AddOperator(InfixOperator("+", ws, 7, Associativity.Left, fun x y -> AstBinop(Add, x, y)))
    aexpOpp.AddOperator
        (InfixOperator("MOD", notFollowedBy letter >>. ws, 8, Associativity.Left, fun x y -> AstBinop(Exp, x, y)))
    aexpOpp.AddOperator(InfixOperator("/", ws, 9, Associativity.Left, fun x y -> AstBinop(Div, x, y)))
    aexpOpp.AddOperator(InfixOperator("*", ws, 10, Associativity.Left, fun x y -> AstBinop(Mul, x, y)))
    aexpOpp.AddOperator(InfixOperator("**", ws, 11, Associativity.Left, fun x y -> AstBinop(Exp, x, y)))
    aexpOpp.AddOperator(PrefixOperator("-", ws, 13, true, fun x -> AstUnop(Negation, x)))
    
    let cmpop = 
        choice [ keyword "=" |>> fun _ -> Eq
                 keyword "<>" |>> fun _ -> NotEq
                 keyword "<=" |>> fun _ -> Leq
                 keyword "<" |>> fun _ -> Lt
                 keyword ">=" |>> fun _ -> Geq
                 keyword ">" |>> fun _ -> Gt ]
        <!> "cmpop"
    
    let cmpexp = (aexp .>> ws .>>. cmpop .>>. aexp .>> ws |>> fun ((a, c), b) -> AstCmp(c, a, b)) <!> "cmpexp"
    let bexpOpp = new OperatorPrecedenceParser<ExpressionAst, unit, unit>()
    let bexp = bexpOpp.ExpressionParser <!> "bexp"
    
    bexpOpp.TermParser <- choice [ attempt cmpexp <!> "bexp.cmpexp"
                                   attempt atom <!> "bexp.atom"
                                   parens bexp <!> "bexp.parens" ]
    bexpOpp.AddOperator
        (InfixOperator("OR", notFollowedBy letter >>. ws, 1, Associativity.Left, fun x y -> AstBinop(Or, x, y)))
    bexpOpp.AddOperator
        (InfixOperator("XOR", notFollowedBy letter >>. ws, 2, Associativity.Left, fun x y -> AstBinop(Xor, x, y)))
    bexpOpp.AddOperator
        (InfixOperator("AND", notFollowedBy letter >>. ws, 3, Associativity.Left, fun x y -> AstBinop(And, x, y)))
    bexpOpp.AddOperator(InfixOperator("&", ws, 3, Associativity.Left, fun x y -> AstBinop(And, x, y)))
    bexpOpp.AddOperator(InfixOperator("=", ws, 4, Associativity.None, fun x y -> AstCmp(Eq, x, y)))
    bexpOpp.AddOperator(PrefixOperator("NOT", notFollowedBy letter >>. ws, 12, true, fun x -> AstUnop(Complement, x)))
    do expressionRef := choice [ attempt bexp
                                 aexp ]
    
    let expression_eof = 
        choice [ attempt (bexp .>> eof)
                 aexp .>> eof ]
    
    let expression_semi = 
        choice [ attempt (bexp .>> semicolon)
                 aexp .>> semicolon ]
    
    let parse = Common.Parsers.parse
    let parse_all p i = Common.Parsers.parse (p .>> eof) i