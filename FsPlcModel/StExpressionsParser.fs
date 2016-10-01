namespace FsPlcModel

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
        ident .>> ws |>> AstDirect >>= variable' .>> notFollowedBy (pstring "#") <!> "variable"
    
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
    
    do indexRef := sepBy1 aexp (comma >>. ws) |>> fun (x :: xs) -> (x, xs)
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

module StExpressionsParserTest = 
    open FsUnit
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    open StExpressions
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type StVariableExpressionTest() = 
        let parse = StExpressionsParser.parse StExpressionsParser.variable
        let b yte = AstLiteral(Values.BASIC(Values.BYTE yte))
        
        [<Test>]
        member x.``can parse My_Var``() = 
            let input = """My_Var"""
            let expected = AstDirect "My_Var"
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse My_Var.My_Field``() = 
            let input = """My_Var.My_Field"""
            let expected = AstDot(AstDirect "My_Var", "My_Field")
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse My_Var.My_Field[1,2,3]``() = 
            let input = """My_Var.My_Field[1,2,3]"""
            
            let expected = 
                AstIndex(AstDot(AstDirect "My_Var", "My_Field"), 
                         (b 1uy, 
                          [ b 2uy
                            b 3uy ]))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse My_Var.My_Field[1,2,3].Other_Field``() = 
            let input = """My_Var.My_Field[1,2,3].Other_Field"""
            
            let expected = 
                AstDot(AstIndex(AstDot(AstDirect "My_Var", "My_Field"), 
                                (b 1uy, 
                                 [ b 2uy
                                   b 3uy ])), "Other_Field")
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type StFunctionExpressionTest() = 
        let parse = StExpressionsParser.parse StExpressionsParser.function_call
        
        [<Test>]
        member x.``can parse My_Fun(First, Second, Third, 4)``() = 
            let input = """My_Fun(First, Second, Third, 4)"""
            
            let expected = 
                (AstDirect "My_Fun", 
                 [ AstAnonymous(AstVariable(AstDirect "First"))
                   AstAnonymous(AstVariable(AstDirect "Second"))
                   AstAnonymous(AstVariable(AstDirect "Third"))
                   AstAnonymous(AstLiteral(Values.BASIC(Values.BYTE 4uy))) ])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse My_Fun(First:=1, Second:=2, Third:=3, Fourth:=4)``() = 
            let input = """My_Fun(First:=1, Second:=2, Third:=3, Fourth:=4)"""
            
            let expected = 
                (AstDirect "My_Fun", 
                 [ AstNamed("First", AstLiteral(Values.BASIC(Values.BYTE 1uy)))
                   AstNamed("Second", AstLiteral(Values.BASIC(Values.BYTE 2uy)))
                   AstNamed("Third", AstLiteral(Values.BASIC(Values.BYTE 3uy)))
                   AstNamed("Fourth", AstLiteral(Values.BASIC(Values.BYTE 4uy))) ])
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type StExpressionTest() = 
        let parse = StExpressionsParser.parse StExpressionsParser.expression_eof
        
        [<Test>]
        member x.``can parse 5+My_Fun(First, Second, Third, 4)``() = 
            let input = """5+My_Fun(First, Second, Third, 4)"""
            
            let expected = 
                AstBinop(Add, AstLiteral(Values.BASIC(Values.BYTE 5uy)), 
                         AstFunction(AstDirect "My_Fun", 
                                     [ AstAnonymous(AstVariable(AstDirect "First"))
                                       AstAnonymous(AstVariable(AstDirect "Second"))
                                       AstAnonymous(AstVariable(AstDirect "Third"))
                                       AstAnonymous(AstLiteral(Values.BASIC(Values.BYTE 4uy))) ]))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse (5+My_Fun(First, Second, Third, 4))``() = 
            let input = """(5+My_Fun(First, Second, Third, 4))"""
            
            let expected = 
                AstBinop(Add, AstLiteral(Values.BASIC(Values.BYTE 5uy)), 
                         AstFunction(AstDirect "My_Fun", 
                                     [ AstAnonymous(AstVariable(AstDirect "First"))
                                       AstAnonymous(AstVariable(AstDirect "Second"))
                                       AstAnonymous(AstVariable(AstDirect "Third"))
                                       AstAnonymous(AstLiteral(Values.BASIC(Values.BYTE 4uy))) ]))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10*(5+My_Fun(First, Second, Third, 4))``() = 
            let input = """10*(5+My_Fun(First, Second, Third, 4))"""
            
            let expected = 
                AstBinop(Mul, AstLiteral(Values.BASIC(Values.BYTE 10uy)), 
                         AstBinop(Add, AstLiteral(Values.BASIC(Values.BYTE 5uy)), 
                                  AstFunction(AstDirect "My_Fun", 
                                              [ AstAnonymous(AstVariable(AstDirect "First"))
                                                AstAnonymous(AstVariable(AstDirect "Second"))
                                                AstAnonymous(AstVariable(AstDirect "Third"))
                                                AstAnonymous(AstLiteral(Values.BASIC(Values.BYTE 4uy))) ])))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10*5+My_Fun(First, Second, Third, 4)``() = 
            let input = """10*5+My_Fun(First, Second, Third, 4)"""
            
            let expected = 
                AstBinop(Add, 
                         AstBinop
                             (Mul, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.BYTE 5uy))), 
                         AstFunction(AstDirect "My_Fun", 
                                     [ AstAnonymous(AstVariable(AstDirect "First"))
                                       AstAnonymous(AstVariable(AstDirect "Second"))
                                       AstAnonymous(AstVariable(AstDirect "Third"))
                                       AstAnonymous(AstLiteral(Values.BASIC(Values.BYTE 4uy))) ]))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10*5+My_Fun(First, Second, Third, 4)**3``() = 
            let input = """10*5+My_Fun(First, Second, Third, 4)**3"""
            
            let expected = 
                AstBinop
                    (Add, 
                     AstBinop
                         (Mul, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.BYTE 5uy))), 
                     AstBinop
                         (Exp, 
                          AstFunction(AstDirect "My_Fun", 
                                      [ AstAnonymous(AstVariable(AstDirect "First"))
                                        AstAnonymous(AstVariable(AstDirect "Second"))
                                        AstAnonymous(AstVariable(AstDirect "Third"))
                                        AstAnonymous(AstLiteral(Values.BASIC(Values.BYTE 4uy))) ]), 
                          AstLiteral(Values.BASIC(Values.BYTE 3uy))))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse true``() = 
            let input = """true"""
            let expected = AstLiteral(Values.BASIC(Values.BOOL true))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10``() = 
            let input = """10"""
            let expected = AstLiteral(Values.BASIC(Values.BYTE 10uy))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse true AND false``() = 
            let input = """true AND false"""
            let expected = 
                AstBinop(And, AstLiteral(Values.BASIC(Values.BOOL true)), AstLiteral(Values.BASIC(Values.BOOL false)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10=1024``() = 
            let input = """10=1024"""
            let expected = 
                AstCmp(Eq, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10 = 1024``() = 
            let input = """10 = 1024"""
            let expected = 
                AstCmp(Eq, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse true = false``() = 
            let input = """true = false"""
            let expected = 
                AstCmp(Eq, AstLiteral(Values.BASIC(Values.BOOL true)), AstLiteral(Values.BASIC(Values.BOOL false)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse true=false``() = 
            let input = """true=false"""
            let expected = 
                AstCmp(Eq, AstLiteral(Values.BASIC(Values.BOOL true)), AstLiteral(Values.BASIC(Values.BOOL false)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse (10) < (1024)``() = 
            let input = """(10) < (1024)"""
            let expected = 
                AstCmp(Lt, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10 < (1024)``() = 
            let input = """10 < (1024)"""
            let expected = 
                AstCmp(Lt, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse (10) < 1024``() = 
            let input = """(10) < 1024"""
            let expected = 
                AstCmp(Lt, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse (10 < 1024)``() = 
            let input = """(10 < 1024)"""
            let expected = 
                AstCmp(Lt, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse (10<1024)``() = 
            let input = """(10<1024)"""
            let expected = 
                AstCmp(Lt, AstLiteral(Values.BASIC(Values.BYTE 10uy)), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse 10*5+My_Fun(First, Second, Third, 4)**3 < 1024``() = 
            let input = """(10*5+My_Fun(First, Second, Third, 4)**3 < 1024)"""
            
            let expected = 
                AstCmp
                    (Lt, 
                     AstBinop
                         (Add, 
                          AstBinop
                              (Mul, AstLiteral(Values.BASIC(Values.BYTE 10uy)), 
                               AstLiteral(Values.BASIC(Values.BYTE 5uy))), 
                          AstBinop
                              (Exp, 
                               AstFunction(AstDirect "My_Fun", 
                                           [ AstAnonymous(AstVariable(AstDirect "First"))
                                             AstAnonymous(AstVariable(AstDirect "Second"))
                                             AstAnonymous(AstVariable(AstDirect "Third"))
                                             AstAnonymous(AstLiteral(Values.BASIC(Values.BYTE 4uy))) ]), 
                               AstLiteral(Values.BASIC(Values.BYTE 3uy)))), AstLiteral(Values.BASIC(Values.INT 1024)))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse (((Sensor_StampStartButton = TRUE) OR (Button_Program_Start_StampModule = TRUE)) AND (AidVar = 0))``() = 
            let input = 
                "(((Sensor_StampStartButton = TRUE) OR (Button_Program_Start_StampModule = TRUE)) AND (AidVar = 0))"
            let expected = 
                AstBinop
                    (And, 
                     AstBinop
                         (Or, 
                          AstCmp
                              (Eq, AstVariable(AstDirect "Sensor_StampStartButton"), 
                               AstLiteral(Values.BASIC(Values.BOOL true))), 
                          AstCmp
                              (Eq, AstVariable(AstDirect "Button_Program_Start_StampModule"), 
                               AstLiteral(Values.BASIC(Values.BOOL true)))), 
                     AstCmp(Eq, AstVariable(AstDirect "AidVar"), AstLiteral(Values.BASIC(Values.BYTE 0uy))))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse Stamp_Timer2(IN:=Timer2StartVarStamp, PT:=T#1000MS)``() = 
            let input = "Stamp_Timer2(IN:=Timer2StartVarStamp, PT:=T#1000MS)"
            
            let expected = 
                AstFunction(AstDirect "Stamp_Timer2", 
                            [ AstNamed("IN", AstVariable(AstDirect "Timer2StartVarStamp"))
                              AstNamed("PT", AstLiteral(Values.BASIC(Values.TIME [ 1000.0, Values.Millisecond ]))) ])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse Stamp_Timer2(IN:=Timer2StartVarStamp, PT:=T#1000MS, NOT IN => MyVar)``() = 
            let input = "Stamp_Timer2(IN:=Timer2StartVarStamp, PT:=T#1000MS, NOT IN => MyVar)"
            
            let expected = 
                AstFunction(AstDirect "Stamp_Timer2", 
                            [ AstNamed("IN", AstVariable(AstDirect "Timer2StartVarStamp"))
                              AstNamed("PT", AstLiteral(Values.BASIC(Values.TIME [ 1000.0, Values.Millisecond ])))
                              AstNamedOutputNot("IN", (AstDirect "MyVar")) ])
            actual_equals_expected (parse input, expected)
