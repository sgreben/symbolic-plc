namespace FsPlcParser.Test
open FsPlcParser
open FsPlcModel

module StExpressionsParserTests = 
    open NUnit
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
