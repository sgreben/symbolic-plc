namespace FsPlcParser
open FsPlcModel

module StStatementsParserTest = 
    open NUnit
    open NUnit.Framework
    open StExpressions
    open StStatements
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type StStatementTest() = 
        let parse = StStatementsParser.parse StStatementsParser.statement
        
        [<Test>]
        member x.``can parse IF MAX(LeftAmplif, RightAmplif) >= MaxValue THEN Overdrive := TRUE; ELSE Overdrive := FALSE; END_IF;``() = 
            let input = 
                """IF MAX(LeftAmplif, RightAmplif) >= MaxValue THEN Overdrive := TRUE; ELSE Overdrive := FALSE; END_IF;"""
            
            let expected = 
                AstIf
                    (AstCmp
                         (Geq, 
                          AstFunction(AstDirect "MAX", 
                                      [ AstAnonymous(AstVariable(AstDirect "LeftAmplif"))
                                        AstAnonymous(AstVariable(AstDirect "RightAmplif")) ]), 
                          AstVariable(AstDirect "MaxValue")), 
                     [ {line=1L;column=50L},AstAssignment(AstDirect "Overdrive", AstLiteral(Values.BASIC(Values.BOOL true))) ], 
                     [ {line=1L;column=74L},AstAssignment(AstDirect "Overdrive", AstLiteral(Values.BASIC(Values.BOOL false))) ])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``IF HeatTime.Q = TRUE THEN Critical := 1; END_IF;``() = 
            let input = "IF HeatTime.Q = TRUE THEN Critical := 1; END_IF;"
            let expected = 
                AstIf
                    (AstCmp
                         (Eq, AstVariable(AstDot(AstDirect "HeatTime", "Q")), AstLiteral(Values.BASIC(Values.BOOL true))), 
                     [ {line=1L;column=27L}, AstAssignment(AstDirect "Critical", AstLiteral(Values.BASIC(Values.BYTE 1uy))) ], [])
            actual_equals_expected (parse input, expected)

        [<Test>]
        member x.``Critical := Critical+1;``() = 
            let input = "Critical := Critical + 1;"
            let expected = 
                AstAssignment(AstDirect "Critical", AstBinop(Add,AstVariable(AstDirect "Critical"), AstLiteral(Values.BASIC(Values.BYTE 1uy))))
            actual_equals_expected (parse input, expected)

        [<Test>]
        member x.``IF HeatTime.Q = TRUE THEN Critical := Critical+1; END_IF;``() = 
            let input = "IF HeatTime.Q = TRUE THEN Critical := Critical + 1; END_IF;"
            let expected = 
                AstIf
                    (AstCmp
                         (Eq, AstVariable(AstDot(AstDirect "HeatTime", "Q")), AstLiteral(Values.BASIC(Values.BOOL true))), 
                     [ {line=1L;column=27L}, AstAssignment(AstDirect "Critical", AstBinop(Add,AstVariable(AstDirect "Critical"), AstLiteral(Values.BASIC(Values.BYTE 1uy)))) ], [])
            actual_equals_expected (parse input, expected)

        [<Test>]
        member x.``IF (* comment1 *) HeatTime.Q (* comment 2*) = TRUE (*comment 3 *) THEN Critical := 1; (*comment 4*) END_IF; (* comment 5 *)``() = 
            let input = "IF (* comment1 *) HeatTime.Q (* comment 2*) = TRUE (*comment 3 *) THEN Critical := 1; (*comment 4*) END_IF; (* comment 5 *) (* comment 6 *)"
            let expected = 
                AstIf
                    (AstCmp
                         (Eq, AstVariable(AstDot(AstDirect "HeatTime", "Q")), AstLiteral(Values.BASIC(Values.BOOL true))), 
                     [ {line=1L;column=72L}, AstAssignment(AstDirect "Critical", AstLiteral(Values.BASIC(Values.BYTE 1uy))) ], [])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``CASE K OF 0: MUX := IN0; 1: MUX := IN1; 5: MUX := INn; END_CASE;``() = 
            let input = "CASE K OF 0: MUX := IN0; 1: MUX := IN1; 5: MUX := IN5; END_CASE;"
            let b0 = Values.BASIC(Values.BYTE 0uy)
            let b1 = Values.BASIC(Values.BYTE 1uy)
            let b5 = Values.BASIC(Values.BYTE 5uy)
            
            let expected = 
                AstCase(AstVariable(AstDirect "K"), 
                        [ CaseValues [ 0L ], [ {line=1L;column=14L},AstAssignment(AstDirect "MUX", AstVariable(AstDirect "IN0")) ]
                          CaseValues [ 1L ], [ {line=1L;column=29L},AstAssignment(AstDirect "MUX", AstVariable(AstDirect "IN1")) ]
                          CaseValues [ 5L ], [ {line=1L;column=44L},AstAssignment(AstDirect "MUX", AstVariable(AstDirect "IN5")) ] ], [])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``CASE K OF 0: MUX := IN0; 1: MUX := IN1; 5: MUX := INn; ELSE MUX := IN_ERROR; END_CASE;``() = 
            let input = "CASE K OF 0: MUX := IN0; 1: MUX := IN1; 5: MUX := IN5; ELSE MUX := IN_ERROR; END_CASE;"
            let b0 = Values.BASIC(Values.BYTE 0uy)
            let b1 = Values.BASIC(Values.BYTE 1uy)
            let b5 = Values.BASIC(Values.BYTE 5uy)
            
            let expected = 
                AstCase
                    (AstVariable(AstDirect "K"), 
                     [ CaseValues [ 0L ], [ {line=1L;column=14L},AstAssignment(AstDirect "MUX", AstVariable(AstDirect "IN0")) ]
                       CaseValues [ 1L ], [ {line=1L;column=29L},AstAssignment(AstDirect "MUX", AstVariable(AstDirect "IN1")) ]
                       CaseValues [ 5L ], [ {line=1L;column=44L},AstAssignment(AstDirect "MUX", AstVariable(AstDirect "IN5")) ] ], 
                     [ {line=1L;column=61L},AstAssignment(AstDirect "MUX", AstVariable(AstDirect "IN_ERROR")) ])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``FOR Index := 1 TO IndexMax BY 1 DO IF TestField[Index] > Maximum THEN Maximum := TestField[Index]; END_IF; END_FOR;``() = 
            let input = 
                """FOR Index := 1 TO IndexMax BY 1 DO 
                        IF TestField[Index] > Maximum THEN Maximum := TestField[Index]; EXIT; 
                        END_IF; 
                   END_FOR;"""
            let expected = 
                AstFor
                    (AstDirect "Index", AstLiteral(Values.BASIC(Values.BYTE 1uy)), AstVariable(AstDirect "IndexMax"), 
                     Some(AstLiteral(Values.BASIC(Values.BYTE 1uy))), 
                     [ {line=2L;column=25L},AstIf
                           (AstCmp
                                (Gt, AstVariable(AstIndex(AstDirect "TestField", (AstVariable(AstDirect "Index"), []))), 
                                 AstVariable(AstDirect "Maximum")),                             
                            [ {line=2L;column=60L}, AstAssignment(AstDirect "Maximum", 
                                   AstVariable(AstIndex(AstDirect "TestField", (AstVariable(AstDirect "Index"), []))))
                              {line=2L;column=89L}, AstExit ], 
                            []) ])
            actual_equals_expected (parse input, expected)