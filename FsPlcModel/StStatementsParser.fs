namespace FsPlcModel

module StStatementsParser = 
    open Common.Parsers
    open FParsec
    open FParsec.Primitives
    open StExpressionsParser
    open StStatements
    
    let statement, statementRef = createParserForwardedToRef<StatementAst, unit>()
    let block = many (getPosition .>>. statement .>> opt semicolon .>> ws)  |>> List.map (fun (p,s) ->
        {line=p.Line;column=p.Column}, s
    )
    let assignment = variable .>> keyword ":=" .>>. expression_semi |>> AstAssignment
    let call = function_call .>> semicolon |>> AstCall
    
    let else_endif : Parser<BlockAst, unit> = 
        choice [ keyword "end_if" .>> opt semicolon |>> fun _ -> []
                 keyword "else" >>. block .>> keyword "end_if" .>> opt semicolon ]
    
    let if_then_else = 
        keyword "if" >>. bexp .>> keyword "then" .>>. block .>>. else_endif |>> (fun ((c, a), b) -> AstIf(c, a, b))
    let case_values_range = pint64 .>> ws .>> dotdot .>>. pint64 .>> ws |>> CaseRange
    let case_values_list = sepBy pint64 (comma >>. ws) |>> CaseValues
    
    let case_values = 
        choice [ attempt case_values_range
                 case_values_list ]
    
    let case = case_values .>> colon .>> ws .>>. block
    
    let cases = 
        many1 case .>>. choice [ keyword "else" >>. block
                                 preturn [] ] .>> keyword "end_case"
    
    let case_of = keyword "case" >>. expression .>> keyword "of" .>>. cases |>> (fun (e, (cs, d)) -> AstCase(e, cs, d))
    let do_while = 
        keyword "while" >>. bexp .>> keyword "do" .>>. block .>> keyword "end_while" |>> fun (e, b) -> AstDoWhile(e, b)
    let repeat_until = 
        keyword "repeat" >>. block .>> keyword "until" .>>. bexp .>> keyword "end_repeat" 
        |>> fun (b, e) -> AstRepeatUntil(e, b)
    let by_clause = keyword "by" >>. aexp
    let for_to = 
        keyword "for" >>. variable .>> keyword ":=" .>>. aexp .>> ws .>> keyword "to" .>>. aexp .>> ws 
        .>>. opt (by_clause) .>> ws .>> keyword "do" .>>. block .>> keyword "end_for" 
        |>> (fun ((((v, e), e'), by), b) -> AstFor(v, e, e', by, b))
    let exit = keyword "exit" |>> fun _ -> AstExit
    
    do statementRef := ws >>. choice [ attempt if_then_else <!> "if_then_else"
                                       attempt case_of  <!> "case_of"
                                       attempt do_while  <!> "do_while"
                                       attempt repeat_until  <!> "repeat_until"
                                       attempt for_to  <!> "for_to"
                                       attempt exit  <!> "exit"
                                       attempt call <!> "call"
                                       attempt assignment  <!> "assignment" ]
    
    let parse = Common.Parsers.parse
    let parse_all p = Common.Parsers.parse (p .>> eof)

module StStatementsParserTest = 
    open FsUnit
    open NUnit
    open NUnit.Core
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