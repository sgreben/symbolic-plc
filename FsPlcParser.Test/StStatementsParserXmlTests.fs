namespace FsPlcParser.Test
open FsPlcParser
open FsPlcModel


module StStatementsParserXmlTest = 
    open NUnit
    open NUnit.Framework
    open StExpressions
    open StStatements
    open Tasks
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type StTests() = 
        let parse = StStatementsParserXml.parse StStatementsParserXml.bodySt
        let expected = 
            [ {line=1L;column=1L}, AstIf
                  (AstCmp(Eq, AstVariable(AstDirect "InitConveyorModule"), AstLiteral(Values.BASIC(Values.BOOL true))), 
                   [ {line=1L;column=1L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 0uy))) ], []) ]
        let strip_positions = function
           | [ _, AstIf
                (AstCmp(Eq, AstVariable(AstDirect "InitConveyorModule"), AstLiteral(Values.BASIC(Values.BOOL true))), 
                [ _,AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 0uy))) ], []) ] -> expected
           | _ -> failwith ""
        let actual_equals_expected (actual : StStatements.BlockAst, expected : StStatements.BlockAst) = Assert.AreEqual(expected, strip_positions actual)
        [<Test>]
        member x.``xml ST block 1``() = 
            let input = """<ST xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <xhtml xmlns="http://www.w3.org/1999/xhtml">
                              IF(InitConveyorModule = TRUE)THEN AidVar := 0; END_IF;</xhtml>
                           </ST>"""
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``xml ST block 2``() = 
            let input = """<ST xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <xhtml xmlns="http://www.w3.org/1999/xhtml">
                              IF (InitConveyorModule = TRUE) THEN 
                                AidVar := 0; 
                              END_IF
                              </xhtml>
                           </ST>"""
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``xml ST block 3``() = 
            let input = """<ST xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <xhtml xmlns="http://www.w3.org/1999/xhtml">
                              IF InitConveyorModule = TRUE THEN 
                                AidVar := 0; 
                              END_IF
                              </xhtml>
                           </ST>"""
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``xml ST block 4``() = 
            let input = """<ST xmlns="http://www.plcopen.org/xml/tc6_0200">
                            <xhtml xmlns="http://www.w3.org/1999/xhtml">
                              IF(InitConveyorModule = TRUE)THEN
                                    AidVar := 0; 
                              END_IF
                              </xhtml>
                           </ST>"""
            actual_equals_expected (parse input, expected)
