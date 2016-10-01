namespace FsPlcModel

module PousParserXml = 
    open Common.ParsersXml
    open Pous
    open System.Xml
    open System.Xml.Linq
    
    exception Expected_pou_interface of XElement
    
    exception Expected_body of XElement
    
    exception Expected_pou of XElement

    exception Expected_pous of XElement
    
    exception Expected_action of XElement
    
    exception Expected_actions of XElement
    
    exception Expected_transition of XElement
    
    exception Expected_transitions of XElement
    
    let pouInterface (xml : XElement) = 
        if (xml.Name.LocalName <> "interface") then raise (Expected_pou_interface xml)
        let returnType = 
            let returnTypeXml = xml.Element(xn_default "returnType")
            if returnTypeXml <> null then Some(TypesParserXml.anyType (returnTypeXml.Elements() |> Seq.head))
            else None
        
        let vars = 
            xml.Elements()
            |> Seq.filter (fun e -> e.Name.LocalName <> "returnType")
            |> Seq.collect DeclarationsParserXml.declaration_block
            |> Seq.toList
        
        { returnType = returnType
          vars = vars }
    
    let bodyElement (xml : XElement) = 
        try 
            AstST(StStatementsParserXml.bodySt xml)
        with _ -> raise (Expected_body xml)
    
    let body (xml : XElement) = Seq.map bodyElement (xml.Elements()) |> List.ofSeq
    
    let action (xml : XElement) : PouActionAst = 
        if (xml.Name.LocalName <> "action") then raise (Expected_action xml)
        let name = xml.Attribute(xn "name").Value
        let body = xml.Elements(xn_default "body") |> Seq.collect body |> List.ofSeq
        { name = name
          body = body }
    
    let actions (xml : XElement) = 
        if (xml.Name.LocalName <> "actions") then raise (Expected_actions xml)
        xml.Elements(xn_default "action")
        |> Seq.map action
        |> List.ofSeq
    
    let transition (xml : XElement) : PouTransitionAst = 
        if (xml.Name.LocalName <> "transition") then raise (Expected_transition xml)
        let name = xml.Attribute(xn "name").Value
        let body = xml.Elements(xn_default "body") |> Seq.collect body |> List.ofSeq
        { name = name
          body = body }
    
    let transitions (xml : XElement) = 
        if (xml.Name.LocalName <> "transitions") then raise (Expected_transitions xml)
        xml.Elements(xn_default "transition")
        |> Seq.map transition
        |> List.ofSeq
    
    let pou (xml : XElement) : PouAst = 
        if (xml.Name.LocalName <> "pou") then raise (Expected_pou xml)
        let name = xml.Attribute(xn "name").Value
        let pouType = PousParser.parse PousParser.pouType (xml.Attribute(xn "pouType").Value)
        
        let pouInterface = 
            let pouInterfaceXml = xml.Element(xn_default "interface")
            if pouInterfaceXml <> null then Some(pouInterface pouInterfaceXml)
            else None
        let pouBody = xml.Elements(xn_default "body") |> Seq.collect body |> List.ofSeq
        
        let pouActions = 
            let pouActionsXml = xml.Element(xn_default "actions")
            if pouActionsXml <> null then actions pouActionsXml
            else []
        
        let pouTransitions = 
            let pouTransitionsXml = xml.Element(xn_default "transitions")
            if pouTransitionsXml <> null then transitions pouTransitionsXml
            else []
        
        { name = name
          typ = pouType
          iface = pouInterface
          bodies = pouBody
          actions = pouActions
          transitions = pouTransitions }

    let pous (xml : XElement) : PouAst list = 
        if (xml.Name.LocalName <> "pous") then raise (Expected_pous xml)
        xml.Elements(xn_default "pou") |> Seq.map pou |> List.ofSeq
    
    let parse = Common.ParsersXml.parse

module PousParserXmlTest = 
    open FsUnit
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    open Pous
    open Declarations
    open StExpressions
    open StStatements
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type PouInterfaceTests() = 
        let parse = PousParserXml.parse PousParserXml.pouInterface
        
        [<Test>]
        member x.``Input-only interface``() = 
            let input = """<interface xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <inputVars>
                                <variable name="StartVar">
                                  <type>
                                    <BOOL />
                                  </type>
                                </variable>
                              </inputVars>
                           </interface>"""
            
            let expected = 
                { returnType = None
                  vars = 
                      [ { kind = VAR_INPUT
                          attr = []
                          id = "StartVar"
                          typ = Types.BASIC Types.BOOL
                          ivalue = None } ] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``Full interface (without return type)``() = 
            let input = """<interface xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <inputVars>
                                <variable name="StartVar">
                                  <type>
                                    <BOOL />
                                  </type>
                                </variable>
                                <variable name="AidVar">
                                  <type>
                                    <INT />
                                  </type>
                                  <initialValue>
                                    <simpleValue value="0" />
                                  </initialValue>
                                </variable>
                              </inputVars>
                              <outputVars>
                                <variable name="StartCommandConveyor">
                                  <type>
                                    <BOOL />
                                  </type>
                                </variable>
                              </outputVars>
                              <localVars>
                                <variable name="Timer_Conveyor_Runtime">
                                  <type>
                                    <derived name="TON" />
                                  </type>
                                </variable>
                                <variable name="Duration_Conveyor_Runtime">
                                  <type>
                                    <TIME />
                                  </type>
                                  <initialValue>
                                    <simpleValue value="TIME#4s500ms" />
                                  </initialValue>
                                </variable>
                              </localVars>
                            </interface>"""
            
            let expected = 
                { returnType = None
                  vars = 
                      [ { kind = VAR_INPUT
                          attr = []
                          id = "StartVar"
                          typ = Types.BASIC Types.BOOL
                          ivalue = None }
                        { kind = VAR_INPUT
                          attr = []
                          id = "AidVar"
                          typ = Types.BASIC Types.INT
                          ivalue = Some(Values.BASIC(Values.INT 0)) }
                        { kind = VAR_OUTPUT
                          attr = []
                          id = "StartCommandConveyor"
                          typ = Types.BASIC Types.BOOL
                          ivalue = None }
                        { kind = VAR
                          attr = []
                          id = "Timer_Conveyor_Runtime"
                          typ = Types.TYPE_REFERENCE ["TON"]
                          ivalue = None }
                        { kind = VAR
                          attr = []
                          id = "Duration_Conveyor_Runtime"
                          typ = Types.BASIC Types.TIME
                          ivalue = 
                              Some(Values.BASIC(Values.TIME([ 4.0, Values.Second
                                                              500.0, Values.Millisecond ]))) } ] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``Full interface (with return type)``() = 
            let input = """<interface xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <returnType>
                                <INT />
                              </returnType>
                              <inputVars>
                                <variable name="StartVar">
                                  <type>
                                    <BOOL />
                                  </type>
                                </variable>
                                <variable name="AidVar">
                                  <type>
                                    <INT />
                                  </type>
                                  <initialValue>
                                    <simpleValue value="0" />
                                  </initialValue>
                                </variable>
                              </inputVars>
                              <outputVars>
                                <variable name="StartCommandConveyor">
                                  <type>
                                    <BOOL />
                                  </type>
                                </variable>
                              </outputVars>
                              <localVars>
                                <variable name="Timer_Conveyor_Runtime">
                                  <type>
                                    <derived name="TON" />
                                  </type>
                                </variable>
                                <variable name="Duration_Conveyor_Runtime">
                                  <type>
                                    <TIME />
                                  </type>
                                  <initialValue>
                                    <simpleValue value="TIME#4s500ms" />
                                  </initialValue>
                                </variable>
                              </localVars>
                            </interface>"""
            
            let expected = 
                { returnType = Some(Types.BASIC Types.INT)
                  vars = 
                      [ { kind = VAR_INPUT
                          attr = []
                          id = "StartVar"
                          typ = Types.BASIC Types.BOOL
                          ivalue = None }
                        { kind = VAR_INPUT
                          attr = []
                          id = "AidVar"
                          typ = Types.BASIC Types.INT
                          ivalue = Some(Values.BASIC(Values.INT 0)) }
                        { kind = VAR_OUTPUT
                          attr = []
                          id = "StartCommandConveyor"
                          typ = Types.BASIC Types.BOOL
                          ivalue = None }
                        { kind = VAR
                          attr = []
                          id = "Timer_Conveyor_Runtime"
                          typ = Types.TYPE_REFERENCE ["TON"]
                          ivalue = None }
                        { kind = VAR
                          attr = []
                          id = "Duration_Conveyor_Runtime"
                          typ = Types.BASIC Types.TIME
                          ivalue = 
                              Some(Values.BASIC(Values.TIME([ 4.0, Values.Second
                                                              500.0, Values.Millisecond ]))) } ] }
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type PouBodyTests() = 
        let parse = PousParserXml.parse PousParserXml.body
        [<Test>]
        member x.``Two ST worksheets``() = 
            let input = """<body xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <ST>
                                <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = TRUE)THEN AidVar := 0; END_IF;
                                </xhtml>
                               </ST>
                              <ST>
                                <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = FALSE)THEN AidVar := 1; END_IF;
                                </xhtml>
                               </ST>
                           </body>"""
            
            let expected = 
                [ AstST
                      ([ {line=1L;column=1L},AstIf
                             (AstCmp
                                  (Eq, AstVariable(AstDirect "InitConveyorModule"), 
                                   AstLiteral(Values.BASIC(Values.BOOL true))), 
                              [ {line=2L;column=71L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 0uy))) ], []) ])
                  
                  AstST
                      ([ {line=1L;column=1L},AstIf
                             (AstCmp
                                  (Eq, AstVariable(AstDirect "InitConveyorModule"), 
                                   AstLiteral(Values.BASIC(Values.BOOL false))), 
                              [ {line=2L;column=72L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 1uy))) ], []) ]) ]
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type PouActionTests() = 
        let parse = PousParserXml.parse PousParserXml.action
        [<Test>]
        member x.``Action with two ST worksheets``() = 
            let input = """<action name="My_Action123" xmlns="http://www.plcopen.org/xml/tc6_0200">
                            <body>
                              <ST>
                                <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = TRUE)THEN AidVar := 0; END_IF;
                                </xhtml>
                               </ST>
                              <ST>
                                <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = FALSE)THEN AidVar := 1; END_IF;
                                </xhtml>
                               </ST>
                           </body>
                          </action>"""
            
            let expected : PouActionAst = 
                { name = "My_Action123"
                  body = 
                      [ AstST
                            ([ {line=1L;column=1L},AstIf
                                   (AstCmp
                                        (Eq, AstVariable(AstDirect "InitConveyorModule"), 
                                         AstLiteral(Values.BASIC(Values.BOOL true))), 
                                    [ {line=2L;column=71L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 0uy))) ], []) ])
                        
                        AstST
                            ([ {line=1L;column=1L},AstIf
                                   (AstCmp
                                        (Eq, AstVariable(AstDirect "InitConveyorModule"), 
                                         AstLiteral(Values.BASIC(Values.BOOL false))), 
                                    [ {line=2L;column=72L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 1uy))) ], []) ]) ] }
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type PouTransitionTests() = 
        let parse = PousParserXml.parse PousParserXml.transition
        [<Test>]
        member x.``Transition with two ST worksheets``() = 
            let input = """<transition name="My_Transition_123" xmlns="http://www.plcopen.org/xml/tc6_0200">
                            <body>
                              <ST>
                                <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = TRUE)THEN AidVar := 0; END_IF;
                                </xhtml>
                               </ST>
                              <ST>
                                <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = FALSE)THEN AidVar := 1; END_IF;
                                </xhtml>
                               </ST>
                           </body>
                          </transition>"""
            
            let expected : PouTransitionAst = 
                { name = "My_Transition_123"
                  body = 
                      [ AstST
                            ([ {line=1L;column=1L},AstIf
                                   (AstCmp
                                        (Eq, AstVariable(AstDirect "InitConveyorModule"), 
                                         AstLiteral(Values.BASIC(Values.BOOL true))), 
                                    [ {line=2L;column=71L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 0uy))) ], []) ])
                        
                        AstST
                            ([ {line=1L;column=1L},AstIf
                                   (AstCmp
                                        (Eq, AstVariable(AstDirect "InitConveyorModule"), 
                                         AstLiteral(Values.BASIC(Values.BOOL false))), 
                                    [ {line=2L;column=72L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 1uy))) ], []) ]) ] }
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type PouTests() = 
        let parse = PousParserXml.parse PousParserXml.pou
        [<Test>]
        member x.``Pou with several ST bodies``() = 
            let input = """<pou name="ConveyorModule" pouType="functionBlock" xmlns="http://www.plcopen.org/xml/tc6_0200">
                              <interface>
                                <inputVars>
                                  <variable name="timerErrorConveyor">
                                    <type>
                                      <BOOL />
                                    </type>
                                  </variable>
                                </inputVars>
                                <outputVars>
                                  <variable name="StartCommandConveyor">
                                    <type>
                                      <BOOL />
                                    </type>
                                  </variable>
                                </outputVars>
                                <localVars>
                                  <variable name="Conveyor_Duration_Conveyor_Failure">
                                    <type>
                                      <TIME />
                                    </type>
                                    <initialValue>
                                      <simpleValue value="TIME#2s0ms" />
                                    </initialValue>
                                  </variable>
                                </localVars>
                              </interface>
                              <body>
                                <ST>
                                  <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = TRUE)THEN AidVar := 0; END_IF;
                                  </xhtml>
                                </ST>
                                <ST>
                                  <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = TRUE)THEN AidVar := 0; END_IF;
                                  </xhtml>
                                </ST>
                              </body>
                              <body>
                                <ST>
                                  <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                    IF(InitConveyorModule = TRUE)THEN AidVar := 0; END_IF;
                                  </xhtml>
                                </ST>
                              </body>
                              <addData />
                            </pou>"""
            let expectedBody = 
                AstST
                    ([ {line=1L;column=1L},AstIf
                           (AstCmp
                                (Eq, AstVariable(AstDirect "InitConveyorModule"), 
                                 AstLiteral(Values.BASIC(Values.BOOL true))), 
                            [ {line=2L;column=71L},AstAssignment(AstDirect "AidVar", AstLiteral(Values.BASIC(Values.BYTE 0uy))) ], []) ])
            
            let expectedInterface = 
                { returnType = None
                  vars = 
                      [ { kind = VAR_INPUT
                          attr = []
                          id = "timerErrorConveyor"
                          typ = Types.BASIC Types.BOOL
                          ivalue = None }
                        { kind = VAR_OUTPUT
                          attr = []
                          id = "StartCommandConveyor"
                          typ = Types.BASIC Types.BOOL
                          ivalue = None }
                        { kind = VAR
                          attr = []
                          id = "Conveyor_Duration_Conveyor_Failure"
                          typ = Types.BASIC Types.TIME
                          ivalue = 
                              Some(Values.BASIC(Values.TIME([ 2.0, Values.Second
                                                              0.0, Values.Millisecond ]))) } ] }
            
            let expected : PouAst = 
                { name = "ConveyorModule"
                  typ = FUNCTION_BLOCK
                  iface = Some expectedInterface
                  actions = []
                  transitions = []
                  bodies = [ expectedBody; expectedBody; expectedBody ] }
            actual_equals_expected (parse input, expected)
