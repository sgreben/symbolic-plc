namespace FsPlcModel

module ProjectsParserXml = 
    open Common.ParsersXml
    open Projects
    open System.Xml
    open System.Xml.Linq
    
    exception Expected_data_type of XElement
    
    exception Expected_data_types of XElement
    
    exception Expected_types of XElement
    
    exception Expected_resource of XElement
    
    exception Expected_pou_instance of XElement
    
    exception Expected_configuration of XElement
    
    exception Expected_configurations of XElement
    
    exception Expected_instances of XElement
    
    exception Expected_project of XElement
    
    let dataType (xml : XElement) = 
        if (xml.Name.LocalName <> "dataType") then raise (Expected_data_type xml)
        let name = xml.Attribute(xn "name").Value
        let baseType = xml.Element(xn_default "baseType") |> TypesParserXml.anyType
        
        let initialValue = 
            let initialValueXml = xml.Element(xn_default "initialValue")
            if initialValueXml <> null then Some(ValuesParserXml.value baseType initialValueXml)
            else None
        { name = name
          typ = baseType
          ivalue = initialValue }
    
    let dataTypes (xml : XElement) = 
        if (xml.Name.LocalName <> "dataTypes") then raise (Expected_data_types xml)
        xml.Elements(xn_default "dataType")
        |> Seq.map dataType
        |> List.ofSeq
    
    let projectTypes (xml : XElement) : ProjectTypesAst = 
        if (xml.Name.LocalName <> "types") then raise (Expected_types xml)
        let pous = xml.Element(xn_default "pous") |> PousParserXml.pous
        let dataTypes = xml.Element(xn_default "dataTypes") |> dataTypes
        { pous = pous
          dataTypes = dataTypes }
    
    let pouInstance (xml : XElement) = 
        if (xml.Name.LocalName <> "pouInstance") then raise (Expected_pou_instance xml)
        let name = xml.Attribute(xn "name").Value |> Common.Parsers.parse Common.Parsers.qident |> List.rev |> List.head
        let typeName = xml.Attribute(xn "typeName").Value |> Common.Parsers.parse Common.Parsers.qident
        (name, typeName)
    
    let resource (xml : XElement) = 
        if (xml.Name.LocalName <> "resource") then raise (Expected_resource xml)
        let name = xml.Attribute(xn "name").Value
        
        let tasks = 
            xml.Elements(xn_default "task")
            |> Seq.map TasksParserXml.task
            |> List.ofSeq
        
        let vars = 
            xml.Elements(xn_default "globalVars")
            |> Seq.map DeclarationsParserXml.named_declaration_block
            |> List.ofSeq
        
        let pouInstances = 
            xml.Elements(xn_default "pouInstance")
            |> Seq.map pouInstance
            |> List.ofSeq
        
        { name = name
          tasks = tasks
          vars = vars
          pouInstances = pouInstances }
    
    let configuration (xml : XElement) = 
        if (xml.Name.LocalName <> "configuration") then raise (Expected_configuration xml)
        let name = xml.Attribute(xn "name").Value
        
        let resources = 
            xml.Elements(xn_default "resource")
            |> Seq.map resource
            |> List.ofSeq
        
        let var_block xs = Seq.map DeclarationsParserXml.named_declaration_block xs |> List.ofSeq
        let gvars = xml.Elements(xn_default "globalVars") |> var_block
        let avars = xml.Elements(xn_default "accessVars") |> var_block
        let cvars = xml.Elements(xn_default "configVars") |> var_block
        { name = name
          resources = resources
          vars = gvars @ avars @ cvars }
    
    let configurations (xml : XElement) = 
        if (xml.Name.LocalName <> "configurations") then raise (Expected_configurations xml)
        xml.Elements(xn_default "configuration")
        |> Seq.map configuration
        |> List.ofSeq
    
    let instances (xml : XElement) = 
        if (xml.Name.LocalName <> "instances") then raise (Expected_instances xml)
        xml.Elements(xn_default "configurations")
        |> Seq.collect configurations
        |> List.ofSeq

    
    let project (xml : XElement) : ProjectAst = 
        let xml = ProjectsParserXmlPreprocess.project xml
        if (xml.Name.LocalName <> "project") then raise (Expected_project xml)
        let types = xml.Element(xn_default "types") |> projectTypes
        let configurations = xml.Element(xn_default "instances") |> instances
        { types = types
          libraries = []
          configurations = configurations }

module ProjectsParserXmlTest = 
    open FsUnit
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    module ProjectTests =
        open Tasks
        open Declarations
        open Values
        open StExpressions
        open StStatements
        open Pous
        open Projects


        let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
        [<TestFixture>]
        type Fixture() = 
            let parse = Common.ParsersXml.parse ProjectsParserXml.project
            [<Test>]
            member x.``Sample project``() = 
                let input = Common.ParsersXml.parse_file (fun doc -> doc.Element(Common.ParsersXml.xn_default "project") |> ProjectsParserXml.project) "Sortieranlage.Reduced.xml"
                printfn "%A" input
            [<Test>]
            member x.``Small project``() = 
                let input="""<?xml version="1.0" encoding="utf-8"?>
                             <project xmlns="http://www.plcopen.org/xml/tc6_0200">
                                <types>
                                <dataTypes />
                                <pous />
                                </types>
                                <instances>
                                <configurations>
                                    <configuration name="Device">
                                    <resource name="Application">
                                        <task name="MainTask" interval="PT0.01S" priority="1">
                                        <pouInstance name="Main" typeName=""></pouInstance>
                                        <addData>
                                            <data name="http://www.3s-software.com/plcopenxml/tasksettings" handleUnknown="implementation">
                                            <TaskSettings KindOfTask="Cyclic" Interval="t#10ms" WithinSPSTimeSlicing="true">
                                                <Watchdog Enabled="false" TimeUnit="ms" Sensitivity="1" />
                                            </TaskSettings>
                                            </data>
                                        </addData>
                                        </task>
                                        <globalVars name="GVL_Emergency_Var">
                                        <variable name="Reset_plantVar">
                                            <type>
                                            <BOOL />
                                            </type>
                                            <initialValue>
                                            <simpleValue value="FALSE" />
                                            </initialValue>
                                        </variable>
                                        </globalVars>
                                        <addData>
                                        <data name="http://www.3s-software.com/plcopenxml/pou" handleUnknown="implementation">
                                            <pou name="Reset" pouType="functionBlock">
                                            <interface>
                                                <localVars>
                                                <variable name="Reset_StampVar">
                                                    <type>
                                                    <BOOL />
                                                    </type>
                                                </variable>
                                                </localVars>
                                            </interface>
                                            <body>
                                                <ST>
                                                <xhtml xmlns="http://www.w3.org/1999/xhtml">
                                                    IF(Reset_ExecuteVar = TRUE) THEN
                                                        Reset_ExecuteVar := FALSE;
                                                        Button_Program_Start_ConveyorModule := FALSE;
                                                    END_IF;
                                                    Reset_Time(IN:=timerResetStartCommand, PT:=Reset_Time_Duration);
                                                    Reset_Time_interconnect := Reset_Time.Q;
                                                    IF(Reset_Time_interconnect = TRUE)THEN
                                                    Reset_RUN := FALSE;
                                                    END_IF;
                                                </xhtml>
                                                </ST>
                                            </body>
                                            <addData />
                                            </pou>
                                        </data>
                                        </addData>
                                    </resource>
                                    </configuration>
                                </configurations>
                                </instances>
                            </project>"""
                let expectedDataTypes = []
                let expectedPous = []
                let expectedTypes = {dataTypes = expectedDataTypes; pous=expectedPous}
                let expectedConfigurations = []
                let expected : ProjectAst = 
                    {types =
                      {dataTypes = [];
                       pous =
                        [{name = "Reset";
                          typ = FUNCTION_BLOCK;
                          iface = Some {returnType = None;
                                        vars = [{kind = Declarations.VAR;
                                                 attr = [];
                                                 id = "Reset_StampVar";
                                                 typ = Types.BASIC Types.BOOL;
                                                 ivalue = None;}];};
                          actions = [];
                          transitions = [];
                          bodies =
                           [AstST
                              [{line=1L;column=1L},AstIf
                                 (AstCmp
                                    (Eq,AstVariable (AstDirect "Reset_ExecuteVar"),
                                     AstLiteral (BASIC (BOOL true))),
                                  [{line=3L;column=57L},AstAssignment
                                     (AstDirect "Reset_ExecuteVar",AstLiteral (BASIC (BOOL false)));
                                   {line=4L;column=57L},AstAssignment
                                     (AstDirect "Button_Program_Start_ConveyorModule",
                                      AstLiteral (BASIC (BOOL false)))],[]);
                               {line=6L;column=53L},AstCall
                                 (AstDirect "Reset_Time",
                                  [AstNamed ("IN",AstVariable (AstDirect "timerResetStartCommand"));
                                   AstNamed ("PT",AstVariable (AstDirect "Reset_Time_Duration"))]);
                               {line=7L;column=53L},AstAssignment
                                 (AstDirect "Reset_Time_interconnect",
                                  AstVariable (AstDot (AstDirect "Reset_Time","Q")));
                               {line=8L;column=53L},AstIf
                                 (AstCmp
                                    (Eq,AstVariable (AstDirect "Reset_Time_interconnect"),
                                     AstLiteral (BASIC (BOOL true))),
                                  [{line=9L;column=53L},AstAssignment
                                     (AstDirect "Reset_RUN",AstLiteral (BASIC (BOOL false)))],[])]];}];};
                     libraries = []
                     configurations =
                      [{name = "Device";
                        resources =
                         [{name = "Application";
                           tasks = [{name = "MainTask";
                                     properties = [INTERVAL (Fixed (Values.time_ms [(0.01, Second)])); PRIORITY 1];
                                     pou = ["Main",["Main"]];}];
                           vars = [{name=Some "GVL_Emergency_Var"
                                    vars =[{kind = VAR_GLOBAL;
                                            attr = [];
                                            id = "Reset_plantVar";
                                            typ = Types.BASIC Types.BOOL;
                                            ivalue = Some (BASIC (BOOL false))}]}];
                           pouInstances = [];}];
                        vars = [];}];}
                actual_equals_expected (parse input, expected)
