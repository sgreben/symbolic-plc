namespace FsPlcModel

module TasksParserXml = 
    open Common.ParsersXml
    open System.Xml
    open System.Xml.Linq
    open Tasks
    
    exception Expected_task of XElement
    
    exception Expected_task_pou_instance of XElement
    
    let taskPouInstance (xml : XElement) = 
        if (xml.Name.LocalName <> "pouInstance") then raise (Expected_task_pou_instance xml)
        let name = Common.Parsers.parse Common.Parsers.ident (xml.Attribute(xn "name").Value)
        let typ =  Common.Parsers.parse Common.Parsers.qident (xml.Attribute(xn "typeName").Value)
        name,typ
    
    let taskProperty (xa : XAttribute) = 
        match xa.Name.LocalName with
        | "interval" -> Some <| INTERVAL(TasksParser.parse TasksParser.taskInterval xa.Value)
        | "single" -> Some <| SINGLE(TasksParser.parse TasksParser.taskSingle xa.Value)
        | "priority" -> Some <| PRIORITY(TasksParser.parse TasksParser.taskPriority xa.Value)
        | _ -> None
    
    let taskProperties (xas : seq<XAttribute>) = List.ofSeq (Seq.choose taskProperty xas)
    
    let task (xml : XElement) = 
        if (xml.Name.LocalName <> "task") then raise (Expected_task xml)
        let ps = taskProperties (xml.Attributes())
        let name = xml.Attribute(xn "name").Value
        let pouInstances = xml.Elements(xn_default "pouInstance") |> Seq.map taskPouInstance |> List.ofSeq
        { name = name
          properties = ps
          pou =  pouInstances }
    

    
    
    let parse = Common.ParsersXml.parse

module TasksParserXmlTests = 
    open FsUnit
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    open Tasks
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type TaskTests() = 
        let parse = TasksParserXml.parse TasksParserXml.task
        
        [<Test>]
        member x.``can parse task w/o properties``() = 
            let input = """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task"></task>"""
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = [] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse task with INTERVAL property``() = 
            let input = """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task" interval="t#10s"></task>"""
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ])) ] }
            actual_equals_expected (parse input, expected)
            let input = """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task" interval="pt10s"></task>"""
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ])) ] }
            actual_equals_expected (parse input, expected)
            let input = 
                """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task" interval="time#10s"></task>"""
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ])) ] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse task with INTERVAL and PRIORITY properties``() = 
            let input = 
                """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task" interval="t#10s" priority="10"></task>"""
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = 
                      [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ]))
                        PRIORITY 10 ] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse task with INTERVAL, PRIORITY and SINGLE properties``() = 
            let input = 
                """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task" interval="t#10s" priority="10" single="Abc.Def.Ghi_Jkl"></task>"""
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = 
                      [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ]))
                        PRIORITY 10
                        SINGLE [ "Abc"; "Def"; "Ghi_Jkl" ] ] }
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type MappedTaskTests() = 
        let parse = TasksParserXml.parse TasksParserXml.task
        
        [<Test>]
        member x.``can parse mapped task with INTERVAL, PRIORITY and SINGLE properties``() = 
            let input = """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task" interval="t#10s" priority="10" single="Abc.Def.Ghi_Jkl">
                                <pouInstance name="Main"/>
                           </task>"""
            
            let expected = 
                { name = "My_Task"
                  properties = 
                            [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ]))
                              PRIORITY 10
                              SINGLE [ "Abc"; "Def"; "Ghi_Jkl" ] ]
                  pou = ["Main", ["Main"]] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse mapped task with INTERVAL, PRIORITY and SINGLE properties and docs inside the pouInstance``() = 
            let input = """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task" interval="t#10s" priority="10" single="Abc.Def.Ghi_Jkl">
                                <pouInstance name="Main">
                                    <documentation></documentation>
                                </pouInstance>
                           </task>"""
            
            let expected = 
                { name = "My_Task"
                  properties = 
                    [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ]))
                      PRIORITY 10
                      SINGLE [ "Abc"; "Def"; "Ghi_Jkl" ] ] 
                  pou = ["Main",[ "Main" ]] }
            actual_equals_expected (parse input, expected)
