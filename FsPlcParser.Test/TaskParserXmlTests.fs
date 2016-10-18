namespace FsPlcParser.Test
open FsPlcParser
open FsPlcModel

module TasksParserXmlTests = 
    open NUnit
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
        member x.``can parse mapped task``() = 
            let input = """<task xmlns="http://www.plcopen.org/xml/tc6_0200" name="My_Task">
                                <pouInstance name="Main" typeName="Main"/>
                           </task>"""
            
            let expected = 
                { name = "My_Task"
                  properties = []
                  pou = ["Main", ["Main"]] }
            actual_equals_expected (parse input, expected)

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
