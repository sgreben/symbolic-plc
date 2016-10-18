namespace FsPlcParser
open FsPlcModel

module TasksParserTests = 
    open NUnit
    open NUnit.Framework
    open Tasks
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type TaskTests() = 
        let parse = TasksParser.parse TasksParser.task
        
        [<Test>]
        member x.``can parse task w/o properties``() = 
            let input = "TASK My_Task;"
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = [] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse task with INTERVAL property``() = 
            let input = "TASK My_Task (INTERVAL := t#10s);"
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ])) ] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse task with INTERVAL and PRIORITY properties``() = 
            let input = "TASK My_Task (INTERVAL := t#10s, PRIORITY:=10);"
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = 
                      [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ]))
                        PRIORITY 10 ] }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse task with INTERVAL, PRIORITY and SINGLE properties``() = 
            let input = "TASK My_Task (INTERVAL := t#10s, PRIORITY:=10,SINGLE := Abc.Def.Ghi_Jkl);"
            
            let expected = 
                { name = "My_Task"
                  pou = []
                  properties = 
                      [ INTERVAL(Fixed(Values.time_ms [ 10.0, Values.Second ]))
                        PRIORITY 10
                        SINGLE [ "Abc"; "Def"; "Ghi_Jkl" ] ] }
            actual_equals_expected (parse input, expected)
