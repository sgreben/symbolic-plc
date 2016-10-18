namespace FsPlcParser.Test
open FsPlcParser
open FsPlcModel

module ValuesParserTests = 
    open NUnit
    open NUnit.Framework
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type BasicValueTests() = 
        
        [<Test>]
        member x.``can parse BOOL values``() = 
            let input = "true"
            let expected = Values.BOOL true
            actual_equals_expected (ValuesParser.parse ValuesParser.bool input, expected)
            let input = "false"
            let expected = Values.BOOL false
            actual_equals_expected (ValuesParser.parse ValuesParser.bool input, expected)
        
        [<Test>]
        member x.``can parse BYTE values``() = 
            let input = "127"
            let expected = Values.BYTE((byte) 127)
            actual_equals_expected (ValuesParser.parse ValuesParser.byte input, expected)
        
        [<Test>]
        member x.``can parse WORD values``() = 
            let input = "127"
            let expected = Values.WORD((uint16) 127)
            actual_equals_expected (ValuesParser.parse ValuesParser.word input, expected)
        
        [<Test>]
        member x.``can parse DWORD values``() = 
            let input = "127"
            let expected = Values.DWORD((uint32) 127)
            actual_equals_expected (ValuesParser.parse ValuesParser.dword input, expected)
        
        [<Test>]
        member x.``can parse LWORD values``() = 
            let input = "127"
            let expected = Values.LWORD((uint64) 127)
            actual_equals_expected (ValuesParser.parse ValuesParser.lword input, expected)
        
        [<Test>]
        member x.``can parse INT values``() = 
            let input = "127"
            let expected = Values.INT((int) 127)
            actual_equals_expected (ValuesParser.parse ValuesParser.int input, expected)
            let input = "-127"
            let expected = Values.INT((int) (-127))
            actual_equals_expected (ValuesParser.parse ValuesParser.int input, expected)
                
        [<Test>]
        member x.``can parse REAL values``() = 
            let input = "3.4e38"
            let expected = Values.REAL 3.4e38
            actual_equals_expected (ValuesParser.parse ValuesParser.real input, expected)
            let input = "1.18e-38"
            let expected = Values.REAL 1.18e-38
            actual_equals_expected (ValuesParser.parse ValuesParser.real input, expected)
        
        [<Test>]
        member x.``can parse LREAL values``() = 
            let input = "1.7e308"
            let expected = Values.LREAL 1.7e308
            actual_equals_expected (ValuesParser.parse ValuesParser.lreal input, expected)
            let input = "-1.7e308"
            let expected = Values.LREAL -1.7e308
            actual_equals_expected (ValuesParser.parse ValuesParser.lreal input, expected)
        
        [<Test>]
        member x.``can parse TIME values``() = 
            let input = "t#1d2h3m4s5ms"
            
            let expected = 
                Values.TIME [ 1.0, Values.Day
                              2.0, Values.Hour
                              3.0, Values.Minute
                              4.0, Values.Second
                              5.0, Values.Millisecond ]
            actual_equals_expected (ValuesParser.parse ValuesParser.time input, expected)
            let input = "t#1d4s5ms"
            
            let expected = 
                Values.TIME [ 1.0, Values.Day
                              4.0, Values.Second
                              5.0, Values.Millisecond ]
            actual_equals_expected (ValuesParser.parse ValuesParser.time input, expected)
            let input = "t#5ms"
            let expected = Values.TIME [ 5.0, Values.Millisecond ]
            actual_equals_expected (ValuesParser.parse ValuesParser.time input, expected)
            let input = "pt0.01s"
            let expected = Values.TIME [ 0.01, Values.Second ]
            actual_equals_expected (ValuesParser.parse ValuesParser.time input, expected)
            let input = "time#5ms"
            let expected = Values.TIME [ 5.0, Values.Millisecond ]
            actual_equals_expected (ValuesParser.parse ValuesParser.time input, expected)
        
        [<Test>]
        member x.``can parse DATE values``() = 
            let input = "date#1989-05-24"
            let expected = Values.DATE(1989, 05, 24)
            actual_equals_expected (ValuesParser.parse ValuesParser.date input, expected)
            let input = "d#1989-05-24"
            let expected = Values.DATE(1989, 05, 24)
            actual_equals_expected (ValuesParser.parse ValuesParser.date input, expected)
        
        [<Test>]
        member x.``can parse TIME_OF_DAY values``() = 
            let input = "tod#15 : 42 : 38"
            let expected = Values.TIME_OF_DAY(15, 42, 38)
            actual_equals_expected (ValuesParser.parse ValuesParser.time_of_day input, expected)
        
        [<Test>]
        member x.``can parse DATE_AND_TIME values``() = 
            let input = "dt#1989-05-24-15 : 42 : 38"
            let expected = Values.DATE_AND_TIME((1989, 05, 24), (15, 42, 38))
            actual_equals_expected (ValuesParser.parse ValuesParser.date_and_time input, expected)
            let input = "date_and_time#1989-05-24-15 : 42 : 38"
            let expected = Values.DATE_AND_TIME((1989, 05, 24), (15, 42, 38))
            actual_equals_expected (ValuesParser.parse ValuesParser.date_and_time input, expected)
            let input = "date_and_time#1989  - 05 - 24   -15 : 42 : 38"
            let expected = Values.DATE_AND_TIME((1989, 05, 24), (15, 42, 38))
            actual_equals_expected (ValuesParser.parse ValuesParser.date_and_time input, expected)
        
        [<Test>]
        member x.``can parse STRING values``() = 
            let input = "\'abc\'"
            let expected = Values.STRING "abc"
            actual_equals_expected (ValuesParser.parse ValuesParser.string input, expected)
        
        [<Test>]
        member x.``can parse WSTRING values``() = 
            let input = "\"abc\""
            let expected = Values.WSTRING "abc"
            actual_equals_expected (ValuesParser.parse ValuesParser.wstring input, expected)
