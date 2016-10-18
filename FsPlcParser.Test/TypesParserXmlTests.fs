namespace FsPlcParser.Test
open FsPlcParser
open FsPlcModel

module TypeParsersXmlTests = 
    open NUnit
    open NUnit.Framework
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type BasicTypeTests() = 
        let parse = TypesParserXml.parse TypesParserXml.basicType
        
        [<Test>]
        member x.``can parse BOOL``() = actual_equals_expected (parse "<BOOL />", Types.BOOL)
        
        [<Test>]
        member x.``can parse BYTE``() = actual_equals_expected (parse "<BYTE />", Types.BYTE)
        
        [<Test>]
        member x.``can parse WORD``() = actual_equals_expected (parse "<WORD />", Types.WORD)
        
        [<Test>]
        member x.``can parse DWORD``() = actual_equals_expected (parse "<DWORD />", Types.DWORD)
        
        [<Test>]
        member x.``can parse LWORD``() = actual_equals_expected (parse "<LWORD />", Types.LWORD)
        
        [<Test>]
        member x.``can parse INT``() = actual_equals_expected (parse "<INT />", Types.INT)
        
        [<Test>]
        member x.``can parse SINT``() = actual_equals_expected (parse "<SINT />", Types.SINT)
        
        [<Test>]
        member x.``can parse DINT``() = actual_equals_expected (parse "<DINT />", Types.DINT)
        
        [<Test>]
        member x.``can parse LINT``() = actual_equals_expected (parse "<LINT />", Types.LINT)
        
        [<Test>]
        member x.``can parse UINT``() = actual_equals_expected (parse "<UINT />", Types.UINT)
        
        [<Test>]
        member x.``can parse USINT``() = actual_equals_expected (parse "<USINT />", Types.USINT)
        
        [<Test>]
        member x.``can parse UDINT``() = actual_equals_expected (parse "<UDINT />", Types.UDINT)
        
        [<Test>]
        member x.``can parse ULINT``() = actual_equals_expected (parse "<ULINT />", Types.ULINT)
        
        [<Test>]
        member x.``can parse REAL``() = actual_equals_expected (parse "<REAL />", Types.REAL)
        
        [<Test>]
        member x.``can parse LREAL``() = actual_equals_expected (parse "<LREAL />", Types.LREAL)
        
        [<Test>]
        member x.``can parse TIME``() = actual_equals_expected (parse "<TIME />", Types.TIME)
        
        [<Test>]
        member x.``can parse DATE``() = actual_equals_expected (parse "<DATE />", Types.DATE)
        
        [<Test>]
        member x.``can parse TIME OF DAY``() = actual_equals_expected (parse "<TOD />", Types.TIME_OF_DAY)
        
        [<Test>]
        member x.``can parse DATE AND TIME``() = actual_equals_expected (parse "<DT />", Types.DATE_AND_TIME)
        
        [<Test>]
        member x.``can parse STRING``() = actual_equals_expected (parse "<STRING />", Types.STRING)
        
        [<Test>]
        member x.``can parse WSTRING``() = actual_equals_expected (parse "<WSTRING />", Types.WSTRING)
    
    [<TestFixture>]
    type RangedBasicTypeTests() =
        let parse = TypesParserXml.parse TypesParserXml.anyType

        [<Test>]
        member x.``can parse ranged UINT``() = 
            let input = """<subrangeUnsigned xmlns="http://www.plcopen.org/xml/tc6_0200"><range lower="0" upper="10"/><baseType><UINT /></baseType></subrangeUnsigned>"""
            let expected = Types.BASIC_RANGE (Types.UINT,(0L,10L))
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse signed ranged INT``() = 
            let input = """<subrangeSigned xmlns="http://www.plcopen.org/xml/tc6_0200"><range lower="-100" upper="100"/><baseType><INT/></baseType></subrangeSigned>"""
            let expected = Types.BASIC_RANGE (Types.INT,(-100L,100L))
            actual_equals_expected (parse input, expected)
        
    
    [<TestFixture>]
    type ArrayTypeTests() = 
        let parse = TypesParserXml.parse TypesParserXml.anyType
        
        [<Test>]
        member x.``can parse ARRAY [1..100] OF TIME``() = 
            let input = 
                """<array xmlns="http://www.plcopen.org/xml/tc6_0200"> <dimension lower="1" upper="100"/><baseType><TIME/></baseType></array>"""
            let expected = Types.ARRAY([ 1, 100 ], Types.BASIC Types.TIME)
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse ARRAY [1..100] OF MyCustomType``() = 
            let input = 
                """<array xmlns="http://www.plcopen.org/xml/tc6_0200"> <dimension lower="1" upper="100"/><baseType><derived name="MyCustomType"/></baseType></array>"""
            let expected = Types.ARRAY([ 1, 100 ], Types.TYPE_REFERENCE ["MyCustomType"])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse ARRAY [1..100,5..25] OF MyCustomType``() = 
            let input = 
                """<array xmlns="http://www.plcopen.org/xml/tc6_0200"> <dimension lower="1" upper="100"/><dimension lower="5" upper="25"/><baseType><derived name="MyCustomType"/></baseType></array>"""
            
            let expected = 
                Types.ARRAY([ 1, 100
                              5, 25 ], Types.TYPE_REFERENCE ["MyCustomType"])
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type StructTypeTests() = 
        let parse = TypesParserXml.parse TypesParserXml.anyType
        
        [<Test>]
        member x.``can parse MotorDaten struct``() = 
            let input = """<struct xmlns="http://www.plcopen.org/xml/tc6_0200">
                                <variable name="Zylinder"><type><USINT/></type></variable>
                                <variable name="Leistung"><type><REAL/></type></variable>
                           </struct>"""
            
            let expected = 
                Types.STRUCT([ "Zylinder", Types.BASIC Types.USINT
                               "Leistung", Types.BASIC Types.REAL ])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse FahrzeugDaten struct``() = 
            let input = """<struct xmlns="http://www.plcopen.org/xml/tc6_0200">
                                <variable name="Motor"><type><derived name="MotorDaten"/></type></variable>
                                <variable name="Gewicht"><type><REAL/></type></variable>
                                <variable name="Kilometer"><type><REAL/></type></variable>
                                <variable name="Zulassung"><type><BOOL/></type></variable>
                           </struct>"""
            
            let expected = 
                Types.STRUCT([ "Motor", Types.TYPE_REFERENCE ["MotorDaten"]
                               "Gewicht", Types.BASIC Types.REAL
                               "Kilometer", Types.BASIC Types.REAL
                               "Zulassung", Types.BASIC Types.BOOL ])
            actual_equals_expected (parse input, expected)