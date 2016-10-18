namespace FsPlcParser.Test
open FsPlcParser
open FsPlcModel

module TypesParserTests = 
    open NUnit
    open NUnit.Framework
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type StructTypeEntryTests() = 
        let parse = TypesParser.parse (TypesParser.structTypeEntry TypesParser.anyType)
        let parseMany = TypesParser.parse (TypesParser.structType TypesParser.anyType)
        
        [<Test>]
        member x.``can parse Zylinder : USINT``() = 
            let input = """Zylinder : USINT"""
            let expected = ("Zylinder", Types.BASIC Types.USINT)
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse Leistung : REAL``() = 
            let input = """Leistung : REAL"""
            let expected = ("Leistung", Types.BASIC Types.REAL)
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse Zylinder : USINT; Leistung : REAL;``() = 
            let input = """Zylinder : USINT; Leistung : REAL;"""
            
            let expected = 
                [ "Zylinder", Types.BASIC Types.USINT
                  "Leistung", Types.BASIC Types.REAL ]
            actual_equals_expected (parseMany input, expected)
        
        [<Test>]
        member x.``can parse Zylinder : USINT;\n\t Leistung : REAL;``() = 
            let input = """Zylinder : USINT;
                           Leistung : REAL;"""
            
            let expected = 
                [ "Zylinder", Types.BASIC Types.USINT
                  "Leistung", Types.BASIC Types.REAL ]
            actual_equals_expected (parseMany input, expected)
    
    [<TestFixture>]
    type ArrayDimensionTests() = 
        let parse = TypesParser.parse TypesParser.arrayDimension
        
        [<Test>]
        member x.``can parse [ 1..100 ]``() = 
            let input = "[ 1..100 ]"
            let expected = [ 1, 100 ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse [1..100]``() = 
            let input = "[1..100]"
            let expected = [ 1, 100 ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse [ 1..100, 5..25 ]``() = 
            let input = "[ 1..100, 5..25 ]"
            
            let expected = 
                [ 1, 100
                  5, 25 ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse [ 1..100,5..25 ]``() = 
            let input = "[ 1..100,5..25 ]"
            
            let expected = 
                [ 1, 100
                  5, 25 ]
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type ArrayTypeTests() = 
        let parse = TypesParser.parse TypesParser.anyType
        
        [<Test>]
        member x.``can parse ARRAY [1..100] OF TIME``() = 
            let input = """ARRAY [ 1..100 ] OF TIME"""
            let expected = Types.ARRAY([ 1, 100 ], Types.BASIC Types.TIME)
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse ARRAY [1..100] OF MyCustomType``() = 
            let input = """ARRAY [1..100] OF MyCustomType"""
            let expected = Types.ARRAY([ 1, 100 ], Types.TYPE_REFERENCE ["MyCustomType"])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse ARRAY [1..100,5..25] OF MyCustomType``() = 
            let input = """ARRAY [1..100,5..25] OF MyCustomType"""
            
            let expected = 
                Types.ARRAY([ 1, 100
                              5, 25 ], Types.TYPE_REFERENCE ["MyCustomType"])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse ARRAY [1..100] OF ARRAY [10..100] OF MyCustomType``() = 
            let input = """ARRAY [1..100] OF ARRAY [10..100] OF MyCustomType"""
            let expected = Types.ARRAY([ 1, 100 ], Types.ARRAY([ 10, 100 ], Types.TYPE_REFERENCE ["MyCustomType"]))
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type StructTypeTests() = 
        let parse = TypesParser.parse TypesParser.anyType
        
        [<Test>]
        member x.``can parse MotorDaten struct``() = 
            let input = """STRUCT
                                Zylinder : USINT;
                                Leistung : REAL;
                           END_STRUCT"""
            
            let expected = 
                Types.STRUCT([ "Zylinder", Types.BASIC Types.USINT
                               "Leistung", Types.BASIC Types.REAL ])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse ranged MotorDaten struct``() = 
            let input = """STRUCT
                                Zylinder : USINT (1..10);
                                Leistung : REAL (-1000..1000);
                           END_STRUCT"""
            
            let expected = 
                Types.STRUCT([ "Zylinder", Types.BASIC_RANGE(Types.USINT, (1L, 10L))
                               "Leistung", Types.BASIC_RANGE(Types.REAL, (-1000L, 1000L)) ])
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse FahrzeugDaten struct``() = 
            let input = """STRUCT
                                Motor : MotorDaten;
                                Gewicht : REAL;
                                Kilometer : REAL;
                                Zulassung : BOOL;
                           END_STRUCT"""
            
            let expected = 
                Types.STRUCT([ "Motor", Types.TYPE_REFERENCE ["MotorDaten"]
                               "Gewicht", Types.BASIC Types.REAL
                               "Kilometer", Types.BASIC Types.REAL
                               "Zulassung", Types.BASIC Types.BOOL ])
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type BasicTypeTests() = 
        let parse = TypesParser.parse TypesParser.basicType
        
        [<Test>]
        member x.``can parse BOOL``() = actual_equals_expected (parse "BOOL", Types.BOOL)
        
        [<Test>]
        member x.``can parse BYTE``() = actual_equals_expected (parse "BYTE", Types.BYTE)
        
        [<Test>]
        member x.``can parse WORD``() = actual_equals_expected (parse "WORD", Types.WORD)
        
        [<Test>]
        member x.``can parse DWORD``() = actual_equals_expected (parse "DWORD", Types.DWORD)
        
        [<Test>]
        member x.``can parse LWORD``() = actual_equals_expected (parse "LWORD", Types.LWORD)
        
        [<Test>]
        member x.``can parse INT``() = actual_equals_expected (parse "INT", Types.INT)
        
        [<Test>]
        member x.``can parse SINT``() = actual_equals_expected (parse "SINT", Types.SINT)
        
        [<Test>]
        member x.``can parse DINT``() = actual_equals_expected (parse "DINT", Types.DINT)
        
        [<Test>]
        member x.``can parse LINT``() = actual_equals_expected (parse "LINT", Types.LINT)
        
        [<Test>]
        member x.``can parse UINT``() = actual_equals_expected (parse "UINT", Types.UINT)
        
        [<Test>]
        member x.``can parse USINT``() = actual_equals_expected (parse "USINT", Types.USINT)
        
        [<Test>]
        member x.``can parse UDINT``() = actual_equals_expected (parse "UDINT", Types.UDINT)
        
        [<Test>]
        member x.``can parse ULINT``() = actual_equals_expected (parse "ULINT", Types.ULINT)
        
        [<Test>]
        member x.``can parse REAL``() = actual_equals_expected (parse "REAL", Types.REAL)
        
        [<Test>]
        member x.``can parse LREAL``() = actual_equals_expected (parse "LREAL", Types.LREAL)
        
        [<Test>]
        member x.``can parse TIME``() = actual_equals_expected (parse "TIME", Types.TIME)
        
        [<Test>]
        member x.``can parse DATE``() = actual_equals_expected (parse "DATE", Types.DATE)
        
        [<Test>]
        member x.``can parse TIME OF DAY``() = actual_equals_expected (parse "TIME OF DAY", Types.TIME_OF_DAY)
        
        [<Test>]
        member x.``can parse DATE AND TIME``() = actual_equals_expected (parse "DATE AND TIME", Types.DATE_AND_TIME)
        
        [<Test>]
        member x.``can parse STRING``() = actual_equals_expected (parse "STRING", Types.STRING)
        
        [<Test>]
        member x.``can parse WSTRING``() = actual_equals_expected (parse "WSTRING", Types.WSTRING)
    
    [<TestFixture>]
    type GenericTypeTests() = 
        let parse = TypesParser.parse TypesParser.genericType
        
        [<Test>]
        member x.``can parse ANY``() = actual_equals_expected (parse "ANY", Types.ANY)
        
        [<Test>]
        member x.``can parse ANY_DERIVED``() = actual_equals_expected (parse "ANY_DERIVED", Types.ANY_DERIVED)
        
        [<Test>]
        member x.``can parse ANY_ELEMENTARY``() = actual_equals_expected (parse "ANY_ELEMENTARY", Types.ANY_ELEMENTARY)
        
        [<Test>]
        member x.``can parse ANY_MAGNITUDE``() = actual_equals_expected (parse "ANY_MAGNITUDE", Types.ANY_MAGNITUDE)
        
        [<Test>]
        member x.``can parse ANY_NUM``() = actual_equals_expected (parse "ANY_NUM", Types.ANY_NUM)
        
        [<Test>]
        member x.``can parse ANY_INT``() = actual_equals_expected (parse "ANY_INT", Types.ANY_INT)
        
        [<Test>]
        member x.``can parse ANY_BIT``() = actual_equals_expected (parse "ANY_BIT", Types.ANY_BIT)
        
        [<Test>]
        member x.``can parse ANY_STRING``() = actual_equals_expected (parse "ANY_STRING", Types.ANY_STRING)
        
        [<Test>]
        member x.``can parse ANY_DATE``() = actual_equals_expected (parse "ANY_DATE", Types.ANY_DATE)

module TypeDeclParserTests = 
    open NUnit
    open NUnit.Framework
    open TypeDeclaration
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type DeclarationTests() = 
        let parse = TypeDeclarationParsers.parse TypeDeclarationParsers.declaration
        
        [<Test>]
        member x.``can parse Zylinder : USINT``() = 
            let input = """Zylinder : USINT"""
            
            let expected = 
                { id = "Zylinder"
                  typ = Types.BASIC Types.USINT }
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse Leistung : REAL``() = 
            let input = """Leistung : REAL"""
            
            let expected = 
                { id = "Leistung"
                  typ = Types.BASIC Types.REAL }
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type DeclarationBlockTests() = 
        let parse = TypeDeclarationParsers.parse TypeDeclarationParsers.declaration_block
        
        [<Test>]
        member x.``can parse TYPE Zylinder : USINT; Leistung: REAL; END_TYPE``() = 
            let input = """TYPE Zylinder : USINT; Leistung: REAL; END_TYPE"""
            
            let expected = 
                [ { id = "Zylinder"
                    typ = Types.BASIC Types.USINT }
                  { id = "Leistung"
                    typ = Types.BASIC Types.REAL } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse TYPE MotorDaten : STRUCT Zylinder : USINT; Leistung: REAL; END_STRUCT; END_TYPE``() = 
            let input = """TYPE MotorDaten : STRUCT Zylinder : USINT; Leistung: REAL; END_STRUCT; END_TYPE"""
            
            let expected = 
                [ { id = "MotorDaten"
                    typ = 
                        Types.STRUCT([ "Zylinder", Types.BASIC Types.USINT
                                       "Leistung", Types.BASIC Types.REAL ]) } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse MotorState struct``() = 
            let input = """TYPE
	                            LimitedRevol : UINT (0..230);
	                            TypLevel : (Idling,SpeedUp1,SpeedUp2,MaxPower);
	                            MotorState:
	                            STRUCT
		                            Revolutions:LimitedRevol;
		                            Level:TypLevel;
		                            MaxReached:BOOL;
		                            Failure:BOOL;
		                            Brake:BYTE;
	                            END_STRUCT;
                            END_TYPE"""
            
            let expected = 
                [ { id = "LimitedRevol"
                    typ = Types.BASIC_RANGE(Types.UINT, (0L, 230L)) }
                  { id = "TypLevel"
                    typ = Types.ENUM(set [ "Idling"; "SpeedUp1"; "SpeedUp2"; "MaxPower" ]) }
                  { id = "MotorState"
                    typ = 
                        Types.STRUCT [ "Revolutions", Types.TYPE_REFERENCE ["LimitedRevol"]
                                       "Level", Types.TYPE_REFERENCE ["TypLevel"]
                                       "MaxReached", Types.BASIC Types.BOOL
                                       "Failure", Types.BASIC Types.BOOL
                                       "Brake", Types.BASIC Types.BYTE ] } ]
            actual_equals_expected (parse input, expected)
