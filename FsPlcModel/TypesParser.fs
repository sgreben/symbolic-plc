namespace FsPlcModel

module TypesParser = 
    open Common.Parsers
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives
    open Types
    
    type TypeParser = Parser<Type, unit>
    
    type BasicTypeParser = Parser<Basic, unit>
    
    let intRange : Parser<Range, unit> = parens (pint64 .>> ws .>> dotdot .>>. pint64 .>> ws)
    
    let basicType : BasicTypeParser = 
        choice [ keyword "BOOL" |>> fun _ -> BOOL
                 keyword "BYTE" |>> fun _ -> BYTE
                 keyword "WORD" |>> fun _ -> WORD
                 keyword "DWORD" |>> fun _ -> DWORD
                 keyword "LWORD" |>> fun _ -> LWORD
                 keyword "INT" |>> fun _ -> INT
                 keyword "SINT" |>> fun _ -> SINT
                 keyword "DINT" |>> fun _ -> DINT
                 keyword "LINT" |>> fun _ -> LINT
                 keyword "UINT" |>> fun _ -> UINT
                 keyword "USINT" |>> fun _ -> USINT
                 keyword "UDINT" |>> fun _ -> UDINT
                 keyword "ULINT" |>> fun _ -> ULINT
                 keyword "REAL" |>> fun _ -> REAL
                 keyword "LREAL" |>> fun _ -> LREAL
                 keyword "TIME OF DAY" |>> fun _ -> TIME_OF_DAY
                 keyword "DATE AND TIME" |>> fun _ -> DATE_AND_TIME
                 keyword "TIME" |>> fun _ -> TIME
                 keyword "DATE" |>> fun _ -> DATE
                 keyword "STRING" |>> fun _ -> STRING
                 keyword "WSTRING" |>> fun _ -> WSTRING ]
    
    type GenericTypeParser = Parser<Generic, unit>
    
    let genericType : GenericTypeParser = 
        choice [ keyword "ANY_DERIVED" |>> fun _ -> ANY_DERIVED
                 keyword "ANY_ELEMENTARY" |>> fun _ -> ANY_ELEMENTARY
                 keyword "ANY_MAGNITUDE" |>> fun _ -> ANY_MAGNITUDE
                 keyword "ANY_NUM" |>> fun _ -> ANY_NUM
                 keyword "ANY_INT" |>> fun _ -> ANY_INT
                 keyword "ANY_BIT" |>> fun _ -> ANY_BIT
                 keyword "ANY_STRING" |>> fun _ -> ANY_STRING
                 keyword "ANY_DATE" |>> fun _ -> ANY_DATE
                 keyword "ANY" |>> fun _ -> ANY ]
    
    type EnumTypeParser = Parser<Enum,unit>
    let enumType : EnumTypeParser = parens (sepEndBy1 ident (keyword ",")) |>> set

    type ArrayDimensionParser = Parser<Common.Array_dimension, unit>
    
    let arrayDimension : ArrayDimensionParser = 
        let implode ss = List.toArray ss |> fun s -> System.String s
        brackets (sepBy1 (many digit .>> dotdot .>>. many digit .>> ws) (comma .>> ws)) 
        |>> List.map (fun (l, u) -> System.Int32.Parse(implode l), System.Int32.Parse(implode u))
    
    type ArrayTypeParser = Parser<Array, unit>
    
    let arrayType : TypeParser -> ArrayTypeParser = fun typeParser -> arrayDimension .>>. (keyword "OF" >>. typeParser)
    
    type StructTypeEntryParser = Parser<Common.Identifier * Type, unit>
    
    let structTypeEntry : TypeParser -> StructTypeEntryParser = 
        fun typeParser -> ident .>> ws .>>. (colon >>. ws >>. typeParser) .>> ws
    
    type StructTypeParser = Parser<Struct, unit>
    
    let structType typeParser : StructTypeParser = 
        sepEndBy1 (notFollowedBy (keyword "END_STRUCT") >>. structTypeEntry typeParser) (semicolon .>> ws)
    let anyType, anyTypeRef = createParserForwardedToRef<Type, unit>()
    
    do anyTypeRef := choice [ keyword "STRUCT" >>. structType anyType .>> keyword "END_STRUCT" |>> STRUCT
                              keyword "ARRAY" >>. arrayType anyType |>> ARRAY
                              enumType |>> ENUM
                              basicType .>>. opt intRange |>> function 
                              | (t, Some r) -> BASIC_RANGE(t, r)
                              | (t, None) -> BASIC t
                              qident |>> TYPE_REFERENCE]
    
    let parse = Common.Parsers.parse

module TypeDeclarationParsers = 
    open Common.Parsers
    open FParsec
    open FParsec.Primitives
    open TypeDeclaration
    
    let parse = Common.Parsers.parse
    
    /// Parse a single type declaration "<identifier> : <type>"
    let declaration = 
        ident .>> ws .>> (colon .>> ws) .>>. TypesParser.anyType |>> fun (id, t) -> 
            { id = id
              typ = t }
    
    /// Parse a block "TYPE <type declaration>* END_TYPE"
    let declaration_block = 
        keyword "TYPE" >>. sepEndBy1 (notFollowedBy (keyword "END_TYPE") >>. declaration) (semicolon .>> ws) 
        .>> keyword "END_TYPE"

module TypesParserTests = 
    open FsUnit
    open NUnit
    open NUnit.Core
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
    open FsUnit
    open NUnit
    open NUnit.Core
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
