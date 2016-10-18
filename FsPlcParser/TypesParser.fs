namespace FsPlcParser
open FsPlcModel

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

