namespace FsPlcModel

module DeclarationsParser = 
    open Common.Parsers
    open FParsec
    open FParsec.Primitives
    open Declarations
    
    type VariableKindParser = Parser<Kind, unit>
    
    type VariableAttributeParser = Parser<Attribute, unit>
    
    let parse = Common.Parsers.parse
    
    let kind : VariableKindParser = 
        choice [ keyword "VAR_INPUT" |>> fun _ -> VAR_INPUT
                 keyword "VAR_OUTPUT" |>> fun _ -> VAR_OUTPUT
                 keyword "VAR_IN_OUT" |>> fun _ -> VAR_IN_OUT
                 keyword "VAR_EXTERNAL" |>> fun _ -> VAR_EXTERNAL
                 keyword "VAR_GLOBAL" |>> fun _ -> VAR_GLOBAL
                 keyword "VAR_ACCESS" |>> fun _ -> VAR_ACCESS
                 keyword "VAR_TEMP" |>> fun _ -> VAR_TEMP
                 keyword "VAR" |>> fun _ -> VAR ]
    
    let attribute : VariableAttributeParser = 
        choice [ keyword "RETAIN" |>> fun _ -> RETAIN
                 keyword "NON_RETAIN" |>> fun _ -> NON_RETAIN
                 keyword "CONSTANT" |>> fun _ -> CONSTANT
                 keyword "R_EDGE" |>> fun _ -> R_EDGE
                 keyword "F_EDGE" |>> fun _ -> F_EDGE
                 keyword "READ_ONLY" |>> fun _ -> READ_ONLY
                 keyword "READ_WRITE" |>> fun _ -> READ_WRITE ]
    
    let attributes = sepBy attribute spaces1
    /// Parse a single variable declaration "<identifier> : <type> [:= <value>]" of the given kind
    let declaration k a = 
        ident .>> ws .>> colon .>> ws >>= fun id -> 
            choice [ (attempt (TypesParser.anyType .>> keyword ":=")) 
                     >>= (fun t -> ValuesParser.value t |>> (fun v -> t, Some v))
                     TypesParser.anyType .>> ws |>> (fun t -> t, None) ] .>>. attributes |>> (fun ((t, v), a') -> 
            { kind = k
              attr = a@a'
              id = id
              typ = t
              ivalue = v })
    
    /// Parse a variable declaration block "VAR[_<KIND>] <declaration>* END_VAR".
    let declaration_block = 
        kind .>>. attributes >>= fun (k, a) -> 
            let declaration = declaration k a
            sepEndBy1 (notFollowedBy (keyword "END_VAR") >>. declaration) (semicolon .>> ws) .>> keyword "END_VAR"

module DeclarationsParserTests = 
    open FsUnit
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    open Declarations
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type DeclarationTests() = 
        let parse = DeclarationsParser.parse DeclarationsParser.declaration_block
        
        [<Test>]
        member x.``can parse VAR CONSTANT Modus:BOOL := false;END_VAR``() = 
            let input = "VAR CONSTANT Modus : BOOL := false; END_VAR"
            
            let expected = 
                [ { kind = VAR
                    id = "Modus"
                    attr = [ CONSTANT ]
                    typ = Types.BASIC Types.BOOL
                    ivalue = Some(Values.BASIC (Values.BOOL false)) } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse VAR_INPUT Druck:REAL := 12.2; END VAR``() = 
            let input = "VAR_INPUT Druck : REAL := 12.2; END_VAR"
            
            let expected = 
                [ { kind = VAR_INPUT
                    id = "Druck"
                    attr = []
                    typ = Types.BASIC Types.REAL
                    ivalue = Some(Values.BASIC (Values.REAL 12.2)) } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse VAR_INPUT RETAIN Druck : REAL := 12.2; END VAR``() = 
            let input = "VAR_INPUT RETAIN Druck : REAL := 12.2; END_VAR"
            
            let expected = 
                [ { kind = VAR_INPUT
                    id = "Druck"
                    attr = [ RETAIN ]
                    typ = Types.BASIC Types.REAL
                    ivalue = Some(Values.BASIC (Values.REAL 12.2)) } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``VAR_INPUT RETAIN Druck : REAL := 12.2; Pumpe : BOOL; Motor:MotorDaten; END_VAR``() = 
            let input = """VAR_INPUT 
                              RETAIN Druck : REAL := 12.2; 
                              Pumpe : BOOL; 
                              Motor:MotorDaten; 
                           END_VAR"""
            
            let expected = 
                [ { kind = VAR_INPUT
                    id = "Druck"
                    attr = [ RETAIN ]
                    typ = Types.BASIC Types.REAL
                    ivalue = Some(Values.BASIC (Values.REAL 12.2)) }
                  { kind = VAR_INPUT
                    id = "Pumpe"
                    attr = [ RETAIN ]
                    typ = Types.BASIC Types.BOOL
                    ivalue = None }
                  { kind = VAR_INPUT
                    id = "Motor"
                    attr = [ RETAIN ]
                    typ = Types.TYPE_REFERENCE ["MotorDaten"]
                    ivalue = None } ]
            actual_equals_expected (parse input, expected)

        [<Test>]
        member x.``VAR_INPUT RETAIN Druck : REAL := 12.2; Pumpe : BOOL F_EDGE; Motor:MotorDaten; END_VAR``() = 
            let input = """VAR_INPUT 
                              RETAIN Druck : REAL := 12.2; 
                              Pumpe : BOOL F_EDGE; 
                              Motor:MotorDaten; 
                           END_VAR"""
            
            let expected = 
                [ { kind = VAR_INPUT
                    id = "Druck"
                    attr = [ RETAIN ]
                    typ = Types.BASIC Types.REAL
                    ivalue = Some(Values.BASIC (Values.REAL 12.2)) }
                  { kind = VAR_INPUT
                    id = "Pumpe"
                    attr = [ RETAIN; F_EDGE ]
                    typ = Types.BASIC Types.BOOL
                    ivalue = None }
                  { kind = VAR_INPUT
                    id = "Motor"
                    attr = [ RETAIN ]
                    typ = Types.TYPE_REFERENCE ["MotorDaten"]
                    ivalue = None } ]
            actual_equals_expected (parse input, expected)
