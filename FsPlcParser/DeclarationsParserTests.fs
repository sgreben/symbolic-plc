namespace FsPlcParser
open FsPlcModel

module DeclarationsParserTests = 
    open NUnit
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
                    ivalue = Some(Values.BASIC(Values.BOOL false)) } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse VAR_INPUT Druck:REAL := 12.2; END VAR``() = 
            let input = "VAR_INPUT Druck : REAL := 12.2; END_VAR"
            
            let expected = 
                [ { kind = VAR_INPUT
                    id = "Druck"
                    attr = []
                    typ = Types.BASIC Types.REAL
                    ivalue = Some(Values.BASIC(Values.REAL 12.2)) } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse VAR_INPUT RETAIN Druck : REAL := 12.2; END VAR``() = 
            let input = "VAR_INPUT RETAIN Druck : REAL := 12.2; END_VAR"
            
            let expected = 
                [ { kind = VAR_INPUT
                    id = "Druck"
                    attr = [ RETAIN ]
                    typ = Types.BASIC Types.REAL
                    ivalue = Some(Values.BASIC(Values.REAL 12.2)) } ]
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
                    ivalue = Some(Values.BASIC(Values.REAL 12.2)) }
                  { kind = VAR_INPUT
                    id = "Pumpe"
                    attr = [ RETAIN ]
                    typ = Types.BASIC Types.BOOL
                    ivalue = None }
                  { kind = VAR_INPUT
                    id = "Motor"
                    attr = [ RETAIN ]
                    typ = Types.TYPE_REFERENCE [ "MotorDaten" ]
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
                    ivalue = Some(Values.BASIC(Values.REAL 12.2)) }
                  { kind = VAR_INPUT
                    id = "Pumpe"
                    attr = [ RETAIN; F_EDGE ]
                    typ = Types.BASIC Types.BOOL
                    ivalue = None }
                  { kind = VAR_INPUT
                    id = "Motor"
                    attr = [ RETAIN ]
                    typ = Types.TYPE_REFERENCE [ "MotorDaten" ]
                    ivalue = None } ]
            actual_equals_expected (parse input, expected)
