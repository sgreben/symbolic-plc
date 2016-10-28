namespace FsPlcSpecParser.Test

open FsPlcSpec
open FsPlcSpecParser

module SpecParserTests = 
    open NUnit.Framework
    open FsPlcModel
    open StExpressions
    open Spec
    open Values
    
    [<TestFixture>]
    type Tests() = 
        
        let parsed_as_expected input expected = 
            let actual = SpecParser.parse input
            try 
                Assert.AreEqual(expected, actual)
            with e -> 
                printfn "%A" actual
                raise e
        
        [<Test>]
        member x.``Can parse {}``() = 
            let input = "{}"
            let expected = Empty
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse pass``() = 
            let input = "pass"
            let expected = Pass
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse {pass}``() = 
            let input = "{pass}"
            let expected = Pass
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse check(X>=0)``() = 
            let input = "check(X >= 0)"
            let expected = Check(AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy))))
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse constraint(X>=0) {}``() = 
            let input = "constraint(X >= 0) {}"
            let expected = Constraint(AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy))), Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse constraint(X>=0) {pass}``() = 
            let input = "constraint(X >= 0) {pass}"
            let expected = Constraint(AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy))), Pass)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse pass; pass``() = 
            let input = "pass; pass"
            let expected = Seq(Pass, Pass)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse {}; {}``() = 
            let input = "{}; {}"
            let expected = Seq(Empty, Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse {}; pass``() = 
            let input = "{}; pass"
            let expected = Seq(Empty, Pass)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse {}; pass; {}``() = 
            let input = "{}; pass; {}"
            let expected = Seq(Seq(Empty, Pass), Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse constraint(X>=0) {pass}; pass; {}``() = 
            let input = "constraint(X >= 0) {pass}; pass; {} "
            let expected = 
                Seq
                    (Seq(Constraint(AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy))), Pass), Pass), 
                     Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse constraint(X>=0) /* comment */ {pass}; pass; {}``() = 
            let input = "constraint(X >= 0) /* comment */ {pass}; pass; {}"
            let expected = 
                Seq
                    (Seq(Constraint(AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy))), Pass), Pass), 
                     Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse constraint(X >= 0) {check(Y>=0);};``() = 
            let input = "constraint(X >= 0) {check(Y>=0);}; "
            let expected = 
                Constraint
                    (AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy))), 
                     Check(AstCmp(Geq, AstVariable(AstDirect("Y")), AstLiteral(BASIC(BYTE 0uy)))))
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse within(<=100ms) {}``() = 
            let input = "within(<=100ms) {}"
            let expected = Within(Time_le 100, Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse within(>= 100ms) {}``() = 
            let input = "within(>= 100ms) {}"
            let expected = Within(Time_ge 100, Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse within(<=100s) {}``() = 
            let input = "within(<=100s) {}"
            let expected = Within(Time_le 100000, Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse delay(<=100ms) {}``() = 
            let input = "delay(<=100ms) {}"
            let expected = Delay(Time_le 100, Empty)
            parsed_as_expected input expected
        
        [<Test>]
        member x.``Can parse delay(<=100ms) {}; within(<=100ms){check(X>=0); pass};``() = 
            let input = "delay(<=100ms) {}; within(<=100ms){check(X>=0); pass};"
            let expected = 
                Seq
                    (Delay(Time_le 100, Empty), 
                     Within
                         (Time_le 100, 
                          Seq(Check(AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy)))), Pass)))
            parsed_as_expected input expected

        [<Test>]
        member x.``Can parse delay(<=100ms) {}; within(<=100ms){check(X>=0); pass}; with newlines``() = 
            let input = """
                delay(<=100ms) {}; 
                /* a
                 * multiline
                 * comment
                 */
                within(<=100ms){
                    check(X>=0); 
                    pass
                };"""
            let expected = 
                Seq
                    (Delay(Time_le 100, Empty), 
                     Within
                         (Time_le 100, 
                          Seq(Check(AstCmp(Geq, AstVariable(AstDirect("X")), AstLiteral(BASIC(BYTE 0uy)))), Pass)))
            parsed_as_expected input expected
