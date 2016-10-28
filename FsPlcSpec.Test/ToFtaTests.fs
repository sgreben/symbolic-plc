namespace FsPlcSpec.Test

open FsPlcSpec

module ToFtaTests = 
    open NUnit.Framework
    open Spec
    open Fta
    open ToFta
    open FsPlcVm.Representation
    open FsPlcModel
    open StExpressions

    module IL = FsPlcModel.IL
    
    [<TestFixture>]
    type ToFtaTests() = 
        
        let X_gt_10 = AstCmp(Gt, AstVariable(AstDirect("X")), AstLiteral(Values.BASIC (Values.INT 10)))
        let Y_eq_1 = AstCmp(Eq, AstVariable(AstDirect("Y")), AstLiteral(Values.BASIC (Values.INT 0)))

        [<Test>]
        member x.``Can compile {}``() = 
            let fta, env = ToFta.compile Empty
            
            let expected_fta = 
                { initial_state = S 1
                  instructions = 
                      [ STATE(S 1)
                        ASSUME []
                        ST_MOMENT 0
                        JMP(S 0) ] }
            Assert.AreEqual(expected_fta, fta)
        
        [<Test>]
        member x.``Can compile check(X > 0)``() = 
            let fta, env = ToFta.compile (Check(X_gt_10))
            
            let expected_fta = 
                { initial_state = S 1
                  instructions = 
                      [ STATE(S 1)
                        ASSUME []
                        ASSERT [ Value_constraint(X_gt_10) ]
                        ST_MOMENT 0
                        JMP(S 0) ] }
            Assert.AreEqual(expected_fta, fta)
        
        [<Test>]
        member x.``Can compile pass``() = 
            let fta, env = ToFta.compile Pass
            
            let expected_fta = 
                { initial_state = S 1
                  instructions = 
                      [ STATE(S 1)
                        ASSUME []
                        PASS
                        SET_STATE(S 0)
                        NEXT ] }
            Assert.AreEqual(expected_fta, fta)
        
        [<Test>]
        member x.``Can compile {};{}``() = 
            let fta, env = ToFta.compile (Seq(Empty, Empty))
            match fta with
            | {   initial_state = S s1
                  // -> s1 -.  .- s2 -.  .- s3
                  //         s4        s5
                  //       (Empty)  (Empty)
                  instructions =  [ STATE(S s1')
                                    ASSUME []
                                    STATE(S s4)
                                    ASSUME []
                                    ST_MOMENT 0
                                    JMP(S s2)
                                    STATE(S s2')
                                    STATE(S s5)
                                    ASSUME []
                                    ST_MOMENT 0
                                    JMP(S s3)
                                    STATE(S s3')
                                    JMP(S 0) ] } -> 
                            Assert.AreEqual(s1, s1')
                            Assert.AreEqual(s2, s2')
                            Assert.AreEqual(s3, s3')
            | _ -> Assert.Fail()
        
        [<Test>]
        member x.``Can compile constraint(Y = 1) {}``() = 
            let fta, env = ToFta.compile (Constraint(Y_eq_1, Empty))
            
            let expected_fta = 
                { initial_state = S 1
                  instructions = 
                      [ STATE(S 1)
                        ASSUME [ Value_constraint(Y_eq_1) ]
                        ST_MOMENT 0
                        JMP(S 0) ] }
            Assert.AreEqual(expected_fta, fta)
        
        [<Test>]
        member x.``Can compile delay(>=t) {}``() = 
            let fta, env = ToFta.compile (Delay(Time_ge 10, Empty))
            let expected_fta = 
                { initial_state = S 1
                  // (s1 @ t0: v1 := t0) -> (s2: assume(t < v1+10)) --. 
                  //                           |                 A    |
                  //                           |                 '....'
                  //                           '-> (s3: assume(t >= v1+10))
                  //                                '-> (s5 @ t1)
                  //                                   '-> (s4)
                  instructions = 
                      [ STATE(S 1)
                        ASSUME []
                        ST_MOMENT 0
                        LD_TIME(V 1)
                        STATE(S 2)
                        NONDET(S 3)
                        ASSUME [ Time_constraint(V 1, IL.LT, 10) ]
                        NEXT
                        STATE(S 3)
                        ASSUME [ Time_constraint(V 1, IL.GE, 10) ]
                        JMP(S 5)
                        STATE(S 5)
                        ASSUME []
                        ST_MOMENT 1
                        JMP(S 4)
                        STATE(S 4)
                        ASSUME []
                        SET_STATE(S 0)
                        JMP(S 0) ] }
            Assert.AreEqual(expected_fta, fta)
