module AnalysisTests
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    open FsPlcModel.IL
    open FsPlcVm
    open Language
    open Representation
    open States
    open Analysis
    open FsCheck
    open FsCheck.NUnit

    
    type Z3Generators = 
        static member BoolExpr() = 
            { new Arbitrary<Microsoft.Z3.BoolExpr>() with
                  
                  member x.Generator = 
                      Gen.oneof [ gen { return SMT.Const.mk_false }
                                  gen { return SMT.Const.mk_true } ]
                  
                  member x.Shrinker _ = Seq.empty }
    
    [<TestFixture>]
    type Tests() = 
        
        [<TestFixtureSetUp>]
        member t.SetUp() = ignore <| Arb.register<Z3Generators>()
        
        [<Property>]
        member x.``Bactrack from empty`` (steps : Step list) = 
            let p = new ResizeArray<Step>([])
            let q = new ResizeArray<Step>(steps)
            let bt = backtrack' p q
            bt.pop = 0 && bt.push = q.Count
        
        [<Property(MaxTest = 500)>]
        member x.``Backtrack correctness`` (prefix : Step list) (push : Step list) = 
            let pop = push |> List.map (fun s -> { s with id = s.id + 1UL })
            let p = new ResizeArray<Step>(prefix @ pop)
            let q = new ResizeArray<Step>(prefix @ push)
            let bt = Analysis.backtrack' p q
            let pop = Array.ofList pop
            let actual_pop = p.GetRange(p.Count - bt.pop, bt.pop).ToArray()
            let actual = 
                Array.append (p.GetRange(0, p.Count - bt.pop).ToArray()) 
                    (q.GetRange(q.Count - bt.push, bt.push).ToArray())
            let expected = q.ToArray()
            let same_array (actual : Step []) (expected : Step []) = 
                actual.Length = expected.Length && Array.forall2 (fun a b -> a.id = b.id) actual expected
            same_array actual expected && same_array actual_pop pop
        
        [<Property>]
        member x.``Fully symbolic states have two guarded successors`` (v : int64) = 
            let v = INT v
            let v' = SYM(SYM_INT(SMT.Var.mk_int "v"))
            
            let code = 
                [| LOADOP_IMM(LD, VBasic v)
                   CMPOP_IMM(EQ, v')
                   JMPOP(JMPC, 0) |]
            
            let vm_state = 
                States.initial (Map.ofList [ 0, Value(VBasic v)
                                             1, Value(VBasic v') ])
            
            let istate = Analysis.initial vm_state
            match Analysis.straightline 3 istate code with
            | Instructions.S2G(s, b, s', b') -> true
            | _ -> false
        
        [<Property>]
        member x.``Fully symbolic states have two guarded successors (2)`` (v : int64) = 
            let v = INT v
            let v' = SYM(SYM_INT(SMT.Var.mk_int "v"))
            let v'' = SYM(SYM_INT(SMT.Var.mk_int "v2"))
            
            let code = 
                [| LOADOP_IMM(LD, VBasic v)
                   ABINOP_IMM(ADD, v'')
                   CMPOP_IMM(EQ, v')
                   JMPOP(JMPC, 0) |]
            
            let vm_state = 
                States.initial (Map.ofList [ 0, Value(VBasic v)
                                             1, Value(VBasic v') ])
            
            let istate = Analysis.initial vm_state
            match Analysis.straightline 4 istate code with
            | Instructions.S2G(s, b, s', b') -> true
            | _ -> false
        
        [<Test>]
        member x.``Trivial counterexample is found``() = 
            let code = 
                [| LOADOP_IMM(LD, VBasic(BOOL true))
                   MONITOROP ASSUME
                   LOADOP_IMM(LD, VBasic(BOOL false))
                   JMPOP(JMPC, 6)
                   MONITOROP FAIL
                   NOP |]
            
            let vm_state = States.initial Map.empty
            let istate = Analysis.initial vm_state
            let queue = Scheduler.Bfs.empty()
            let schedule = Scheduler.Bfs.schedule queue
            let deschedule = Scheduler.Bfs.deschedule queue
            schedule istate
            try 
                ignore (Analysis.run code schedule deschedule)
                Assert.Fail()
            with Analysis.Fail s -> Assert.Pass()
        
        [<Test>]
        member x.``Symbolic counterexample is found``() = 
            let code = 
                [| OSOP(READ_INPUT(TINT, 0))
                   LOADOP_IMM(LD, VBasic(BOOL true))
                   MONITOROP ASSUME
                   LOADOP_IMM(LD, VBasic(BOOL false))
                   JMPOP(JMPC, 6)
                   MONITOROP FAIL
                   NOP |]
            
            let vm_state = States.initial (Map.ofList [ 0, Value(VBasic(INT 0L)) ])
            let istate = Analysis.initial vm_state
            let queue = Scheduler.Bfs.empty()
            let schedule = Scheduler.Bfs.schedule queue
            let deschedule = Scheduler.Bfs.deschedule queue
            schedule istate
            try 
                ignore (Analysis.run code schedule deschedule)
                Assert.Fail()
            with Analysis.Fail s -> Assert.Pass()
        
        [<Test>]
        member x.``Count to 100_000``() = 
            let _break = 17
            let _loop = 5
            
            let code = 
                [| NOP
                   NOP
                   NOP
                   NOP
                   NOP
                   LOADOP_REF(LD, 0)
                   CMPOP_IMM(LE, INT 0L)
                   JMPOP(JMPC, _break)
                   NOP
                   NOP
                   NOP
                   NOP
                   NOP
                   LOADOP_REF(LD, 0)
                   ABINOP_IMM(SUB, INT 1L)
                   STOREOP_IMM(ST, 0)
                   JMPOP(JMP, _loop)
                   NOP
                   MONITOROP FAIL |]
            
            let vm_state = 
                States.initial (Map.ofList [ 0, Value(VBasic(INT 100000L))
                                             1, Value(VBasic(INT 0L))
                                             2, Value(VBasic(INT 1L))
                                             3, Value(VBasic(INT 0L)) ])
            
            let istate = Analysis.initial vm_state
            let queue = Scheduler.Bfs.empty()
            let schedule = Scheduler.Bfs.schedule queue
            let deschedule = Scheduler.Bfs.deschedule queue
            schedule istate
            try 
                ignore (Analysis.run code schedule deschedule)
                Assert.Fail()
            with Analysis.Fail s -> 
                printf "%A" s.vm_state.cr
                Assert.Pass()
        
        [<Test>]
        member x.``Fibonacci#42``() = 
            let _break = 17
            let _loop = 5
            
            let code = 
                [| LOADOP_IMM(LD, VBasic(INT 1L))
                   STOREOP_IMM(ST, 1)
                   STOREOP_IMM(ST, 2)
                   LOADOP_IMM(LD, VBasic(INT 2L))
                   STOREOP_IMM(ST, 3)
                   LOADOP_REF(LD, 0)
                   CMPOP_IMM(LE, INT 0L)
                   JMPOP(JMPC, _break)
                   MOV(3, 1) // t := a
                   MOV(1, 2) // a := b
                   LOADOP_REF(LD, 3)
                   ABINOP_REF(ADD, 2)
                   STOREOP_IMM(ST, 2) // b := a + b
                   LOADOP_REF(LD, 0)
                   ABINOP_IMM(SUB, INT 1L)
                   STOREOP_IMM(ST, 0)
                   JMPOP(JMP, _loop)
                   LOADOP_REF(LD, 2)
                   MONITOROP FAIL |]
            
            let vm_state = 
                States.initial (Map.ofList [ 0, Value(VBasic(INT 41L))
                                             1, Value(VBasic(INT 0L))
                                             2, Value(VBasic(INT 1L))
                                             3, Value(VBasic(INT 0L)) ])
            
            let istate = Analysis.initial vm_state
            let queue = Scheduler.Bfs.empty()
            let schedule = Scheduler.Bfs.schedule queue
            let deschedule = Scheduler.Bfs.deschedule queue
            schedule istate
            try 
                ignore (Analysis.run code schedule deschedule)
                Assert.Fail()
            with Analysis.Fail s -> 
                printf "%A" s.vm_state.cr
                Assert.AreEqual(Value(VBasic(INT 433494437L)), s.vm_state.cr)
        
        [<Test>]
        member x.``Branching counterexample``() = 
            let code = 
                Array.map snd [| 0, OSOP(READ_INPUT(TINT, 0)) // READ X
                                 1, LOADOP_REF(LD, 0)
                                 2, CMPOP_IMM(EQ, INT 0L)
                                 3, JMPOP(JMPCN, 7) // IF X = 0
                                 4, LOADOP_IMM(LD, VBasic(INT 123L))
                                 5, STOREOP_IMM(ST, 1) // Y := 123
                                 6, JMPOP(JMP, 8) // ELSE
                                 7, LOADOP_IMM(LD, VBasic(INT 456L)) // Y := 456
                                 8, STOREOP_IMM(ST, 1)
                                 // monitor
                                 9, LOADOP_REF(LD, 1)
                                 10, CMPOP_IMM(EQ, INT 456L)
                                 11, JMPOP(JMPCN, 13) // IF Y != 456
                                 12, MONITOROP FAIL // FAIL!
                                 13, LOADOP_IMM(LD, VBasic(BOOL false)) // ELSE OK
                                 14, MONITOROP ASSUME |]
            
            let vm_state = 
                States.initial (Map.ofList [ 0, Value(VBasic(INT 0L))
                                             1, Value(VBasic(INT 0L)) ])
            
            let istate = Analysis.initial vm_state
            let queue = Scheduler.Bfs.empty()
            let schedule = Scheduler.Bfs.schedule queue
            let deschedule = Scheduler.Bfs.deschedule queue
            schedule istate
            try 
                ignore (Analysis.run code schedule deschedule)
                Assert.Fail()
            with Analysis.Fail s -> 
                SMT.Solver.print_assertions()
                Assert.Pass()
            SMT.Solver.clear()

    open NUnit.Core
    open NUnit.Framework
    open FsCheck
    open FsCheck.NUnit
    open FsPlcModel.IL
    open Analysis
    open FsPlcSpec.Monitor_compiler
    open FsPlcModel.IL
    
    let inline print_instructions is = Array.iteri (fun i op -> printfn "%s" (sprintf "%d\t%A" i op)) is
    
    [<TestFixture>]
    type Tests() = 
        
        [<Test>]
        member x.``Passing and failing cases are found``() = 
            // 0:IN
            // 1:IN_PREV
            // 2:PT
            // 3:TIMESTAMP
            // 4:OUT
            // 5: Sensor(INT)
            // 6: ERROR
            let memory = 
                Map.ofList [ 0, Value(VBasic(BOOL false))
                             1, Value(VBasic(BOOL false))
                             2, Value(VBasic(TIME 50L))
                             3, Value(VBasic(TIME 0L))
                             4, Value(VBasic(BOOL false))
                             5, Value(VBasic(INT 0L))
                             6, Value(VBasic(BOOL false)) ]
            
            let code = 
                [| // BEGIN TON(IN,PT)
                   LOADOP_REF(LDN, 1) // NOT PREV
                   LBINOP_REF(AND, 0) // AND CURRENT
                   JMPOP_REL(JMPCN, 4)
                   NULLOP LD_CURRENT_TIME_NOCHANGE
                   ABINOP_REF(ADD, 2)
                   STOREOP_IMM(ST, 3)
                   NULLOP LD_CURRENT_TIME
                   CMPOP_REF(GE, 3)
                   LBINOP_REF(AND, 0)
                   STOREOP_IMM(ST, 4)
                   MOV(1, 0)
                   RETOP RET
                   // END
                   // BEGIN POU
                   LOADOP_IMM(LD, VBasic(INT 123L))
                   CMPOP_REF(LE, 5)
                   STOREOP_IMM(ST, 0)
                   ST_AUX(Reg 0, Reference 0)
                   CALLOP_IMM(CAL, 0, Reg 0) // CALL TON
                   LOADOP_REF(LD, 4)
                   LBINOP_REF(OR, 6)
                   STOREOP_IMM(ST, 6)
                   RETOP RET |]
            
            // END
            let spec = 
                Seq
                    (Seq
                         (Constraint((Reference 5, GE, Value(VBasic(INT 123L))), Delay(Time_ge 100, Empty)), 
                          Delay(Time_ge 100, Check((Reference 6, EQ, Value(VBasic(BOOL true)))))), Pass)
            let spec, env = Monitor_compiler.compile' spec
            let compiled = Monitor_compiler.to_vm env spec 7
            
            let tasks = 
                [ { id = 0
                    pou = 
                        [ { entry = 12
                            data = 0 } ]
                    interval = 20L
                    priority = 0 } ]
            
            let task_count = Map.ofList [ 0, 0 ]
            let runtime = Runtime.make code tasks compiled.code [ TINT, 5 ]
            let memory = Seq.append (Map.toSeq compiled.memory) (Map.toSeq memory) |> Map.ofSeq
            printfn "%A" memory
            let vm_state = 
                { States.initial (memory) with pc = runtime.os_entry
                                               task_count = task_count }
            print_instructions runtime.code
            let istate = Analysis.initial vm_state
            let queue = Scheduler.Bfs.empty()
            let schedule = Scheduler.Bfs.schedule queue
            let deschedule = Scheduler.Bfs.deschedule queue
            schedule istate
            try 
                ignore (Analysis.run runtime.code schedule deschedule)
                Assert.Fail()
            with
            | Analysis.Pass s -> 
                let testcase_env = 
                    Testcase.env (runtime.location) (compiled.state_mem) (compiled.moment_mem) [ 0; 1; 2; 3; 4; 5; 6 ] 
                        [ 5 ] runtime.code
                let testcase = Testcase.build testcase_env s
                Testcase.print_testcase testcase
                Testcase.print_concrete_sensors testcase_env testcase
                Testcase.print_concrete_value testcase 6
                SMT.Solver.print_assertions()
                printf "%A PASSED!" s.vm_state.cr
                Assert.Pass()
            | Analysis.Fail s -> 
                let testcase_env = 
                    Testcase.env (runtime.location) (compiled.state_mem) (compiled.moment_mem) [ 0; 1; 2; 3; 4; 5; 6 ] 
                        [ 5 ] runtime.code
                let testcase = Testcase.build testcase_env s
                Testcase.print_testcase testcase
                Testcase.print_concrete_sensors testcase_env testcase
                Testcase.print_concrete_value testcase 6
                SMT.Solver.print_assertions()
                printf "%A" s.vm_state.cr
                Assert.Pass()
        
        [<Test>]
        member x.``Tiny runtime test``() = 
            let code = 
                [| LOADOP_IMM(LD, VBasic(INT 1L))
                   ABINOP_REF(ADD, 0)
                   STOREOP_IMM(ST, 0)
                   RETOP RET
                   LOADOP_IMM(LD, VBasic(INT 2L))
                   ABINOP_REF(ADD, 0)
                   STOREOP_IMM(ST, 0)
                   CMPOP_IMM(EQ, INT 7L)
                   JMPOP_REL(JMPCN, 2)
                   MONITOROP FAIL
                   RETOP RET |]
            
            let memory = Map.ofList [ 0, Value(VBasic(INT 0L)) ]
            let spec = Empty // Delay(Time_ge 101, Constraint((Value (VBasic (INT 0L)),LT,Value (VBasic (INT 1L))),Empty))
            let spec, env = Monitor_compiler.compile' spec
            let compiled = Monitor_compiler.to_vm env spec 1
            
            let tasks = 
                [ { id = 0
                    pou = 
                        [ { entry = 0
                            data = 0 } ]
                    interval = 50L
                    priority = 1 }
                  { id = 1
                    pou = 
                        [ { entry = 4
                            data = 0 } ]
                    interval = 100L
                    priority = 1 } ]
            
            let task_count = 
                Map.ofList [ 0, 0
                             1, 0 ]
            
            let runtime = Runtime.make code tasks compiled.code []
            printfn "%A" runtime
            let memory = Seq.append (Map.toSeq compiled.memory) (Map.toSeq memory) |> Map.ofSeq
            printfn "%A" memory
            let vm_state = 
                { States.initial (memory) with pc = runtime.os_entry
                                               task_count = task_count }
            print_instructions runtime.code
            let istate = Analysis.initial vm_state
            let queue = Scheduler.Bfs.empty()
            let schedule = Scheduler.Bfs.schedule queue
            let deschedule = Scheduler.Bfs.deschedule queue
            schedule istate
            try 
                ignore (Analysis.run runtime.code schedule deschedule)
                Assert.Fail()
            with Analysis.Fail s -> 
                printf "%A" s.vm_state.cr
                Assert.Pass()
