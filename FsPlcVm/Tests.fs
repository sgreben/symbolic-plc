namespace FsPlcVm

open Microsoft.Z3


module VmTests = 
    open FsUnit
    open NUnit
    open NUnit.Core
    open NUnit.Framework
    open States
    open Representation
    open Language
    open Instructions
    open FsPlcModel.IL
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    let cr_actual_equals_expected (actual : State, expected : State) = Assert.AreEqual(expected.cr, actual.cr)
    
    [<TestFixture>]
    type Z3Tests() = 
        
        [<Test>]
        member x.``casting to IntExpr works``() = 
            let one, two = SMT.Const.mk_int 1L, SMT.Const.mk_int 2L
            let sum = SMT.Op.add_int one two
            Assert.AreEqual(sum.Sort, SMT.IntSort)
            Assert.AreEqual(SMT.Solver.check_bool (SMT.Rel.neq sum (SMT.Const.mk_int 3L)), Some false)
        
        [<Test>]
        member x.``Variables with same name are equal``() = 
            let v0 = SMT.Var.mk_int "var"
            let v1 = SMT.Var.mk_int "var"
            Assert.IsTrue(v0.Equals(v1))
//    [<TestFixture>]
//    type MemoryTests() = 
//        [<Test>]
//        member x.``first concrete test ever``() = 
//                let memory = 
//                    Map.ofList [ 0,  // struct S = {X,Y,Z}
//                                 Value([ 0, 1
//                                         1, 2
//                                         2, 3 ]
//                                       |> Map.ofList
//                                       |> VStruct) 
//                                 1, Value (mkint 2L) // s.X
//                                 2, Value (mkint 123L) // s.Y
//                                 3, Value (mkint 456L) // s.Z
//                                 4, // struct fS = {fX,fY,fZ,fRes}
//                                 Value([ 0, 5
//                                         1, 6
//                                         2, 7
//                                         3, 8 ]
//                                       |> Map.ofList
//                                       |> VStruct)
//                                 5, Value Semantics.dummy_int // fX
//                                 6, Value Semantics.dummy_int // fY
//                                 7, Value Semantics.dummy_int // fZ
//                                 8, Value Semantics.dummy_int // fRes
//                                 ]
//                let _f = 0
//                let _while = 4
//                let _break = 16
//                let _main = 20
//                let _os_main = 35
//                let code = 
//                    [| NOP // _f
//                       LOADOP_IMM(LD,mkint 10L)
//                       STOREOP_IMM(ST, 7) // ST fZ
//                       LOADOP_REF(LD, 5) // LD fX
//                       NOP // _while
//                       CMPOP_PUSH LT // LT_PUSH
//                       LOADOP_REF(LD, 6) // LD fY
//                       POP
//                       JMPOP(JMPCN, _break)
//                       LOADOP_REF(LD, 5) // LD fX
//                       FUN_BUILTIN_EXTOP(ADD_EXT, [ Value (mkint 1L); Value (mkint 2L) ]) // ADD(1,2)
//                       STOREOP_IMM(ST, 5) // ST fX
//                       LOADOP_REF(LD, 7) // LD fZ
//                       ABINOP_IMM(ADD, INT 1L)
//                       STOREOP_IMM(ST, 7) // ST fZ
//                       JMPOP(JMP, _while)
//                       NOP // _break
//                       LOADOP_REF(LD, 7) // LD fZ
//                       STOREOP_IMM(ST, 8) // ST fRes
//                       RETOP RET
//                       // main
//                       LOADOP_REF(LD, 4) // LD fS
//                       MOV_CR_AUX
//                       LOADOP_REF(LD, 0) // LD S
//                       LOADOP_FIELD(LD, 0) // LD_FIELD S.X
//                       STOREOP_IMM_FIELD(ST, 0) // ST_FIELD fS.X
//                       LOADOP_REF(LD, 0) // LD S
//                       LOADOP_FIELD(LD, 1) // LD_FIELD S.Y
//                       STOREOP_IMM_FIELD(ST, 1) // ST_FIELD fS.Y
//                       FUN _f
//                       LOADOP_REF(LD, 0) // LD S
//                       MOV_CR_AUX
//                       LOADOP_REF(LD, 4) // LD fS
//                       LOADOP_FIELD(LD, 3) // LD fS.fRes
//                       STOREOP_IMM_FIELD(ST, 2) // ST S.Z                    
//                       RETOP RET
//                       NOP // os_main
//                       OSOP (READ_INPUT (TINT,1))
//                       OSOP (READ_INPUT (TINT,2))
//                       CALLOP_IMM (CAL,_main)
//                       LOADOP_REF(LD,8)
//                       STOREOP_IMM(ST,1) 
//                       OSOP (WRITE_OUTPUT 3)
//                       OSOP (NEXT_CYCLE)
//                       MONITOROP LOG_STATE
//                       JMPOP (JMP,_os_main)
//                |]
//                
//                let state = 
//                    { cr = Value(mkint 0L)
//                      aux = Value(mkint 0L)
//                      call_stack = []
//                      op_stack = []
//                      memory = memory
//                      pc = _os_main
//                      cycle = 0
//                      time = 0UL }
//                let do_step state =
//                    let instr = code.[state.pc]
//                    let successor = match Instructions.step code.[state.pc] state with S1 s -> s
//                    successor
//                let state_ref = ref state
//                for i = 0 to 1000 do
//                    state_ref := do_step !state_ref
//                done
//                printfn "%A" (!state_ref).cr
//                //ignore (state |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step  |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step  |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step |> do_step)
//
//
