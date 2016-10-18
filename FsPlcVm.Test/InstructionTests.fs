namespace FsPlcVm.Test

open FsPlcVm
open Representation
open Language
open FsPlcModel.IL

module InstructionTests = 
    open Instructions
    open NUnit
    open NUnit.Framework
    
    module Vmp = Primitives
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type InstructionTests() = 
        let empty_state = States.initial Map.empty
        
        let inc_pc_ic (state : States.State) = 
            { state with pc = state.pc + 1
                         instruction_count = state.instruction_count + 1UL }
        
        [<Test>]
        member x.BUILTIN_ADD() = 
            let initial = 
                empty_state 
                |> Vmp.st_aux (Reg 0) (Value (VBasic (INT 123L))) 
                |> Vmp.st_aux (Reg 1) (Value (VBasic (INT 456L))) 
                |> Vmp.st_aux (Reg 2) (Value (VBasic (INT 789L))) 
            let actual = Instructions.step (FUN_BUILTIN_EXTOP(ADD_EXT, [Reg 0; Reg 1; Reg 2])) initial 
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(INT 1368L, Vmp.cr_value_basic state)
            | _ -> Assert.Fail()

        [<Test>]
        member x.BUILTIN_MUL() = 
            let initial = 
                empty_state 
                |> Vmp.set_cr_value_basic (INT 1L)
                |> Vmp.st_aux (Reg 0) (Value (VBasic (INT 123L))) 
                |> Vmp.st_aux (Reg 1) (Value (VBasic (INT 456L))) 
                |> Vmp.st_aux (Reg 2) (Value (VBasic (INT 789L))) 
            let actual = Instructions.step (FUN_BUILTIN_EXTOP(MUL_EXT, [Reg 0; Reg 1; Reg 2])) initial 
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(INT 44253432L, Vmp.cr_value_basic state)
            | _ -> Assert.Fail()

        [<Test>]
        member x.BUILTIN_EQ_False() = 
            let initial = 
                empty_state 
                |> Vmp.set_cr_value_basic (INT 0L)
                |> Vmp.st_aux (Reg 0) (Value (VBasic (INT 123L))) 
                |> Vmp.st_aux (Reg 1) (Value (VBasic (INT 456L))) 
                |> Vmp.st_aux (Reg 2) (Value (VBasic (INT 789L))) 
            let actual = Instructions.step (FUN_BUILTIN_EXTOP(EQ_EXT, [Reg 0; Reg 1; Reg 2])) initial 
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(BOOL false, Vmp.cr_value_basic state)
            | _ -> Assert.Fail()

        [<Test>]
        member x.BUILTIN_EQ_True() = 
            let initial = 
                empty_state 
                |> Vmp.set_cr_value_basic (INT 0L)
                |> Vmp.st_aux (Reg 0) (Value (VBasic (INT 0L))) 
                |> Vmp.st_aux (Reg 1) (Value (VBasic (INT 0L))) 
                |> Vmp.st_aux (Reg 2) (Value (VBasic (INT 0L))) 
            let actual = Instructions.step (FUN_BUILTIN_EXTOP(EQ_EXT, [Reg 0; Reg 1; Reg 2])) initial 
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(BOOL true, Vmp.cr_value_basic state)
            | _ -> Assert.Fail()

        member x.BUILTIN_LE() = 
            let initial = 
                empty_state 
                |> Vmp.set_cr_value_basic (INT 0L)
                |> Vmp.st_aux (Reg 0) (Value (VBasic (INT 123L))) 
                |> Vmp.st_aux (Reg 1) (Value (VBasic (INT 456L))) 
                |> Vmp.st_aux (Reg 2) (Value (VBasic (INT 789L))) 
            let actual = Instructions.step (FUN_BUILTIN_EXTOP(LE_EXT, [Reg 0; Reg 1; Reg 2])) initial 
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(BOOL true, Vmp.cr_value_basic state)
            | _ -> Assert.Fail()

        [<Test>]
        member x.BUILTIN_MAX() = 
            let initial = 
                empty_state 
                |> Vmp.set_cr (Value (VBasic (INT 1L)))
                |> Vmp.st_aux (Reg 0) (Value (VBasic (INT 123L))) 
                |> Vmp.st_aux (Reg 1) (Value (VBasic (INT 456L))) 
                |> Vmp.st_aux (Reg 2) (Value (VBasic (INT 789L))) 
            let actual = Instructions.step (FUN_BUILTIN_EXTOP(MAX, [Reg 0; Reg 1; Reg 2])) initial 
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(INT 789L, Vmp.cr_value_basic state)
            | _ -> Assert.Fail()

        [<Test>]
        member x.BUILTIN_MUX() = 
            let initial = 
                empty_state 
                |> Vmp.set_cr_value_basic (INT 2L)
                |> Vmp.st_aux (Reg 0) (Value (VBasic (INT 123L))) 
                |> Vmp.st_aux (Reg 1) (Value (VBasic (INT 456L))) 
                |> Vmp.st_aux (Reg 2) (Value (VBasic (INT 789L)))
            let actual = Instructions.step (FUN_BUILTIN_EXTOP(MUX, [Reg 0; Reg 1; Reg 2])) initial 
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(INT 789L, Vmp.cr_value_basic state)
            | _ -> Assert.Fail()


        [<Test>]
        member x.NOP() = 
            let actual = Instructions.step NOP empty_state
            let expected = S1(inc_pc_ic empty_state)
            actual_equals_expected (actual, expected)
        
        [<Test>]
        member x.NOT_boolTrue() = 
            let actual = Instructions.step (NULLOP NOT) { empty_state with cr = Value(VBasic(BOOL true)) }
            let expected = S1(inc_pc_ic { empty_state with cr = Value(VBasic(BOOL false)) })
            actual_equals_expected (actual, expected)
        
        [<Test>]
        member x.NOT_boolFalse() = 
            let actual = Instructions.step (NULLOP NOT) { empty_state with cr = Value(VBasic(BOOL false)) }
            let expected = S1(inc_pc_ic { empty_state with cr = Value(VBasic(BOOL true)) })
            actual_equals_expected (actual, expected)
        
        [<Test>]
        member x.LD_CURRENT_TIME() = 
            let actual = Instructions.step (NULLOP LD_CURRENT_TIME) empty_state
            printfn "%A" actual
            match actual with
            | Instructions.S1G (state, _) -> Assert.AreEqual(SYM(SYM_TIME(Vmp.time state)), Vmp.cr_value_basic state)
            | _ -> Assert.Fail()
        
        [<Test>]
        member x.LD_CURRENT_TIME_NOCHANGE() = 
            let actual = Instructions.step (NULLOP LD_CURRENT_TIME_NOCHANGE) empty_state
            match actual with
            | Instructions.S1 state -> 
                Assert.AreEqual(SYM(SYM_TIME(Vmp.time state)), Vmp.cr_value_basic state)
                Assert.AreEqual(Vmp.time empty_state, Vmp.time state)
            | _ -> Assert.Fail()
