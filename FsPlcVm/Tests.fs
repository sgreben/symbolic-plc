namespace FsPlcVm

open Microsoft.Z3


module VmTests = 
    open NUnit
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