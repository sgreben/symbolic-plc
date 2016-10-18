namespace FsPlcSpec

module SpecTests = 
    open NUnit.Framework
    open Monitor_compiler
    open FsPlcModel.IL
    open FsPlcVm
    open Language
    open Representation
    
    [<TestFixture>]
    type Tests() = 
        
        [<Test>]
        member x.``Can compile delay[>=100]``() = 
            let spec = Delay(Time_ge 100, Empty)
            let spec, env = Monitor_compiler.compile' spec
            let compiled = Monitor_compiler.to_vm env spec 0
            printfn "%A" compiled
        
        [<Test>]
        member x.``Can compile delay[>=100]{constraint[r0>10L]{}}``() = 
            let spec = Delay(Time_ge 100, Constraint((Reference 0, GT, Value(VBasic(INT 10L))), Empty))
            let spec, env = Monitor_compiler.compile' spec
            let compiled = Monitor_compiler.to_vm env spec 1
            printfn "%A" compiled
        
        [<Test>]
        member x.``Can compile delay[>=100]{constraint[r0>10L]{}};{}``() = 
            let spec = Seq(Delay(Time_ge 100, Constraint((Reference 0, GT, Value(VBasic(INT 10L))), Empty)), Empty)
            let spec, env = Monitor_compiler.compile' spec
            let compiled = Monitor_compiler.to_vm env spec 1
            printfn "%A" compiled
