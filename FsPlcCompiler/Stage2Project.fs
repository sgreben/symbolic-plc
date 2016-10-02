namespace FsPlcCompiler.Stage2

module Project = 
    open FsPlcVm.Representation
    open FsPlcCompiler.Stage0.Access
    
    type Program = Language.Statement array
    
    type Task_pou = FsPlcVm.Runtime.Task_pou
    
    type Task = FsPlcVm.Runtime.Task
    
    type Project = 
        { tasks : Task list
          code : Program }
