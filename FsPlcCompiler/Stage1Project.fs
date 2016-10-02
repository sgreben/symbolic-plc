namespace FsPlcCompiler.Stage1

open FsPlcModel.IL
open FsPlcModel

module Project = 
    open Language
    open FsPlcCompiler.Stage0.Access
    open FsPlcVm.Language
    open FsPlcVm.Representation
    open FsPlcCompiler
    
    type Pou_body = Language.Statement list
    
    type Pou = 
        { name : Pou_type_path
          typ : FsPlcModel.Pous.PouType
          returnType : FsPlcModel.Types.Type option
          vars : FsPlcModel.Declarations.Declaration_block
          bodies : Pou_body list
          code_pointer : Code_pointer }
    
    type Project_types = 
        { dataTypes : FsPlcModel.Projects.Data_type list
          pous : Pou list }
    
    type Task_pou = 
        { typ : Pou_type_path
          data : Memory_address }
    
    type Task = 
        { name : Common.Identifier
          interval : int64
          priority : int
          pous : Task_pou list }
    
    type Project = 
        { types : Project_types
          libraries : (Common.Identifier * Project_types) list
          tasks : Task list }