namespace FsPlcCompiler.Stage0

module Project =         
    open FsPlcModel.Pous
    type Pou_body = ST of Language.Statement.Block
    type Pou = {
        name : Access.Pou_type_path
        typ : PouType
        returnType : FsPlcModel.Types.Type option
        vars : FsPlcModel.Declarations.Declaration_block
        bodies : Pou_body list
    }
    open FsPlcModel.Projects
    type Project_types = { 
        dataTypes : Data_type list
        pous : Pou list 
    }
     type Task = {
        name:FsPlcModel.Common.Identifier
        interval : int64
        priority : int
        pous : (Access.Pou_instance_path*Access.Pou_type_path) list
    }
    type Project = {
        types : Project_types
        libraries : (FsPlcModel.Common.Identifier * Project_types) list
        tasks : Task list
    }

