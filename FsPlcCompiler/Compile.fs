namespace FsPlcCompiler

module Compile =
    open FsPlcModel.Projects

    let to_vm_image (p:Stage2.Project.Project) =
        FsPlcVm.Runtime.make p.code p.tasks [||] []

    let compile_project (p:ProjectAst) =   
        let stage0_scope = Stage0.Scope.Build.project_scope p
        let stage0_project = Stage0.Compile.Project.compile p
        let stage1_memmap,stage1_mem = Stage1.Memmap.project_memmap stage0_scope
        let stage1_project = Stage1.Compile.Project.compile stage1_memmap stage0_project
        let stage2_codemap = Stage2.Code_map.project_code_map stage1_project
        let stage2_project = Stage2.Compile.Project.compile stage2_codemap stage1_project
        let vm_image = to_vm_image stage2_project
        //stage1_project,stage1_memmap,stage1_mem,stage2_codemap,stage2_project
        vm_image