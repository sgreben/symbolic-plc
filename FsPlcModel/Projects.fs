namespace FsPlcModel

module Projects = 
    type Data_type = 
        { name : Common.Identifier
          ivalue : Values.Value option
          typ : Types.Type }
    
    type Resource = 
        { name : Common.Identifier
          tasks : Tasks.Task list
          vars : Declarations.Declaration_block_named list
          pouInstances : (Common.Identifier * Common.Qualified_identifier) list }
    
    type Configuration = 
        { name : Common.Identifier
          resources : Resource list
          vars : Declarations.Declaration_block_named list }
    
    type ProjectTypesAst = 
        { dataTypes : Data_type list
          pous : Pous.PouAst list }
    
    type ProjectAst = 
        { types : ProjectTypesAst
          libraries : (Common.Identifier * ProjectTypesAst) list
          configurations : Configuration list }
    
    let project_ast_empty = 
        { types = 
              { dataTypes = []
                pous = [] }
          libraries = []
          configurations = [] }
