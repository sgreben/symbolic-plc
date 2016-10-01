namespace FsPlcCompiler.Stage0

module Access =
    open FsPlcModel

    /// Project-level scope selection
    type Access_project = 
        /// Access a library
        | Library of Common.Identifier
        /// Access user-defined items
        | User
    
    //// Module-local simple type path
    type Access_module_simple_type = Common.Identifier
    
    //// Module-local POU type path
    type Access_module_pou_type = Common.Identifier
    
    /// Module-level type selection
    type Access_module_types = 
        /// Select a simple type definition
        | Simple_type_id of Access_module_simple_type
        /// Select a POU type
        | Pou_type_id of Access_module_pou_type
    
    /// Fully-qualified simple type path
    type Simple_type_path = Access_project * Access_module_simple_type
    
    /// Fully-qualified POU type path
    type Pou_type_path = Access_project * Access_module_pou_type
    
    /// Fully-qualified function type path
    type Function_type_path = 
        | Project_function of Access_project * Common.Identifier
        | Builtin of FsPlcModel.IL.Builtin
    
    /// Fully-qualified type path. A type access of the form [<Library ID> '.'] (<Simple type ID>|<Pou type ID>)
    type Type_path = Access_project * Access_module_types

    type Generalized_type = 
        | Pou_type of Pou_type_path
        | Simple_type of Simple_type_path option * Types.Type
    
    //// VALUE PATHS
    ////
    type Access_module_gvl_var = Common.Identifier * Common.Identifier
    
    type Access_module_pou_instance = Common.Identifier
    
    /// Module-level value selection
    type Access_module_values = 
        /// Access a variable defined in a GVL
        | Gvl of Access_module_gvl_var
        /// Access a member variable of a module POU instance (e.g. the implicit instance of a task's POU)
        | Pou_instance of Access_module_pou_instance
    
    /// Structure-level value selection
    type Access_structure = 
        /// Access a field of a STRUCT or POU
        | Field of Generalized_type * Common.Identifier
        /// Access an array at an integer index
        | Index of int
        /// Access an array at the index given by the value of a variable
        | IndexVar of Value_path
    
    /// Fully-qualified GVL variable path
    and Gvl_var_path = Access_project * Access_module_gvl_var
    
    /// Fully-qualified POU instance path
    and Pou_instance_path = Access_project * Access_module_pou_instance
    
    /// Fully-qualified module variable (GVL member or POU instance) path    
    and Module_value_path = Access_project * Access_module_values
    
    (** A value access is covered by exactly one of the following cases:
        Global instance: [<Library> '.'] (<Gvl>|<Task POU instance>) (<Field/index>)*
        The value accessed is a globally instantiated value (or POU).

        Local instance: [<Library> '.'] <Pou type> (<Field/index>)*
        The value accessed is a POU-local value.

        Function: [<Library> '.'] <Pou type>
        The value accessed is a FUNCTION; An uninstantiated value that is referenced purely by type. **)
    and Value_path = 
        | Global_instance of Module_value_path * Access_structure list
        | Local_instance of Pou_type_path * Access_structure list
        | Function of Function_type_path

    exception Invalid_structure_access
    
    let value_path_structure_access vp sa = 
        match vp with
        | Global_instance(module_value_path, sas) -> Global_instance(module_value_path, sas @ [ sa ])
        | Local_instance(pou_type_path, sas) -> Local_instance(pou_type_path, sas @ [ sa ])
        | Function f -> raise Invalid_structure_access
    
    let (<|>) l r = 
        match l with
        | Some _ -> l
        | None -> r()

