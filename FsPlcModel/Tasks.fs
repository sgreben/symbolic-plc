namespace FsPlcModel

module Tasks = 
    type Task_interval = 
        | Fixed of Values.time
        | Variable of Common.Qualified_identifier
    
    type Task_property = 
        | SINGLE of Common.Qualified_identifier
        | INTERVAL of Task_interval
        | PRIORITY of int
    
    type Task = 
        { name : Common.Identifier
          properties : Task_property list
          pou : (Common.Identifier*Common.Qualified_identifier) list }
    let priority task = List.tryPick (function |PRIORITY i -> Some i| _ -> None) task.properties
    let interval task = List.tryPick (function |INTERVAL i -> Some i| _ -> None) task.properties
