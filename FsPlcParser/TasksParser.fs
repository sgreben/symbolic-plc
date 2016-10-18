namespace FsPlcParser
open FsPlcModel

module TasksParser = 
    open Common.Parsers
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives
    open Tasks
    
    type TaskParser = Parser<Task, unit>
    
    let taskInterval = 
        choice [ ValuesParser.time' |>> fun v -> Fixed (Values.time_ms v)
                 qident |>> fun v -> Variable v ]
    
    let taskPriority = pint()
    let taskSingle = qident
    
    let taskProperty = 
        choice [ keyword "INTERVAL" >>. keyword ":=" >>. taskInterval |>> fun i -> INTERVAL i
                 keyword "PRIORITY" >>. keyword ":=" >>. taskPriority |>> fun p -> PRIORITY p
                 keyword "SINGLE" >>. keyword ":=" >>. taskSingle |>> fun s -> SINGLE s ]
    
    let taskProperties = 
        opt (parens (sepBy taskProperty (pstring "," >>. ws))) |>> function 
        | Some ps -> ps
        | None -> []
    

    let task : TaskParser = 
        keyword "TASK" >>. (ident .>> ws) .>>. taskProperties .>> semicolon |>> fun (n, ps) -> 
            { name = n
              pou = []
              properties = ps }
    
    let parse = Common.Parsers.parse