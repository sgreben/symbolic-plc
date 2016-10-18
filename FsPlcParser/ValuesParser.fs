namespace FsPlcParser
open FsPlcModel

module ValuesParser = 
    open Common.Parsers
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives
    
    type BasicValueParser = Parser<Values.Basic, unit>
    
    let bool : BasicValueParser = 
        choice [ keyword "true" |>> fun _ -> true
                 keyword "false" |>> fun _ -> false ] |>> fun v -> Values.BOOL v
    
    let byte : BasicValueParser = puint8 |>> fun i -> Values.BYTE((byte) i)
    let word : BasicValueParser = puint16 |>> fun v -> Values.WORD v
    let dword : BasicValueParser = puint32 |>> fun v -> Values.DWORD v
    let lword : BasicValueParser = puint64 |>> fun v -> Values.LWORD v
    let int : BasicValueParser = pint32 |>> fun v -> Values.INT v
    let sint : BasicValueParser = pint8 |>> fun v -> Values.SINT v
    let dint : BasicValueParser = pint32 |>> fun v -> Values.DINT v
    let lint : BasicValueParser = pint64 |>> fun v -> Values.LINT v
    let uint : BasicValueParser = puint16 |>> fun v -> Values.UINT v
    let usint : BasicValueParser = puint8 |>> fun v -> Values.USINT v
    let udint : BasicValueParser = puint32 |>> fun v -> Values.UDINT v
    let ulint : BasicValueParser = puint64 |>> fun v -> Values.ULINT v
    let real : BasicValueParser = pfloat |>> fun v -> Values.REAL v
    let lreal : BasicValueParser = pfloat |>> fun v -> Values.LREAL v
    
    let timeUnit = 
        choice [ keyword "d" |>> fun _ -> Values.Day
                 keyword "h" |>> fun _ -> Values.Hour
                 keyword "ms" |>> fun _ -> Values.Millisecond
                 keyword "m" |>> fun _ -> Values.Minute
                 keyword "s" |>> fun _ -> Values.Second ]
    
    let time' = 
        choice [ keyword "t#"
                 keyword "pt"
                 keyword "time#" ] >>. many1 (pfloat .>> ws .>>. timeUnit)
    let time : BasicValueParser = time' |>> fun v -> Values.TIME v
    
    let time_of_day' = (pint32 .>> ws .>> colon .>> ws) .>>. (pint32 .>> ws .>> colon .>> ws) .>>. (pint32 .>> ws)
    
    let time_of_day : BasicValueParser = 
        choice [ keyword "tod#"
                 keyword "time_of_day#" ] >>. time_of_day' |>> fun ((h, m), s) -> Values.TIME_OF_DAY(h, m, s)
    
    let date' = (pint32 .>> ws .>> dash .>> ws) .>>. (pint32 .>> ws .>> dash .>> ws) .>>. (pint32 .>> ws)
    
    let date : BasicValueParser = 
        choice [ keyword "d#"
                 keyword "date#" ] >>. date' |>> fun ((y, m), d) -> Values.DATE(y, m, d)
    
    let date_and_time : BasicValueParser = 
        choice [ keyword "dt#"
                 keyword "date_and_time#" ] >>. date' .>> (dash .>> ws) .>>. time_of_day' 
        |>> fun (((y, mo), d), ((h, m), s)) -> Values.DATE_AND_TIME((y, mo, d), (h, m, s))
    
    let string = between (skipString "\'") (skipString "\'") (manySatisfy ((<>) '\'')) |>> Values.STRING
    let wstring = between (skipString "\"") (skipString "\"") (manySatisfy ((<>) '\"')) |>> Values.WSTRING
    
    /// Parse a value of the given type
    let basicValue : Types.Basic -> BasicValueParser = 
        function 
        | Types.BOOL -> bool
        | Types.BYTE -> byte
        | Types.WORD -> word
        | Types.DWORD -> dword
        | Types.LWORD -> lword
        | Types.INT -> int
        | Types.SINT -> sint
        | Types.DINT -> dint
        | Types.LINT -> lint
        | Types.UINT -> uint
        | Types.USINT -> usint
        | Types.UDINT -> udint
        | Types.ULINT -> ulint
        | Types.REAL -> real
        | Types.LREAL -> lreal
        | Types.TIME -> time
        | Types.DATE -> date
        | Types.TIME_OF_DAY -> time_of_day
        | Types.DATE_AND_TIME -> date_and_time
        | Types.STRING -> string
        | Types.WSTRING -> wstring
    
    let enumValue = ident

    let anyBasicValue : BasicValueParser = 
        choice [ attempt <| basicValue Types.BOOL
                 attempt <| basicValue Types.BYTE
                 attempt <| basicValue Types.INT
                 attempt <| basicValue Types.SINT
                 attempt <| basicValue Types.DINT
                 attempt <| basicValue Types.LINT
                 attempt <| basicValue Types.UINT
                 attempt <| basicValue Types.USINT
                 attempt <| basicValue Types.UDINT
                 attempt <| basicValue Types.ULINT
                 attempt <| basicValue Types.WORD
                 attempt <| basicValue Types.DWORD
                 attempt <| basicValue Types.LWORD
                 attempt <| basicValue Types.REAL
                 attempt <| basicValue Types.LREAL
                 attempt <| basicValue Types.TIME
                 attempt <| basicValue Types.DATE
                 attempt <| basicValue Types.TIME_OF_DAY
                 attempt <| basicValue Types.DATE_AND_TIME
                 attempt <| basicValue Types.STRING
                 basicValue Types.WSTRING ]
   
    exception Not_implemented 
    let value = function
    | Types.BASIC t -> basicValue t |>> fun v -> Values.BASIC v
    | Types.ENUM _ -> enumValue |>> fun v -> Values.ENUM v
    | _ -> raise Not_implemented

    let anyValue = choice [ attempt anyBasicValue |>> fun v -> Values.BASIC v
                            attempt enumValue |>> fun v -> Values.ENUM v ]
    
    let parse = Common.Parsers.parse