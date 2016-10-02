namespace FsPlcModel

module PousParser = 
    open Common.Parsers
    open FParsec
    open FParsec.Primitives
    open Pous
    
    let pouType : Parser<PouType, unit> = 
        choice [ keyword "FUNCTION_BLOCK" |>> fun _ -> FUNCTION_BLOCK
                 keyword "functionBlock" |>> fun _ -> FUNCTION_BLOCK
                 keyword "FUNCTION" |>> fun _ -> FUNCTION
                 keyword "PROGRAM" |>> fun _ -> PROGRAM ]
    
    let parse = Common.Parsers.parse