namespace FsPlcParser
open FsPlcModel

module DeclarationsParser = 
    open Common.Parsers
    open FParsec
    open FParsec.Primitives
    open Declarations
    
    type VariableKindParser = Parser<Kind, unit>
    
    type VariableAttributeParser = Parser<Attribute, unit>
    
    let parse = Common.Parsers.parse
    
    let kind : VariableKindParser = 
        choice [ keyword "VAR_INPUT" |>> fun _ -> VAR_INPUT
                 keyword "VAR_OUTPUT" |>> fun _ -> VAR_OUTPUT
                 keyword "VAR_IN_OUT" |>> fun _ -> VAR_IN_OUT
                 keyword "VAR_EXTERNAL" |>> fun _ -> VAR_EXTERNAL
                 keyword "VAR_GLOBAL" |>> fun _ -> VAR_GLOBAL
                 keyword "VAR_ACCESS" |>> fun _ -> VAR_ACCESS
                 keyword "VAR_TEMP" |>> fun _ -> VAR_TEMP
                 keyword "VAR" |>> fun _ -> VAR ]
    
    let attribute : VariableAttributeParser = 
        choice [ keyword "RETAIN" |>> fun _ -> RETAIN
                 keyword "NON_RETAIN" |>> fun _ -> NON_RETAIN
                 keyword "CONSTANT" |>> fun _ -> CONSTANT
                 keyword "R_EDGE" |>> fun _ -> R_EDGE
                 keyword "F_EDGE" |>> fun _ -> F_EDGE
                 keyword "READ_ONLY" |>> fun _ -> READ_ONLY
                 keyword "READ_WRITE" |>> fun _ -> READ_WRITE ]
    
    let attributes = sepBy attribute spaces1
    
    /// Parse a single variable declaration "<identifier> : <type> [:= <value>]" of the given kind
    let declaration k a = 
        ident .>> ws .>> colon .>> ws >>= fun id -> 
            choice [ (attempt (TypesParser.anyType .>> keyword ":=")) 
                     >>= (fun t -> ValuesParser.value t |>> (fun v -> t, Some v))
                     TypesParser.anyType .>> ws |>> (fun t -> t, None) ] .>>. attributes |>> (fun ((t, v), a') -> 
            { kind = k
              attr = a @ a'
              id = id
              typ = t
              ivalue = v })
    
    /// Parse a variable declaration block "VAR[_<KIND>] <declaration>* END_VAR".
    let declaration_block = 
        kind .>>. attributes >>= fun (k, a) -> 
            let declaration = declaration k a
            sepEndBy1 (notFollowedBy (keyword "END_VAR") >>. declaration) (semicolon .>> ws) .>> keyword "END_VAR"