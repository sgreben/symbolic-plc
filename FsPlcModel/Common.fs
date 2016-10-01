namespace FsPlcModel

module Common = 
    type Identifier = string
    type Qualified_identifier = string list
    type Array_dimension = (int * int) list
    
    module ParsersXml = 
        open System.Xml
        open System.Xml.Linq
        open System.Xml.XPath
        
        let xn (tag : string) = XName.Get(tag)
        let xn_default (tag : string) = XName.Get(tag, "http://www.plcopen.org/xml/tc6_0200")
        let xns n s = XName.Get(s, n)
        
        let parse_file parser (file : string) = 
            try 
                parser (System.Xml.Linq.XDocument.Load file)
            with e -> 
                printfn "%A" e
                raise e
        
        let parse parser input = 
            try 
                parser (System.Xml.Linq.XElement.Parse input)
            with e -> 
                printfn "%A" e
                raise e
    
    module Parsers = 
        open FParsec
        open FParsec.CharParsers
        open FParsec.Primitives
        
        let multilineComment = skipString "(*" >>. skipMany (notFollowedBy (skipString "*") >>. skipAnyChar) >>. skipString "*)" .>> spaces
        let comment = multilineComment
        let ws : Parser<unit, unit> = 
            spaces >>. skipMany comment
                
        let keyword s = (skipStringCI s) .>> ws
        let between l r p = between (skipString l .>> ws) (skipString r .>> ws) p
        let braces p = between "{" "}" p .>> ws
        let brackets p = between "[" "]" p .>> ws
        let parens p = between "(" ")" p .>> ws
        let colon : Parser<unit, unit> = skipString ":"
        let semicolon : Parser<unit, unit> = skipString ";"
        let comma : Parser<unit, unit> = skipString ","
        let dash : Parser<unit, unit> = skipString "-"
        let dot : Parser<unit, unit> = skipString "."
        let dotdot : Parser<unit, unit> = skipString ".."
        let numberFormat = 
            NumberLiteralOptions.AllowMinusSign ||| NumberLiteralOptions.AllowFraction 
            ||| NumberLiteralOptions.AllowExponent
        let pint() = numberLiteral numberFormat "integer" |>> fun n -> int32 n.String
        
        let ident : Parser<string, unit> = 
            let keywords = 
                System.Collections.Generic.HashSet<string> 
                    [| "action"; "add"; "and"; "any"; "any_bit"; "any_date"; "any_derived"; "any_elementary"; "any_int"; 
                       "any_magnitude"; "any_num"; "any_real"; "array"; "bool"; "by"; "case"; "configuration"; 
                       "constant"; "date"; "date_and_time"; "delete"; "dint"; "do"; "dword"; "else"; "elsif"; 
                       "end_action"; "end_case"; "end_configuration"; "end_for"; "end_function"; "end_function_block"; 
                       "end_if"; "end_program"; "end_repeat"; "end_resource"; "end_step"; "end_struct"; "end_transition"; 
                       "end_type"; "end_var"; "end_while"; "exit"; "false"; "for"; "from"; "function"; "function_block"; 
                       "if"; "initial_step"; "int"; "interval"; "jmp"; "jmpc"; "jmpcn"; "left"; "lint"; "lreal"; 
                       "lt"; "lword"; "move"; "non_retain"; "not"; "of"; "on"; "or"; "priority"; "program"; "read_only"; 
                       "read_write"; "real"; "release"; "repeat"; "replace"; "resource"; "ret"; "retain"; "retc"; 
                       "retcn"; "return"; "right"; "single"; "sint"; "sqrt"; "step"; "string"; "struct"; "sub"; "task"; 
                       "then"; "time"; "time_of_day"; "tod"; "transition"; "true"; "type"; "udint"; "uint"; "ulint"; 
                       "until"; "usint"; "var"; "var_access"; "var_config"; "var_external"; "var_global"; "var_in_out"; 
                       "var_input"; "var_output"; "var_temp"; "while"; "with"; "word"; "wstring" |]
            let isAsciiIdStart c = isAsciiLetter c || c = '_'
            let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_'
            identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue)) >>= (fun s -> 
            if keywords.Contains(s.ToLower()) then pzero
            else preturn s)
        
        let qident : Parser<string list, unit> = sepBy1 ident (skipString ".")

//        let qident : Parser<Variable, unit> = sepBy1 ident (skipString ".") |>> function
//            | [id] -> Direct id
//            | id::ids -> List.fold (fun v id -> Dot(v,id)) (Direct id) ids
        
        exception ParseError of string
        
        let parse parser input = 
            match run parser input with
            | Success(atoms, _, _) -> atoms
            | Failure(error, _, _) -> 
                printfn "%s" error
                raise (ParseError error)
        
        let debug_ntabs = ref 0
        let debug_tabs() = List.replicate !debug_ntabs "\t" |> String.concat ""
        let debug = false
        
        let (<!>) (p : Parser<_, _>) label : Parser<_, _> = 
            if debug then 
                fun stream -> 
                    printfn "%s" (sprintf "%s %A: Entering %s" (debug_tabs()) stream.Position label)
                    debug_ntabs := !debug_ntabs + 1
                    let reply = p stream
                    debug_ntabs := !debug_ntabs - 1
                    printfn "%s" 
                        (sprintf "%s %A: Leaving %s (%A) (%A)" (debug_tabs()) stream.Position label reply.Status 
                             reply.Result)
                    reply
            else p
