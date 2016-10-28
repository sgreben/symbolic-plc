namespace FsPlcSpecParser

module SpecParser = 
    module Common = FsPlcParser.Common.Parsers
    
    open FsPlcSpec.Spec
    open FParsec
    open FParsec.CharParsers
    open FParsec.Primitives
    
    let private multilineComment = 
        skipString "/*" >>. skipMany (notFollowedBy (skipString "*/") >>. skipAnyChar) >>. skipString "*/" .>> spaces
    let private comment = multilineComment
    let private ws : Parser<unit, unit> = spaces >>. skipMany comment
    let private parse_empty = skipString "{" >>. ws >>. skipString "}"
    let private parse_pass = skipStringCI "pass" >>. ws
    let private parse_spec, parse_specRef = createParserForwardedToRef<Formula, unit>()
    
    let braced_spec_or_empty = 
        choice [ attempt (Common.braces parse_spec)
                 parse_empty >>. preturn Empty ]
    
    let private parse_constraint = FsPlcParser.StExpressionsParser.expression
    let private parse_check = skipStringCI "check" >>. ws >>. Common.parens parse_constraint
    let private parse_constraint_expr = 
        skipStringCI "constraint" >>. ws >>. Common.parens parse_constraint .>> ws .>>. braced_spec_or_empty
    
    let private parse_time = 
        Common.pint() .>> ws >>= fun n -> 
            choice [ skipStringCI "ms" >>. preturn n
                     skipStringCI "s" >>. preturn (n * 1000) ]
            .>> ws
    
    let private parse_time_constraint = 
        choice [ skipString "<=" >>. ws >>. parse_time >>= fun n -> preturn (Time_le n)
                 skipString ">=" >>. ws >>. parse_time >>= fun n -> preturn (Time_ge n) ]
    
    let private parse_within = 
        skipStringCI "within" >>. ws >>. Common.parens parse_time_constraint .>> ws .>>. braced_spec_or_empty
    let private parse_delay = 
        skipStringCI "delay" >>. ws >>. Common.parens parse_time_constraint .>> ws .>>. braced_spec_or_empty
    
    let parse_non_seq_spec = 
        choice [ attempt parse_empty >>. preturn Empty
                 Common.braces parse_spec
                 parse_pass >>. preturn Pass
                 parse_check |>> (fun e -> Check e)
                 parse_constraint_expr |>> (fun (c, e) -> Constraint(c, e))
                 parse_within |>> (fun (t, e) -> Within(t, e))
                 parse_delay |>> (fun (t, e) -> Delay(t, e)) ]
        .>> ws
    
    let rec private parse_seq = 
        sepEndBy1 (parse_non_seq_spec .>> ws) (skipString ";" .>> ws) 
        |>> (fun (s :: ss) -> List.fold (fun s s' -> Seq(s, s')) s ss)
    
    do parse_specRef := ws >>. parse_seq .>> ws
    
    let parse = Common.parse (parse_spec .>> eof)
