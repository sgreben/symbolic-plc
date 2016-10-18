namespace FsPlcParser
open FsPlcModel

module StStatementsParser = 
    open Common.Parsers
    open FParsec
    open FParsec.Primitives
    open StExpressionsParser
    open StStatements
    
    let statement, statementRef = createParserForwardedToRef<StatementAst, unit>()
    let block = many (getPosition .>>. statement .>> opt semicolon .>> ws)  |>> List.map (fun (p,s) ->
        {line=p.Line;column=p.Column}, s
    )
    let assignment = variable .>> keyword ":=" .>>. expression_semi |>> AstAssignment
    let call = function_call .>> semicolon |>> AstCall
    
    let else_endif : Parser<BlockAst, unit> = 
        choice [ keyword "end_if" .>> opt semicolon |>> fun _ -> []
                 keyword "else" >>. block .>> keyword "end_if" .>> opt semicolon ]
    
    let if_then_else = 
        keyword "if" >>. bexp .>> keyword "then" .>>. block .>>. else_endif |>> (fun ((c, a), b) -> AstIf(c, a, b))
    let case_values_range = pint64 .>> ws .>> dotdot .>>. pint64 .>> ws |>> CaseRange
    let case_values_list = sepBy pint64 (comma >>. ws) |>> CaseValues
    
    let case_values = 
        choice [ attempt case_values_range
                 case_values_list ]
    
    let case = case_values .>> colon .>> ws .>>. block
    
    let cases = 
        many1 case .>>. choice [ keyword "else" >>. block
                                 preturn [] ] .>> keyword "end_case"
    
    let case_of = keyword "case" >>. expression .>> keyword "of" .>>. cases |>> (fun (e, (cs, d)) -> AstCase(e, cs, d))
    let do_while = 
        keyword "while" >>. bexp .>> keyword "do" .>>. block .>> keyword "end_while" |>> fun (e, b) -> AstDoWhile(e, b)
    let repeat_until = 
        keyword "repeat" >>. block .>> keyword "until" .>>. bexp .>> keyword "end_repeat" 
        |>> fun (b, e) -> AstRepeatUntil(e, b)
    let by_clause = keyword "by" >>. aexp
    let for_to = 
        keyword "for" >>. variable .>> keyword ":=" .>>. aexp .>> ws .>> keyword "to" .>>. aexp .>> ws 
        .>>. opt (by_clause) .>> ws .>> keyword "do" .>>. block .>> keyword "end_for" 
        |>> (fun ((((v, e), e'), by), b) -> AstFor(v, e, e', by, b))
    let exit = keyword "exit" |>> fun _ -> AstExit
    
    do statementRef := ws >>. choice [ attempt if_then_else <!> "if_then_else"
                                       attempt case_of  <!> "case_of"
                                       attempt do_while  <!> "do_while"
                                       attempt repeat_until  <!> "repeat_until"
                                       attempt for_to  <!> "for_to"
                                       attempt exit  <!> "exit"
                                       attempt call <!> "call"
                                       attempt assignment  <!> "assignment" ]
    
    let parse = Common.Parsers.parse
    let parse_all p = Common.Parsers.parse (p .>> eof)