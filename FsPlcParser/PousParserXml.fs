namespace FsPlcParser
open FsPlcModel

module PousParserXml = 
    open Common.ParsersXml
    open Pous
    open System.Xml
    open System.Xml.Linq
    
    exception Expected_pou_interface of XElement
    
    exception Expected_body of XElement
    
    exception Expected_pou of XElement

    exception Expected_pous of XElement
    
    exception Expected_action of XElement
    
    exception Expected_actions of XElement
    
    exception Expected_transition of XElement
    
    exception Expected_transitions of XElement
    
    let pouInterface (xml : XElement) = 
        if (xml.Name.LocalName <> "interface") then raise (Expected_pou_interface xml)
        let returnType = 
            let returnTypeXml = xml.Element(xn_default "returnType")
            if returnTypeXml <> null then Some(TypesParserXml.anyType (returnTypeXml.Elements() |> Seq.head))
            else None
        
        let vars = 
            xml.Elements()
            |> Seq.filter (fun e -> e.Name.LocalName <> "returnType")
            |> Seq.collect DeclarationsParserXml.declaration_block
            |> Seq.toList
        
        { returnType = returnType
          vars = vars }
    
    let bodyElement (xml : XElement) = 
        try 
            AstST(StStatementsParserXml.bodySt xml)
        with _ -> raise (Expected_body xml)
    
    let body (xml : XElement) = Seq.map bodyElement (xml.Elements()) |> List.ofSeq
    
    let action (xml : XElement) : PouActionAst = 
        if (xml.Name.LocalName <> "action") then raise (Expected_action xml)
        let name = xml.Attribute(xn "name").Value
        let body = xml.Elements(xn_default "body") |> Seq.collect body |> List.ofSeq
        { name = name
          body = body }
    
    let actions (xml : XElement) = 
        if (xml.Name.LocalName <> "actions") then raise (Expected_actions xml)
        xml.Elements(xn_default "action")
        |> Seq.map action
        |> List.ofSeq
    
    let transition (xml : XElement) : PouTransitionAst = 
        if (xml.Name.LocalName <> "transition") then raise (Expected_transition xml)
        let name = xml.Attribute(xn "name").Value
        let body = xml.Elements(xn_default "body") |> Seq.collect body |> List.ofSeq
        { name = name
          body = body }
    
    let transitions (xml : XElement) = 
        if (xml.Name.LocalName <> "transitions") then raise (Expected_transitions xml)
        xml.Elements(xn_default "transition")
        |> Seq.map transition
        |> List.ofSeq
    
    let pou (xml : XElement) : PouAst = 
        if (xml.Name.LocalName <> "pou") then raise (Expected_pou xml)
        let name = xml.Attribute(xn "name").Value
        let pouType = PousParser.parse PousParser.pouType (xml.Attribute(xn "pouType").Value)
        
        let pouInterface = 
            let pouInterfaceXml = xml.Element(xn_default "interface")
            if pouInterfaceXml <> null then Some(pouInterface pouInterfaceXml)
            else None
        let pouBody = xml.Elements(xn_default "body") |> Seq.collect body |> List.ofSeq
        
        let pouActions = 
            let pouActionsXml = xml.Element(xn_default "actions")
            if pouActionsXml <> null then actions pouActionsXml
            else []
        
        let pouTransitions = 
            let pouTransitionsXml = xml.Element(xn_default "transitions")
            if pouTransitionsXml <> null then transitions pouTransitionsXml
            else []
        
        { name = name
          typ = pouType
          iface = pouInterface
          bodies = pouBody
          actions = pouActions
          transitions = pouTransitions }

    let pous (xml : XElement) : PouAst list = 
        if (xml.Name.LocalName <> "pous") then raise (Expected_pous xml)
        xml.Elements(xn_default "pou") |> Seq.map pou |> List.ofSeq
    
    let parse = Common.ParsersXml.parse