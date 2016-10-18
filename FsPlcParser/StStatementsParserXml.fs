namespace FsPlcParser
open FsPlcModel

module StStatementsParserXml = 
    open Common.ParsersXml
    open System.Xml
    open System.Xml.Linq
    
    exception Expected_pou_body_st of XElement
    
    let bodySt (xml : XElement) = 
        if (xml.Name.LocalName <> "ST") then raise (Expected_pou_body_st xml)
        let text = (xml.Elements() |> Seq.head).Value
        StStatementsParser.parse_all StStatementsParser.block text
    
    let parse = Common.ParsersXml.parse