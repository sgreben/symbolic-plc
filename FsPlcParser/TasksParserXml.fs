namespace FsPlcParser
open FsPlcModel

module TasksParserXml = 
    open Common.ParsersXml
    open System.Xml
    open System.Xml.Linq
    open Tasks
    
    exception Expected_task of XElement
    
    exception Expected_task_pou_instance of XElement
    
    let taskPouInstance (xml : XElement) = 
        if (xml.Name.LocalName <> "pouInstance") then raise (Expected_task_pou_instance xml)
        let name = Common.Parsers.parse Common.Parsers.ident (xml.Attribute(xn "name").Value)
        let typ =  Common.Parsers.parse Common.Parsers.qident (xml.Attribute(xn "typeName").Value)
        name,typ
    
    let taskProperty (xa : XAttribute) = 
        match xa.Name.LocalName with
        | "interval" -> Some <| INTERVAL(TasksParser.parse TasksParser.taskInterval xa.Value)
        | "single" -> Some <| SINGLE(TasksParser.parse TasksParser.taskSingle xa.Value)
        | "priority" -> Some <| PRIORITY(TasksParser.parse TasksParser.taskPriority xa.Value)
        | _ -> None
    
    let taskProperties (xas : seq<XAttribute>) = List.ofSeq (Seq.choose taskProperty xas)
    
    let task (xml : XElement) = 
        if (xml.Name.LocalName <> "task") then raise (Expected_task xml)
        let ps = taskProperties (xml.Attributes())
        let name = xml.Attribute(xn "name").Value
        let pouInstances = xml.Elements(xn_default "pouInstance") |> Seq.map taskPouInstance |> List.ofSeq
        { name = name
          properties = ps
          pou =  pouInstances }
    

    
    
    let parse = Common.ParsersXml.parse