namespace FsPlcParser
open FsPlcModel

module ProjectsParserXml = 
    open Common.ParsersXml
    open Projects
    open System.Xml
    open System.Xml.Linq
    
    exception Expected_data_type of XElement
    
    exception Expected_data_types of XElement
    
    exception Expected_types of XElement
    
    exception Expected_resource of XElement
    
    exception Expected_pou_instance of XElement
    
    exception Expected_configuration of XElement
    
    exception Expected_configurations of XElement
    
    exception Expected_instances of XElement
    
    exception Expected_project of XElement
    
    let dataType (xml : XElement) = 
        if (xml.Name.LocalName <> "dataType") then raise (Expected_data_type xml)
        let name = xml.Attribute(xn "name").Value
        let baseType = xml.Element(xn_default "baseType") |> TypesParserXml.anyType
        
        let initialValue = 
            let initialValueXml = xml.Element(xn_default "initialValue")
            if initialValueXml <> null then Some(ValuesParserXml.value baseType initialValueXml)
            else None
        { name = name
          typ = baseType
          ivalue = initialValue }
    
    let dataTypes (xml : XElement) = 
        if (xml.Name.LocalName <> "dataTypes") then raise (Expected_data_types xml)
        xml.Elements(xn_default "dataType")
        |> Seq.map dataType
        |> List.ofSeq
    
    let projectTypes (xml : XElement) : ProjectTypesAst = 
        if (xml.Name.LocalName <> "types") then raise (Expected_types xml)
        let pous = xml.Element(xn_default "pous") |> PousParserXml.pous
        let dataTypes = xml.Element(xn_default "dataTypes") |> dataTypes
        { pous = pous
          dataTypes = dataTypes }
    
    let pouInstance (xml : XElement) = 
        if (xml.Name.LocalName <> "pouInstance") then raise (Expected_pou_instance xml)
        let name = xml.Attribute(xn "name").Value |> Common.Parsers.parse Common.Parsers.qident |> List.rev |> List.head
        let typeName = xml.Attribute(xn "typeName").Value |> Common.Parsers.parse Common.Parsers.qident
        (name, typeName)
    
    let resource (xml : XElement) = 
        if (xml.Name.LocalName <> "resource") then raise (Expected_resource xml)
        let name = xml.Attribute(xn "name").Value
        
        let tasks = 
            xml.Elements(xn_default "task")
            |> Seq.map TasksParserXml.task
            |> List.ofSeq
        
        let vars = 
            xml.Elements(xn_default "globalVars")
            |> Seq.map DeclarationsParserXml.named_declaration_block
            |> List.ofSeq
        
        let pouInstances = 
            xml.Elements(xn_default "pouInstance")
            |> Seq.map pouInstance
            |> List.ofSeq
        
        { name = name
          tasks = tasks
          vars = vars
          pouInstances = pouInstances }
    
    let configuration (xml : XElement) = 
        if (xml.Name.LocalName <> "configuration") then raise (Expected_configuration xml)
        let name = xml.Attribute(xn "name").Value
        
        let resources = 
            xml.Elements(xn_default "resource")
            |> Seq.map resource
            |> List.ofSeq
        
        let var_block xs = Seq.map DeclarationsParserXml.named_declaration_block xs |> List.ofSeq
        let gvars = xml.Elements(xn_default "globalVars") |> var_block
        let avars = xml.Elements(xn_default "accessVars") |> var_block
        let cvars = xml.Elements(xn_default "configVars") |> var_block
        { name = name
          resources = resources
          vars = gvars @ avars @ cvars }
    
    let configurations (xml : XElement) = 
        if (xml.Name.LocalName <> "configurations") then raise (Expected_configurations xml)
        xml.Elements(xn_default "configuration")
        |> Seq.map configuration
        |> List.ofSeq
    
    let instances (xml : XElement) = 
        if (xml.Name.LocalName <> "instances") then raise (Expected_instances xml)
        xml.Elements(xn_default "configurations")
        |> Seq.collect configurations
        |> List.ofSeq

    
    let project (xml : XElement) : ProjectAst = 
        let xml = ProjectsParserXmlPreprocess.project xml
        if (xml.Name.LocalName <> "project") then raise (Expected_project xml)
        let types = xml.Element(xn_default "types") |> projectTypes
        let configurations = xml.Element(xn_default "instances") |> instances
        { types = types
          libraries = []
          configurations = configurations }