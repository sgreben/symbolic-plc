namespace FsPlcParser
open FsPlcModel

module ValuesParserXml = 
    open Common.ParsersXml
    open System.Xml
    open System.Xml.Linq
    
    exception Expected_simple_value of XElement
    
    exception Expected_array_value of XElement
    
    exception Expected_struct_value of XElement
    
    exception Expected_enum_value of XElement
    
    exception Expected_enum_value_of_type of XElement * Set<Common.Identifier>
    
    exception Invalid_argument of Types.Type
    
    let simpleValue t (xml : XElement) = 
        if (xml.Name.LocalName <> "simpleValue") then raise (Expected_simple_value xml)
        ValuesParser.parse (ValuesParser.basicValue t) (xml.Attribute(xn "value").Value)
    
    let enumValue (t : Set<string>) (xml : XElement) = 
        if (xml.Name.LocalName <> "enumValue") then raise (Expected_enum_value xml)
        let value = xml.Attribute(xn "name").Value
        if not (t.Contains value) then raise (Expected_enum_value_of_type(xml, t))
        value
    
    let rec arrayValue t (xml : XElement) = 
        if (xml.Name.LocalName <> "arrayValue") then raise (Expected_array_value xml)
        Seq.toList (Seq.map (value t) (xml.Elements()))
    
    and structValue t (xml : XElement) : (Common.Identifier * Values.Value) list = 
        if (xml.Name.LocalName <> "structValue") then raise (Expected_struct_value xml)
        let t_lookup = Map.ofList t
        Seq.toList (xml.Elements()) |> List.map (fun e -> 
                                           let field = e.Attribute(xn "member").Value
                                           let t = t_lookup.Item field
                                           field, value t e)
    
    and value t (xml : XElement) = 
        match t with
        | Types.TYPE_REFERENCE _ -> raise (Invalid_argument t)
        | Types.BASIC t -> Values.BASIC(simpleValue t xml)
        | Types.ARRAY(_, t) -> Values.ARRAY(arrayValue t xml)
        | Types.STRUCT t -> Values.STRUCT(structValue t xml)
        | Types.BASIC_RANGE(t, r) -> Values.BASIC(simpleValue t xml)
        | Types.ENUM t -> Values.ENUM(enumValue t xml)
    
    let parse = Common.ParsersXml.parse
