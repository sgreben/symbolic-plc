namespace FsPlcParser
open FsPlcModel

module TypesParserXml = 
    open Common.ParsersXml
    open System.Xml
    open System.Xml.Linq
    open Types
    
    exception Expected_basic_type of XElement
    
    let basicType (xml : XElement) = 
        match xml.Name.LocalName with
        | "BOOL" -> BOOL
        | "BYTE" -> BYTE
        | "WORD" -> WORD
        | "DWORD" -> DWORD
        | "LWORD" -> LWORD
        | "INT" -> INT
        | "SINT" -> SINT
        | "DINT" -> DINT
        | "LINT" -> LINT
        | "UINT" -> UINT
        | "USINT" -> USINT
        | "UDINT" -> UDINT
        | "ULINT" -> ULINT
        | "REAL" -> REAL
        | "LREAL" -> LREAL
        | "TIME" -> TIME
        | "DATE" -> DATE
        | "TOD" -> TIME_OF_DAY
        | "DT" -> DATE_AND_TIME
        | "STRING" -> STRING
        | "WSTRING" -> WSTRING
        | _ -> raise (Expected_basic_type xml)
    
    exception Expected_derived_type of XElement
    
    exception Expected_array_dimension of XElement
    
    exception Expected_array_type of XElement
    
    exception Expected_struct_type_entry of XElement
    
    exception Expected_struct_type of XElement
    
    exception Expected_type of XElement

    exception Expected_range of XElement

    exception Expected_subrange_type of XElement

    exception Expected_enum_type of XElement

    exception Expected_enum_type_value of XElement

    
    let arrayDimension (xml : XElement) = 
        if (xml.Name.LocalName <> "dimension") then raise (Expected_array_dimension xml)
        let l, u = xml.Attribute(xn "lower"), xml.Attribute(xn "upper")
        (System.Int32.Parse l.Value, System.Int32.Parse u.Value)
    
    let arrayDimensions (xmls : seq<XElement>) = Seq.toList (Seq.map arrayDimension xmls)
    
    let range (xml:XElement) = 
        if (xml.Name.LocalName <> "range") then raise (Expected_array_dimension xml)
        let l, u = xml.Attribute(xn "lower"), xml.Attribute(xn "upper")
        (System.Int64.Parse l.Value, System.Int64.Parse u.Value)

    let enumTypeValue (xml:XElement) = 
        if (xml.Name.LocalName <> "value") then raise (Expected_enum_type_value xml)
        xml.Attribute(xn "name").Value

    let enumType (xml : XElement) =
        if (xml.Name.LocalName <> "enum") then raise (Expected_enum_type xml)
        Set.ofSeq (Seq.map enumTypeValue (xml.Element(xn_default "values").Elements(xn_default "value")))

    let rec arrayType (xml : XElement) = 
        if (xml.Name.LocalName <> "array") then raise (Expected_array_type xml)
        let dimensions = arrayDimensions (xml.Elements(xn_default "dimension"))
        let baseType = anyType (xml.Element(xn_default "baseType").Elements() |> Seq.head)
        (dimensions, baseType)

    and structTypeEntry (xml : XElement) = 
        if (xml.Name.LocalName <> "variable") then raise (Expected_struct_type_entry xml)
        let id = xml.Attribute(xn "name").Value
        let typ = anyType (xml.Element(xn_default "type").Elements() |> Seq.head)
        (id, typ)
    
    and structTypeEntries (xmls : seq<XElement>) = Seq.toList (Seq.map structTypeEntry xmls)
    
    and structType (xml : XElement) = 
        if (xml.Name.LocalName <> "struct") then raise (Expected_struct_type xml)
        structTypeEntries (xml.Elements(xn_default "variable"))
    
    and srangedType (xml : XElement) = 
        if (xml.Name.LocalName <> "subrangeSigned") then raise (Expected_subrange_type xml)
        let range = range (xml.Element(xn_default "range"))
        let baseType = basicType (xml.Element(xn_default "baseType").Elements() |> Seq.head)
        (baseType, range)
    
    and urangedType (xml : XElement) = 
        if (xml.Name.LocalName <> "subrangeUnsigned") then raise (Expected_array_type xml)
        let range = range (xml.Element(xn_default "range"))
        let baseType = basicType (xml.Element(xn_default "baseType").Elements() |> Seq.head)
        (baseType, range)
    
    and derivedType (xml : XElement) = 
        if (xml.Name.LocalName <> "derived") then raise (Expected_derived_type xml)
        xml.Attribute(xn "name").Value |> Common.Parsers.parse Common.Parsers.qident


    and anyType (xml : XElement) = 
        try ARRAY(arrayType xml) with _ -> 
        try STRUCT(structType xml) with _ -> 
        try BASIC(basicType xml) with _ -> 
        try BASIC_RANGE(srangedType xml) with _ -> 
        try BASIC_RANGE(urangedType xml) with _ -> 
        try TYPE_REFERENCE(derivedType xml) with _ -> 
        try ENUM(enumType xml) with _ ->
        raise (Expected_type xml)
    
    let parse = Common.ParsersXml.parse