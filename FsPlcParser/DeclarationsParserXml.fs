namespace FsPlcParser
open FsPlcModel

module DeclarationsParserXml = 
    open Common.ParsersXml
    open System.Xml
    open System.Xml.Linq
    open Declarations
    
    exception Expected_var_list of XElement
    
    let kind (xml : XElement) = 
        match xml.Name.LocalName with
        | "localVars" -> VAR
        | "inputVars" -> VAR_INPUT
        | "outputVars" -> VAR_OUTPUT
        | "inOutVars" -> VAR_IN_OUT
        | "externalVars" -> VAR_EXTERNAL
        | "globalVars" -> VAR_GLOBAL
        | "accessVars" -> VAR_ACCESS
        | "tempVars" -> VAR_TEMP
        | _ -> raise (Expected_var_list xml)
    
    exception Expected_initial_value of XElement
    
    let initialValue t (xml : XElement) = 
        if (xml.Name.LocalName <> "initialValue") then raise (Expected_initial_value xml)
        xml.Elements()
        |> Seq.head
        |> ValuesParserXml.value t
    
    exception Expected_declaration of XElement
    
    let declaration k a (xml : XElement) = 
        if (xml.Name.LocalName <> "variable") then raise (Expected_declaration xml)
        let id = xml.Attribute(xn "name").Value
        let typ = TypesParserXml.anyType (xml.Element(xn_default "type").Elements() |> Seq.head)
        
        let value = 
            match xml.Elements(xn_default "initialValue") |> Seq.toList with
            | value :: _ -> Some(initialValue typ value)
            | _ -> None
        { id = id
          typ = typ
          kind = k
          attr = a
          ivalue = value }
    
    let attributes = 
        dict [ "retain", ()
               "non_retain", ()
               "constant", ()
               "r_edge", ()
               "f_edge", ()
               "read_only", ()
               "read_write", () ]
    
    let declaration_block (xml : XElement) = 
        let k = kind xml
        
        let a = 
            xml.Attributes()
            |> Seq.filter (fun a -> attributes.ContainsKey a.Name.LocalName)
            |> Seq.choose (fun a -> 
                   try 
                       let attr = DeclarationsParser.parse DeclarationsParser.attribute a.Name.LocalName
                       let value = XmlConvert.ToBoolean a.Value
                       if value then Some attr
                       else None
                   with _ -> None)
        xml.Elements(xn_default "variable")
        |> Seq.toList
        |> List.map (declaration k (Seq.toList a))
    
    let named_declaration_block (xml : XElement) = 
        let ds = declaration_block xml
        let name = 
            let nameXml = xml.Attribute(xn "name")
            if nameXml <> null then Some nameXml.Value else None
        { name = name
          vars = ds }
    
    let parse = Common.ParsersXml.parse

module DeclarationsParserXmlTests = 
    open NUnit
    open NUnit.Framework
    open Declarations
    
    let actual_equals_expected (actual : obj, expected : obj) = Assert.AreEqual(expected, actual)
    
    [<TestFixture>]
    type DeclarationTests() = 
        let parse = DeclarationsParserXml.parse DeclarationsParserXml.declaration_block
        
        [<Test>]
        member x.``can parse VAR CONSTANT Modus:BOOL := false;END_VAR``() = 
            let input = """<localVars xmlns="http://www.plcopen.org/xml/tc6_0200" constant="true">
                                <variable name="Modus">
                                    <type><BOOL/></type>
                                    <initialValue><simpleValue value="false"/></initialValue>
                                </variable>
                            </localVars>"""
            
            let expected = 
                [ { kind = VAR
                    id = "Modus"
                    attr = [ CONSTANT ]
                    typ = Types.BASIC Types.BOOL
                    ivalue = Some(Values.BASIC <| Values.BOOL false) } ]
            actual_equals_expected (parse input, expected)
        
        [<Test>]
        member x.``can parse VAR CONSTANT Modus:BOOL := false; Druck:Real := 0.01; END_VAR``() = 
            let input = """<localVars xmlns="http://www.plcopen.org/xml/tc6_0200" constant="true">
                                <variable name="Modus">
                                    <type><BOOL/></type>
                                    <initialValue><simpleValue value="false"/></initialValue>
                                </variable>
                                <variable name="Druck">
                                    <type><REAL/></type>
                                    <initialValue><simpleValue value="0.01"/></initialValue>
                                </variable>
                            </localVars>"""
            
            let expected = 
                [ { kind = VAR
                    id = "Modus"
                    attr = [ CONSTANT ]
                    typ = Types.BASIC Types.BOOL
                    ivalue = Some(Values.BASIC <| Values.BOOL false) }
                  { kind = VAR
                    id = "Druck"
                    attr = [ CONSTANT ]
                    typ = Types.BASIC Types.REAL
                    ivalue = Some(Values.BASIC <| Values.REAL 0.01) } ]
            actual_equals_expected (parse input, expected)
    
    [<TestFixture>]
    type NamedDeclarationTests() = 
        let parse = DeclarationsParserXml.parse DeclarationsParserXml.named_declaration_block
        [<Test>]
        member x.``can parse VAR CONSTANT Modus:BOOL := false; Druck:Real := 0.01; END_VAR``() = 
            let input = """<globalVars name="GVL_WHATEVER" xmlns="http://www.plcopen.org/xml/tc6_0200" constant="true">
                                <variable name="Modus">
                                    <type><BOOL/></type>
                                    <initialValue><simpleValue value="false"/></initialValue>
                                </variable>
                                <variable name="Druck">
                                    <type><REAL/></type>
                                    <initialValue><simpleValue value="0.01"/></initialValue>
                                </variable>
                            </globalVars>"""
            
            let expected = 
                { name = Some "GVL_WHATEVER"
                  vars = 
                      [ { kind = VAR_GLOBAL
                          id = "Modus"
                          attr = [ CONSTANT ]
                          typ = Types.BASIC Types.BOOL
                          ivalue = Some(Values.BASIC <| Values.BOOL false) }
                        { kind = VAR_GLOBAL
                          id = "Druck"
                          attr = [ CONSTANT ]
                          typ = Types.BASIC Types.REAL
                          ivalue = Some(Values.BASIC <| Values.REAL 0.01) } ] }
            actual_equals_expected (parse input, expected)
