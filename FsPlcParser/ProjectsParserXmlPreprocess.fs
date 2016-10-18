namespace FsPlcParser
open FsPlcModel

module ProjectsParserXmlPreprocess = 
    open Common.ParsersXml
    open Projects
    open System.Xml
    open System.Xml.Linq
    
    exception Expected_project of XElement
    
    // CoDeSys exports task pou instances with the @typeName attribute empty and the type name in the @name attribute.
    // We fix this by copying @name into @typeName if the latter is empty and renaming @name to the last path component.
    // Ex.: <pouInstance name="MyLibrary.MyPOU" typeName="">...</pouInstance>
    //      <pouInstance name="MyPOU" typeName="MyLibrary.MyPOU">...</pouInstance>
    let fixCodesysTaskPouInstances (xml : XElement) = 
        let pouInstances = xml.Descendants(xn_default "pouInstance")
        pouInstances |> Seq.iter (fun pouInstance -> 
                            let typeName = pouInstance.Attribute(xn "typeName").Value
                            let name = pouInstance.Attribute(xn "name").Value
                            if typeName.Length = 0 then pouInstance.SetAttributeValue(xn "typeName", name))
        xml
    
    // CoDeSys exports pou type definitions as addData within <instances/>
    // We rewrite this to plain plcOpen type definitions within <types/>
    let fixCodesysPouDefinitions (xml : XElement) = 
        if (xml.Name.LocalName <> "project") then raise (Expected_project xml)
        let pous = xml.Descendants(xn_default "pou")
        if not (Seq.isEmpty pous) then
            xml.Element(xn_default "types").Element(xn_default "pous").Add(Seq.toArray pous)
            let pous_to_remove = xml.Descendants(xn_default "instances").Descendants(xn_default "pou")
            pous_to_remove.Remove()
        xml

    let project : XElement -> XElement =  fixCodesysPouDefinitions >> fixCodesysTaskPouInstances
