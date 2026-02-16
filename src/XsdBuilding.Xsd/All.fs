[<RequireQualifiedAccess>]
module All

type All<'TContentElements, 'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes> = {
    AllAttributes: AllAttributes;
    AllContent: AllContent<'TContentElements, 'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes>;
}
and AllAttributes = {
    Id: Xml.Id option;
    MaxOccurs: MaxOccurs;
    MinOccurs: MinOccurs
}
and AllContent<'TContentElements, 'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes> = {
    Annotation: Annotation.Annotation<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes> option;
    Elements: 'TContentElements
}
and MaxOccurs =
    | One
and MinOccurs =
    | Zero
    | One