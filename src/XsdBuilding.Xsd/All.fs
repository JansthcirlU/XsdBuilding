[<RequireQualifiedAccess>]
module All

type All<'TContentElements, 'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes, 'TAttributes> = {
    AllAttributes: AllAttributes<'TAttributes>;
    AllContent: AllContent<'TContentElements, 'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes>;
}
and AllAttributes<'TAttributes> = {
    Id: Xml.Id option;
    MaxOccurs: MaxOccurs;
    MinOccurs: MinOccurs;
    AdditionalAttributes: 'TAttributes
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