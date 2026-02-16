[<RequireQualifiedAccess>]
module SimpleType

type SimpleType<'TAnnotationAppinfoContent,'TAnnotationDocumentationContent,'TAnnotationAttributes, 'TAttributes> = {
    SimpleTypeAttributes: SimpleTypeAttributes<'TAttributes>;
    SimpleTypeContent: SimpleTypeContent<'TAnnotationAppinfoContent,'TAnnotationDocumentationContent,'TAnnotationAttributes>;
}
and SimpleTypeAttributes<'TAttributes> = {
    Final: Final;
    Id: Xml.Id;
    Name: Xml.NCName;
    AdditionalAttributes: 'TAttributes
}
and SimpleTypeContent<'TAnnotationAppinfoContent,'TAnnotationDocumentationContent,'TAnnotationAttributes> = {
    Annotation: Annotation.Annotation<'TAnnotationAppinfoContent,'TAnnotationDocumentationContent,'TAnnotationAttributes> option;
    Restriction: RestrictionCategory
}
and RestrictionCategory =
    | Restriction of SimpleTypeRestriction.SimpleTypeRestriction
    | List of ListElement.List
    | Union of Union.Union
and Final =
    | All
    | Derivations of FinalDerivation list
and FinalDerivation =
    | Restriction
    | List
    | Union