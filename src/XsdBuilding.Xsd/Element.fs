[<RequireQualifiedAccess>]
module Element

type Element<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes, 'TAttributes> = {
    ElementAttributes: ElementAttributes<'TAttributes>;
    ElementContent: ElementContent<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes>;
}
and ElementAttributes<'TAttributes> = {
    Abstract: Xml.Bool;
    Block: Attribute.Attributes.Block;
    Default: Xml.String;
    Final: Attribute.Attributes.Final;
    Fixed: Xml.String;
    Form: Attribute.Attributes.Form;
    MaxOccurs: Attribute.Attributes.MaxOccurs;
    MinOccurs: Attribute.Attributes.MinOccurs;
    Name: Xml.NCName;
    Nillable: Xml.Bool;
    Ref: Xml.QName;
    SubstitutionGroup: Xml.QName;
    Type: Xml.QName;
    AdditionalAttributes: 'TAttributes option
}
and ElementContent<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes> = {
    Annotation: Annotation.Annotation<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes> option;
    TypeCategory: TypeCategory option;
    ReferenceCategories: ReferenceCategory list
}
and TypeCategory =
    | ComplexType of ComplexType.ComplexType
    | SimpleType of SimpleType.SimpleType
and ReferenceCategory =
    | Key of Key.Key
    | Keyref of Keyref.Keyref
    | Unique of Unique.Unique