[<RequireQualifiedAccess>]
module ComplexType

type ComplexType<'TAttributes> = {
    ComplexTypeAttributes: ComplexTypeAttributes<'TAttributes>;
    ComplexTypeContent: ComplexTypeContent;
}
and ComplexTypeAttributes<'TAttributes> = {
    Abstract: Xml.Bool;
    Block: Block;
    Final: Final;
    Id: Xml.Id;
    Mixed: Xml.Bool;
    Name: Xml.NCName;
    AdditionalAttributes: 'TAttributes
}
//  (
//      annotation?,
//      (
//          simpleContent | 
//          complexContent | 
//          (
//              (
//                  group |
//                  all |
//                  choice |
//                  sequence
//              )?,
//              (
//                  (
//                      attribute |
//                      attributeGroup
//                  )*,
//                  anyAttribute?
//              )
//          )
//      )
//  )
and ComplexTypeContent = {
    Annotation: Annotation.Annotation option;
    ContentCategory: ComplexTypeContentCategory;
}
and ComplexTypeContentCategory =
    | SimpleContent
    | ComplexContent
    | Hierarchal of Hierarchal
and Hierarchal = {
    Collection: HierarchalCollection option;
    Attribute: HierarchalAttribute
}
and HierarchalCollection =
    | All
    | Group
    | Choice
    | Sequence
and HierarchalAttribute = {
    AttributeCategory: AttributeCategory list;
    AnyAttribute: AnyAttribute.AnyAttribute option;
}
and AttributeCategory =
    | Attribute
    | AttributeGroup
and Block =
    | All
    | Derivations of BlockDerivation list
and BlockDerivation =
    | Extension
    | Restriction
and Final =
    | All
    | Derivations of FinalDerivation list
and FinalDerivation =
    | Extension
    | Restriction