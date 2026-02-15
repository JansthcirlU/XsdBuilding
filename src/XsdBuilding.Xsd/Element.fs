[<RequireQualifiedAccess>]
module Element

type Element<'TAttributes> = {
    ElementAttributes: ElementAttributes<'TAttributes>;
    ElementContent: ElementContent;
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
and ElementContent = ElementContent

// <element
//   abstract = Boolean : false
//   block = (#all | List of (extension | restriction | substitution))
//   default = string
//   final = (#all | List of (extension | restriction))
//   fixed = string
//   form = (qualified | unqualified)
//   id = ID
//   maxOccurs = (nonNegativeInteger | unbounded) : 1
//   minOccurs = nonNegativeInteger : 1
//   name = NCName
//   nillable = Boolean : false
//   ref = QName
//   substitutionGroup = QName
//   type = QName
//   {any attributes with non-schema Namespace}...>
// Content: (annotation?, ((simpleType | complexType)?, (unique | key | keyref)*))
// </element>