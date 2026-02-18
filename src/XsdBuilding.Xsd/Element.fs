[<RequireQualifiedAccess>]
module Element

type Element<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes, 'TSimpleTypeAnnotationAppinfoContent, 'TSimpleTypeAnnotationDocumentationContent, 'TSimpleTypeAnnotationAttributes, 'TSimpleTypeAttributes, 'TAttributes>
    =
    { ElementAttributes: ElementAttributes<'TAttributes>
      ElementContent:
          ElementContent<
              'TAnnotationAppinfoContent,
              'TAnnotationDocumentationContent,
              'TAnnotationAttributes,
              'TSimpleTypeAnnotationAppinfoContent,
              'TSimpleTypeAnnotationDocumentationContent,
              'TSimpleTypeAnnotationAttributes,
              'TSimpleTypeAttributes
           > }

and ElementAttributes<'TAttributes> =
    { Abstract: Xml.Bool
      Block: Block
      Default: Xml.String
      Final: Final
      Fixed: Xml.String
      Form: Form
      MaxOccurs: MaxOccurs
      MinOccurs: MinOccurs
      Name: Xml.NCName
      Nillable: Xml.Bool
      Ref: Xml.QName
      SubstitutionGroup: Xml.QName
      Type: Xml.QName
      AdditionalAttributes: 'TAttributes option }

and ElementContent<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes, 'TSimpleTypeAnnotationAppinfoContent, 'TSimpleTypeAnnotationDocumentationContent, 'TSimpleTypeAnnotationAttributes, 'TSimpleTypeAttributes>
    =
    { Annotation:
        Annotation.Annotation<'TAnnotationAppinfoContent, 'TAnnotationDocumentationContent, 'TAnnotationAttributes> option
      TypeCategory:
          TypeCategory<
              'TSimpleTypeAnnotationAppinfoContent,
              'TSimpleTypeAnnotationDocumentationContent,
              'TSimpleTypeAnnotationAttributes,
              'TSimpleTypeAttributes
           > option
      ReferenceCategories: ReferenceCategory list }

and TypeCategory<'TSimpleTypeAnnotationAppinfoContent, 'TSimpleTypeAnnotationDocumentationContent, 'TSimpleTypeAnnotationAttributes, 'TSimpleTypeAttributes>
    =
    // | ComplexType of ComplexType.ComplexType
    | SimpleType of
        SimpleType.SimpleType<
            'TSimpleTypeAnnotationAppinfoContent,
            'TSimpleTypeAnnotationDocumentationContent,
            'TSimpleTypeAnnotationAttributes,
            'TSimpleTypeAttributes
         >

and ReferenceCategory =
    | Key of Key.Key
    | Keyref of Keyref.Keyref
    | Unique of Unique.Unique

and Block =
    | All
    | Derivations of BlockDerivation list

and BlockDerivation =
    | Extension
    | Restriction
    | Substitution

and Final =
    | All
    | Derivations of FinalDerivation list

and FinalDerivation =
    | Extension
    | Restriction

and Form =
    | Qualified
    | Unqualified

and MaxOccurs =
    | NonNegative of Xml.NonNegativeInteger
    | Unbounded

and MinOccurs = NonNegative of Xml.NonNegativeInteger
