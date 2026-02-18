open XsdType

let appinfo =
    """
<appinfo
  source = anyURI>
Content: ({any})*
</appinfo>
    """
let documentation =
    """
<documentation
  source = anyURI
  xml:lang = language>
Content: ({any})*
</documentation>
    """
let annotation =
    """
<annotation
  id = ID 
  {any attributes with non-schema Namespace...}>
Content: (appinfo | documentation)*
</annotation>
    """
let simpleContent =
    """
<simpleContent
  id = ID 
  {any attributes with non-schema Namespace...}>
Content: (annotation?, (restriction | extension))
</simpleContent>
    """
let complexType =
    """
<complexType
  abstract = Boolean : false 
  block = (#all | List of (extension | restriction))
  final = (#all | List of (extension | restriction))
  id = ID 
  mixed = Boolean : false
  name = NCName 
  {any attributes with non-schema Namespace...}>
Content: (annotation?, (simpleContent | complexContent | ((group | all | 
choice | sequence)?, ((attribute | attributeGroup)*, anyAttribute?))))
</complexType>
    """
let simpleContentRestriction =
    """
<restriction
  base = QName 
  id = ID 
  {any attributes with non-schema Namespace...}>
Content: (annotation?, (simpleType?, (minExclusive | minInclusive | 
maxExclusive | maxInclusive | totalDigits |fractionDigits | length | 
minLength | maxLength | enumeration | whiteSpace | pattern)*)?, 
((attribute | attributeGroup)*, anyAttribute?))
</restriction>
    """
let simpleType =
    """
<simpleType
  final = (#all | (list | union | restriction)) 
  id = ID 
  name = NCName 
  {any attributes with non-schema Namespace...}>
Content: (annotation?, (restriction | list | union))
</simpleType>
    """
let simpleTypeRestriction =
    """
<restriction
  base = QName 
  id = ID 
  {any attributes with non-schema Namespace...}>
Content: (annotation?, (simpleType?, (minExclusive | minInclusive | 
maxExclusive | maxInclusive | totalDigits |fractionDigits | length | 
minLength | maxLength | enumeration | whiteSpace | pattern)*))
</restriction>
    """
let parsedElements =
    Map.empty
    |> validateAndParse appinfo "appinfo"
    |> validateAndParse documentation "documentation"
    |> validateAndParse annotation "annotation"
    // |> validateAndParse simpleTypeRestriction "simpleType/restriction"
    // |> validateAndParse simpleType "simpleType"
    // |> validateAndParse simpleContentRestriction "simpleContent/restriction"
    // |> validateAndParse simpleContent "simpleContent"
    // |> validateAndParse complexType "complexType"

printf $"{parsedElements}"