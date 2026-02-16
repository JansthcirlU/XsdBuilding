[<RequireQualifiedAccess>]
module rec Attribute

type Attribute = {
    AttributeAttributes: AttributeAttributes;
    AttributeContent: AttributeContent;
}
and AttributeAttributes = AttributeAttributes
and AttributeContent = AttributeContent

// module Attributes =
//     type Form =
//         | Qualified
//         | Unqualified
//     type Use =
//         | Optional
//         | Prohibited
//         | Required
//     type MinOccurs =
//         | NoValue
//         | Zero
//         | NonNegative of Xml.NonNegativeInteger
//     type MaxOccurs =
//         | NoValue
//         | Unbounded
//         | NonNegative of Xml.NonNegativeInteger
//     type Block =
//         | All
//         | Derivations of BlockDerivation list
//     type BlockDerivation =
//         | Extension
//         | Restriction
//         | Substitution
//     type Final =
//         | All
//         | Derivations of FinalDerivation list
//     type FinalDerivation =
//         | Extension
//         | Restriction
