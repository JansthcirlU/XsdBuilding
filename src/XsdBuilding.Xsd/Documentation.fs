[<RequireQualifiedAccess>]
module Documentation

type Documentation<'TContent> = {
    DocumentationAttributes: DocumentationAttributes;
    DocumentationContent: DocumentationContent<'TContent>;
}
and DocumentationAttributes = {
    Source: Xml.AnyURI option;
    Language: Xml.Lang option
}
and DocumentationContent<'TContent> = DocumentationContent of 'TContent
