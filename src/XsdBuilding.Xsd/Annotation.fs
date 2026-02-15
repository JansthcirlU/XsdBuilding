[<RequireQualifiedAccess>]
module Annotation

type Annotation<'TAppinfoContent, 'TDocumentationContent, 'TAttributes> = {
    AnnotationAttributes: AnnotationAttributes<'TAttributes>;
    AnnotationContent: AnnotationContent<'TAppinfoContent, 'TDocumentationContent>;
}
and AnnotationAttributes<'TAttributes> = {
    Id: Xml.Id option;
    AdditionalAttributes: 'TAttributes option
}
and AnnotationContent<'TAppinfoContent, 'TDocumentationContent> = AnnotationContent of AnnotationContentType<'TAppinfoContent, 'TDocumentationContent> list
and AnnotationContentType<'TAppinfoContent, 'TDocumentationContent> =
    | Appinfo of Appinfo.Appinfo<'TAppinfoContent>
    | Documentation of Documentation.Documentation<'TDocumentationContent>
