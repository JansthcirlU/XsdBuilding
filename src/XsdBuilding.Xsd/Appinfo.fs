[<RequireQualifiedAccess>]
module Appinfo

type Appinfo<'TContent> = {
    AppinfoAttributes: AppinfoAttributes;
    AppinfoContent: AppinfoContent<'TContent>;
}
and AppinfoAttributes = {
    Source: Xml.AnyURI option
}
and AppinfoContent<'TContent> = AppinfoContent of 'TContent
