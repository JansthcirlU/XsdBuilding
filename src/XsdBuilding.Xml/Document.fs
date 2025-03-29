namespace XsdBuilding.Xml

module Document =
    open Prolog
    open Element
    open Misc

    type MiscList = Misc list

    type Document = {
        Prolog: Prolog
        Element: Element
        Misc: MiscList option
    }