namespace XsdBuilding.Xml

module Prolog =
    type VersionInfo = string
    type XMLDecl = string
    type MiscList = Misc.Misc list
    type Prolog = {
        XMLDecl: XMLDecl option
        Misc: MiscList option
    }