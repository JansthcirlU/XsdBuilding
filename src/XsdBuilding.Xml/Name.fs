namespace XsdBuilding.Xml

module Name =
    open Char
    open Separated

    type NameStartChar = private NameStartChar of Char
    type NameChar = private NameStartChar of Char
    type Name = private {
        NameStartChar: NameStartChar
        NameChar: NameChar list
    }
    type Names = private Separated of Separated<Name>
    type Nmtoken = private Nmtoken of NameChar list
    type Nmtokens = private Nmtokens of Separated<Nmtoken>