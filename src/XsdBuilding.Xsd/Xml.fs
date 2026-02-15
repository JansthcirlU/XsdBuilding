[<RequireQualifiedAccess>]
module Xml

type Id = Id of string
type AnyURI = AnyURI of string
type Lang = Lang of string
type NCName = NCName of string
type QName = QName of string
type Bool =
    | True
    | False
type String = string
type NonNegativeInteger = private NonNegativeInteger of int

module Id =
    let create (id: string) =
        Ok (Id id)

module AnyURI =
    let create (uri: string) =
        Ok (AnyURI uri)

module Lang =
    let create (lang: string) =
        Ok (Lang lang)

module NonNegativeInteger =
    type NonNegativeIntegerError =
        | Negative of int
    let create (num: int) =
        if num > 0 then Ok (NonNegativeInteger num)
        else Error (Negative num)
    let int (NonNegativeInteger x) = x