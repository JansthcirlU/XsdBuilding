[<RequireQualifiedAccess>]
module Xml

type Id = private Id of string
type AnyURI = private AnyURI of string
type Lang = private Lang of string

module Id =
    let create (id: string) =
        Ok (Id id)

module AnyURI =
    let create (uri: string) =
        Ok (AnyURI uri)

module Lang =
    let create (lang: string) =
        Ok (Lang lang)