namespace XsdBuilding.Xml

module Helpers =
    let rec concatWhile<'TValue, 'TResult,'TError> (values: 'TValue list) (mapper: 'TValue -> Result<'TResult, 'TError>) =
        match values with
        | [] -> Ok []
        | x :: xs ->
            match mapper x with
            | Error e -> Error e
            | Ok v ->
                match concatWhile xs mapper with
                | Error e -> Error e
                | Ok rest -> Ok (v :: rest)