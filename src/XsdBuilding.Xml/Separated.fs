namespace XsdBuilding.Xml

module Separated =
    type SeparatedError =
        | Empty
    type Padded<'TSeparated> = private Padded of 'TSeparated
    let padded value = Padded value
    type Separated<'TSeparated> = private {
        First: 'TSeparated
        Rest: Padded<'TSeparated> list
    }
    let create values =
        match values with
        | first :: rest -> Ok {
            First = first
            Rest = rest |> List.map Padded }
        | [] -> Error Empty