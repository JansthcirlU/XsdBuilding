namespace XsdBuilding.Xml

module WhiteSpace =
    type WhiteSpaceCharError =
        | InvalidChoice
    
    type WhiteSpaceChar =
        private
        | Space
        | Control9
        | ControlA
        | ControlD
    
    let createWhiteSpaceChar (char: char) =
        match int char with
        | 0x20 -> Ok Space
        | 0x9 -> Ok Control9
        | 0xA -> Ok ControlA
        | 0xD -> Ok ControlD
        | _ -> Error InvalidChoice
    
    type WhiteSpaceError =
        | InvalidWhiteSpaceChar
        | EmptyString
    type WhiteSpace = private WhiteSpace of WhiteSpaceChar list

    let create (s: string) =
        match Helpers.concatWhile (Seq.toList s) createWhiteSpaceChar with
        | Ok [] -> Error EmptyString
        | Ok ws -> Ok (WhiteSpace ws)
        | _ -> Error InvalidWhiteSpaceChar