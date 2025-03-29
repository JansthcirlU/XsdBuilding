namespace XsdBuilding.Xml

module RestrictedChar =
    type CharError =
        private
        | CharOutOfRange
    type RestrictedChar = private RestrictedChar of char

    let restrictedCharBlocks = CharRange.createMany [
        0x1, 0x8
        0xB, 0xC
        0xE, 0x1F
        0x7F, 0x84
        0x86, 0x9F
    ]

    let private isValid (char: char) =
        match restrictedCharBlocks with
        | Ok blocks -> CharRange.containsMany char blocks
        | _ -> false

    let create char =
        if isValid char
        then Ok (RestrictedChar char)
        else Error CharOutOfRange