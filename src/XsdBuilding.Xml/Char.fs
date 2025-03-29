namespace XsdBuilding.Xml

module Char =
    type CharError =
        private
        | CharOutOfRange
    type Char = private Char of char
    
    let private charBlocks = CharRange.createMany [
        0x1, 0xD7FF
        0xE000, 0xFFFD
        0x10000, 0x10FFFF
    ]

    let private isValid (char: char) =
        match charBlocks with
        | Ok blocks -> CharRange.containsMany char blocks
        | _ -> false

    let create char =
        if isValid char
        then Ok (Char char)
        else Error CharOutOfRange