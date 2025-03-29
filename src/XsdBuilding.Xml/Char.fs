namespace XsdBuilding.Xml

module Char =
    type CharError =
        | CharOutOfRange
    type Char = private Char of char
    
    let firstBlock = CharRange.create 0x1 0xD7FF
    let secondBlock = CharRange.create 0xE000 0xFFFD
    let thirdBlock = CharRange.create 0x10000 0x10FFFF

    let isValid (char: char) =
        CharRange.containsResult char firstBlock 
        || CharRange.containsResult char secondBlock
        || CharRange.containsResult char thirdBlock

    let create char =
        if isValid char
        then Ok (Char char)
        else Error CharOutOfRange