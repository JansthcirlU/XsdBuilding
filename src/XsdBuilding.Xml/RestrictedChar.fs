namespace XsdBuilding.Xml

module RestrictedChar =
    open CharRange
    
    type CharError =
        | CharOutOfRange
    type RestrictedChar = private RestrictedChar of char
    
    let firstBlock = create 0x1 0x8
    let secondBlock = create 0xB 0xC
    let thirdBlock = create 0xE 0x1F
    let fourthBlock = create 0x7F 0x84
    let fifthBlock = create 0x86 0x9F

    let isValid (char: char) =
        containsResult char firstBlock 
        || containsResult char secondBlock
        || containsResult char thirdBlock
        || containsResult char fourthBlock
        || containsResult char fifthBlock

    let create char =
        if isValid char
        then Ok (RestrictedChar char)
        else Error CharOutOfRange