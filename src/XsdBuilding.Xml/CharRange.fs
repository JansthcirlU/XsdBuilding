namespace XsdBuilding.Xml

module CharRange =
    type CharRangeError =
        | MinGreaterThanMax
    type CharRange = 
        private {
        Min: int
        Max: int
    }

    let create min max =
        if min > max
        then Error MinGreaterThanMax
        else Ok { 
            Min = min
            Max = max
        }
    
    let createMany (ranges: (int * int) list) =
        Helpers.concatWhile ranges (fun (min, max) -> create min max)

    let contains (char: char) range =
        let value = int char
        range.Min <= value && value <= range.Max

    let containsMany (char: char) (ranges: CharRange list) =
        List.exists (contains char) ranges