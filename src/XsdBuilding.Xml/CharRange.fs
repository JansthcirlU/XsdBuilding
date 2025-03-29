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

    let contains (char: char) range =
        let value = int char
        range.Min <= value && value <= range.Max
    
    let containsResult char rangeResult =
        match rangeResult with
        | Ok range -> contains char range
        | Error _ -> false