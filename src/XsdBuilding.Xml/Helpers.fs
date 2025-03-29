namespace XsdBuilding.Xml

module Helpers =
    let rec concatWhile<'TValue, 'TResult,'TError> (values: 'TValue list) (mapper: 'TValue -> Result<'TResult, 'TError>) =
        let folder state element =
            match state with
            | Error err -> Error err
            | Ok accumulator ->
                match mapper element with
                | Ok mapped -> Ok (mapped :: accumulator)
                | Error err -> Error err
        List.fold folder (Ok []) values
        |> Result.map List.rev

    let rec private chunkByPredicateLazy<'TSource, 'TAggregated>
        (source: seq<'TSource>)
        (predicate: 'TSource -> bool)
        (chunkSize: int)
        (transform: seq<'TSource> -> 'TAggregated) =
        source
        |> Seq.unfold (fun remaining ->
            if Seq.isEmpty remaining then
                None
            else
                let firstElement = Seq.head remaining
                let size =
                    if predicate firstElement
                    then chunkSize 
                    else 1
                
                // Take the chunk and transform it
                let chunk = remaining |> Seq.truncate size
                let result = transform chunk
                
                // Skip safely to the next starting position
                let rest = System.Linq.Enumerable.Skip (remaining, size)
                Some (result, rest))

    let rec chunkByPredicate<'TSource, 'TAggregated> (source: seq<'TSource>) (predicate: 'TSource -> bool) (chunkSize: int) (transform: seq<'TSource> -> 'TAggregated) =
        chunkByPredicateLazy (Seq.cache source) predicate chunkSize transform