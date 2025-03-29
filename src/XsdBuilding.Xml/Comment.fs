namespace XsdBuilding.Xml

module Comment =
    type CommentChar = private NonDashChar of Char.Char
    type DashCommentString = private DashCommentChar of CommentChar
    type CommentContent =
        private
        | CommentChar of CommentChar
        | DashCommentString of DashCommentString
    type Comment = private Comment of CommentContent list
    type CommentError =
        | CharIsDash
        | InvalidChar
        | EmptyString
        | DoesNotStartWithDash
        | TooManyCharacters

    let private createCommentChar char =
        match char with
        | '-' -> Error CharIsDash
        | nonDashChar ->
            match Char.create nonDashChar with
            | Ok validChar -> Ok (NonDashChar validChar)
            | Error _ -> Error InvalidChar

    let private createDashCommentString (s: string) =
        match Seq.toList s with
        | '-' :: xs ->
            match xs with
            | [ char ] ->
                match createCommentChar char with
                | Ok validChar -> Ok (DashCommentChar validChar)
                | Error error -> Error error
            | _ -> Error TooManyCharacters
        | _ -> Error DoesNotStartWithDash

    let create (s: string) =
        let chunks = Helpers.chunkByPredicate s (fun c -> c = '-') 2 Seq.toArray
        let parseChunk chunk =
            match chunk with
            | [| single |] -> createCommentChar single |> Result.map CommentChar
            | _ -> createDashCommentString (System.String.Concat chunk) |> Result.map DashCommentString
        let folder state chunk =
            match state, parseChunk chunk with
            | Ok contents, Ok content -> Ok (content :: contents)
            | Ok _, Error error -> Error error
            | Error error, _ -> Error error
        match Seq.fold folder (Ok []) chunks with
        | Ok contents -> Ok (Comment (List.rev contents))
        | Error error -> Error error