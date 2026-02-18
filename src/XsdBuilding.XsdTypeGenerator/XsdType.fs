module XsdType

open System
open System.Text.RegularExpressions

// ============================================================
//  Type definitions
// ============================================================

type XsdType = {
    TypeName: string
    Attributes: Map<string, AttributeCombination>
    AllowsAdditionalAttributes: bool
    Content: XsdTypeCombination
}
and AttributeCombination =
    | AttributeValue of string
    | JustOneAttribute of JustOneAttribute
    | AndAttributes of AttributeCombination list
    | OrAttributes of AttributeCombination list
    | ListAttribute of AttributeCombination
and JustOneAttribute =
    | OptionalAttribute of AttributeCombination
    | RequiredAttribute of AttributeCombination
and XsdTypeCombination =
    | Any
    | TypeValue of XsdType
    | JustOneType of JustOneType
    | AndTypes of XsdTypeCombination list
    | OrTypes of XsdTypeCombination list
    | ListType of XsdTypeCombination
and JustOneType =
    | OptionalType of XsdTypeCombination
    | RequiredType of XsdTypeCombination

// ============================================================
//  Shared helpers
// ============================================================

let private skipWs (s: string) i =
    let mutable j = i
    while j < s.Length && Char.IsWhiteSpace s.[j] do j <- j + 1
    j

/// Wraps a combination in JustOneType (RequiredType ...) unless it is
/// already a fully resolved combination (i.e. Any), in which case it
/// is returned as-is to avoid redundant nesting.
let private wrapRequired = function
    | Any   -> Any
    | combo -> JustOneType (RequiredType combo)

// ============================================================
//  Content expression parser
//
//  Precedence (low → high): or < and < item < atom
//
//  'prefix' is the key of the element currently being parsed,
//  used to resolve context-dependent child elements.
// ============================================================

let rec private parseContentOr (prefix: string) (typeMap: Map<string, XsdType>) (s: string) i : XsdTypeCombination * int =
    let head, i = parseContentAnd prefix typeMap s i
    let acc = ResizeArray [head]
    let mutable pos = skipWs s i
    while pos < s.Length && s.[pos] = '|' do
        let item, i2 = parseContentAnd prefix typeMap s (pos + 1)
        acc.Add item
        pos <- skipWs s i2
    (if acc.Count = 1 then acc.[0] else OrTypes (Seq.toList acc)), pos

and private parseContentAnd prefix typeMap s i : XsdTypeCombination * int =
    let head, i = parseContentItem prefix typeMap s i
    let acc = ResizeArray [head]
    let mutable pos = skipWs s i
    while pos < s.Length && s.[pos] = ',' do
        let item, i2 = parseContentItem prefix typeMap s (pos + 1)
        acc.Add item
        pos <- skipWs s i2
    (if acc.Count = 1 then acc.[0] else AndTypes (Seq.toList acc)), pos

and private parseContentItem prefix typeMap s i : XsdTypeCombination * int =
    let atom, i = parseContentAtom prefix typeMap s i
    if i < String.length s then
        match s.[i] with
        | '?' -> JustOneType (OptionalType atom), i + 1
        | '*'
        | '+' -> ListType atom, i + 1
        | _   -> wrapRequired atom, i
    else
        wrapRequired atom, i

and private parseContentAtom (prefix: string) (typeMap: Map<string, XsdType>) (s: string) i : XsdTypeCombination * int =
    let pos = skipWs s i
    if pos >= s.Length then
        failwith "parseContentAtom: unexpected end of input"
    if s.[pos] = '{' then
        let closing = s.IndexOf('}', pos)
        if closing < 0 then
            failwith "parseContentAtom: unclosed '{'"
        let token = s.[pos + 1 .. closing - 1].Trim()
        if token <> "any" then
            failwithf "parseContentAtom: unknown brace token '{%s}'" token
        Any, closing + 1
    elif s.[pos] = '(' then
        let inner, i2 = parseContentOr prefix typeMap s (pos + 1)
        let pos2 = skipWs s i2
        if pos2 >= s.Length || s.[pos2] <> ')' then
            failwithf "parseContentAtom: expected ')' at position %d" pos2
        inner, pos2 + 1
    else
        let mutable k = pos
        while k < s.Length && (Char.IsLetterOrDigit s.[k] || s.[k] = '-' || s.[k] = '_') do
            k <- k + 1
        if k = pos then
            failwithf "parseContentAtom: unexpected character '%c' at position %d" s.[pos] pos
        let name = s.[pos .. k - 1]
        // Look up the plain name first; if not found, try the qualified key.
        let qualifiedName = prefix + "/" + name
        match Map.tryFind name typeMap with
        | Some t -> TypeValue t, k
        | None   ->
            match Map.tryFind qualifiedName typeMap with
            | Some t -> TypeValue t, k
            | None   -> failwithf "parseContentAtom: unknown type '%s' (also tried '%s')" name qualifiedName

// ============================================================
//  Attribute value parser
//
//    attrOr   = attrItem ('|' attrItem)*
//    attrItem = '(' attrOr ')'
//             | 'List' 'of' '(' attrOr ')'
//             | TOKEN
// ============================================================

let rec private parseAttrOr (s: string) i : AttributeCombination * int =
    let head, i = parseAttrItem s i
    let acc = ResizeArray [head]
    let mutable pos = skipWs s i
    while pos < s.Length && s.[pos] = '|' do
        let item, i2 = parseAttrItem s (pos + 1)
        acc.Add item
        pos <- skipWs s i2
    (if acc.Count = 1 then acc.[0] else OrAttributes (Seq.toList acc)), pos

and private parseAttrItem (s: string) i : AttributeCombination * int =
    let pos = skipWs s i
    if pos >= s.Length then
        failwith "parseAttrItem: unexpected end of input"
    if s.[pos] = '(' then
        let inner, i2 = parseAttrOr s (pos + 1)
        let pos2 = skipWs s i2
        if pos2 >= s.Length || s.[pos2] <> ')' then
            failwithf "parseAttrItem: expected ')' at position %d" pos2
        inner, pos2 + 1
    else
        let mutable k = pos
        while k < s.Length && (Char.IsLetterOrDigit s.[k] || s.[k] = '-' || s.[k] = '_' || s.[k] = '#') do
            k <- k + 1
        if k = pos then
            failwithf "parseAttrItem: unexpected character '%c' at position %d" s.[pos] pos
        let token = s.[pos .. k - 1]
        let k2 = skipWs s k
        if token = "List" && k2 + 1 < s.Length && s.[k2 .. k2 + 1] = "of" then
            let k3 = skipWs s (k2 + 2)
            if k3 >= s.Length || s.[k3] <> '(' then
                failwith "parseAttrItem: expected '(' after 'List of'"
            let inner, k4 = parseAttrOr s (k3 + 1)
            let k5 = skipWs s k4
            if k5 >= s.Length || s.[k5] <> ')' then
                failwith "parseAttrItem: expected ')' to close 'List of ('"
            ListAttribute inner, k5 + 1
        else
            AttributeValue token, k

// ============================================================
//  Splitting the raw text into its logical sections
// ============================================================

let private findOpenTagEnd (s: string) =
    let mutable depth = 0
    let mutable i = 1
    let mutable result = -1
    while i < s.Length && result = -1 do
        match s.[i] with
        | '(' -> depth <- depth + 1; i <- i + 1
        | ')' -> depth <- depth - 1; i <- i + 1
        | '>' when depth = 0 -> result <- i
        | _ -> i <- i + 1
    if result = -1 then failwith "findOpenTagEnd: no closing '>' found"
    result

// ============================================================
//  Attribute line parsing
// ============================================================

let private stripDefault (s: string) =
    let mutable depth = 0
    let mutable colonIdx = -1
    let mutable i = 0
    while i < s.Length && colonIdx = -1 do
        match s.[i] with
        | '(' -> depth <- depth + 1; i <- i + 1
        | ')' -> depth <- depth - 1; i <- i + 1
        | ':' when depth = 0 -> colonIdx <- i
        | _ -> i <- i + 1
    if colonIdx >= 0 then s.[0 .. colonIdx - 1].Trim() else s

let private parseAttributeLine (line: string) =
    let eqIdx = line.IndexOf '='
    if eqIdx <= 0 then None
    else
        let name  = line.[0 .. eqIdx - 1].Trim()
        let value = line.[eqIdx + 1 ..].Trim() |> stripDefault
        let combo, _ = parseAttrOr value 0
        Some (name, combo)

// ============================================================
//  Top-level entry points
// ============================================================

let parseXsdType (key: string) (typeMap: Map<string, XsdType>) (input: string) : XsdType =
    let input          = input.Trim()
    let openTagEnd     = findOpenTagEnd input
    let openTagContent = input.[1 .. openTagEnd - 1].Trim()
    let nameEnd =
        openTagContent
        |> Seq.tryFindIndex Char.IsWhiteSpace
        |> Option.defaultValue openTagContent.Length
    let elementName = openTagContent.[0 .. nameEnd - 1]
    try
        let attrSection =
            if nameEnd < openTagContent.Length
            then openTagContent.[nameEnd ..].Trim()
            else ""
        let mutable allowsAdditional = false
        let mutable attrs = Map.empty<string, AttributeCombination>
        for line in attrSection.Split '\n' do
            let line = line.Trim()
            if line.StartsWith "{" then
                allowsAdditional <- true
            elif line.Length > 0 then
                match parseAttributeLine line with
                | Some (name, combo) -> attrs <- Map.add name combo attrs
                | None               -> ()
        let remainder    = input.[openTagEnd + 1 ..].Trim()
        let contentLabel = "Content:"
        let labelIdx     = remainder.IndexOf(contentLabel, StringComparison.OrdinalIgnoreCase)
        let content =
            if labelIdx < 0 then Any
            else
                let afterLabel = remainder.[labelIdx + contentLabel.Length ..].Trim()
                let closingTag = "</" + elementName + ">"
                let closingIdx = afterLabel.LastIndexOf(closingTag, StringComparison.OrdinalIgnoreCase)
                let raw        = (if closingIdx >= 0 then afterLabel.[0 .. closingIdx - 1] else afterLabel).Trim()
                let flat       = Regex.Replace(raw, @"\s+", " ")
                let combo, _   = parseContentOr key typeMap flat 0
                combo
        {
            TypeName                   = elementName
            Attributes                 = attrs
            AllowsAdditionalAttributes = allowsAdditional
            Content                    = content
        }
    with ex ->
        failwithf "Error parsing element '%s' (key '%s'): %s" elementName key ex.Message

let private parse (key: string) (input: string) (typeMap: Map<string, XsdType>) : Map<string, XsdType> =
    let parsed = parseXsdType key typeMap input
    if Map.containsKey key typeMap then
        failwithf "parse: duplicate key '%s'" key
    Map.add key parsed typeMap

let validateAndParse (input: string) (key: string) (typeMap: Map<string, XsdType>) : Map<string, XsdType> =
    parse key (XsdValidator.validate input) typeMap
