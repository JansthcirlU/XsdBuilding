module XsdValidator

open System
open System.Text.RegularExpressions

// ============================================================
//  Helpers
// ============================================================

/// Extracts the element name from the first non-empty token after '<'.
let private extractElementName (input: string) =
    let start = input.IndexOf '<'
    if start < 0 then None
    else
        let rest = input.[start + 1 ..].TrimStart()
        let nameEnd =
            rest |> Seq.tryFindIndex (fun c -> Char.IsWhiteSpace c || c = '>')
            |> Option.defaultValue rest.Length
        if nameEnd = 0 then None
        else Some rest.[0 .. nameEnd - 1]

/// Returns true if a trimmed line looks like an attribute definition,
/// i.e. matches "token = ..." where token may contain letters, digits,
/// hyphens, underscores, and colons (for xml:lang style names).
let private looksLikeAttributeLine (line: string) =
    Regex.IsMatch(line.Trim(), @"^[\w\-][\w\-:]*\s*=")

/// Finds the index of the '>' that closes the opening tag, respecting
/// parenthesis depth (same logic as in the parser).
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
    result

// ============================================================
//  Individual checks
// ============================================================

/// Check 1: the opening tag must not be closed on the same line as the
/// element name, i.e. <elementName> with attributes outside the tag.
let private checkOpenTagNotOnSameLine (input: string) (errors: string list) =
    match extractElementName input with
    | None -> "Could not extract element name from input." :: errors
    | Some name ->
        let openTagStart = input.IndexOf('<')
        let firstNewline = input.IndexOf('\n', openTagStart)
        let openTagEnd   = findOpenTagEnd input
        if openTagEnd >= 0 && (firstNewline < 0 || openTagEnd < firstNewline) then
            sprintf "Opening tag '<%s>' is closed on the same line as the element name; attributes must appear before the '>'." name :: errors
        else
            errors

/// Check 2: no lines between the closing '>' of the opening tag and
/// 'Content:' should look like attribute definitions.
let private checkNoAttributesOutsideTag (input: string) (errors: string list) =
    let openTagEnd = findOpenTagEnd input
    if openTagEnd < 0 then errors
    else
        let remainder  = input.[openTagEnd + 1 ..]
        let contentIdx = remainder.IndexOf("Content:", StringComparison.OrdinalIgnoreCase)
        let between    = if contentIdx >= 0 then remainder.[0 .. contentIdx - 1] else remainder
        between.Split('\n')
        |> Array.mapi (fun i line -> i, line)
        |> Array.filter (fun (_, line) -> looksLikeAttributeLine line)
        |> Array.fold (fun acc (_, line) ->
            sprintf "Attribute definition '%s' found outside the opening tag; move it before the '>'." (line.Trim()) :: acc
        ) errors

/// Check 3: if a brace token is present inside the opening tag, the '...'
/// must be inside the braces, not after them. Only the opening tag is
/// inspected so that {any} in the content section is not mistakenly flagged.
let private checkBraceTokenFormat (input: string) (errors: string list) =
    let openTagEnd = findOpenTagEnd input
    let openTag    = if openTagEnd >= 0 then input.[0 .. openTagEnd] else input
    let m          = Regex.Match(openTag, @"\{([^}]*)\}(\s*\.\.\.)?")
    if not m.Success then errors
    else
        let inside  = m.Groups.[1].Value
        let outside = m.Groups.[2].Value.Trim()
        if outside = "..." then
            sprintf "Additional attributes token has '...' outside the braces; use '{%s...}' instead of '{%s}...'." inside inside :: errors
        elif not (inside.TrimEnd().EndsWith "...") then
            sprintf "Additional attributes token '{%s}' is missing the trailing '...' inside the braces." inside :: errors
        else
            errors

/// Check 4: a closing tag exists and its name matches the opening tag.
let private checkClosingTag (input: string) (errors: string list) =
    match extractElementName input with
    | None -> errors
    | Some name ->
        let closing = "</" + name + ">"
        if not (input.Contains(closing, StringComparison.OrdinalIgnoreCase)) then
            sprintf "Missing or mismatched closing tag; expected '</%s>'." name :: errors
        else
            errors

/// Check 5: a 'Content:' label must be present between the tags.
let private checkContentLabel (input: string) (errors: string list) =
    let openTagEnd = findOpenTagEnd input
    if openTagEnd < 0 then errors
    else
        let remainder = input.[openTagEnd + 1 ..]
        if not (remainder.Contains("Content:", StringComparison.OrdinalIgnoreCase)) then
            "Missing 'Content:' label after the opening tag." :: errors
        else
            errors

// ============================================================
//  Public entry point
// ============================================================

let private collectErrors (input: string) : string list =
    []
    |> checkOpenTagNotOnSameLine input
    |> checkNoAttributesOutsideTag input
    |> checkBraceTokenFormat input
    |> checkClosingTag input
    |> checkContentLabel input
    |> List.rev

/// Validates the raw string representation of an XSD element definition.
/// Returns the input unchanged if valid, or throws with a full list of
/// errors if not — allowing it to be used directly in a parsing pipeline:
///
///   Map.empty
///   |> validateAndParse annotationDefinition
///   |> validateAndParse elementDefinition
let validate (input: string) : string =
    match collectErrors input with
    | [] -> input
    | errors ->
        let name = extractElementName input |> Option.defaultValue "<unknown>"
        failwithf "Validation failed for element '%s':\n%s" name (String.concat "\n" errors)