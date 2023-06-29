module Fsia.Demo.SimpleParser

type ParseState = private {
    Text: string
    Offset: int
}

type 'a Parser = private Parser of (ParseState -> Result<'a * ParseState, string>)

let runParser (Parser parseFn) parseState =
    parseFn parseState

let parse parser text =
    {
        Text = text
        Offset = 0
    }
    |> runParser parser
    |> Result.map fst

let identity value =
    Parser (fun parseState -> Ok (value, parseState))

module String =
    let toHeadAndTail (text: string) =
        text.ToCharArray()
        |> List.ofArray
        |> function
            | [] -> None
            | head :: tail -> Some (head, tail)

    let substring index (text: string) =
        text.Substring index

    let fromCharArray (chars: char array) =
        System.String chars
    
    let charAt index (text: string) =
        text.ToCharArray()
        |> Array.tryItem index

let map projector parser =
    let parseFn parseState =
        match runParser parser parseState with
        | Ok (thing, newParseState) -> Ok (projector thing, newParseState)
        | Error err -> Error err

    Parser parseFn


let parseChar =
    let parseFn { Text = text; Offset = offset } =
        match String.charAt offset text with
            | None -> Error "no more input"
            | Some char ->
                let newState = { Text = text; Offset = offset + 1 }
                Ok (char, newState)

    Parser parseFn

let peekChar =
    let parseFn parseState =
        match String.charAt parseState.Offset parseState.Text with
            | None -> Ok (None, parseState)
            | Some char -> Ok (Some char, parseState)

    Parser parseFn


let parseWhile predicate =
    failwith "todo"