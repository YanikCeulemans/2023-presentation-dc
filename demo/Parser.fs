module Fsia.Demo.Parser

let (|Integer|_|) (input: string)=
    match System.Int32.TryParse input with
    | false, _ -> None
    | true, n -> Some n

module String =
    let split (separator: string) (input: string) =
        input.Split separator

    let toCharArray (input: string) =
        input.ToCharArray ()

    let fromCharArray (chars: char array) =
        new System.String(chars)

module Array =
    let snoc xs =
        match List.ofArray xs with
        | [] -> None
        | head :: tail -> Some (head, Array.ofList tail)

type ParseState = ParseState of {| Text : string; Offset : int64 |}

type 'a Parser = Parser of (ParseState -> Result<'a * ParseState, string>)

let runParse (Parser parseFn: 'a Parser) (parseState: ParseState) =
    parseFn parseState

let identity (x: 'a) : 'a Parser =
    Parser (fun s -> Ok (x, s))

let parse ((Parser parseFn): 'a Parser) (input: string) : Result<'a, string> =
    match parseFn (ParseState {| Text = input; Offset = 0 |}) with
    | Error err -> Error err
    | Ok (x, _) -> Ok x

let bind (f: 'a -> 'b Parser) (parser: 'a Parser) =
    Parser (fun parseState ->
        match runParse parser parseState with
        | Error err -> Error err
        | Ok (x, newState) ->
            runParse (f x) newState
        )

let map (f: 'a -> 'b) (parser) =
    bind (f >> identity) parser

let bail msg : 'a Parser =
    Parser (fun (ParseState state) -> Error $"character offset {state.Offset}: %s{msg}")

let getState : ParseState Parser =
    Parser (fun parseState -> Ok (parseState, parseState))

let putState (newState: ParseState) : Unit Parser =
    Parser (fun _ -> Ok ((), newState))

type ParserBuilder() =
    member _.Bind(comp, func) = bind func comp
    member _.Return(x) = identity x
    member _.ReturnFrom(x) = x

let parser = ParserBuilder()

let parseChar : char Parser =
    parser {
        let! (ParseState state) = getState
        match String.toCharArray state.Text |> Array.snoc with
        | None ->
            return! bail "no more input"
        | Some (head, tail) ->
            let newState =
                ParseState ({| state with Offset = state.Offset + 1L |})
            do! putState newState
            return head
    }

let peekChar : char option Parser =
    getState
    |> map (fun (ParseState state) -> state.Text |> String.toCharArray |> Array.tryHead)

let rec parseWhile (predicate: char -> bool): char list Parser =
    parser {
        match! map (Option.map predicate) peekChar with
        | Some true ->
            let! c = parseChar
            let! rest = parseWhile predicate
            return c :: rest
        | _ -> return []
    }

let parseInt : int Parser =
    parser {
        match! parseWhile System.Char.IsDigit with
        | [] ->
            return! bail "no more input"
        | digits ->
            match System.String (Array.ofList digits) with
            | Integer n ->
                return n
            | other ->
                return! bail "not an integer"
    }
