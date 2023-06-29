module Fsia.Demo.JsonParser

open FParsec
open FParsec.Primitives
open FParsec.CharParsers

type Json =
    | JsonObject of Map<string, Json>
    | JsonArray of Json array
    | JsonBool of bool
    | JsonString of string
    | JsonNumber of float
    | JsonNull

let nullParser : Parser<Json, Unit> =
    pstring "null"
    |>> (fun _ -> JsonNull)

let boolParser : Parser<Json, Unit> =
    let trueParser = pstring "true" |>> (fun _ -> JsonBool true)
    let falseParser = pstring "false" |>> (fun _ -> JsonBool false)
    trueParser <|> falseParser

let numberParser : Parser<Json, Unit> =
    let zeroParser = pchar '0' >>% 0.0 |>> JsonNumber
    let oneToNineDigitParser = anyOf "123456789" |>> (string >> System.Double.Parse)
    let signParser = 
        opt (pchar '-') 
        |>> (function
            | Some _ -> fun (x: float) -> x * -1.0
            | None -> id
        )
    let negativeZeroParser =
        signParser >>. zeroParser >>% -0.0 |>> JsonNumber

    choice
        [ zeroParser; negativeZeroParser ]