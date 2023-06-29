// Advent of code 2021 day 3: https://adventofcode.com/2021/day/3
let input =
    """
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
"""

type Bit =
    | One
    | Zero

module Bit =
    let parse character =
        match character with
        | '0' -> Zero
        | '1' -> One
        | _ ->
            failwith
                $"expected input character for a bit to be either 1 or zero, \
                instead got: {character}"


    let flip bit =
        match bit with
        | Zero -> One
        | One -> Zero

    let bitsToDecimal bits =
        let toDecimal position bit =
            let mult =
                match bit with
                | Zero -> 0.0
                | One -> 1.0

            mult * (2.0 ** position)

        Seq.rev bits
        |> Seq.mapi (fun i bit -> toDecimal i bit)
        |> Seq.sum

module String =
    let split (separator: string) (text: string) = text.Split separator |> Seq.ofArray
    let lines (text: string) = split "\n" text
    let isEmpty (text: string) = System.String.IsNullOrEmpty text
    let toChars (text: string) = text.ToCharArray()

let mostFrequentElement xs =
    Seq.groupBy id xs
    |> Seq.maxBy (fun (x, group) -> Seq.length group)
    |> fst

let foldMostFrequent accumulator bits =
    seq {
        yield! accumulator
        yield mostFrequentElement bits
    }

input
|> String.lines
|> Seq.filter (not << String.isEmpty)
|> Seq.map (String.toChars >> Seq.map Bit.parse)
|> Seq.transpose
|> Seq.fold foldMostFrequent Seq.empty
|> Bit.bitsToDecimal
