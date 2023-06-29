module Fsia.Demo.Expression

type Expression =
    | Plus of Expression * Expression
    | Minus of Expression * Expression
    | Literal of int

let rec evaluate (expression : Expression) =
    match expression with
    | Plus (left, right) -> (evaluate left) + (evaluate right)
    | Minus (left, right) -> (evaluate left) - (evaluate right)
    | Literal lit -> lit

Plus ((Literal 3), (Minus ((Literal 10), (Literal 8))))
|> evaluate
