#r "nuget: FParsec"

open FParsec

type Expression =
    | Constant of int
    | Binary of string * Expression * Expression

// 2 + (2 * 20)

let e = 
    Binary("+", Constant(2),
        Binary("*", Constant(2), Constant(20)))

let rec evaluate e =
    match e with
    | Constant(n) -> n
    | Binary(op, e1, e2) ->
        let n1 = evaluate e1
        let n2 = evaluate e2
        match op with
        | "+" -> n1 + n2
        | "*" -> n1 * n2
        | _ -> failwith "unsupported operator"

evaluate e

let ops = 
    Map.ofList [
        "+", (fun a b -> a + b)
        "*", (fun a b -> a * b)
    ]
// let ops1 = Map.add "+" (fun a b -> a + b) ops
// let ops2 = Map.add "*" (fun a b -> a * b) ops1

ops.["+"] 40 2
ops.["*"] 21 2

let ws = spaces
let str_ws s = pstring s >>. ws

let opp = new OperatorPrecedenceParser<Expression,unit,unit>()
let expr = opp.ExpressionParser
let term = (pint32 .>> ws |>> Constant) <|> between (str_ws "(") (str_ws ")") expr
opp.TermParser <- term

type Assoc = Associativity

opp.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, fun x y -> Binary("+", x, y)))
opp.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, fun x y -> Binary("*", x, y)))

let parseAndEval source =
    match run expr source with
        | Success (res, _, _) -> Result.Ok (evaluate res)
        | Failure (err, _, _) -> Result.Error err


parseAndEval "2 + (2 * 20) +3"
