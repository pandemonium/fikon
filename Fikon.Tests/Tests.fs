module Tests

open System
open Xunit
open Combinatrix.Combine
open Fikon

let tokenize =
    Tokenizer.run
    >> ParseResult.toOption
    >> Option.get

[<AutoOpen>]
module Construction =
    let produces expected =
        tokenize >> fun actual ->
            Assert.Equal<Types.Token list> (expected, actual)    

    let kw = Types.Keyword
    let id = Types.Id >> Types.Identifier
    let sep = Types.Separator
    let text = Types.Text >> Types.Literal
    let int = Types.Integer >> Types.Literal
    let float = Types.FloatingPoint >> Types.Literal
    let semi = sep Types.Semicolon
    let assign = sep Types.Assign

[<Fact>]
let ``Tokenize integer`` () =
    let source = "123"
    let tokens =
        Parse.Text.run Tokenizer.integer source
        |> ParseResult.toOption
        |> Option.get

    printfn "%A" tokens

    Assert.Equal<Types.Literal list> ([Types.Integer 123], [tokens])

[<Fact>]
let ``Tokenize literals`` () =
    let literalTokens =
        Parse.Text.run Tokenizer.literal
        >> ParseResult.toOption
        >> Option.get

    let literal expected =
        literalTokens >> fun actual -> 
            Assert.Equal<Types.Token> (expected, actual)    

    "-123.456" |> literal (float -123.456)

    "\"123\"" |> literal (text "123")

    "123" |> literal (int 123)

[<Fact>]
let ``Let statement`` () =
    let case rhs =
        produces [ kw Types.Let; id "x"; assign; rhs; semi ]

    "let x = \"123\";" |> case (text "123")

    "let x = foo;" |> case (id "foo")

    "let x = 123;" |> case (int 123)

    "let x = -427.314 ; " |> case (float -427.314)
