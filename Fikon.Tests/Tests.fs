module Tests

open System
open Xunit
open Combinatrix.Combine
open Fikon

let tokenize =
    Tokenizer.run
    >> ParseResult.toOption
    >> Option.get

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
let ``Tokenize identifier or keyword`` () =
    let source = "let let"
    let tokens =
        Parse.Text.run (Parse.oneOrMore (Tokenizer.lexeme Tokenizer.keywordOrIdentifier)) source
        |> ParseResult.toOption
        |> Option.get

    printfn "%A" tokens

    Assert.Equal<Types.Token list> ([], [tokens])

[<Fact>]
let ``Let statement`` () =
//    let source = "let x = \"123\";"
    let source = "let let"
    let tokens = tokenize source

    printfn "%A" tokens

    Assert.Equal<Types.Token list> ([], tokens)
