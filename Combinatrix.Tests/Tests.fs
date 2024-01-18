module Tests

open System
open Xunit

open Combinatrix.Combine


[<Fact>]
let ``String literals`` () =
    let p = Parser.Text.literal "Hi, mom"
    let outcome = Parser.Text.run p "Hi, mom. What are you doing?"
    Assert.Equal(Some "Hi, mom", outcome.Returns)

[<Fact>]
let ``Sequencing`` () =
    let p = Parser.Text.literal "Hi, mom."
    let q = Parser.Text.literal " What are you doing?"
    let parser = Parser.andAlso p q
    let outcome = Parser.Text.run parser "Hi, mom. What are you doing?"
    Assert.Equal(Some ("Hi, mom.", " What are you doing?"), outcome.Returns)

[<Fact>]
let ``Zero or more`` () =
    let p = Parser.Text.char 'a' |> Parser.zeroOrMore
    let q = Parser.Text.char 'b' |> Parser.zeroOrMore
    let parser = Parser.andAlso p q
    let outcome = Parser.Text.run parser "aaabb"

    printfn "%A" outcome

    Assert.Equal(Some ([ 'a'; 'a'; 'a' ], ['b'; 'b']), outcome.Returns)
    
[<Fact>]
let ``accept sequence`` () =
    let a = Parser.Text.char 'a'
    let b = Parser.Text.char 'b'
    let c = Parser.Text.char 'c'
    let p = Parser.andAlso a <| Parser.andAlso b c

    let outcome = Parser.Text.run p "abcd"
    printfn "%A" outcome

    Assert.Equal(Some ('a', ('b', 'c')), outcome.Returns)
