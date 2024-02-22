module Tests

open System
open Xunit

open Combinatrix.Combine


[<Fact>]
let ``String literals`` () =
    let p = Parse.Text.literal "Hi, mom"
    let outcome = Parse.Text.run p "Hi, mom. What are you doing?"
    Assert.Equal(Some "Hi, mom", outcome.Returns)

[<Fact>]
let ``Sequencing`` () =
    let p = Parse.Text.literal "Hi, mom."
    let q = Parse.Text.literal " What are you doing?"
    let parser = Parse.andAlso p q
    let outcome = Parse.Text.run parser "Hi, mom. What are you doing?"
    Assert.Equal(Some ("Hi, mom.", " What are you doing?"), outcome.Returns)

[<Fact>]
let ``Zero or more`` () =
    let p = Parse.Text.char 'a' |> Parse.zeroOrMore
    let q = Parse.Text.char 'b' |> Parse.zeroOrMore
    let parser = Parse.andAlso p q

    let outcome = Parse.Text.run parser "aaabb"
    Assert.Equal(Some ([ 'a'; 'a'; 'a' ], ['b'; 'b']), outcome.Returns)

    let outcome = Parse.Text.run parser "aaa"
    Assert.Equal(Some ([ 'a'; 'a'; 'a' ], []), outcome.Returns)

    let outcome = Parse.Text.run parser "bb"
    Assert.Equal(Some ([], ['b'; 'b']), outcome.Returns)

[<Fact>]
let ``Accept sequence`` () =
    let a = Parse.Text.char 'a'
    let b = Parse.Text.char 'b'
    let c = Parse.Text.char 'c'
    let p = Parse.andAlso a <| Parse.andAlso b c

    let outcome = Parse.Text.run p "abcd"
    printfn "%A" outcome
    Assert.Equal(Some ('a', ('b', 'c')), outcome.Returns)

    let outcome = Parse.Text.run p "abc"
    printfn "%A" outcome
    Assert.Equal(Some ('a', ('b', 'c')), outcome.Returns)

    let outcome = Parse.Text.run p "ab"
    printfn "%A" outcome
    Assert.Equal(None, outcome.Returns)

[<Fact>]
let ``Builder`` () =
    let abc = parse {
        let! a = Parse.Text.char 'a'
        let! b = Parse.Text.char 'b'
        let! c = Parse.Text.char 'c'
        return a, b, c
    }

    let outcome = Parse.Text.run abc "abcd"
    printfn "%A" outcome

    Assert.Equal(Some ('a', 'b', 'c'), outcome.Returns)

[<Fact>]
let ``Separated sequences`` () =
    let letter = Parse.accept Char.IsLetter
    let comma = Parse.Text.char ','
    let p = Parse.separatedBy comma letter

    let outcome = Parse.Text.run p "a,b,c,d,e"
    printfn "%A" outcome
    Assert.Equal(Some ['a'; 'b'; 'c'; 'd'; 'e'], outcome.Returns)

    let outcome = Parse.Text.run p "a,b,c,d,e,"
    printfn "%A" outcome
    Assert.Equal(Some ['a'; 'b'; 'c'; 'd'; 'e'], outcome.Returns)

[<Fact>]
let ``Enclosed within`` () =
    let doubleQuote = Parse.Text.char '"'
    let letters = 
        Parse.accept Char.IsLetter
        |> Parse.zeroOrMore
        |> Parse.map String.Concat
    let p = Parse.enclosedWithin doubleQuote doubleQuote letters

    let outcome = Parse.Text.run p "\"Himom\""
    printfn "%A" outcome
    Assert.Equal(Some "Himom", outcome.Returns)

    let outcome = Parse.Text.run p "\"\""
    printfn "%A" outcome
    Assert.Equal(Some "", outcome.Returns)

[<Fact>]
let ``Pick from any of the alternatives`` () =
    let abc =
        Parse.anyOf ['a'; 'b'; 'c']
        |> Parse.oneOrMore
        |> Parse.map String.Concat

    let outcome = Parse.Text.run abc "acbabcabc"
    printfn "%A" outcome
    Assert.Equal(Some "acbabcabc", outcome.Returns)

    let abc = 
        Parse.anyOf [| 'a'; 'b'; 'c' |]
        |> Parse.oneOrMore
        |> Parse.map String.Concat

    let outcome = Parse.Text.run abc "b"
    printfn "%A" outcome
    Assert.Equal(Some "b", outcome.Returns)

[<Fact>]
let ``Choices`` () =
    let abc = parse {
        let! abcs = 
            [ Parse.Text.char 'a'
              Parse.Text.char 'b'
              Parse.Text.char 'c' ]
            |> Parse.choice
            |> Parse.oneOrMore

        return String.Concat abcs
    }

    let outcome = Parse.Text.run abc "cbabcabc"
    printfn "%A" outcome
    Assert.Equal(Some "cbabcabc", outcome.Returns)

[<Fact>]
let ``This or else that`` () =
    let a = Parse.Text.char 'a'
    let b = Parse.Text.char 'b'
    let ab = Parse.orElse a b

    let outcome = Parse.Text.run ab "a"
    Assert.Equal(Some 'a', outcome.Returns)

    let outcome = Parse.Text.run ab "b"
    Assert.Equal(Some 'b', outcome.Returns)

    let outcome = Parse.Text.run ab ""
    Assert.Equal(None, outcome.Returns)

