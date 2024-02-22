namespace Fikon

open Combinatrix.Combine

module Types =
    type Fault = Expeced of string | Eof

    type Token = Separator  of Separator
               | Identifier of Identifier
               | Keyword    of Keyword
               | Literal    of Literal

    and Separator = LeftParen          | Semicolon
                  | RightParen         | Comma
                  | LeftBrace          | Period
                  | RightBrace         | DoubleQuote
                  | LeftBracket        | SingleQuote
                  | RightBracket       | Assign
                  | LessThan           | Plus
                  | LessThanOrEqual    | Minus
                  | GreaterThan        | Star
                  | GreaterThanOrEqual | Slash
                  | Equals             | Percent
                  | NotEqual           | ThinRightArrow
                  | Colon              | ReAssign

    and Identifier = Id of string

    and Keyword = If | Else | While | Let | Fn | Return | And | Or

    and Literal = Text          of string
                | Integer       of int64
                | FloatingPoint of float
                | Boolean       of bool

    module Separator =
        let intoChar =
            function LeftParen    -> '(' | SingleQuote        -> '\''
                   | RightParen   -> ')' | Assign             -> '='
                   | LeftBrace    -> '{' | Plus               -> '+'
                   | RightBrace   -> '}' | Minus              -> '-'
                   | LeftBracket  -> ']' | Star               -> '*'
                   | RightBracket -> '[' | Slash              -> '/'
                   | LessThan     -> '<' | Percent            -> '%'
                   | GreaterThan  -> '>' | LessThanOrEqual    -> '$'
                   | Colon        -> ':' | GreaterThanOrEqual -> '$'
                   | Semicolon    -> ';' | Equals             -> '$'
                   | Comma        -> ',' | NotEqual           -> '$'
                   | Period       -> '.' | ThinRightArrow     -> '$'
                   | DoubleQuote  -> '"' | ReAssign           -> '$'

        let tryFrom =
            function '('  -> Some LeftParen    | '.'  -> Some Period
                   | ')'  -> Some RightParen   | '"'  -> Some DoubleQuote
                   | '{'  -> Some LeftBrace    | '\'' -> Some SingleQuote
                   | '}'  -> Some RightBrace   | '='  -> Some Assign
                   | '['  -> Some LeftBracket  | '+'  -> Some Plus
                   | ']'  -> Some RightBracket | '-'  -> Some Minus
                   | '<'  -> Some LessThan     | '*'  -> Some Star
                   | '>'  -> Some GreaterThan  | '/'  -> Some Slash
                   | ':'  -> Some Colon        | '%'  -> Some Percent
                   | ';'  -> Some Semicolon    | ','  -> Some Comma
                   | _    -> None

    module Keyword =
        let tryFrom =
            function "if"     -> Some If
                   | "else"   -> Some Else
                   | "while"  -> Some While
                   | "let"    -> Some Let
                   | "fn"     -> Some Fn
                   | "return" -> Some Return
                   | "and"    -> Some And
                   | "or"     -> Some Or
                   | _        -> None

module Tokenizer =
    open System
    open Parse
    open Types

    let whitespace =
        accept Char.IsWhiteSpace
        |> zeroOrMore

    let lexeme<'t, 'a> =
        skipLeft whitespace

    let separator : Parse<char, Token> =
        acceptIf Separator.tryFrom
        |> map Separator

    let compoundSeparator : Parse<char, Token> =
        [ Text.literal ">=" |> produce GreaterThanOrEqual
          Text.literal "<=" |> produce LessThanOrEqual
          Text.literal "==" |> produce Equals
          Text.literal "!=" |> produce NotEqual
          Text.literal "->" |> produce ThinRightArrow
          Text.literal ":=" |> produce ReAssign ]
        |> choice
        |> map Separator

    let digit =
        accept Char.IsDigit

    let period = Text.char '.'
    let doubleQuote = Text.char '"'

    let boolean =
        let t = Text.literal "True"  |> produce true
        let f = Text.literal "False" |> produce false
        map Boolean <| orElse t f
        
    let floatingPoint =
        let mantissa = oneOrMore digit
        mantissa
        |> skipLeft period
        |> andAlso mantissa
        |> filterMap (fun (a, b) -> 
                printfn "%A" (a, b)
                Text.number Double.TryParse $"{a}.{b}"
           )
        |> map FloatingPoint
            
    let integer =
        oneOrMore digit
        |> filterMap (String.Concat >> Text.number Int64.TryParse)
        |> map Integer
                
    let text =
        let quotableString = 
            accept (fun c -> c <> '"')
            |> zeroOrMore
            |> map String.Concat
            |> map Text
        in enclosedWithin doubleQuote doubleQuote quotableString

    let literal =
//        integer
//        |> orElse boolean
//        |> orElse text
//        |> orElse floatingPoint

        [ floatingPoint
          text 
          boolean
          integer ]
        |> choice
        |> map Literal

    let id =
        seq { yield! seq { 'a'..'z' }
              yield! seq { 'A'..'Z' }
              yield '_' }
        |> anyOf
        |> oneOrMore
        |> map String.Concat

    let keywordOrIdentifier =
        let keyword =
            id
            |> filterMap Keyword.tryFrom
            |> map Keyword

        let identifier =
            id
            |> map (Identifier << Id)

        orElse keyword identifier

    let token =
        [ literal
          keywordOrIdentifier
          compoundSeparator
          separator ]
        |> choice
        |> lexeme

    let run =
        oneOrMore token
        |> Text.run