module Combinatrix.Combine

(* If I wrap this, then I can have extension methods on it. *)
type ('t, 'a) Parser = 't ParseState -> ParseResult<'t, 'a>

and 't ParseState =
    { Input  : 't []
      TokenStart : int 
      TokenLength : int }

and ('t, 'a) ParseResult =
    { State   : 't ParseState
      Returns : 'a option }

and Fault = Expected of string 
          | Eof

module ParseState =
    let from input =
        { Input  = input
          TokenStart = 0
          TokenLength = 0 }

    let offset { TokenStart = start; TokenLength = length } =
        start + length

    let peek self =
        self.Input.[offset self]

    let canAdvance by self =
        offset self + by <= self.Input.Length

    let advance by self =
        { self with TokenStart  = offset self
                    TokenLength = by }

    let token self =
        let endOffset = offset self - 1
        self.Input.[self.TokenStart..endOffset]

module ParseResult =
    let accepted returns input =
        { State   = input
          Returns = Some returns }

    let balked input =
        { State   = input
          Returns = None }

    let map f { State = input; Returns = returns } =
        { State   = input
          Returns = Option.map f returns }

    let bind f { State = input; Returns = returns } : ParseResult<'t, 'b> =
        f input returns

module Parser =
    let run (self : Parser<'t, 'a>) =
        ParseState.from >> self

    let empty returns : Parser<'t, 'a> =
        ParseResult.accepted returns

    let take count : Parser<'t, 't []> = fun input ->
        if ParseState.canAdvance count input then
            let input = ParseState.advance count input
            let token = ParseState.token input
            ParseResult.accepted token input
        else
            ParseResult.balked input

    let accept suchThat : Parser<'t, 't> = fun input ->
        if ParseState.canAdvance 1 input && ParseState.peek input |> suchThat then
            let input = ParseState.advance 1 input
            let token = ParseState.token input
            ParseResult.accepted token.[0] input
        else
            ParseResult.balked input

    let map f self : Parser<'t, 'b> =
        self >> ParseResult.map f

    let filterMap f self : Parser<'t, 'b> =
        self
        >> ParseResult.map f
        >> function { State = input; Returns = Some (Some x) } -> ParseResult.accepted x input
                  | { State = input }                          -> ParseResult.balked input

    let bind f self : Parser<'t, 'b> =
         self
         >> ParseResult.map f
         >> function { State = input; Returns = Some q } -> q input
                   | { State = input }                   -> ParseResult.balked input

    let orElse (p : Parser<'t, 'a>) (q : Parser<'t, 'a>) : Parser<'t, 'a> =
        p >> function { Returns = Some _ } as it -> it
                    | { State = input }          -> q input

    let andAlso p q =
        bind (fun px -> map (fun qx -> (px, qx)) q) p

    let rec zeroOrMore (self : Parser<'t, 'a>) : Parser<'t, 'a list> =
        let iterate = 
            function [] -> empty []
                   | xs -> zeroOrMore self |> map (List.append xs)
        // Is this as reducesd as it can be?
        empty []
        |> orElse (map List.singleton self)
        |> bind iterate

    let oneOrMore self =
        zeroOrMore self
        |> andAlso self
        |> map (fun (p, ps) -> p::ps) // Lis

    let optionally self =
        let p = map Some self
        empty None |> orElse p

    let skipLeft self =
        andAlso self >> map snd

    let skipRight self =
        andAlso self >> map fst

    let separatedBy separator self =
        let group = skipLeft separator self
        zeroOrMore group
        |> andAlso self
        |> map (fun (p, ps) -> p::ps)

    let enclosedWithin left right self =
        skipRight self right
        |> skipLeft left

    let anyOf (alternatives : 't seq) =
        accept (fun tok -> Seq.contains tok alternatives)

    module Text =
        let literal (text : string) : Parser<char, string> =
            let textChars = text.ToCharArray ()
            take text.Length
            |> filterMap (fun t -> if t = textChars then Some text else None)

        let char ch : Parser<char, char> =
            accept (fun tok -> tok = ch)

        let run (parser : Parser<char, 'a>) (input : string) =
            input.ToCharArray ()
            |> run parser
    