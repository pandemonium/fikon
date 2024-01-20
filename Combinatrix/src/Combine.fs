module Combinatrix.Combine

(* If I wrap this, then I can have extension methods on it. *)
type ('t, 'a) Parse = 
    't ParseState -> ParseResult<'t, 'a>

and 't ParseState =
    { Input       : 't []
      TokenStart  : int 
      TokenLength : int }

and ('t, 'a) ParseResult =
    { State   : 't ParseState
      Returns : 'a option }

module ParseState =
    let from input =
        { Input       = input
          TokenStart  = 0
          TokenLength = 0 }

    let offset { TokenStart  = start
                 TokenLength = length } =
        start + length

    let peek self =
        self.Input.[offset self]

    let canAdvance by self =
        offset self + by <= self.Input.Length

    let advance by self =
        { self with TokenStart  = offset self
                    TokenLength = by }

    let token self =
        let tokenEnd = offset self - 1
        self.Input.[self.TokenStart..tokenEnd]

module ParseResult =
    let accepted returns input =
        { State   = input
          Returns = Some returns }

    let balked input =
        { State   = input
          Returns = None }

    let map f { State   = input
                Returns = returns } =
        { State   = input
          Returns = Option.map f returns }

    let bind f { State   = input
                 Returns = returns } : ParseResult<'t, 'b> =
        f input returns

module Parse =
    let run (self : Parse<'t, 'a>) =
        ParseState.from >> self

    let empty returns : Parse<'t, 'a> =
        ParseResult.accepted returns

    (* These are very similar - they ought to be re-written in terms of one another. *)
    let take count : Parse<'t, 't []> = fun input ->
        if ParseState.canAdvance count input then
            let input = ParseState.advance count input
            let token = ParseState.token input
            ParseResult.accepted token input
        else
            ParseResult.balked input

    (* These are very similar - they ought to be re-written in terms of one another. *)
    let accept suchThat : Parse<'t, 't> = fun input ->
        if ParseState.canAdvance 1 input && ParseState.peek input |> suchThat then
            let input = ParseState.advance 1 input
            let token = ParseState.token input
            ParseResult.accepted token.[0] input
        else
            ParseResult.balked input

    let acceptIf (select : 't -> 'a option) : Parse<'t, 'a> = fun input ->
        (* This is very busy. *)
        if ParseState.canAdvance 1 input then
            match select <| ParseState.peek input with
            | Some tok -> 
                ParseState.advance 1 input
                |> ParseResult.accepted tok
            | None ->
                ParseResult.balked input
        else
            ParseResult.balked input

    let map f self : Parse<'t, 'b> =
        self >> ParseResult.map f

    let produce production =
        map (fun _ -> production)

    let filterMap f self : Parse<'t, 'b> =
        self
        >> ParseResult.map f
        >> function { State   = input
                      Returns = Some (Some x) } -> ParseResult.accepted x input
                  | { State = input }           -> ParseResult.balked input

    let bind f self : Parse<'t, 'b> =
         self
         >> ParseResult.map f
         >> function { State   = input
                       Returns = Some next } -> next input
                   | { State = input }       -> ParseResult.balked input

    let orElse (p : Parse<'t, 'a>) (q : Parse<'t, 'a>) : Parse<'t, 'a> =
        p >> function { Returns = Some _ } as it -> it
                    | { State = input }          -> q input

    let choice alternatives : Parse<'t, 'a> =
        Seq.reduce orElse alternatives

    let andAlso p q =
        bind (fun px -> map (fun qx -> (px, qx)) q) p

    let rec zeroOrMore (self : Parse<'t, 'a>) : Parse<'t, 'a list> =
        let iterate = 
            function [] -> empty []
                   | xs -> zeroOrMore self |> map (List.append xs)
        empty []
        |> orElse (map List.singleton self)
        |> bind iterate

    let oneOrMore self =
        zeroOrMore self
        |> andAlso self
        |> map (fun (p, ps) -> p::ps)

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
        let literal (text : string) : Parse<char, string> =
            let textChars = text.ToCharArray ()
            take text.Length
            |> filterMap (fun t -> if t = textChars then Some text else None)

        let char ch : Parse<char, char> =
            accept (fun tok -> tok = ch)

        let number (tryParse : string -> bool * 'a) (image : string) : 'a option =
            match tryParse image with
            | true, out -> Some out
            | _ -> None

        let run (parser : Parse<char, 'a>) (input : string) =
            input.ToCharArray ()
            |> run parser

    type Builder () =
        member __.Bind (self, f) =
            bind f self

        member __.Return (x) =
            empty x

        member __.ReturnFrom (self) =
            self

let parse = new Parse.Builder ()