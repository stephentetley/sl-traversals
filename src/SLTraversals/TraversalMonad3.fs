// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLTraversals

module TraversalMonad3 =
    
    type ErrMsg = string

    type Answer<'a> = Result<'a, ErrMsg>

    type TraversalM<'a, 'src> = 
        TraversalM of ('src -> Answer<'a>)

    let apply1 (ma : TraversalM<'a, 'src>)
               (source : 'src) : Answer<'a> = 
        let (TraversalM f) = ma in f source

    let failM () : TraversalM<'a, 'ans> = 
        TraversalM <| fun _ -> Error "failM"

    let mreturn (x : 'a) : TraversalM<'a, 'ans> =
        TraversalM <| fun _ -> Ok x


    let bindM (ma : TraversalM<'a, 'src>)
              (fn : 'a -> TraversalM<'b, 'src>) : TraversalM<'b, 'src> =
        TraversalM <| fun src -> 
            match apply1 ma src with
            | Error msg -> Error msg
            | Ok a -> apply1 (fn a) src

    let mzero () : TraversalM<'a, 'ans> = 
        TraversalM <| fun _ -> Error "mzero"

    let mplus (ma : TraversalM<'a, 'ans>) 
              (mb : TraversalM<'a, 'ans>) : TraversalM<'a, 'ans> = 
        TraversalM <| fun src ->
            match apply1 ma src with
            | Error _ -> apply1 mb src
            | Ok a -> Ok a
    
    let inline private delayM (fn:unit -> TraversalM<'a, 'ans>) : TraversalM<'a, 'ans> = 
        bindM (mreturn ()) fn 


    type TraversalBuilder() = 
        member self.Return x    = mreturn x
        member self.Bind (p,f)  = bindM p f
        member self.Zero ()     = mzero ()
        member self.Combine (ma: TraversalM<'a, 'ans>, mb: TraversalM<'a, 'ans>) : TraversalM<'a, 'ans> = mplus ma mb
        member self.ReturnFrom(ma:TraversalM<'a, 'ans>) : TraversalM<'a, 'ans> = ma
        
    let (traversal:TraversalBuilder) = new TraversalBuilder()


    let runTraversal (source :'src) (ma : TraversalM<'a, 'src>)  : Result<'a, ErrMsg> = 
        apply1 ma source

    let traversalError (msg : string) : TraversalM<'a, 'src> = 
        TraversalM <| fun _ -> Error msg


    let mcatch (ma : TraversalM<'a, 'src>) (fn : string -> TraversalM<'a, 'src>) : TraversalM<'a, 'src> = 
        TraversalM <| fun src -> 
            match apply1 ma src with
            | Error msg -> apply1 (fn msg) src
            | Ok a -> Ok a 


    type Transform<'a, 'b> = TraversalM<'b, 'a>

    type Rewrite<'a> = Transform<'a, 'a>

    let idR () : Rewrite<'a> = 
        TraversalM <| fun src -> Ok src

    let failT () : Transform<'a, 'b> = traversalError "failT"

    let ( <+ ) (t1 : Transform<'a, 'b>) (t2 : Transform<'a, 'b>) : Transform<'a, 'b> = 
        mcatch t1 (fun _ -> t2)

    let tryR (rw : Rewrite<'a>) = rw <+ idR ()

        
