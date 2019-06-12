// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLTraversals.Internal

module TraversalMonad =
    
    type ErrMsg = string

    type Answer<'ans, 'st> = Result<'ans * 'st, ErrMsg>

    type TraversalM<'ans, 'st, 'env, 'src> = 
        TraversalM of ('src -> 'env -> 'st -> Answer<'ans, 'st>)

    let apply1 (ma : TraversalM<'ans, 'st, 'env, 'src>)
               (source : 'src) 
               (env : 'env) 
               (state : 'st) : Answer<'ans, 'st> = 
        let (TraversalM f) = ma in f source env state

    let failM () : TraversalM<'ans, 'st, 'env, 'src> = 
        TraversalM <| fun _ _ _ -> Error "failM"

    let mreturn (a : 'ans) : TraversalM<'ans, 'st, 'env, 'src> =
        TraversalM <| fun _ _ st -> Ok (a, st)


    let bindM (ma : TraversalM<'ans1, 'st, 'env, 'src>)
              (fn : 'ans1 -> TraversalM<'ans2, 'st, 'env, 'src>) : TraversalM<'ans2, 'st, 'env, 'src> =
        TraversalM <| fun src env st -> 
            match apply1 ma src env st with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 (fn a) src env st1

    let mzero () : TraversalM<'ans, 'st, 'env, 'src> = 
        TraversalM <| fun _ _ _ -> Error "mzero"

    let mplus (ma : TraversalM<'ans, 'st, 'env, 'src>) 
              (mb : TraversalM<'ans, 'st, 'env, 'src>) : TraversalM<'ans, 'st, 'env, 'src> = 
        TraversalM <| fun src env st ->
            match apply1 ma src env st with
            | Error _ -> apply1 mb src env st
            | Ok res -> Ok res
    
    let inline private delayM (fn:unit -> TraversalM<'ans, 'st, 'env, 'src>) : TraversalM<'ans, 'st, 'env, 'src> = 
        bindM (mreturn ()) fn 


    type TraversalBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = mzero ()
        member self.Combine ma mb   = mplus ma mb
        member self.ReturnFrom(ma:TraversalM<'ans, 'st, 'env, 'src>) : TraversalM<'ans, 'st, 'env, 'src> = ma
        
    let (traversal:TraversalBuilder) = new TraversalBuilder()


    let runTraversal (env : ' env) 
                     (initialState : 'st) 
                     (source : 'src) 
                     (ma : TraversalM<'ans, 'st, 'env, 'src>)  : Result<'ans, ErrMsg> = 
        match apply1 ma source env initialState with
        | Error msg -> Error msg
        | Ok (ans, _ ) -> Ok ans

    let traversalError (msg : string) : TraversalM<'ans, 'st, 'env, 'src> = 
        TraversalM <| fun _ _ _ -> Error msg


    let mcatch (ma : TraversalM<'ans, 'st, 'env, 'src>) 
               (handler : string -> TraversalM<'ans, 'st, 'env, 'src>) : TraversalM<'ans, 'st, 'env, 'src> = 
        TraversalM <| fun src env st -> 
            match apply1 ma src env st with
            | Error msg -> apply1 (handler msg) src env st
            | Ok res -> Ok res 


    type GenTransform<'ans, 'st, 'env, 'src> = TraversalM<'ans, 'st, 'env, 'src>

    type GenRewrite<'ans, 'st, 'env> = GenTransform<'ans, 'st, 'env, 'ans>

    let idR () : GenRewrite<'ans, 'st, 'env> = 
        TraversalM <| fun src _ st -> Ok (src, st)

    let failT () : GenTransform<'ans, 'st, 'env, 'src> = traversalError "failT"

    let ( <+ ) (t1 : GenTransform<'ans, 'st, 'env, 'src>) 
               (t2 : GenTransform<'ans, 'st, 'env, 'src>) : GenTransform<'ans, 'st, 'env, 'src> = 
        mcatch t1 (fun _ -> t2)

    let tryR (rw : GenRewrite<'ans, 'st, 'env>) : GenRewrite<'ans, 'st, 'env> = 
        rw <+ idR ()

        
    let ( >>> ) (t1 : GenTransform<'ans1, 'st, 'env, 'src>) 
                (t2 : GenTransform<'ans2, 'st, 'env, 'ans1>) : GenTransform<'ans2, 'st, 'env, 'src> = 
        TraversalM <| fun src env st -> 
            match apply1 t1 src env st with
            | Error msg -> Error msg
            | Ok (a1, st1) -> apply1 t2 a1 env st1