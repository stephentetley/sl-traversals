// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLTraversals.Internal

module TraversalMonad =
    

    /// TODO - would be better with an exception so we could match on it
    type ErrMsg = string

    type Answer<'ans, 'st> = Result<'ans * 'st, ErrMsg>

    type TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM of ('ctx -> 'st -> 'src -> Answer<'ans, 'st>)

    let apply1 (ma : TraversalM<'ans, 'ctx, 'st, 'src>)
               (context : 'ctx)
               (state : 'st) 
               (source : 'src) : Answer<'ans, 'st> = 
        let (TraversalM f) = ma in f context state source

    let failM () : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun _ _ _ -> Error "failM"

    let mreturn (a : 'ans) : TraversalM<'ans, 'env, 'st, 'src> =
        TraversalM <| fun _ st _ -> Ok (a, st)


    let bindM (ma : TraversalM<'ans1, 'st, 'env, 'src>)
              (fn : 'ans1 -> TraversalM<'ans2, 'st, 'env, 'src>) : TraversalM<'ans2, 'st, 'env, 'src> =
        TraversalM <| fun ctx st src -> 
            match apply1 ma ctx st src with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 (fn a) ctx st1 src

    let mzero () : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun _ _ _ -> Error "mzero"

    let mplus (ma : TraversalM<'ans, 'ctx, 'st, 'src>) 
              (mb : TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun ctx st src ->
            match apply1 ma ctx st src with
            | Error _ -> apply1 mb ctx st src
            | Ok res -> Ok res
    
    let inline private delayM (fn:unit -> TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        bindM (mreturn ()) fn 


    type TraversalBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = bindM p f
        member self.Zero ()         = mzero ()
        member self.Combine ma mb   = mplus ma mb
        member self.ReturnFrom(ma:TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = ma
        
    let (traversal:TraversalBuilder) = new TraversalBuilder()


    let runTraversal (context : 'ctx) 
                     (initialState : 'st) 
                     (source : 'src) 
                     (ma : TraversalM<'ans, 'ctx, 'st, 'src>)  : Result<'ans, ErrMsg> = 
        match apply1 ma context initialState source with
        | Error msg -> Error msg
        | Ok (ans, _ ) -> Ok ans

    let traversalError (msg : string) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun _ _ _ -> Error msg


    let mcatch (ma : TraversalM<'ans, 'ctx, 'st, 'src>) 
               (handler : string -> TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun ctx st src -> 
            match apply1 ma ctx st src with
            | Error msg -> apply1 (handler msg) ctx st src
            | Ok res -> Ok res 


    type GenTransform<'ans, 'ctx, 'st, 'src> = TraversalM<'ans, 'ctx, 'st, 'src>

    type GenRewrite<'ans, 'ctx, 'st> = GenTransform<'ans, 'ctx, 'st, 'ans>

    let idR () : GenRewrite<'ans, 'st, 'env> = 
        TraversalM <| fun _ st src -> Ok (src, st)

    let failT () : GenTransform<'ans, 'ctx, 'st, 'src> = traversalError "failT"

    let ( <+ ) (t1 : GenTransform<'ans, 'ctx, 'st, 'src>) 
               (t2 : GenTransform<'ans, 'ctx, 'st, 'src>) : GenTransform<'ans, 'ctx, 'st, 'src> = 
        mcatch t1 (fun _ -> t2)

    let tryR (rw : GenRewrite<'ans, 'st, 'env>) : GenRewrite<'ans, 'st, 'env> = 
        rw <+ idR ()

        
    let ( >>> ) (t1 : GenTransform<'ans1, 'st, 'env, 'src>) 
                (t2 : GenTransform<'ans2, 'st, 'env, 'ans1>) : GenTransform<'ans2, 'st, 'env, 'src> = 
        TraversalM <| fun ctx st src -> 
            match apply1 t1 ctx st src with
            | Error msg -> Error msg
            | Ok (a1, st1) -> apply1 t2 ctx st1 a1