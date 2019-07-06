// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLTraversals.Internal

module TraversalMonad =
    

    
    type TraversalError = System.Exception

    exception StrategyFailure of string


    type Answer<'ans, 'st> = Result<'ans * 'st, TraversalError>

    type TraversalM<'ans, 'ctx, 'st, 'src> = 
        internal TraversalM of ('ctx -> 'st -> 'src -> Answer<'ans, 'st>)

    let internal apply1 (ma : TraversalM<'ans, 'ctx, 'st, 'src>)
               (context : 'ctx)
               (state : 'st) 
               (source : 'src) : Answer<'ans, 'st> = 
        let (TraversalM f) = ma in f context state source

    let failM () : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun _ _ _ -> Error (StrategyFailure "failM")

    let mreturn (a : 'ans) : TraversalM<'ans, 'env, 'st, 'src> =
        TraversalM <| fun _ st _ -> Ok (a, st)


    let mbind (ma : TraversalM<'ans1, 'st, 'env, 'src>)
              (fn : 'ans1 -> TraversalM<'ans2, 'st, 'env, 'src>) : TraversalM<'ans2, 'st, 'env, 'src> =
        TraversalM <| fun ctx st src -> 
            match apply1 ma ctx st src with
            | Error msg -> Error msg
            | Ok (a, st1) -> apply1 (fn a) ctx st1 src

    let mzero () : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun _ _ _ -> Error (StrategyFailure "mzero")

    let mplus (ma : TraversalM<'ans, 'ctx, 'st, 'src>) 
              (mb : TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun ctx st src ->
            match apply1 ma ctx st src with
            | Error _ -> apply1 mb ctx st src
            | Ok res -> Ok res
    
    let inline private mdelay (fn:unit -> TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        mbind (mreturn ()) fn 


    type TraversalBuilder() = 
        member self.Return x        = mreturn x
        member self.Bind (p,f)      = mbind p f
        member self.Zero ()         = mzero ()
        member self.Combine ma mb   = mplus ma mb
        member self.ReturnFrom(ma:TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = ma
        member self.Delay fn        = mdelay fn
        
    let (traversal:TraversalBuilder) = new TraversalBuilder()


    type ErrMsg = string

    let runTraversal (context : 'ctx) 
                     (initialState : 'st) 
                     (source : 'src) 
                     (ma : TraversalM<'ans, 'ctx, 'st, 'src>)  : Result<'ans, ErrMsg> = 
        match apply1 ma context initialState source with
        | Error ex -> 
            match ex with
            | StrategyFailure(msg) -> Error (sprintf "StrategyFailure: %s" msg)
            | _ -> Error (ex.Message)
        | Ok (ans, _ ) -> Ok ans

    let traversalError (ex : System.Exception) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun _ _ _ -> Error ex

    let strategyFailure (message : string) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        traversalError (StrategyFailure message)


    let mcatch (ma : TraversalM<'ans, 'ctx, 'st, 'src>) 
               (handler : System.Exception -> TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> = 
        TraversalM <| fun ctx st src -> 
            match apply1 ma ctx st src with
            | Error msg -> apply1 (handler msg) ctx st src
            | Ok res -> Ok res 


    let mapM (modify : 'a -> 'b)
              (action : TraversalM<'a, 'ctx, 'st, 'src>) : TraversalM<'b, 'ctx, 'st, 'src> = 
        TraversalM <| fun ctx st src -> 
            match apply1 action ctx st src with
            | Error msg -> Error msg
            | Ok (ans, st1) -> Ok (modify ans, st1)

    /// Operator for fmapM
    let ( <<| ) (modify : 'a -> 'b)
                (action : TraversalM<'a, 'ctx, 'st, 'src>) : TraversalM<'b, 'ctx, 'st, 'src> = 
        mapM modify action

    /// Flipped fmapM
    let ( |>> ) (action : TraversalM<'a, 'ctx, 'st, 'src>) 
                (modify : 'a -> 'b) : TraversalM<'b, 'ctx, 'st, 'src> = 
        mapM modify action  

    let ( >>= ) (ma : TraversalM<'ans1, 'st, 'env, 'src>)
                (fn : 'ans1 -> TraversalM<'ans2, 'st, 'env, 'src>) : TraversalM<'ans2, 'st, 'env, 'src> =
        mbind ma fn

    
    /// Operator for mplus (alternative choice)
    let ( <|> ) (actionA : TraversalM<'ans, 'ctx, 'st, 'src>) 
                (actionB : TraversalM<'ans, 'ctx, 'st, 'src>) : TraversalM<'ans, 'ctx, 'st, 'src> =  
        mplus actionA actionB


    let mguard (condition : bool) : TraversalM<unit, 'ctx, 'st, 'src> = 
        if condition then mreturn () else failM ()


    let mwhen (condition : bool) (action : TraversalM<'a, 'ctx, 'st, 'src>) : TraversalM<'a, 'ctx, 'st, 'src> = 
        if condition then action else failM ()

    let whenM (test : TraversalM<bool, 'ctx, 'st, 'src>) (action : TraversalM<'a, 'ctx, 'st, 'src>) : TraversalM<'a, 'ctx, 'st, 'src> = 
        test >>= fun ans -> 
        mwhen ans action
    
    let munless (condition : bool) (action : TraversalM<'a, 'ctx, 'st, 'src>) : TraversalM<'a, 'ctx, 'st, 'src> = 
        if condition then failM () else action

    let unlessM (test : TraversalM<bool, 'ctx, 'st, 'src>) (action : TraversalM<'a, 'ctx, 'st, 'src>) : TraversalM<'a, 'ctx, 'st, 'src> = 
        test >>= fun ans -> 
        munless ans action


    let ifM (condition : TraversalM<bool, 'ctx, 'st, 'src>) 
             (successCase : TraversalM<'a, 'ctx, 'st, 'src>) 
             (elseCase : TraversalM<'a, 'ctx, 'st, 'src>) : TraversalM<'a, 'ctx, 'st, 'src> = 
        condition >>= fun ans->
        if ans then successCase else elseCase

        

