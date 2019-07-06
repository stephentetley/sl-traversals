// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLTraversals

module Transform =
    
    open SLTraversals.Internal.TraversalMonad

    
    type GenTransform<'ans, 'ctx, 'st, 'src> = TraversalM<'ans, 'ctx, 'st, 'src>

    type GenRewrite<'ans, 'ctx, 'st> = GenTransform<'ans, 'ctx, 'st, 'ans>

    let idR () : GenRewrite<'ans, 'st, 'env> = 
        TraversalM <| fun _ st src -> Ok (src, st)

    let failT () : GenTransform<'ans, 'ctx, 'st, 'src> = 
        traversalError (StrategyFailure "failT")

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


    let transform (fn : 'ctx -> 'a -> Result<'ans, TraversalError>) 
                    : GenTransform<'ans, 'ctx, 'st, 'a> = 
        TraversalM <| fun ctx st src -> 
            match fn ctx src with
            | Error excp -> Error excp
            | Ok ans -> Ok (ans, st)


    let rewrite (fn : 'ctx -> 'ans -> Result<'ans, TraversalError>) : GenRewrite<'ans, 'ctx, 'st> = 
        transform fn

    let successT () : GenTransform<unit, 'ctx, 'st, 'a> = mreturn ()

    let contextT () : GenTransform<'ctx, 'ctx, 'st, 'a> = 
        TraversalM <| fun ctx st _ -> Ok (ctx, st)

    let exposeT () : GenTransform<'ctx * 'a, 'ctx, 'st, 'a> = 
        TraversalM <| fun ctx st src -> Ok ((ctx, src), st)

    let contextfreeT (fn : 'a -> Result<'ans, TraversalError>) : GenTransform<'ans, 'ctx, 'st, 'a> = 
        TraversalM <| fun _ st src -> 
            match fn src with
            | Error excp -> Error excp
            | Ok ans -> Ok (ans, st)

    

    let liftContext (modify : 'ctx1 -> 'ctx2) 
                    (trafo : GenTransform<'ans, 'ctx2, 'st, 'a>) : GenTransform<'ans, 'ctx1, 'st, 'a> = 
        TraversalM <| fun ctx st src -> apply1 trafo (modify ctx) st src

    let readerT (operate : 'a -> GenTransform<'ans, 'ctx, 'st, 'a>) : GenTransform<'ans, 'ctx, 'st, 'a> = 
        TraversalM <| fun ctx st src -> 
            let trafo = operate src in apply1 trafo ctx st src

    let guardT () : GenTransform<unit, 'ctx, 'st, bool> = 
        TraversalM <| fun ctx st src -> 
            if src then Ok ((), st) else Error (StrategyFailure "guardT")

    let ifT (test : GenTransform<bool, 'ctx, 'st, 'a>) 
            (thenTrafo : GenTransform<'ans, 'ctx, 'st, 'a>)
            (elseTrafo : GenTransform<'ans, 'ctx, 'st, 'a>) = 
        traversal { 
            match! test with
            | true -> return! thenTrafo
            | false -> return! elseTrafo
        } 

    let whenT (test : GenTransform<bool, 'ctx, 'st, 'a>) 
              (successTrafo : GenTransform<'ans, 'ctx, 'st, 'a>) = 
        ifT test successTrafo (traversalError (StrategyFailure "whenT"))

    let unlessT (test : GenTransform<bool, 'ctx, 'st, 'a>) 
                (failureTrafo : GenTransform<'ans, 'ctx, 'st, 'a>) = 
        ifT test (traversalError (StrategyFailure "whenT")) failureTrafo

    let andR (rewrites:GenRewrite<'a, 'ctx, 'st> list) : GenRewrite<'a, 'ctx, 'st> = 
        TraversalM <| fun ctx stZero src -> 
            let rec work ops st ans fk sk = 
                match ops with
                | [] -> sk st ans
                | mf :: rest -> 
                    match apply1 mf ctx st ans with
                    | Error msg -> fk msg
                    | Ok (ans1, st1) -> 
                        work rest st1 ans1 fk sk
            work rewrites stZero src (fun msg -> Error msg) (fun st ans -> Ok (ans, st))
                
    let orR (rewrites:GenRewrite<'a, 'ctx, 'st> list) : GenRewrite<'a, 'ctx, 'st> = 
        TraversalM <| fun ctx stZero src -> 
            let rec work ops st ans fk sk = 
                match ops with
                | [] -> fk (StrategyFailure "orR")
                | mf :: rest -> 
                    match apply1 mf ctx st ans with
                    | Error msg -> work rest st ans fk sk
                    | Ok (ans1, st1) -> sk st1 ans1
            work rewrites stZero src (fun msg -> Error msg) (fun st ans -> Ok (ans, st))

    let acceptWithFailR (test : 'a -> bool) (excp : System.Exception) : GenRewrite<'a, 'ctx, 'st> = 
        readerT (fun a -> if test a then idR () else traversalError excp)

    let contextonlyT (fn : 'ctx -> Result<'ans, System.Exception>) : GenTransform<'ans, 'ctx, 'st, 'a> = 
        TraversalM <| fun ctx st _ -> 
            match fn ctx with
            | Error msg -> Error msg
            | Ok ans -> Ok (ans, st)

    let effectfreeT (fn : 'ctx -> 'a -> 'ans) : GenTransform<'ans, 'ctx, 'st, 'a> = 
        TraversalM <| fun ctx st src-> 
            Ok (fn ctx src, st)



