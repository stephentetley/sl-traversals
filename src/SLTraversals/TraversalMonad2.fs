// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLTraversals

module TraversalMonad2 =
    
    type ErrMsg = string

    type Ans<'ans> = Result<'ans, ErrMsg>

    type Fk<'ans> = ErrMsg -> Ans<'ans>

    type Sk<'a, 'ans> = 'a -> Fk<'ans> -> Ans<'ans>  

    type TraversalM<'a, 'ans> = 
        TraversalM of (Sk<'a, 'ans> -> Fk<'ans> -> Ans<'ans>)

    let apply1 (ma : TraversalM<'a, 'ans>)
               (success : Sk<'a, 'ans>)
               (failure : Fk<'ans>) : Ans<'ans> = 
        let (TraversalM f) = ma in f success failure

    let failM () : TraversalM<'a, 'ans> = 
        TraversalM <| fun _ fk -> fk "failM"

    let mreturn (x : 'a) : TraversalM<'a, 'ans> =
        TraversalM <| fun sk fk -> sk x fk


    let bindM (ma : TraversalM<'a, 'ans>)
              (next : 'a -> TraversalM<'b, 'ans>) : TraversalM<'b, 'ans> =
        TraversalM <| fun sk -> 
            apply1 ma (fun a -> apply1 (next a) sk)

    let mzero () : TraversalM<'a, 'ans> = 
        TraversalM <| fun _ _ -> Error "mzero"

    let mplus (ma : TraversalM<'a, 'ans>) 
              (mb : TraversalM<'a, 'ans>) : TraversalM<'a, 'ans> = 
        TraversalM <| fun sk fk ->
            match apply1 ma sk fk with
            | Error _ -> apply1 mb sk fk
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


    let runTraversal (ma : TraversalM<'a, 'a>) : Ans<'a> = 
        apply1 ma (fun a _ -> Ok a) (fun msg -> Error msg)

    let traversalError (msg : string) : TraversalM<'a, 'ans> = 
        TraversalM <| fun _ _ -> Error msg


    let mcatch (ma : TraversalM<'a, 'ans>) (fn : string -> TraversalM<'a, 'ans>) : TraversalM<'a, 'ans> = 
        TraversalM <| fun sk fk -> 
            match apply1 ma sk fk with
            | Error msg -> apply1 (fn msg) sk fk
            | Ok a -> Ok a 


    type Transform<'a, 'b> = 'a -> TraversalM<'b, 'b>

    type Rewrite<'a> = Transform<'a, 'a>

    let idR : Rewrite<'a> = mreturn

    let failT : Transform<'a, 'b> = fun _ -> traversalError "failT"

    let ( <+ ) (t1 : Transform<'a, 'b>) (t2 : Transform<'a, 'b>) : Transform<'a, 'b> = 
        fun a -> mcatch (t1 a) (fun _ -> t2 a)

    let tryR (rw : Rewrite<'a>) = rw <+ idR

        
