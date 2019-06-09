// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLTraversals

module TraversalMonad2 =
    
    type ErrMsg = string

    type Fk<'ans> = 'ans

    type Sk<'a, 'ans> = 'a -> Fk<'ans> -> 'ans  

    type TraversalM<'a, 'ans> = 
        TraversalM of (Sk<'a, 'ans> -> Fk<'ans> -> 'ans)

    let apply1 (ma : TraversalM<'a, 'ans>)
               (success : Sk<'a, 'ans>)
               (failure : Fk<'ans>) : 'ans = 
        let (TraversalM f) = ma in f success failure


    let mreturn (x : 'a) : TraversalM<'a, 'ans> =
        TraversalM <| fun sk fk -> sk x fk


    let bindM (ma : TraversalM<'a, 'ans>)
              (f : 'a -> TraversalM<'ans, 'b>) : TraversalM<'ans, 'b> =
        TraversalM <| fun sk fk -> 
            apply1 ma (fun a -> apply1 (f a) sk) fk

    let mzero () : TraversalM<'a, 'ans> = 
        TraversalM <| fun _ fk -> fk

    let mplus (ma : TraversalM<'ans,'a>) 
              (mb : TraversalM<'ans,'a>) : TraversalM<'ans,'a> = 
        TraversalM <| fun sk fk ->
            apply1 ma sk (apply1 mb sk fk)


    type TraversalBuilder() = 
        member self.Return x    = mreturn x
        member self.Bind (p,f)  = bindM p f
        member self.Zero ()     = mzero ()

            
    let (traversal:TraversalBuilder) = new TraversalBuilder()


    let runTraversal (ma : TraversalM<'a,'ans>) : 'a = 
        apply1 ma (fun a _ -> a) id 