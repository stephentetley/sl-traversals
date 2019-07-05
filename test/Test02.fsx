// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#load @"..\src\SLTraversals\Internal\TraversalMonad.fs"
#load @"..\src\SLTraversals\Transform.fs"
open SLTraversals.Internal.TraversalMonad
open SLTraversals.Transform



type RewriteDict<'a, 'ctx, 'st> = 
    { AllRewrite : GenRewrite<'a, 'ctx, 'st>
      AnyRewrite : GenRewrite<'a, 'ctx, 'st>
      OneRewrite : GenRewrite<'a, 'ctx, 'st>
    }


type RoseTree<'a> = RoseTree of 'a * RoseTree<'a> list

let tree1 : RoseTree<int> = RoseTree(1, [RoseTree(2,[]); RoseTree(3,[])])



exception StrategyFailure of string

let sfail () : string = 
    raise(StrategyFailure "sfail")

