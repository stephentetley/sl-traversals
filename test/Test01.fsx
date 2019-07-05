// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#load @"..\src\SLTraversals\Internal\TraversalMonad.fs"
#load @"..\src\SLTraversals\Transform.fs"
open SLTraversals.Internal.TraversalMonad
open SLTraversals.Transform

type RoseTree<'a> = RoseTree of 'a * RoseTree<'a> list

let tree1 : RoseTree<int> = RoseTree(1, [RoseTree(2,[]); RoseTree(3,[])])


let test01 () = 
    runTraversal () () tree1 <| 
        traversal { 
            return 0
        }

let test02 () : Result<int, ErrMsg> = 
    runTraversal () () tree1 <| 
        traversal { 
            return! strategyFailure "Bad"
        }

let test03 () : Result<string, ErrMsg> = 
    let step = 
        traversal { 
            let! a = mreturn 2
            let! b = mreturn 4
            
            return (a + b).ToString()
        }
    runTraversal () () tree1 step


let test04 () : Result<string, ErrMsg> = 
    let failStep = 
        traversal { 
            return! strategyFailure "Bad"
        }
    runTraversal () () tree1 <| mcatch failStep (fun excp -> mreturn (excp.Message + "!!!"))

let test05 () : Result<string, ErrMsg> = 
    let failStep = 
        traversal { 
            return! strategyFailure "Bad"
        }
    runTraversal () () tree1 <| mplus failStep (mreturn "Good")
