﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"

#load @"..\src\SLTraversals\TraversalMonad3.fs"
open SLTraversals.TraversalMonad3

let test01 () = 
    runTraversal <| 
        traversal { 
            return 0
        }

let test02 () : Result<int, ErrMsg> = 
    runTraversal 12 <| 
        traversal { 
            return! traversalError "Bad"
        }

let test03 () : Result<string, ErrMsg> = 
    let step = 
        traversal { 
            let! a = mreturn 2
            let! b = mreturn 4
            
            return (a + b).ToString()
        }
    runTraversal () step


let test04 () : Result<string, ErrMsg> = 
    let failStep = 
        traversal { 
            return! traversalError "Bad"
        }
    runTraversal "input" <| mcatch failStep (fun msg -> mreturn (msg + "!!!"))

let test05 () : Result<string, ErrMsg> = 
    let failStep = 
        traversal { 
            return! traversalError "Bad"
        }
    runTraversal "input" <| mplus failStep (mreturn "Good")