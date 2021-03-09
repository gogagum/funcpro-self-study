module ListAndSort.FoldImpl

open System

/// Example from lecture
let rec Fold f emptyListAns = function
    | [] -> emptyListAns
    | head::tail -> f head (Fold f emptyListAns tail)
    
let sum = Fold (+) 0
let prod = Fold (*) 1
let minEl = Fold min Int32.MaxValue
