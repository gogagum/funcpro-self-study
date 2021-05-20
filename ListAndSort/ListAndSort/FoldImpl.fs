module FoldImpl

open System

/// Example from lecture
let rec Fold f emptyListAns = function
    | [] -> emptyListAns
    | head::tail -> f head (Fold f emptyListAns tail)
    
let rec foldr f i = function
    | [] -> i
    | h::t -> f h (foldr f i t)
    
let foldl f i =
    let rec foldlImpl acc = function
        | [] -> acc
        | h::t -> foldlImpl (f acc h) t
    foldlImpl i
        
let sum = Fold (+) 0
let prod = Fold (*) 1
let minEl = Fold min Int32.MaxValue
