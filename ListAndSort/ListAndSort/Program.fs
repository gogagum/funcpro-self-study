// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

let rec splitByBorder (l : 'a list, border : 'a) =
   match l with
      | [] -> ([], [])
      | head::tail ->
          let (left, right) = splitByBorder(tail, border)
          if head >= border then (left, [head] @ right)
          else (left @ [head], right)
    
let rec quickSort (l : 'a list) =
   match l with
      | [] -> []
      | head::tail ->
         let (left, right) = splitByBorder(tail, head)
         quickSort(left) @ [head] @ quickSort(right)
    

[<EntryPoint>]
let main argv =
    let test_list = [1; 10; 2; 9; 3; 8; 4; 7; 5; 6]
    printfn "%i" (test_list.Length) 
    printfn "%A" test_list
    printfn "%A" (quickSort test_list)
    0
    