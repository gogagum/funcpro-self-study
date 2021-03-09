///-----------------------------------------------------------------------------
/// "Choose min" sort (from lec)

let rec fMin = function
   | [] -> failwith "No minimum element."
   | [x] -> (x, [])
   | head::tail ->
      let (min, z) = fMin tail
      if min < head then (min, head::z)
      else (head, tail)

let rec sort = function
   | [] -> []
   | x -> let(min, t) = fMin x
          min::(sort t)

///-----------------------------------------------------------------------------
/// Quicksort

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
    printfn "%A" (sort test_list)
    0
    