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

let rec betterQuickSort = function
   | [] -> []
   | x::xs ->
      let (l, r) = List.partition ((>) x) xs  // use standard library split func 
      betterQuickSort l @ x::betterQuickSort(r)

//------------------------------------------------------------------------------
// Permute 

// Insert x in every possible position and return list of lists
let rec insertions (x : 'a, l : 'a list) =
   match l with
      | [] -> [[x]]
      | h::t -> (x::h::t)::(List.map (fun l -> h::l) (insertions(x, t)))

// Permute
let rec permute = function
   | [] -> []
   | h::t -> List.collect (fun z -> insertions(h, z)) (permute t)

//------------------------------------------------------------------------------
// Reverse

let rev L =
   let rec revImpl s = function
      | [] -> s
      | h::t -> revImpl (h::s) t
   revImpl [] L

//------------------------------------------------------------------------------
// Main
[<EntryPoint>]
let main argv =
    let test_list = [1; 10; 2; 9; 3; 8; 4; 7; 5; 6]
    printfn "%i" (test_list.Length) 
    printfn "%A" test_list
    printfn "%A" (quickSort test_list)
    printfn "%A" (sort test_list)
    printfn "%A" (betterQuickSort test_list)
    
    // Concat method sample
    let listOfLists = [[1; 2]; [3; 4]; [5; 6]]
    printfn "%A" (List.concat listOfLists)
    
    let listToUseChoose = [1; 2; 6; 4; 2; 4; 4; 5; 7]
    printfn "%A" (List.choose (fun x->if x % 2 = 0 then Some(x) else None) listToUseChoose)

    let toReverse = [1; 2; 3; 4; 5; 6]
    printfn "%A" (rev toReverse)
        
    0
    