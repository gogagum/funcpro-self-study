module ListAndSort.List

/// There is not type list in F#
/// But standard constructions of the language like
/// [num1; num2; num3] or num1::num2::num3 creates an object
/// that can by described the following way: 
type 'a List =
     | ([])
     | (::) of 'a * ' a List
    
