module ListAndSort.List

/// list analog 
type 'a List =
     | ([])
     | (::) of 'a * ' a List
    