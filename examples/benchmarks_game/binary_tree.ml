type 'a succ = Succ of 'a
type 'a tree =
   | Node : ('a tree, 'a tree) -> 'a succ tree
   | Leaf : unit tree
