type Key = String
data Tree a = Leaf | Node (Tree a) Key a (Tree a) deriving Show

insert :: Tree a -> Key -> a -> Tree a
insert Leaf key val = Node Leaf key val Leaf
insert (Node l k v r) key val | key < k   = Node (insert l key val) k v r
                              | key > k   = Node l k v (insert r key val)
                              | otherwise = Node l key val r

member :: Tree a -> Key -> Bool
member Leaf _ = False
member (Node l k v r) key | key < k   = member l key
                          | key > k   = member r key
                          | otherwise = True

_lookup :: Tree a -> Key -> Maybe a
_lookup Leaf _ = Nothing
_lookup (Node l k v r) key | key < k   = _lookup l key 
                           | key > k   = _lookup r key 
                           | otherwise = Just v

empty :: Tree Integer
empty = Leaf

tree :: Tree Integer
tree = Node (Node (Node Leaf "1" 1 Leaf) "2" 2 Leaf) "3" 3 (Node Leaf "4" 4 (Node Leaf "5" 5 Leaf))
