import Data.Eq

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
  foldMap f Nil = mempty
  foldMap f (Node l v r) =
           (foldMap f l) <> f v <> (foldMap f r)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
instance Foldable Preorder where
    foldMap f (PreO (Nil)) = mempty
    foldMap f (PreO (Node l v r)) = f v <> (foldMap f $ PreO l) <> (foldMap f $ PreO r)

newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
instance Foldable Postorder where
    foldMap f (PostO (Nil)) = mempty
    foldMap f (PostO (Node l v r)) = (foldMap f $ PostO l) <> (foldMap f $ PostO r) <> f v

newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)
instance Foldable Levelorder where
    foldMap f tree = foldMap' f (tbf [tree]) where
        foldMap' f [] = mempty
        foldMap' f (x:xs) = f x <> foldMap' f xs

        tbf [] = []
        tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs)) where
            nodeValue (LevelO (Node _ a _)) = a
            leftAndRightNodes (LevelO (Node Nil _ Nil)) = []
            leftAndRightNodes (LevelO (Node Nil _ b))   = [LevelO b]
            leftAndRightNodes (LevelO (Node a _ Nil))   = [LevelO a]
            leftAndRightNodes (LevelO (Node a _ b))     = [LevelO a, LevelO b]


inorderTree = Node Nil 2 (Node (Node Nil 4 Nil) 3 (Node Nil 5 Nil))
preorderTree = PreO (inorderTree)
postorderTree = PostO (inorderTree)
levelorderTree = LevelO (inorderTree)
