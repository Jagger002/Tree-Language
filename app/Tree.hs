module Tree where

data Tree = Leaf | Stem Tree | Branch Tree Tree
  deriving (Show)

f :: Tree -> Tree -> Tree
f Leaf x = Stem x
f (Stem a) x = Branch a x
f (Branch Leaf a) b = a
f (Branch (Stem a) b) c = f (f a c) (f b c)
f (Branch (Branch a b) c) Leaf = a
f (Branch (Branch a b) c) (Stem x) = f b x
f (Branch (Branch a b) c) (Branch x y) = f (f c x) y

calculateSize :: Tree -> Integer
calculateSize Leaf = 0
calculateSize (Stem tree) = 1 + calculateSize tree
calculateSize (Branch l r) = 1 + (calculateSize l) + (calculateSize r)
