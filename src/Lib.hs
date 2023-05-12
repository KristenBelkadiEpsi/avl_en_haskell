module Lib where

data Avl a = Vide | Avl (Avl a) a (Avl a) deriving (Show)

hauteur :: Ord t => Avl t -> Int
hauteur Vide = -1
hauteur (Avl g _ d) = 1 + max (hauteur g) (hauteur d)

balance :: Ord t => Avl t -> Int
balance Vide = 0
balance (Avl g _ d) = hauteur d - hauteur g

taille :: Ord t => Avl t -> Int
taille Vide = 0
taille (Avl g _ d) = taille g + taille d + 1

maxArbre :: Ord t => Avl t -> t
maxArbre Vide = error "arbre vide, il n'éxiste pas de maximum"
maxArbre (Avl Vide e Vide) = e
maxArbre (Avl _ _ d) = maxArbre d

minArbre :: Ord t => Avl t -> t
minArbre Vide = error "arbre vide, il n'éxiste pas de minimum"
minArbre (Avl Vide e Vide) = e
minArbre (Avl g _ _) = minArbre g

rotation :: Ord t => Avl t -> Avl t
rotation (Avl a y (Avl b x g)) | balance (Avl a y (Avl b x g)) == 2 && balance (Avl b x g) == 1 = Avl (Avl a y b) x g
rotation (Avl (Avl a x b) y g) | balance (Avl (Avl a x b) y g) == -2 && balance (Avl a x b) == -1 = Avl a x (Avl b y g)
rotation (Avl a z (Avl (Avl b x g) y p)) | balance (Avl a z (Avl (Avl b x g) y p)) == 2 && balance (Avl (Avl b x g) y p) == -1 = Avl (Avl a z b) x (Avl g y p)
rotation (Avl (Avl a y (Avl b x g)) z p) | balance (Avl (Avl a y (Avl b x g)) z p) == -2 && balance (Avl a y (Avl b x g)) == 1 = Avl (Avl a y b) y (Avl g z p)
rotation a = a

inserer :: Ord t => Avl t -> t -> Avl t
inserer Vide elm = Avl Vide elm Vide
inserer (Avl g e d) elm
  | elm < e = rotation (Avl (inserer g elm) e d)
  | otherwise = rotation (Avl g e (inserer d elm))

supprimer :: Ord t => Avl t -> t -> Avl t
supprimer Vide _ = Vide
supprimer (Avl Vide e Vide) elm | elm == e = Vide
supprimer (Avl Vide e d) elm | elm == e = d
supprimer (Avl g e Vide) elm | elm == e = g
supprimer (Avl g e d) elm | elm == e = let y = maxArbre g in Avl (supprimer g y) y d
supprimer (Avl g e d) elm | elm > e = Avl g e (supprimer d elm)
supprimer (Avl g e d) elm | elm < e = Avl (supprimer g elm) e d

construire :: Ord t => [t] -> Avl t
construire = foldl inserer Vide

test1 :: [Int]
test1 = [5, 6, 2, 3, 1, 4, 5, 5, 6, 9, 0, 0, 1, 1, 2, 3, 5, 7, 9]

test2 :: [Int]
test2 = [1 .. 31]

testArbre :: Avl Int
testArbre = construire test2

testLib :: IO ()
testLib = do
    print testArbre