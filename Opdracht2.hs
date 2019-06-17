import Data.Ord (comparing)
import Data.List (sortBy)

type Prijs = Double
type Titel = String
type Auteur = String

-- Create Boek DataType
data Boek = Boek Auteur Titel Prijs
    deriving (Eq, Ord, Show)

-- Create DataType Box (Optional/Option)
data Box a = Some a
    | None 
        deriving Show

-- Create DataType Zak (Optional/Option)
data Zak a = Iets a
    | Niets 
        deriving Show

-- Create DataType Lijst
data Lijst a =  Hoofd a (Lijst a)
    | Leeg
        deriving Show

-- Derive Box, Zak, and Lijst from Functor
instance Functor Box where
    fmap f None = None
    fmap f (Some a) = Some (f a)

instance Functor Zak where
    fmap f Niets = Niets
    fmap f (Iets a) = Iets (f a)

instance Functor Lijst where
    fmap f Leeg = Leeg
    fmap f (Hoofd a rest) = (Hoofd (f a) (fmap f rest))

-- Create Some Test Data
boekA = Boek "SchrijverA" "TitelAB" 9.99
boekB = Boek "SchrijverA" "TitelAB" 9.99
boekC = Boek "SchrijverB" "TitelAC" 9.99
boekD = Boek "SchrijverC" "TitelAA" 10.99
boekE = Boek "SchrijverD" "TitelDE" 5.99

-- boekA == boekB
-- boekB != boekC

boekenLijst = [boekA, boekB, boekC, boekD, boekE]

-- Define a function that can sort Books on Title
sorteerOpTitel :: [Boek]->[Boek]
sorteerOpTitel = sortBy( comparing (titel))
    where titel (Boek _ t _) = t

-- Define a function that can pack stuff in Optionals ( Box )
pakInBox :: a-> Box a
pakInBox a = Some a

-- Define a function that can unpack stuff from Optionals ( Box )
pakBoxUit :: Box a->a
pakBoxUit (Some a) = a

-- Define a function that can encapsulate Books in Optionals( Boxes )
-- No longer needed after the implementing Functor 
pakInBoxen :: [a]->[Box a]
pakInBoxen [] = [None]
pakInBoxen [x] = [pakInBox x]
pakInBoxen (x:xs) = pakInBox x  : pakInBoxen xs

-- Define a function that can push an item on our 'Lijst'
push :: a -> Lijst a -> Lijst a
push a Leeg = Hoofd a Leeg
push a (Hoofd h rest) = Hoofd a (Hoofd h rest)

-- Define a function that can push multiple itmes on our 'Lijst'
pushList :: [a] -> Lijst a -> Lijst a
pushList lijst a = foldr push a lijst

-- Oneliners:
boekInZakInBoxLijst = fmap (Some) (fmap (Iets) boekenLijst)
randomNumberList = fmap (Some) [5,4,9,7,5,342,8,321,65,1,60,29,4,58,77]

lijstMetGetallenInBoxen = pushList randomNumberList Leeg
lijstMetGetallenInZakken = fmap (Iets) (fmap pakBoxUit lijstMetGetallenInBoxen)

