module PracticePaperP1 where
import Data.Char (digitToInt, intToDigit)

isHaskell :: String -> Bool
isHaskell "Haskell" = True
isHaskell _         = False

-- 1 mark

testHaskell'' :: Bool
testHaskell'' =
 (isHaskell "Haskell!" == False) &&
 (isHaskell "1Haskell" == False) &&
 (isHaskell "haskeLL"  == False) &&
 (isHaskell "HASKELL"  == False) &&
 (isHaskell " Haskell" == False) &&
 (isHaskell "Haskell " == False)

 
lowerVowel :: String -> Bool
lowerVowel = all (`elem` "aeiou")

-- 1 mark
testlv' :: Bool
testlv' = 
    (lowerVowel ""                  == True) &&
    (lowerVowel "uet"               == False) &&
    (lowerVowel " uea"              == False) &&
    (lowerVowel "aaa eee"           == False) &&
    (lowerVowel "abeIou"            == False) &&
    (lowerVowel "uoiea"             == True)  &&
    (lowerVowel "iiiiaaaaeeeooouuu" == True)  &&
    (lowerVowel "oooEaaauuu"        == False) &&
    (lowerVowel "iii0uuuu"          == False) &&
    (lowerVowel "eeeIooou"          == False) &&
    (lowerVowel "aaaeeeiiUuuo"      == False) &&
    (lowerVowel "uuuaaaeeeOooiiI"   == False)

prodCube :: [Int] -> Int
prodCube xs = foldr (\x y -> x * x * x * y) 1 (filter evenAndNotMultiFour xs)
    where evenAndNotMultiFour a = ((/= 0) . snd $ divMod a 4) && even a         
        
-- 1 mark
testc', testc'' :: Bool
testc' = (prodCube []           == 1)
     && (prodCube [4, 8]        == 1)
     && (prodCube [4, 6, 8]     == 216)
     && (prodCube [4, 6, 8, 12] == 216)
     && (prodCube [2..11]       == 1728000)  

-- 1 mark
testc'' = (prodCube [1,3,4,5]       == 1)
     && (prodCube [-5..5]           == -64)
     && (prodCube [-10..5]          == -13824000)
     && (prodCube [-5..10]          == -13824000)
     && (prodCube (map (*2) [2..5]) == 216000) 

consDiff :: String -> Int
consDiff = sum . map translate
    where translate x | x `elem` "bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ" = 1
                      | x `elem` "0123456789" = -1
                      | otherwise = 0

-- 1 mark
testco' :: Bool
testco' = 
    (consDiff ""                           == 0)
    && (consDiff "SOf3in2021"              == -2)
    && (consDiff "Software123andTheory123" == 5)
    && (consDiff "HASkellprogramming2021"  == 9)
-- 1 mark   
testco'' :: Bool
testco'' = 
    (consDiff "abc12"                  == 0)
    && (consDiff "strength00122"       == 2)
    && (consDiff "fada95610asamoah6"   == -1)
    && (consDiff "27ho74keta3nyakwan"  == 3)

isIATA :: String -> Bool
isIATA xs = length xs == 3 && all (`elem` "BCDFGHJKLMNPQRSTVWXYZ") xs

testia', testia'' :: Bool
-- 1 mark
testia' =
    (isIATA ""     == False) &&
    (isIATA "MAN"  == False) &&
    (isIATA "LHR"  == True)  &&
    (isIATA "LHRT" == False) &&
    (isIATA "lhr"  == False) &&
    (isIATA "JFK"  == True)  &&
    (isIATA "BHX"  == True)  

-- 1 mark    
testia'' =
    (isIATA "   "     == False) &&
    (isIATA "M1K"     == False) &&
    (isIATA "XYZ"     == True)  &&
    (isIATA "MYWHH"   == False) &&
    (isIATA "lhr"     == False) &&
    (isIATA "QFG"     == True)  &&
    (isIATA "CDM"     == True)  &&
    (isIATA "TVW"     == True)  &&
    (isIATA "cmd"     == False) &&
    (isIATA "NPS"     == True)

countIATA :: [String] -> Int
countIATA = length . filter isIATA

-- 1 mark
testiaa', testiaa'' :: Bool
testiaa' = 
    (countIATA ["LHR"]                                     == 1) &&
    (countIATA ["LHR", "Lhr", "MAN", "JFK", "", "jfk"]     == 2) &&
    (countIATA ["LHR", "BHX", "MAN", "JFK", "ACC", "LRHT"] == 3)

-- 1 mark    
testiaa'' = 
    (countIATA ["LHR", "   "]                                  == 1) &&
    (countIATA ["M1K", "XYZ", "MYWHH", "lhr", "QFG", "JFK"]    == 3) &&
    (countIATA ["CDM", "TVW", "cmd", "NPS", "nps"]             == 3) &&
    (countIATA ["cmd", "twv", "YML", "ZZZ", "zzz", "BBBR", ""] == 2)

type Students = [(Name, Age, College)]
type Name = String
type Age = Int
type College = String

onBus66 :: Students
onBus66 = [("Zain", 18, "Halifax"), ("Julia", 20, "Constantine"),
           ("Mandy", 22, "Goodricke"), ("Jack", 24, "Constantine"),
           ("Emma", 21, "Langwith"), ("Zack", 19, "Halifax"),
           ("Alice", 21, "Halifax"), ("Bob", 19, "Alcuin"),
           ("Lui", 22, "Goodricke")]

colleges :: Students -> Int ->  [College]
colleges xs x= map extract $ filter checkAge xs
    where extract (x,_,y) = y
          checkAge  (_,z,_) = z == x

-- 1 mark
testcolleges' :: Bool
testcolleges'  = 
  (colleges onBus66 19 == ["Halifax","Alcuin"]) &&
  (colleges onBus66 20 == ["Constantine"]) &&
  (colleges onBus66 21 == ["Langwith","Halifax"]) &&
  (colleges onBusxx 78 == ["Halifax","Goodricke"]) &&
  (colleges onBusxx 44 == ["Constantine","Halifax","Alcuin","Constantine"]) &&
  (colleges onBusxx 96 == ["Goodricke"]) &&
  (colleges onBusxx 19 == ["Langwith"]) &&
  (colleges onBusxx 71 == ["Constantine","Langwith","Halifax","Halifax"]) &&
  (colleges onBusxx 18 == []) &&
  (colleges onBusxx 22 == ["Alcuin"])   

onBusxx :: Students
onBusxx = [("Peter", 78, "Halifax"), ("Mary", 44, "Constantine"), ("Paul", 96, "Goodricke"),
          ("Janet", 71, "Constantine"), ("Jayju", 71, "Langwith"), ("Matt", 44, "Halifax"),
          ("Aaron", 71, "Halifax"), ("Collins", 44, "Alcuin"), ("Shuliang", 78, "Goodricke"), 
          ("Alikanta", 71, "Halifax"), ("Jenny", 22, "Alcuin"), ("Mandy", 19, "Langwith"), ("Johnson", 44, "Constantine")]

joinBus :: Students -> Name ->  Age ->  College ->  Students
joinBus xs n a c = (n, a, c) : xs

testc2 :: Bool
-- 2 marks 
testc2 = 
 (joinBus onBus66 "Adam" 19 "Constantine"  == 
 [("Adam",19,"Constantine"),("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")]) &&
 (joinBus onBusxy "Jenny" 22 "Alcuin"      == 
 [("Jenny",22,"Alcuin"),("Peter",78,"Halifax")])                        &&
 (joinBus onEmptyBus "Matt" 77 "Langwith"  == [("Matt",77,"Langwith")]) &&
 (joinBus onBus66 "Simon" 23 "Langwith"    == 
 [("Simon",23,"Langwith"),("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")])
 
  
  
onBusxy, onEmptyBus :: Students
onBusxy = [("Peter", 78, "Halifax")]
onEmptyBus = []


offBus :: Students -> Name ->  Age ->  College -> Students
offBus xs n a g = filter (/= (n,a,g)) xs

-- 1 mark
testc1, testc2' :: Bool
testc1= 
 (offBus onBus66 "Jack" 24 "Constantine" == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")]) &&
 (offBus onBusxy "Peter" 78 "Halifax"    == []) &&
 (offBus onBusxy "Peter" 70 "Halifax"    == [("Peter",78,"Halifax")]) &&
 (offBus onEmptyBus "Peter" 78 "Halifax" == []) &&
 (offBus onBus66 "Peter" 78 "Halifax"    == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")]) &&
 (offBus onBus66 "Bob" 19 "Alcuin"       == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Jack",24,"Constantine"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Lui",22,"Goodricke")])

-- 1 mark
testc2' = (offBus onBus66 "Jack" 24 "Constantine" == 
 [("Zain",18,"Halifax"),("Julia",20,"Constantine"),("Mandy",22,"Goodricke"),("Emma",21,"Langwith"),("Zack",19,"Halifax"),("Alice",21,"Halifax"),("Bob",19,"Alcuin"),("Lui",22,"Goodricke")])
 


type BoolD a = (Bool, a)

bd2m :: BoolD a -> Maybe a 
bd2m (False, _) = Nothing
bd2m (True,  x) = Just x

testcb' :: Bool
-- 1 mark
testcb' = 
 (bd2m (True, 55)               == Just 55)     &&
 (bd2m (True, "SOF3")           == Just "SOF3") &&
 (bd2m (False, "SOF1")          == Nothing)     &&
 (bd2m (False, 34)              == Nothing)     &&
 (bd2m (True, [1..10])          == Just [1,2,3,4,5,6,7,8,9,10])  &&
 (bd2m (False, [1..10])         == Nothing)     &&
 (bd2m (True, (reverse "SOF3")) == Just "3FOS") &&
 (bd2m (True, [1])              == Just [1])    &&
 (bd2m (False, ['a'])           == Nothing)

bDSum :: String -> BoolD Int
bDSum xs = foldr cal (False, 0) xs
    where cal x (y1, y2) | x `elem` "0123456789" = (True, digitToInt x + y2)
                  | otherwise = (y1,y2)


testc1', testcc2 :: Bool
-- 1 mark
testc1' = (bDSum "sof3the3" == (True,6)) &&
 (bDSum "software"         == (False,0)) &&
 (bDSum ""                 == (False,0)) &&
 (bDSum "cos0"             == (True,0))

-- 2 marks
testcc2 = (bDSum " "              == (False,0)) &&
    (bDSum "000OOO"              == (True,0))  &&
    (bDSum "This is one"         == (False,0)) &&
    (bDSum "1+2+3+4+5+6"         == (True,21)) &&
    (bDSum "Haskell300is45"      == (True,12)) &&
    (bDSum "[1..9]"              == (True,10)) &&
    (bDSum ['3','e','4','r','6'] == (True,13)) &&
    (bDSum ['!','8','£','7',')','e','4','r','6'] 
                                 == (True,25))


mBSum :: String -> Maybe Int
mBSum = bd2m . bDSum

testc1s' :: Bool
-- 1 mark
testc1s' = 
    (mBSum "sof3the3"                            == Just 6)  &&
    (mBSum "software"                            == Nothing) &&
    (mBSum ""                                    == Nothing) &&
    (mBSum "cos0"                                == Just 0)  && 
    (mBSum " "                                   == Nothing) &&
    (mBSum "000OOO"                              == Just 0)  &&
    (mBSum "This is one"                         == Nothing) &&
    (mBSum "1+2+3+4+5+6"                         == Just 21) &&
    (mBSum "Haskell300is45"                      == Just 12) &&
    (mBSum "[1..9]"                              == Just 10) &&
    (mBSum ['3','e','4','r','6']                 == Just 13) &&
    (mBSum ['!','8','£','7',')','e','4','r','6'] == Just 25) &&
    (mBSum "<>()*&^!ue"                          == Nothing)

data TreeP a = Leaf Int | Node (TreeP a) a (TreeP a) deriving (Eq, Show)

emptyTreeP :: TreeP a 
emptyTreeP = Leaf 0 

itp :: Ord a => a -> TreeP a -> TreeP a 
itp x (Leaf y) = Node (Leaf nextNode) x (Leaf nextNode)
    where nextNode = y + 1
itp x (Node a b c) | b < x = Node a b (itp x c)
                   | b > x = Node (itp x a) b c
                   | otherwise = Node a b c

testtccc', testtc'', testtcc''' :: Bool
-- 1 mark
testtccc' = foldl (flip itp) emptyTreeP "hello world!"
          == Node (Node (Node (Leaf 3)
                              ' '
                              (Node (Node (Leaf 5)
                                          '!'
                                          (Leaf 5))
                                    'd'
                                    (Leaf 4)))
                        'e'
                        (Leaf 2))
                  'h'
                  (Node (Leaf 2)
                        'l'
                        (Node (Leaf 3)
                              'o'
                              (Node (Node (Leaf 5)
                                          'r'
                                          (Leaf 5))
                                    'w'
                                    (Leaf 4))))

-- 2 marks
testtc'' = 
     (itp 2 sTree      == Node (Node (Leaf 2) 2 (Leaf 2)) 6 (Leaf 1)) &&
     (itp 9 sTree      == Node (Leaf 1) 6 (Node (Leaf 2) 9 (Leaf 2))) &&
     (itp 4 emptyTreeP == Node (Leaf 1) 4 (Leaf 1))

-- 3 marks
testtcc''' = 
     (itp 't' emptyTreeP == Node (Leaf 1) 't' (Leaf 1))                                   &&
     (itp 5 sTree'       == Node (Node (Leaf 2) 5 (Leaf 2)) 6 (Node (Leaf 2) 9 (Leaf 2))) &&
     (itp 'm' cTree      == Node (Node (Node (Leaf 3) 'a' (Leaf 3)) 'd' (Node (Leaf 3) 'e' (Node (Leaf 4) 'f' (Leaf 4)))) 'h' (Node (Leaf 2) 'j' (Node (Leaf 3) 'm' (Leaf 3)))) &&
     (foldl (flip itp) emptyTreeP [2..6] == Node (Leaf 1) 2 (Node (Leaf 2) 3 (Node (Leaf 3) 4 (Node (Leaf 4) 5 (Node (Leaf 5) 6 (Leaf 5))))))
        
--sample TreeP's 
sTree  = Node (Leaf 1) 6 (Leaf 1)
sTree' = Node (Leaf 1) 6 (Node (Leaf 2) 9 (Leaf 2))
cTree  = Node (Node (Node (Leaf 3) 'a' (Leaf 3)) 'd' (Node (Leaf 3) 'e' (Node (Leaf 4) 'f' (Leaf 4)))) 'h' (Node (Leaf 2) 'j' (Leaf 2))

treeList :: Ord a => TreeP a ->  [a]
treeList (Leaf _) = []
treeList (Node a x b) = treeList a ++ [x] ++ treeList b

testcl', testcl'' :: Bool
-- 1 mark
testcl' = 
      (treeList (Node (Node (Leaf 2) '3' (Node (Leaf 3) '4' (Node (Leaf 4) '6' (Leaf 4)))) '7' (Leaf 1)) 
                       == "3467")      && 
      (treeList (Node (Node (Node (Node (Leaf 4) 'a' (Node (Leaf 5) 'e' (Leaf 5))) 'f' (Leaf 3)) 'o' (Node (Leaf 3) 'r' (Leaf 3))) 's' (Node (Leaf 2) 't' (Node (Leaf 3) 'w' (Leaf 3)))) 
                       == "aeforstw")
-- 2 marks 
testcl'' = 
      (treeList (Node (Node (Node (Leaf 3) ' ' (Node (Node (Leaf 5) '!' (Leaf 5)) 'd' (Leaf 4))) 'e' (Leaf 2)) 'h' (Node (Leaf 2) 'l' (Node (Leaf 3) 'o' (Node (Node (Leaf 5) 'r' (Leaf 5)) 'w' (Leaf 4))))) 
                       == " !dehlorw") &&
      (treeList sTree  == [6])         &&
      (treeList sTree' == [6,9])       &&
      (treeList cTree  == "adefhj")


data DBTree = DBLeaf | DBNode DBTree (Int, String) DBTree deriving (Eq, Show)

type Error a = Either String a


stdUpdateCal :: Int -> Name -> DBTree ->  DBTree
stdUpdateCal x y DBLeaf = DBLeaf
stdUpdateCal x y (DBNode a (ni, nm) b) | ni == x = DBNode a (ni, y) b
                                       | ni < x = DBNode a (ni, nm) (stdUpdateCal x y b)
                                       | otherwise = DBNode (stdUpdateCal x y a) (ni, nm) b

stdUpdate :: Int -> Name -> DBTree ->  Error DBTree
stdUpdate x y z | new == z = Left ("There is no such student with ID: " ++ [show x])
                | otherwise = Right new 
    where new = stdUpdateCal x y z


testsu', testsu'' :: Bool

emptyDB :: DBTree
emptyDB = DBLeaf

bigDB :: DBTree
bigDB = DBNode (DBNode (DBNode DBLeaf (0, "Juliana") DBLeaf) (2,"Dubios") DBLeaf) (3,"Caroline") 
   (DBNode (DBNode DBLeaf (5, "Ella") DBLeaf) (6,"Barns") (DBNode DBLeaf (10, "Adam") (DBNode DBLeaf (34, "Fred") DBLeaf)))
smallDB :: DBTree
smallDB = DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Maxwell") (DBNode DBLeaf (6,"Helen") DBLeaf)


-- 2 marks
testsu'=
  (stdUpdate 6 "Abi" smallDB   == 
  Right (DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Maxwell") (DBNode DBLeaf (6,"Abi") DBLeaf))) &&
  (stdUpdate 8 "Mandy" smallDB == 
  Left "There is no such student with ID: 8")

-- 6 marks
testsu'' = 
    (stdUpdate 6 "Garry" bigDB        == 
    Right (DBNode (DBNode (DBNode DBLeaf (0,"Juliana") DBLeaf) (2,"Dubios") DBLeaf) (3,"Caroline") (DBNode (DBNode DBLeaf (5,"Ella") DBLeaf) (6,"Garry") (DBNode DBLeaf (10,"Adam") (DBNode DBLeaf (34,"Fred") DBLeaf))))) &&
    (stdUpdate 4 "Martine" bigDB      == 
    Left "There is no such student with ID: 4") &&
    (stdUpdate 0 "Nanny" emptyDB      == 
    Left "There is no such student with ID: 0") &&
    (stdUpdate 3 "Murray" smallDB     == 
    Right (DBNode (DBNode DBLeaf (2,"James") DBLeaf) (3,"Murray") (DBNode DBLeaf (6,"Helen") DBLeaf))) &&
    (stdUpdate (-3) "Murray" smallDB  == 
    Left "There is no such student with ID: -3") && 
    (stdUpdate (2*17) "Patrick" bigDB == 
    Right (DBNode (DBNode (DBNode DBLeaf (0,"Juliana") DBLeaf) (2,"Dubios") DBLeaf) (3,"Caroline") (DBNode (DBNode DBLeaf (5,"Ella") DBLeaf) (6,"Barns") (DBNode DBLeaf (10,"Adam") (DBNode DBLeaf (34,"Patrick") DBLeaf)))))
    



infixr 0 :=: -- the fixity and priority of the operator
data ProofLayout a = QED | a :=: ProofLayout a deriving Show

caput :: [a] -> a 
caput = foldr const undefined

caputHead :: a -> [a] -> ProofLayout a
caputHead x xs = caput (x:xs)
                 :=: -- caput.0
                 foldr const undefined (x:xs)
                 :=: -- foldr.1
                 const x (foldr const undefined xs)
                 :=: -- const.0
                 x 
                 :=: -- head.0
                 head (x:xs)
                 :=:
                 QED




