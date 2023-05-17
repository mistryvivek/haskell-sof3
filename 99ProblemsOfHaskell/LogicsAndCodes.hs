module LogicsAndCodes where

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True True = True
xor' _ _ = False

impl' :: Bool -> Bool -> Bool
impl' False True = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"

table2 :: (Bool -> Bool -> Bool) -> IO ()
table2 fn = do
    let combo1 = fn True True
    putStrLn (boolToString True ++ " " ++ boolToString True ++ " " ++ boolToString combo1 ++ " ")
    let combo2 = fn True False
    putStrLn (boolToString True ++ " " ++ boolToString False ++ " " ++ boolToString combo2 ++ " ")
    let combo3 = fn False True
    putStrLn (boolToString False ++ " " ++ boolToString True ++ " " ++ boolToString combo3 ++ " ")
    let combo4 = fn False False
    putStrLn (boolToString False ++ " " ++ boolToString False ++ " " ++ boolToString combo4 ++ " ")


