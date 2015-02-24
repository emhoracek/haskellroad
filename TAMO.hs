module TAMO where

infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infix 2 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

-- Validity checker for boolean propositions of one variable
valid1 :: (Bool -> Bool) -> Bool
valid1 bf = bf True && bf False

excludedMiddle :: Bool -> Bool
excludedMiddle p = p || not p

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf =    bf True  True
            && bf True  False
            && bf False True
            && bf False False

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [ True, False ],
                             q <- [ True, False ],
                             r <- [ True, False ]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [ bf p q r s | p <- [ True, False ],
                               q <- [ True, False ],
                               r <- [ True, False ],
                               s <- [ True, False ]]

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = 
    (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)


