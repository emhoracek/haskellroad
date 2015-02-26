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

logEquiv2 :: (Bool -> Bool -> Bool) -> 
                (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 =
    and [(bf1 p q) <=> (bf2 p q) | p <- [True, False],
                                   q <- [True, False]] 

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) ->
                (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 =
    and [(bf1 p q r) <=> (bf2 p q r) | p <- [True, False],
                                       q <- [True, False],
                                       r <- [True, False]]

formula3 p q = p
formula4 p q = (p <+> q) <+> q

test1 = logEquiv1 id (\p -> not (not p))
test2a = logEquiv1 id (\p -> p && p)
test2b = logEquiv1 id (\p -> p || p)
test3a = logEquiv2 (\p q -> p ==> q) (\p q -> not p || q)
test3b = logEquiv2 (\p q -> not (p ==> q)) (\p q -> p && not q)
test4a = logEquiv2 (\p q -> not p ==> not q) (\p q -> q ==> p)
test4b = logEquiv2 (\p q -> p ==> not q) (\p q -> q ==> not p)
test4c = logEquiv2 (\p q -> not p ==> q) (\p q -> not q ==> p)
test5a = logEquiv2 (\p q -> p <=> q)
                   (\p q -> (p ==> q) && (q ==> p))
test5b = logEquiv2 (\p q -> p <=> q)
                   (\p q -> (p && q) || (not p && not q))
test6a = logEquiv2 (\p q -> p && q) (\p q -> q && p)
test6b = logEquiv2 (\p q -> p || q) (\p q -> q || p)
test7a = logEquiv2 (\p q -> not p || not q)
                   (\p q -> not (p && q))
test7b = logEquiv2 (\p q -> not (p || q))
                   (\p q -> not p && not q)
test8a = logEquiv3 (\p q r -> p && (q && r))
                   (\p q r -> (p && q) && r)
test8b = logEquiv3 (\p q r -> p || (q || r))
                   (\p q r -> (p || q) || r)
test9a = logEquiv3 (\p q r -> p || (q && r))
                   (\p q r -> (p || q) && (p || r))
test9b = logEquiv3 (\p q r -> p || (q && r))
                   (\p q r -> (p || q) && (p || r))

test21a = logEquiv1 (\p -> not True) (\p -> False)
test21b = logEquiv1 (\p -> not False) (\p -> True)
test22 = logEquiv2 (\p q -> (p ==> False)) (\p q -> not p)
test23a = logEquiv2 (\p q -> (p || True)) (\p q -> True)
test23b = logEquiv2 (\p q -> (p && False)) (\p q -> False)
test24a = logEquiv2 (\p q -> (p || False)) (\p q -> p)
test24b = logEquiv2 (\p q -> (p && True)) (\p q -> p)
test25 = logEquiv2 (\p q -> p || not p) (\p q -> True)
test26 = logEquiv2 (\p q -> p && not p) (\p q -> False)

logContra1 :: (Bool -> Bool) -> Bool
logContra1 bf1 bf2 =
    not (bf1 True <=> bf2 True || bf1 True <=> bf2 True)
