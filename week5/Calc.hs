import ExprT
import Parser

-- Exercise 1

eval ::  ExprT -> Integer
eval (Lit v) = v
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr expS = case parseExp Lit Add Mul expS of
    Nothing -> Nothing
    Just expr -> Just $ eval expr

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


instance Expr ExprT where    
    lit = Lit
    add  = Add
    mul = Mul


-- Exercise 4
instance Expr Integer where
   lit x = x
   add l r = l + r
   mul :: Integer -> Integer -> Integer
   mul l r = l * r


instance Expr Bool where
  lit x | x <=0      = True
        | otherwise  = False
  add l r = l || r
  mul l r = l&& r

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit :: Integer -> MinMax
  lit =  MinMax
  add = min
  mul = max

instance Num Mod7 where
   (+) (Mod7 x) (Mod7 y) = Mod7 $ (`mod` 7) (x + y)
   (*) (Mod7 x) (Mod7 y) = Mod7 $ (`mod` 7) (x * y)
   fromInteger = Mod7 . (`mod` 7)


instance Expr Mod7 where
  lit = fromInteger
  add = (+)
  mul = (*)


-- testing
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMm      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
