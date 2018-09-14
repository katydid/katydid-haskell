module UserDefinedFuncs
  ( userLib
  , incExpr
  , concatExpr
  , isPrimeExpr
  )
where

import qualified Data.Text

import           Data.Numbers.Primes            ( isPrime )

import           Data.Katydid.Relapse.Expr

-- |
-- userLib is a library of user defined functions that can be passed to the parser.
userLib :: String -> [AnyExpr] -> Either String AnyExpr
userLib "inc"     args = mkIncExpr args
userLib "concat"  args = mkConcatExpr args
userLib "isPrime" args = mkIsPrime args
userLib n         _    = Left $ "undefined function: " ++ n

-- |
-- mkIncExpr tries to create an incExpr from a variable number and dynamically types arguments.
-- This function is used by the Relapse parser to insert your user defined expression into the expression tree.
mkIncExpr :: [AnyExpr] -> Either String AnyExpr
mkIncExpr args = do
  arg <- assertArgs1 "inc" args
  mkIntExpr . incExpr <$> assertInt arg

-- |
-- incExpr creates an expression that increases its input argument by 1.
-- This function is also useful if we want to build up our own well typed expression tree, 
-- bypassing the Relapse parser.
incExpr :: Expr Int -> Expr Int
incExpr intArg = trimInt Expr
  { desc = mkDesc "inc" [desc intArg]
  , eval = \fieldValue -> (+ 1) <$> eval intArg fieldValue
  }

-- |
-- mkConcatExpr tries to create an concatExpr from a variable number and dynamically types arguments.
mkConcatExpr :: [AnyExpr] -> Either String AnyExpr
mkConcatExpr args = do
  (arg1, arg2) <- assertArgs2 "inc" args
  strArg1      <- assertString arg1
  strArg2      <- assertString arg2
  return $ mkStringExpr $ concatExpr strArg1 strArg2

-- |
-- concatExpr creates an expression that concatenates two string together.
concatExpr :: Expr Data.Text.Text -> Expr Data.Text.Text -> Expr Data.Text.Text
concatExpr strExpr1 strExpr2 = trimString Expr
  { desc = mkDesc "concat" [desc strExpr1, desc strExpr2]
  , eval = \fieldValue -> do
    str1 <- eval strExpr1 fieldValue
    str2 <- eval strExpr2 fieldValue
    return $ Data.Text.concat [str1, str2]
  }

-- |
-- mkIsPrimeExpr tries to create an isPrimeExpr from a variable number and dynamically types arguments.
mkIsPrime :: [AnyExpr] -> Either String AnyExpr
mkIsPrime args = do
  arg <- assertArgs1 "isPrime" args
  case arg of
    (AnyExpr _ (IntFunc  _)) -> mkBoolExpr . isPrimeExpr <$> assertInt arg
    (AnyExpr _ (UintFunc _)) -> mkBoolExpr . isPrimeExpr <$> assertUint arg

-- |
-- isPrime creates an expression checks whether a number is prime.
isPrimeExpr :: Integral a => Expr a -> Expr Bool
isPrimeExpr numExpr = trimBool Expr
  { desc = mkDesc "isPrime" [desc numExpr]
  , eval = \fieldValue -> isPrime <$> eval numExpr fieldValue
  }
