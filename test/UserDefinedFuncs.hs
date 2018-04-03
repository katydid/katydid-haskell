module UserDefinedFuncs (
    mkUserDefinedLibrary
    , incExpr
    , concatExpr
    , isPrimeExpr
) where

import Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.Text

import Data.Numbers.Primes (isPrime)

import Expr

-- |
-- mkUserDefinedLibrary is a library of user defined functions that can be passed to the parser.
mkUserDefinedLibrary :: String -> [AnyExpr] -> Except String AnyExpr
mkUserDefinedLibrary "inc" args = mkIncExpr args
mkUserDefinedLibrary "concat" args = mkConcatExpr args
mkUserDefinedLibrary "isPrime" args = mkIsPrime args
mkUserDefinedLibrary n _ = throwError $ "undefined function: " ++ n

-- |
-- mkIncExpr tries to create an incExpr from a variable number and dynamically types arguments.
-- This function is used by the Relapse parser to insert your user defined expression into the expression tree.
mkIncExpr :: [AnyExpr] -> Except String AnyExpr
mkIncExpr args = do {
    arg <- assertArgs1 "inc" args;
    mkIntExpr . incExpr <$> assertInt arg;
}

-- |
-- incExpr creates an expression that increases its input argument by 1.
-- This function is also useful if we want to build up our own well typed expression tree, 
-- bypassing the Relapse parser.
incExpr :: Expr Int -> Expr Int
incExpr intArg = trimInt Expr {
    desc = mkDesc "inc" [desc intArg]
    , eval = \fieldValue -> (+1) <$> eval intArg fieldValue
}

-- |
-- mkConcatExpr tries to create an concatExpr from a variable number and dynamically types arguments.
mkConcatExpr :: [AnyExpr] -> Except String AnyExpr
mkConcatExpr args = do {
    (arg1, arg2) <- assertArgs2 "inc" args;
    strArg1 <- assertString arg1;
    strArg2 <- assertString arg2;
    return $ mkStringExpr $ concatExpr strArg1 strArg2;
}

-- |
-- concatExpr creates an expression that concatenates two string together.
concatExpr :: Expr Data.Text.Text -> Expr Data.Text.Text -> Expr Data.Text.Text
concatExpr strExpr1 strExpr2 = trimString Expr {
    desc = mkDesc "concat" [desc strExpr1, desc strExpr2]
    , eval = \fieldValue -> do {
        str1 <- eval strExpr1 fieldValue;
        str2 <- eval strExpr2 fieldValue;
        return $ Data.Text.concat [str1,str2];
    }
}

-- |
-- mkIsPrimeExpr tries to create an isPrimeExpr from a variable number and dynamically types arguments.
mkIsPrime :: [AnyExpr] -> Except String AnyExpr
mkIsPrime args = do {
    arg <- assertArgs1 "isPrime" args;
    case arg of
    (AnyExpr _ (IntFunc _)) -> mkBoolExpr . isPrimeExpr <$> assertInt arg;
    (AnyExpr _ (UintFunc _)) ->  mkBoolExpr . isPrimeExpr <$> assertUint arg;
}

-- |
-- isPrime creates an expression checks whether a number is prime.
isPrimeExpr :: Integral a => Expr a -> Expr Bool
isPrimeExpr numExpr = trimBool Expr {
    desc = mkDesc "isPrime" [desc numExpr]
    , eval = \fieldValue -> isPrime <$> eval numExpr fieldValue
}