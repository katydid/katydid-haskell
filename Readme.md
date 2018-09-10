# Katydid

[![Build Status](https://travis-ci.org/katydid/katydid-haskell.svg?branch=master)](https://travis-ci.org/katydid/katydid-haskell)

A Haskell implementation of Katydid.

![Katydid Logo](https://cdn.rawgit.com/katydid/katydid.github.io/master/logo.png)

This includes:

  - [Relapse](https://katydid.github.io/katydid-haskell/Relapse.html): Validation Language 
  - Parsers: [JSON](https://katydid.github.io/katydid-haskell/Json.html) and [XML](https://katydid.github.io/katydid-haskell/Xml.html)

[Documentation for katydid](http://katydid.github.io/)

[Documentation for katydid-haskell](https://katydid.github.io/katydid-haskell/)

[Documentation for katydid-haskell/Relapse](https://katydid.github.io/katydid-haskell/Relapse.html)

All JSON and XML tests from [the language agnostic test suite](https://github.com/katydid/testsuite) [passes].

[Hackage](https://hackage.haskell.org/package/katydid)

## Example

Validating a single structure can be done using the validate function:
```haskell
validate :: Tree t => Grammar -> [t] -> Bool
```

, where a tree is a class in the [Parsers](https://katydid.github.io/katydid-haskell/Parsers.html) module:
```haskell
class Tree a where
    getLabel :: a -> Label
    getChildren :: a -> [a]
```

Here is an example that validates a single JSON tree:
```haskell
main = either 
    (\err -> putStrLn $ "error:" ++ err) 
    (\valid -> if valid 
        then putStrLn "dragons exist" 
        else putStrLn "dragons are fictional"
    ) $
    Relapse.validate <$> 
        Relapse.parse ".DragonsExist == true" <*> 
        Json.decodeJSON "{\"DragonsExist\": false}"
```

## Efficiency

If you want to validate multiple trees using the same grammar then the filter function does some internal memoization, which makes a huge difference.

```haskell
filter :: Tree t => Grammar -> [[t]] -> [[t]]
```

## User Defined Functions

If you want to create your own extra functions for operating on the leaves,
then you can inject them into the parse function:

```haskell
main = either
    (\err -> putStrLn $ "error:" ++ err)
    (\valid -> if valid
        then putStrLn "prime birthday !!!"
        else putStrLn "JOMO"
    ) $
    Relapse.validate <$>
        Relapse.parseWithUDFs userLib ".Survived->isPrime($int)" <*>
        Json.decodeJSON "{\"Survived\": 104743}"
```

Defining your own user library to inject is easy.
The `Expr` library provides many useful helper functions:

```haskell
import Data.Numbers.Primes (isPrime)
import Data.Katydid.Relapse.Expr

userLib :: String -> [AnyExpr] -> Either String AnyExpr
userLib "isPrime" args = mkIsPrime args
userLib n _ = throwError $ "undefined function: " ++ n

mkIsPrime :: [AnyExpr] -> Either String AnyExpr
mkIsPrime args = do {
    arg <- assertArgs1 "isPrime" args;
    mkBoolExpr . isPrimeExpr <$> assertInt arg;
}

isPrimeExpr :: Integral a => Expr a -> Expr Bool
isPrimeExpr numExpr = trimBool Expr {
    desc = mkDesc "isPrime" [desc numExpr]
    , eval = \fieldValue -> isPrime <$> eval numExpr fieldValue
}
```

## Roadmap

  - Protobuf parser
  - Profile and Optimize (bring up to par with Go version)
  - Typed DSL (Combinator)