# Katydid

[![Build Status](https://travis-ci.org/katydid/katydid-haskell.svg?branch=master)](https://travis-ci.org/katydid/katydid-haskell)

A Haskell implementation of Katydid.

This includes:

  - [Relapse](https://katydid.github.io/katydid-haskell/Relapse.html): Validation Language 
  - Parsers: [JSON](https://katydid.github.io/katydid-haskell/Json.html) and [XML](https://katydid.github.io/katydid-haskell/Xml.html)

[Documentation for katydid](http://katydid.github.io/)

[Documentation for katydid-haskell](https://katydid.github.io/katydid-haskell/)

[Documentation for katydid-haskell/Relapse](https://katydid.github.io/katydid-haskell/Relapse.html)

All JSON and XML tests from [the language agnostic test suite](https://github.com/katydid/testsuite) [passes].

[Hackage](https://hackage.haskell.org/package/katydid-0.1.0.0).

## Example

Validating a single structure can be done using the validate function:
```haskell
validate :: Tree t => Refs -> [t] -> Bool
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
        runExcept (Relapse.parseGrammar ".DragonsExist == true") <*> 
        Json.decodeJSON "{\"DragonsExist\": false}"
```

## Efficiency

If you want to validate multiple trees using the same grammar then the filter function does some internal memoization, which makes a huge difference.

```haskell
filter :: Tree t => Refs -> [[t]] -> [[t]]
```

