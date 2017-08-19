# Katydid

A Haskell implementation of Katydid.

This includes:

  - [Relapse](https://katydid.github.io/katydid-haskell/Relapse.html): Validation Language 
  - Parsers: [JSON](https://katydid.github.io/katydid-haskell/Json.html) and [XML](https://katydid.github.io/katydid-haskell/Xml.html)

[Documentation for katydid](http://katydid.github.io/)

[Documentation for katydid-haskell](https://katydid.github.io/katydid-haskell/)

[Documentation for katydid-haskell/Relapse](https://katydid.github.io/katydid-haskell/Relapse.html)

All JSON and XML tests from [the language agnostic test suite](https://github.com/katydid/testsuite) [passes].

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
main = 
  case Relapse.parseGrammar ".DragonsExist == true" of
    (Left err) -> putStrLn $ "relapse parse error: " ++ show err
    (Right refs) -> case Json.decodeJSON "{\"DragonsExist\": false}" of
      (Left err) -> putStrLn $ "json parse error: " ++ show err
      (Right tree) -> if (Relapse.validate refs tree)
        then putStrLn "dragons exist"
        else putStrLn "dragons are fictional"
```

## Efficiency

If you want to validate multiple trees using the same grammar then the filter function does some internal memoization, which makes a huge difference.

```haskell
filter :: Tree t => Refs -> [[t]] -> [[t]]
```

