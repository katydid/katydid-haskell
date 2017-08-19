# Katydid

A Haskell implementation of Katydid.

This includes:

  - Relapse: Validation Language 
  - Parsers: JSON and XML

[Documentation for katydid](http://katydid.github.io/)

[Documentation for katydid-haskell](https://katydid.github.io/katydid-haskell/)

All JSON and XML tests from [the language agnostic test suite](https://github.com/katydid/testsuite) [passes].

## Example

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