# Purescript-Bibimbap

[![Build Status](https://travis-ci.org/justinwoo/purescript-bibimbap.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-bibimbap)

[Docs on Pursuit](https://pursuit.purescript.org/packages/purescript-bibimbap)

The flip side of [Chirashi](https://github.com/justinwoo/purescript-chirashi), simply storing sum type values and retrieving them by using the constructor name as a test value, by using `Generic.Rep` to get constructor names from a sum type definition.

Works by subclassing Error.

![](https://i.imgur.com/LG0UqbA.jpg)

## Usage

```purs
data ErrorGenericSum
  = Apple
  | Banana { info :: String }
  | Cherry Int Int Int
derive instance genericErrorGenericSum :: GR.Generic ErrorGenericSum _

main = do
  let
    knownGenericSum :: ErrorGenericSum
    knownGenericSum = Banana { info: "hello" }
    knownError = Bibimbap.mkGenericSumError knownGenericSum
    resultKnownError = Bibimbap.readGenericSum knownError

  Assert.assertEqual
    { expected: Just knownGenericSum
    , actual: resultKnownError
    }
```

See tests for more examples.
