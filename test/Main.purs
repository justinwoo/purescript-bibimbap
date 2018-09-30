module Test.Main where

import Prelude

import Bibimbap as Bibimbap
import Data.Generic.Rep as GR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception as Exception
import Test.Assert as Assert

data ErrorGenericSum
  = Apple
  | Banana { info :: String }
  | Cherry Int Int Int
derive instance genericErrorGenericSum :: GR.Generic ErrorGenericSum _
derive instance eqErrorGenericSum :: Eq ErrorGenericSum
instance showErrorGenericSum :: Show ErrorGenericSum where
  show Apple = "Apple"
  show (Banana x) = "Banana " <> show x
  show (Cherry x y z) = "Cherry " <> show x <> " " <> show y <> " " <> show z


data SomeOtherType
  = Whatever
  | Else
derive instance genericSomeOtherType :: GR.Generic SomeOtherType _

main :: Effect Unit
main = do
  let
    knownGenericSum :: ErrorGenericSum
    knownGenericSum = Banana { info: "hello" }
    knownError = Bibimbap.mkGenericSumError knownGenericSum
    resultKnownError = Bibimbap.readGenericSum knownError

  Assert.assertEqual
    { expected: "GenericSumError"
    , actual: Exception.message knownError
    }

  Assert.assertEqual
    { expected: Just knownGenericSum
    , actual: resultKnownError
    }

  let
    unknownGenericSum :: SomeOtherType
    unknownGenericSum = Whatever
    unknownError = Bibimbap.mkGenericSumError unknownGenericSum
    resultUnknownError :: Maybe ErrorGenericSum
    resultUnknownError = Bibimbap.readGenericSum unknownError

  Assert.assertEqual
    { expected: "GenericSumError"
    , actual: Exception.message unknownError
    }

  Assert.assertEqual
    { expected: Nothing
    , actual: resultUnknownError
    }

  let
    plainError = Exception.error "plain error"
    resultPlainError :: Maybe ErrorGenericSum
    resultPlainError = Bibimbap.readGenericSum plainError

  Assert.assertEqual
    { expected: "plain error"
    , actual: Exception.message plainError
    }

  Assert.assertEqual
    { expected: Nothing
    , actual: resultPlainError
    }
