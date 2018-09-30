module Bibimbap where

import Prelude

import Data.Function.Uncurried as FU
import Data.Generic.Rep as GR
import Data.Maybe (Maybe(..))
import Effect.Exception (Error)
import Type.Prelude (class IsSymbol, Proxy(..), SProxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- | An instance of the GenericSumError class, a subclass of Error, which holds a genericSum for extracting error information.
foreign import data GenericSumError :: Type -> Type

-- | Create an Error using a GenericSum value
mkGenericSumError :: forall a. GetTagName a => a -> Error
mkGenericSumError x = upcastGenericSumError $ FU.runFn2 _mkGenericSumError (getTagName x) x

-- | Create a GenericSumError using a GenericSum value
mkGenericSumError' :: forall a. GetTagName a => a -> GenericSumError a
mkGenericSumError' x = FU.runFn2 _mkGenericSumError (getTagName x) x

-- | Upcast a GenericSumError to Error
upcastGenericSumError :: forall a. GenericSumError a -> Error
upcastGenericSumError = unsafeCoerce

-- | Read a GenericSum from a Error
readGenericSum :: forall a. MatchKey a => Error -> Maybe a
readGenericSum err = _getGenericSum <$> FU.runFn4  _readGenericSumError (matchKey (Proxy :: Proxy a)) Nothing Just err

-- | Read a GenericSumError from a Error
readGenericSumError :: forall a. MatchKey a => Error -> Maybe (GenericSumError a)
readGenericSumError err = FU.runFn4  _readGenericSumError (matchKey (Proxy :: Proxy a)) Nothing Just err

-- | Get the GenericSum value out of a GenericSumError
getGenericSum :: forall a. GenericSumError a -> a
getGenericSum = _getGenericSum

foreign import _mkGenericSumError :: forall a. FU.Fn2 String a (GenericSumError a)
foreign import _readGenericSumError :: forall a b. FU.Fn4 (String -> Boolean) (Maybe b) (a -> Maybe a) Error (Maybe (GenericSumError a))
foreign import _getGenericSum :: forall a. GenericSumError a -> a

class GetTagName (a :: Type) where
  getTagName :: a -> String

instance getTagNameInstance ::
  ( GetTagNameImpl rep
  , GR.Generic a rep
  ) => GetTagName a where
  getTagName x = getTagNameImpl rep
    where
      rep :: rep
      rep = GR.from x

class GetTagNameImpl (rep :: Type) where
  getTagNameImpl :: rep -> String

instance sumGetTagNameImpl ::
  ( GetTagNameImpl a
  , GetTagNameImpl b
  ) => GetTagNameImpl (GR.Sum a b) where
  getTagNameImpl sum = case sum of
    GR.Inl x -> getTagNameImpl x
    GR.Inr x -> getTagNameImpl x

instance constructorGetTagNameImpl ::
  ( IsSymbol name
  ) => GetTagNameImpl (GR.Constructor name a) where
  getTagNameImpl _ = reflectSymbol (SProxy :: SProxy name)

class MatchKey (a :: Type) where
  matchKey :: Proxy a -> String -> Boolean

instance matchKeyInst ::
  ( MatchKeyImpl rep
  , GR.Generic a rep
  ) => MatchKey a where
  matchKey _ = matchKeyImpl (Proxy :: Proxy rep)

class MatchKeyImpl (rep :: Type) where
  matchKeyImpl :: Proxy rep -> String -> Boolean

instance sumMatchKeyImpl ::
  ( MatchKeyImpl a
  , MatchKeyImpl b
  ) => MatchKeyImpl (GR.Sum a b) where
  matchKeyImpl _ s =
    if matchKeyImpl (Proxy :: Proxy a) s
       then true
      else matchKeyImpl (Proxy :: Proxy b) s

instance constructorMatchKeyImpl ::
  ( IsSymbol name
  ) => MatchKeyImpl (GR.Constructor name ty) where
  matchKeyImpl _ s = s == reflectSymbol (SProxy :: SProxy name)
