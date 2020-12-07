{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep, TypeRep)


------------ Singleton API


data family Sing (a :: k) -- Allow new instances to be declared external to module
data SomeSing k where
    SomeSing :: Sing (a :: k) -> SomeSing k

withSomeSing :: SomeSing k
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
    type Demote k = r | r -> k
    toSing :: Demote k -> SomeSing k
    fromSing :: Sing (a :: k) -> Demote k

-- | Reify types
class SingI (a :: k) where
    sing :: Sing a


------------ Example: Bool


data instance Sing (a :: Bool) where
    STrue :: Sing 'True
    SFalse :: Sing 'False

instance SingKind Bool where
    type Demote Bool = Bool
    toSing True = SomeSing STrue
    toSing False = SomeSing SFalse
    fromSing STrue = True
    fromSing SFalse = False

instance SingI 'True where
    sing = STrue

instance SingI 'False where
    sing = SFalse


------------ Example: Maybe


data instance Sing (a :: Maybe k) where
    SJust :: Sing (a :: k) -> Sing ('Just a)
    SNothing :: Sing 'Nothing


instance SingI a => SingI ('Just a) where
    sing = SJust sing

instance SingI 'Nothing where
    sing = SNothing


instance (k ~ Demote k, SingKind k)
    => SingKind (Maybe k) where
    type Demote (Maybe k) = Maybe k
    toSing (Just a) =
        withSomeSing (toSing a) $ SomeSing . SJust
    toSing Nothing = SomeSing SNothing
    fromSing (SJust a) = Just $ fromSing a
    fromSing SNothing = Nothing


------------ Example: []


data instance Sing (a :: [k]) where
    SNil :: Sing '[]
    SCons :: Sing (h :: k) -> Sing (t :: [k]) -> Sing (h ': t)


instance (k ~ Demote k, SingKind k)
    => SingKind [k] where
    type Demote [k] = [k]

    toSing [] = SomeSing SNil
    toSing (h : t) =
        withSomeSing (toSing h) $ \sh ->
            withSomeSing (toSing t) $ \st ->
                SomeSing $ SCons sh st

    fromSing SNil = []
    fromSing (SCons sh st) =
        fromSing sh : fromSing st

instance SingI '[] where
    sing = SNil

instance (SingI a, SingI t) => SingI ((a :: k) ': (t :: [k])) where
    sing = SCons sing sing

main :: IO ()
main = undefined
