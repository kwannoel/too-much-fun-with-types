{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep, TypeRep)

-- | Singleton for Boolean values
data SBool (b :: Bool) where
    STrue :: SBool 'True
    SFalse :: SBool 'False


--------------- Type to value level

class Reify a where
    reify :: a

instance Reify (SBool 'True) where
    reify = STrue

instance Reify (SBool 'False) where
    reify = SFalse

-- | Here STrue and SFalse serve as reified values of the type level 'True, 'False
fromSBool :: SBool b -> Bool
fromSBool STrue = True
fromSBool SFalse = False


--------------- Value to type level

-- Naive solution, does not work because we can only return a concrete type
{-
toSBool :: Bool -> (forall (b :: Bool). SBool b)
toSBool True = STrue
toSBool False = SFalse
-}

-- | Use an existential wrapper and an eliminator
toSBool :: Bool -> SomeSBool
toSBool True = SomeSBool STrue
toSBool False = SomeSBool SFalse

data SomeSBool where
    SomeSBool :: SBool b -> SomeSBool

-- | Our eliminator
withSomeSBool
    :: SomeSBool
    -> (forall (b :: Bool). SBool b -> r)
    -> r
withSomeSBool (SomeSBool s) f = f s

main :: IO ()
main = do
    -- value -> type -> value
    print $ withSomeSBool (toSBool True) fromSBool
    print $ withSomeSBool (toSBool False) fromSBool

    -- type -> value
    print $ fromSBool (reify :: SBool 'True)

    -- value -> type
    print $ withSomeSBool (toSBool True) f
    where
        f :: (forall (b :: Bool). SBool b -> TypeRep)
        f STrue = typeRep $ Proxy @'True
        f SFalse = typeRep $ Proxy @'False
