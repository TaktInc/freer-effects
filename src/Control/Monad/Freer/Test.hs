{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Control.Monad.Freer.Test where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Functor.Identity
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Type.Equality (type (==))
import Unsafe.Coerce
import Control.Exception.Lifted


type family Fixed m r where
  Fixed Identity r = (r :++ '[FixBase Identity r])
  Fixed m r = (r :++ '[FixBase m r, m])

type family (:++) (xs :: [k]) (ys :: [k]) where
  '[]       :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)

data FixBase m r a where
  Fix :: FixBase m r (Eff (Fixed m r) a -> m a)

getFix :: forall m r r' a. Member (FixBase m r) r' => Eff r' (Eff (Fixed m r) a -> m a)
getFix = send $ Fix @m @r


runFix :: forall r a
        . (forall x. Eff (Fixed Identity r) x -> Eff '[FixBase Identity r] x)
       -> Eff (Fixed Identity r) a
       -> a
runFix i = fixed
  where
    fixed :: Eff (Fixed Identity r) x -> x
    fixed = run . runFixNat . i

    runFixNat :: Eff (FixBase Identity r ': r') x -> Eff r' x
    runFixNat = handleRelay pure (\Fix arr -> arr $ fmap Identity fixed)


runFixM :: forall m r a
         . Monad m
        => (forall x. Eff (Fixed m r) x -> Eff '[FixBase m r, m] x)
        -> Eff (Fixed m r) a
        -> m a
runFixM i = fixed
  where
    fixed :: Eff (Fixed m r) x -> m x
    fixed = runM . runNat runFixNat . i

    runFixNat :: FixBase m r x -> m x
    runFixNat Fix = return fixed


class Member eff effs => LastMember (eff :: * -> *) (effs :: [* -> *]) | effs -> eff

-- | Base case for 'LastMember'.
instance LastMember eff '[eff]

-- | Recursively look for a base effect. We need to make sure that this
-- instance is not overlapping with @instance LastMember m '[m]@, which is the
-- reason for the complex pattern matching on list of effects.
instance
    ( (any1 == eff) ~ 'False
    , Member eff (any1 ': any2 ': effs)
    ) => LastMember eff (any1 ': any2 ': effs)


class (Monad m, LastMember m effs) => BaseMember m effs | effs -> m

-- | Last effect that is also a monad is considered to be a base monad, i.e.
-- base effect.
instance (Monad m, LastMember m effs) => BaseMember m effs

instance (BaseMember m effs, MonadBase b m) => MonadBase b (Eff effs) where
    liftBase = send . (liftBase :: b a -> m a)


instance ( r' ~ Fixed m r
         , BaseMember m r'
         , MonadBase m m
         , Member (FixBase m r) r') => MonadBaseControl m (Eff r') where
  type StM (Eff r') a = a
  restoreM = return
  liftBaseWith f = do
    i <- getFix @m @r
    send $ f $ unsafeCoerce i

prog :: Eff (Fixed IO '[State Char]) Bool
prog = do
  put 'g'
  bracket get (\c -> send $ putStrLn $ show c) (\c -> send $ putStrLn $ show c)
  return True

