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
import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Type.Equality (type (==))
import Unsafe.Coerce
import Control.Exception.Lifted


data FixBase m r a where
  Fix :: FixBase m r (Eff (FixBase m r ': r) a -> m a)

getFix :: forall m r r' a. Member (FixBase m r) r' => Eff r' (Eff (FixBase m r ': r) a -> m a)
getFix = send $ Fix @m @r


runFixM :: forall m r a
         . BaseMember m r
        => (forall x. Eff r x -> m x)
        -> Eff (FixBase m r ': r) a
        -> m a
runFixM i = fixed
  where
    fixed :: Eff (FixBase m r ': r) x -> m x
    fixed = i . runNat runFixNat

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

instance ( LastMember m (FixBase m r ': r)
         , MonadBase m m
         ) => MonadBaseControl m (Eff (FixBase m r ': r)) where
  type StM (Eff (FixBase m r ': r)) a = a
  liftBaseWith f = do
    i <- getFix @m @r
    send $ f $ unsafeCoerce i
  restoreM = return

type Fixed cs m a = forall r
                  . ( Members cs r
                    , LastMember m (FixBase m r ': r)
                    )
                 => Eff (FixBase m r ': r) a

data Console a where
  Write :: Show a => a -> Console ()

write :: (Member Console r, Show a) => a -> Eff r ()
write = send . Write

runWriter :: Member IO r => Eff (Console ': r) a -> Eff r a
runWriter = runNat nat
  where
    nat :: Console x -> IO x
    nat (Write a) = putStrLn $ show a

prog :: Fixed '[State Char, Console] IO ()
prog = do
  put 'g'
  x <- get @Char
  bracket get (\c -> write $ c == x)
              (\_ -> get >>= write . (x ==))
  get @Char >>= write

main :: IO ()
main = do
  runFixM (fmap fst . runM . runWriter . flip runState 'c') prog
  return ()
