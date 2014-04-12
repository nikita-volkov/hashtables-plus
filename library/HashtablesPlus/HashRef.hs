module HashtablesPlus.HashRef where

import HashtablesPlus.Prelude


-- | 
-- A reference to a mutable value, 
-- which provides instances for 'Hashable' and 'Eq'.
-- 
-- It allows to use the values without those instances as keys in hash tables.
data HashRef a = HashRef {-# UNPACK #-} !(StableName a) !a

-- |
-- Create a new reference.
-- 
-- Two references created from the same value are not guaranteed to be equal or
-- produce the same hash. 
-- However the references created from different values are guaranteed
-- to be different.
new :: a -> IO (HashRef a)
new a = do
  sn <- makeStableName a
  return $ HashRef sn a

-- |
-- Extract the value from this reference.
value :: HashRef a -> a
value (HashRef _ a) = a

instance Hashable (HashRef a) where
  hashWithSalt s (HashRef sn _) = hashWithSalt s sn
  hash (HashRef sn _) = hash sn

instance Eq (HashRef a) where
  HashRef a _ == HashRef b _ = a == b
