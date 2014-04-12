{-# LANGUAGE UndecidableInstances #-}
module HashtablesPlus where

import HashtablesPlus.Prelude hiding (null, insert, delete, lookup, foldM, forM_)
import qualified HashtablesPlus.HashRef as HR
import qualified Data.HashTable.IO as T


-- * Shared Interface
-------------------------

class Collection c where
  -- | 
  -- A row of the collection. 
  -- For tables and multitables it's a key-value pair, 
  -- for sets it's just the item.
  type Row c
  -- | 
  -- A row identifier.
  -- For tables it's a key,
  -- for multitables it's a key-value pair,
  -- for sets it's the item itself.
  type RowID c
  -- | 
  -- A lookup result.
  -- For tables it's a 'Maybe' value,
  -- for multitables and sets it's a 'Bool'.
  type Lookup c
  new :: IO c
  lookup :: c -> RowID c -> IO (Lookup c)
  foldM :: c -> r -> (r -> Row c -> IO r) -> IO r

class Collection c => Insert c where
  -- | 
  -- Insert a row into a collection.
  -- 
  -- Returns a boolean signifying the success of the operation.
  -- I.e., whether it produced any changes.
  insert :: c -> Row c -> IO Bool
  -- |
  -- Same as 'insert', but avoiding the calculation of the operation result.
  insertFast :: c -> Row c -> IO ()
  insertFast = (void .) . insert

class Collection c => Delete c where
  -- |
  -- Delete a row from a collection by its identifier.
  -- 
  -- Returns a boolean signifying the success of the operation.
  -- I.e., whether it produced any changes.
  delete :: c -> RowID c -> IO Bool
  -- |
  -- Same as 'delete', but avoiding the calculation of the operation result.
  deleteFast :: c -> RowID c -> IO ()
  deleteFast = (void .) . delete

class Collection c => Size c where
  -- |
  -- /O(1)/.
  -- Get the size of the collection.
  size :: c -> IO Int

forM_ :: (Collection c) => c -> (Row c -> IO ()) -> IO ()
forM_ c f = foldM c () (\() r -> f r)

-- |
-- /O(n)/.
-- Convert the collection to a list.
toList :: (Collection c) => c -> IO [Row c]
toList c = foldM c [] (\li ro -> return $ ro : li)

-- |
-- /O(1)/.
-- Check whether a sizeable collection is empty.
null :: (Size s) => s -> IO Bool
null = fmap (<= 0) . size

-- |
-- A constraint for values usable as hash table key.
type Key k = (Hashable k, Eq k)



-- * Sets
-------------------------

-- ** Set
-------------------------

-- | 
-- A set of values, 
-- which have instances for 'Eq' and 'Hashable'.
newtype Set t a = Set (T.IOHashTable t a ())

instance (HashTable t, Key a) => Collection (Set t a) where
  type Row (Set t a) = a
  type RowID (Set t a) = a
  type Lookup (Set t a) = Bool
  new = Set <$> T.new
  lookup (Set table) a = T.lookup table a >>= return . isJust
  foldM (Set table) z f = T.foldM f' z table where 
    f' z (a, _) = f z a

instance (HashTable t, Key a) => Insert (Set t a) where
  insert (Set table) a = do
    T.lookup table a >>= \case
      Just _ -> return False
      Nothing -> do
        T.insert table a ()
        return True
  insertFast (Set table) a = T.insert table a ()

instance (HashTable t, Key a) => Delete (Set t a) where
  delete (Set table) a = do
    T.lookup table a >>= \case
      Just _ -> do
        T.delete table a
        return True
      Nothing -> return False
  deleteFast (Set table) a = T.delete table a

-- ** HashRefSet
-------------------------

-- | A set of 'HR.HashRef's.
newtype HashRefSet t a = HashRefSet (T.IOHashTable t (StableName a) a)

instance (HashTable t) => Collection (HashRefSet t a) where
  type Row (HashRefSet t a) = HR.HashRef a
  type RowID (HashRefSet t a) = HR.HashRef a
  type Lookup (HashRefSet t a) = Bool
  new = HashRefSet <$> T.new
  lookup (HashRefSet table) (HR.HashRef sn a) = T.lookup table sn >>= return . isJust
  foldM (HashRefSet table) z f = T.foldM f' z table where 
    f' z (sn, a) = f z (HR.HashRef sn a)

instance (HashTable t) => Insert (HashRefSet t a) where
  insert (HashRefSet table) (HR.HashRef sn a) = do
    T.lookup table sn >>= \case
      Just _ -> return False
      Nothing -> do
        T.insert table sn a
        return True
  insertFast (HashRefSet table) (HR.HashRef sn a) = T.insert table sn a

instance (HashTable t) => Delete (HashRefSet t a) where
  delete (HashRefSet table) (HR.HashRef sn a) = do
    T.lookup table sn >>= \case
      Just _ -> do
        T.delete table sn
        return True
      Nothing -> return False
  deleteFast (HashRefSet table) (HR.HashRef sn a) = T.delete table sn



-- * Sized
-------------------------

-- |
-- A wrapper over a collection,
-- which adds cheap 'null' and 'size' functions.
data Sized c = Sized !c {-# UNPACK #-} !(IORef Int)

instance (Collection c) => Collection (Sized c) where
  type Row (Sized c) = Row c
  type RowID (Sized c) = RowID c
  type Lookup (Sized c) = Lookup c
  new = Sized <$> new <*> newIORef 0
  lookup (Sized set _) hr = lookup set hr
  foldM (Sized set _) = foldM set

instance (Insert c) => Insert (Sized c) where
  insert (Sized set size) hr = do
    ok <- insert set hr  
    when ok $ modifyIORef size succ
    return ok

instance (Delete c) => Delete (Sized c) where
  delete (Sized set size) hr = do
    ok <- delete set hr
    when ok $ modifyIORef size pred
    return ok

instance (Collection c) => Size (Sized c) where
  size (Sized _ s) = readIORef s



-- * Multitable
-------------------------

-- |
-- A multitable with underlying hashtable @t@, key @k@ and 
-- a set implementation @s@.
-- 
-- E.g.:
-- 
-- @
-- type MyMultiTable k v = MultiTable 'T.BasicHashTable' k ('Set' 'T.BasicHashTable' v)
-- @
newtype MultiTable t k s = MultiTable (T.IOHashTable t k s)

instance (HashTable t, Key k, Collection s, Lookup s ~ Bool) => 
         Collection (MultiTable t k s) where
  type Row (MultiTable t k s) = (k, Row s)
  type RowID (MultiTable t k s) = (k, RowID s)
  type Lookup (MultiTable t k s) = Lookup s
  new = MultiTable <$> T.new
  lookup (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return False
      Just s -> lookup s v
  foldM (MultiTable t) z f = T.foldM f' z t where
    f' z (k, s) = foldM s z f'' where
      f'' z v = f z (k, v)

instance (HashTable t, Key k, Insert s, Lookup s ~ Bool) => 
         Insert (MultiTable t k s) where
  insert (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> do
        s <- new
        insertFast s v
        T.insert t k s
        return True
      Just s -> do
        insert s v
  insertFast (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> do
        s <- new
        insertFast s v
        T.insert t k s
      Just s -> do
        insertFast s v

instance (HashTable t, Key k, Delete c, Lookup c ~ Bool) => Delete (MultiTable t k c) where
  delete (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return False
      Just s -> delete s v
  deleteFast (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return ()
      Just s -> deleteFast s v
      
instance (HashTable t, Key k, Delete c, Lookup c ~ Bool) => Delete (MultiTable t k (Sized c)) where
  delete (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return False
      Just s -> do
        delete s v >>= \case
          False -> return False
          True -> do
            null s >>= \case
              False -> return ()
              True -> T.delete t k
            return True
  deleteFast (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return ()
      Just s -> do
        deleteFast s v
        null s >>= \case
          False -> return ()
          True -> T.delete t k
      
