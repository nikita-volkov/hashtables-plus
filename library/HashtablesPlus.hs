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
  -- A unique row identifier.
  -- For tables it's a key,
  -- for multitables it's a key-value pair,
  -- for sets it's the item itself.
  type UniqueKey c
  -- | 
  -- A lookup result.
  -- For tables it's a 'Maybe' value,
  -- for multitables and sets it's a 'Bool'.
  type LookupResult c
  new :: IO c
  lookup :: c -> UniqueKey c -> IO (LookupResult c)
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
  delete :: c -> UniqueKey c -> IO Bool
  -- |
  -- Same as 'delete', but avoiding the calculation of the operation result.
  deleteFast :: c -> UniqueKey c -> IO ()
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



-- * Standard HashTables
-------------------------

-- | A newtype wrapper over 'T.BasicHashTable'.
newtype BasicHashTable k v = BasicHashTable (T.BasicHashTable k v)

instance (Key k) => Collection (BasicHashTable k v) where
  type Row (BasicHashTable k v) = (k, v)
  type UniqueKey (BasicHashTable k v) = k
  type LookupResult (BasicHashTable k v) = Maybe v
  new = BasicHashTable <$> T.new
  lookup (BasicHashTable t) = T.lookup t
  foldM (BasicHashTable t) z f = T.foldM f z t

instance (Key k, Eq v) => Insert (BasicHashTable k v) where
  insert (BasicHashTable t) (k, v) = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.insert t k v >> return True
  insertFast (BasicHashTable t) (k, v) = T.insert t k v

instance (Key k, Eq v) => Delete (BasicHashTable k v) where
  delete (BasicHashTable t) k = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.delete t k >> return True
  deleteFast (BasicHashTable t) k = T.delete t k


-- | A newtype wrapper over 'T.CuckooHashTable'.
newtype CuckooHashTable k v = CuckooHashTable (T.CuckooHashTable k v)

instance (Key k) => Collection (CuckooHashTable k v) where
  type Row (CuckooHashTable k v) = (k, v)
  type UniqueKey (CuckooHashTable k v) = k
  type LookupResult (CuckooHashTable k v) = Maybe v
  new = CuckooHashTable <$> T.new
  lookup (CuckooHashTable t) = T.lookup t
  foldM (CuckooHashTable t) z f = T.foldM f z t

instance (Key k, Eq v) => Insert (CuckooHashTable k v) where
  insert (CuckooHashTable t) (k, v) = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.insert t k v >> return True
  insertFast (CuckooHashTable t) (k, v) = T.insert t k v

instance (Key k, Eq v) => Delete (CuckooHashTable k v) where
  delete (CuckooHashTable t) k = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.delete t k >> return True
  deleteFast (CuckooHashTable t) k = T.delete t k


-- | A newtype wrapper over 'T.LinearHashTable'.
newtype LinearHashTable k v = LinearHashTable (T.LinearHashTable k v)

instance (Key k) => Collection (LinearHashTable k v) where
  type Row (LinearHashTable k v) = (k, v)
  type UniqueKey (LinearHashTable k v) = k
  type LookupResult (LinearHashTable k v) = Maybe v
  new = LinearHashTable <$> T.new
  lookup (LinearHashTable t) = T.lookup t
  foldM (LinearHashTable t) z f = T.foldM f z t

instance (Key k, Eq v) => Insert (LinearHashTable k v) where
  insert (LinearHashTable t) (k, v) = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.insert t k v >> return True
  insertFast (LinearHashTable t) (k, v) = T.insert t k v

instance (Key k, Eq v) => Delete (LinearHashTable k v) where
  delete (LinearHashTable t) k = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.delete t k >> return True
  deleteFast (LinearHashTable t) k = T.delete t k




-- * Sets
-------------------------

-- ** Set
-------------------------

-- | 
-- A set of values, 
-- which have instances for 'Eq' and 'Hashable'.
-- 
-- @t@ is the underlying 'HashTable' implementation, 
-- @a@ is the item.
newtype Set t a = Set (T.IOHashTable t a ())

instance (HashTable t, Key a) => Collection (Set t a) where
  type Row (Set t a) = a
  type UniqueKey (Set t a) = a
  type LookupResult (Set t a) = Bool
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

-- | 
-- A specialized set of 'HR.HashRef's.
-- 
-- @t@ is the underlying 'HashTable' implementation, 
-- @a@ is the item.
newtype HashRefSet t a = HashRefSet (T.IOHashTable t (StableName a) a)

instance (HashTable t) => Collection (HashRefSet t a) where
  type Row (HashRefSet t a) = HR.HashRef a
  type UniqueKey (HashRefSet t a) = HR.HashRef a
  type LookupResult (HashRefSet t a) = Bool
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
-- A wrapper over a 'Collection',
-- which adds cheap 'null' and 'size' functions.
data Sized c = Sized !c {-# UNPACK #-} !(IORef Int)

instance (Collection c) => Collection (Sized c) where
  type Row (Sized c) = Row c
  type UniqueKey (Sized c) = UniqueKey c
  type LookupResult (Sized c) = LookupResult c
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
-- A multitable with underlying 'HashTable' @t@, key @k@ and 
-- a set implementation @s@.
-- 
-- E.g.:
-- 
-- @
-- type MyMultiTable k v = MultiTable 'T.BasicHashTable' k ('Set' 'T.BasicHashTable' v)
-- @
newtype MultiTable t k s = MultiTable (T.IOHashTable t k s)

instance (HashTable t, Key k, Collection s, LookupResult s ~ Bool) => 
         Collection (MultiTable t k s) where
  type Row (MultiTable t k s) = (k, Row s)
  type UniqueKey (MultiTable t k s) = (k, UniqueKey s)
  type LookupResult (MultiTable t k s) = LookupResult s
  new = MultiTable <$> T.new
  lookup (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return False
      Just s -> lookup s v
  foldM (MultiTable t) z f = T.foldM f' z t where
    f' z (k, s) = foldM s z f'' where
      f'' z v = f z (k, v)

instance (HashTable t, Key k, Insert s, LookupResult s ~ Bool) => 
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

instance (HashTable t, Key k, Delete c, LookupResult c ~ Bool) => Delete (MultiTable t k c) where
  delete (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return False
      Just s -> delete s v
  deleteFast (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return ()
      Just s -> deleteFast s v
      
instance (HashTable t, Key k, Delete c, LookupResult c ~ Bool) => Delete (MultiTable t k (Sized c)) where
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
      
