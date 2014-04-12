{-# LANGUAGE UndecidableInstances #-}
module HashtablesPlus 
(
  -- * HashTable Implementations
  -- | 
  -- These are implementations of a class 'HashTable',
  -- which provide different performance characteristics.
  -- They are used as parameters to data structures.
  Basic,
  Cuckoo,
  Linear,
  -- * Data Structures
  Table,
  Set,
  HashRefSet,
  MultiTable,
  Sized,
  -- * Interface
  Collection(..),
  Insert(..),
  Delete(..),
  Size(..),
  forM_,
  toList,
  null,
)
where

import HashtablesPlus.Prelude hiding (toList, null, insert, delete, lookup, foldM, forM_)
import qualified HashtablesPlus.HashRef as HR
import qualified Data.HashTable.IO as T
import qualified Data.HashTable.ST.Basic
import qualified Data.HashTable.ST.Cuckoo
import qualified Data.HashTable.ST.Linear


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



-- * 'HashTable' Implementations
-------------------------

type Basic = Data.HashTable.ST.Basic.HashTable
type Cuckoo = Data.HashTable.ST.Cuckoo.HashTable
type Linear = Data.HashTable.ST.Linear.HashTable



-- * HashTable
-------------------------

-- | 
-- A newtype wrapper over a 'HashTable' implementation @t@.
-- 
-- E.g.:
-- 
-- @
-- type CuckooTable k v = 'Table' 'Cuckoo' k v
-- @
newtype Table t k v = Table (T.IOHashTable t k v)

instance (HashTable t, Key k) => Collection (Table t k v) where
  type Row (Table t k v) = (k, v)
  type UniqueKey (Table t k v) = k
  type LookupResult (Table t k v) = Maybe v
  new = Table <$> T.new
  lookup (Table t) = T.lookup t
  foldM (Table t) z f = T.foldM f z t

instance (HashTable t, Key k, Eq v) => Insert (Table t k v) where
  insert (Table t) (k, v) = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.insert t k v >> return True
  insertFast (Table t) (k, v) = T.insert t k v

instance (HashTable t, Key k, Eq v) => Delete (Table t k v) where
  delete (Table t) k = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.delete t k >> return True
  deleteFast (Table t) k = T.delete t k



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
-- 
-- E.g.:
-- 
-- @
-- type CuckooSet a = 'Set' 'Cuckoo' a
-- @
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
-- 
-- E.g.:
-- 
-- @
-- type LinearHashRefSet a = 'HashRefSet' 'Linear' a
-- @
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
-- which adds 'null' and 'size' functions of /O(1)/ complexity.
-- 
-- E.g.:
-- 
-- @
-- type SizedLinearTable k v = 'Sized' ('Table' 'Linear' k v)
-- @
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



-- * MultiTable
-------------------------

-- |
-- A multitable with underlying 'HashTable' @t@, key @k@ and 
-- a set implementation @s@.
-- 
-- E.g.:
-- 
-- @
-- type BasicMultiTable k v = 'MultiTable' 'Basic' k ('Set' 'Basic' v)
-- @
-- 
-- If a 'Sized' implementation of set is specified, 
-- a more space efficient instance of 'Delete' will be used. E.g.:
-- 
-- @
-- MultiTable Basic k ('Sized' (Set Basic v))
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
      
