{-# LANGUAGE UndecidableInstances #-}
module HashtablesPlus 
(
  -- * Data Structures
  Table,
  Set,
  HashRefSet,
  MultiTable,
  Sized,
  -- * Algorithm
  Algorithm,
  -- ** Implementations
  -- | 
  -- Aliases of implementations of a class 'Data.HashTable.Class.HashTable',
  -- which provide different performance and memory consumption characteristics.
  -- They are used as parameters to data structures.
  -- For more info refer to the documentation on aliased types.
  Basic,
  Cuckoo,
  Linear,
  -- * Interface
  Key,
  Row,
  UniqueKey,
  MultiKey,
  Value,
  Collection(..),
  Lookup(..),
  LookupMulti(..),
  Elem(..),
  Insert(..),
  Delete(..),
  Size(..),
  Null(..),
  forM_,
  toList,
)
where

import HashtablesPlus.Prelude hiding (elem, toList, null, insert, delete, lookup, foldM, forM_)
import qualified HashtablesPlus.HashRef as HR
import qualified Data.HashTable.IO as T
import qualified Data.HashTable.ST.Basic
import qualified Data.HashTable.ST.Cuckoo
import qualified Data.HashTable.ST.Linear
import qualified Data.HashTable.Class


-- * Shared Interface
-------------------------

-- | 
-- A row of a collection. 
-- For tables and multitables it's a key-value pair, 
-- for sets it's just the item.
type family Row c
-- | 
-- A unique row identifier.
-- For tables it's a key,
-- for multitables it's a key-value pair,
-- for sets it's the item itself.
type family UniqueKey c
-- |
-- A non-unique row identifier.
-- For tables and sets there is none,
-- for multitables it's a key.
type family MultiKey c
-- |
-- An item of a collection.
-- For tables and multitables it's a value (from the key-value pair),
-- for sets it's the item.
type family Value c

class Collection c where
  -- |
  -- Create a new collection.
  new :: IO c
  -- |
  -- Strictly fold over the rows.
  foldM :: c -> r -> (r -> Row c -> IO r) -> IO r

class Collection c => Lookup c where
  -- |
  -- Lookup an item by a unique key.
  lookup :: c -> UniqueKey c -> IO (Maybe (Value c))

class Collection c => LookupMulti c where
  -- |
  -- Lookup multiple items by a non-unique key.
  lookupMulti :: c -> MultiKey c -> IO [Value c]

class Collection c => Elem c where
  -- |
  -- Check whether the collection contains a row by the given unique key.
  elem :: c -> UniqueKey c -> IO Bool
  default elem :: Lookup c => c -> UniqueKey c -> IO Bool
  elem = ((fmap isJust) .) . lookup

class Collection c => Insert c where
  -- | 
  -- Insert a row into a collection.
  -- 
  -- Returns a boolean signifying whether a new row has been inserted.
  -- Note that if a row has been replaced it returns 'False'.
  insert :: c -> Row c -> IO Bool
  -- |
  -- Same as 'insert', but avoiding the calculation of the operation result.
  insertFast :: c -> Row c -> IO ()
  insertFast = (void .) . insert

class Collection c => Delete c where
  -- |
  -- Delete a row from a collection by its identifier.
  -- 
  -- Returns a boolean signifying whether a row has been removed.
  delete :: c -> UniqueKey c -> IO Bool
  -- |
  -- Same as 'delete', but avoiding the calculation of the operation result.
  deleteFast :: c -> UniqueKey c -> IO ()
  deleteFast = (void .) . delete

class Collection c => Size c where
  -- |
  -- Get the size of a collection.
  size :: c -> IO Int

class Collection c => Null c where
  -- |
  -- Check whether a collection is empty.
  null :: c -> IO Bool
  default null :: (Size c) => c -> IO Bool
  null = fmap (<= 0) . size

-- |
-- Traverse thru all the rows of a collection with side effects.
forM_ :: (Collection c) => c -> (Row c -> IO ()) -> IO ()
forM_ c f = foldM c () (\() r -> f r)

-- |
-- /O(n)/.
-- Convert a collection to a list.
toList :: (Collection c) => c -> IO [Row c]
toList c = foldM c [] (\li ro -> return $ ro : li)

-- |
-- A constraint for values usable as hash table key.
type Key k = (Hashable k, Eq k)



-- * Algorithm
-------------------------

-- |
-- An alias to a 'Data.HashTable.Class.HashTable' constraint of the 
-- \"hashtables\" library.
type Algorithm = Data.HashTable.Class.HashTable

-- ** Implementations
-------------------------

-- |
-- The fastest, but the most memory-hungry implementation.
type Basic = Data.HashTable.ST.Basic.HashTable

-- |
-- The implementation with a medium performance and memory consumption.
type Cuckoo = Data.HashTable.ST.Cuckoo.HashTable

-- |
-- The implementation with a low performance, but also a low memory consumption.
type Linear = Data.HashTable.ST.Linear.HashTable



-- * HashTable
-------------------------

-- | 
-- A type synonym for an 'T.IOHashTable' with 'Algorithm' @a@.
-- 
-- E.g.:
-- 
-- @
-- type CuckooTable k v = 'Table' 'Cuckoo' k v
-- @
type Table a k v = a RealWorld k v

type instance Row (Table a k v) = (k, v)
type instance UniqueKey (Table a k v) = k
type instance Value (Table a k v) = v

instance (Algorithm a, Key k) => Collection (Table a k v) where
  {-# INLINE new #-}
  new = T.new
  {-# INLINE foldM #-}
  foldM t z f = T.foldM f z t

instance (Algorithm a, Key k) => Lookup (Table a k v) where
  {-# INLINE lookup #-}
  lookup t = T.lookup t

instance (Algorithm a, Key k) => Elem (Table a k v)

instance (Algorithm a, Key k) => Insert (Table a k v) where
  {-# INLINE insert #-}
  insert t (k, v) = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.insert t k v >> return True
  {-# INLINE insertFast #-}
  insertFast t (k, v) = T.insert t k v

instance (Algorithm a, Key k) => Delete (Table a k v) where
  {-# INLINE delete #-}
  delete t k = do
    T.lookup t k >>= \case
      Just v' -> return False 
      Nothing -> T.delete t k >> return True
  {-# INLINE deleteFast #-}
  deleteFast t k = T.delete t k



-- * Sets
-------------------------

-- ** Set
-------------------------

-- | 
-- A set of values, 
-- which have instances for 'Eq' and 'Hashable'.
-- 
-- @a@ is the underlying 'Algorithm', 
-- @v@ is the item.
-- 
-- E.g.:
-- 
-- @
-- type CuckooSet v = 'Set' 'Cuckoo' v
-- @
newtype Set a v = Set (T.IOHashTable a v ())

type instance Row (Set a v) = v
type instance UniqueKey (Set a v) = v
type instance Value (Set a v) = v

instance (Algorithm a, Key v) => Collection (Set a v) where
  new = Set <$> T.new
  foldM (Set table) z f = T.foldM f' z table where 
    f' z (a, _) = f z a

instance (Algorithm a, Key v) => Elem (Set a v) where
  elem (Set table) a = T.lookup table a >>= return . isJust

instance (Algorithm a, Key v) => Insert (Set a v) where
  insert (Set table) a = do
    T.lookup table a >>= \case
      Just _ -> return False
      Nothing -> do
        T.insert table a ()
        return True
  insertFast (Set table) a = T.insert table a ()

instance (Algorithm a, Key v) => Delete (Set a v) where
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
-- @a@ is the underlying 'Algorithm', 
-- @v@ is the item.
-- 
-- E.g.:
-- 
-- @
-- type LinearHashRefSet v = 'HashRefSet' 'Linear' v
-- @
newtype HashRefSet a v = HashRefSet (T.IOHashTable a (StableName v) v)

type instance Row (HashRefSet a v) = HR.HashRef v
type instance UniqueKey (HashRefSet a v) = HR.HashRef v
type instance Value (HashRefSet a v) = HR.HashRef v

instance (Algorithm a) => Collection (HashRefSet a v) where
  new = HashRefSet <$> T.new
  foldM (HashRefSet table) z f = T.foldM f' z table where 
    f' z (sn, a) = f z (HR.HashRef sn a)

instance (Algorithm a) => Elem (HashRefSet a v) where
  elem (HashRefSet table) (HR.HashRef sn a) = T.lookup table sn >>= return . isJust

instance (Algorithm a) => Insert (HashRefSet a v) where
  insert (HashRefSet table) (HR.HashRef sn a) = do
    T.lookup table sn >>= \case
      Just _ -> return False
      Nothing -> do
        T.insert table sn a
        return True
  insertFast (HashRefSet table) (HR.HashRef sn a) = T.insert table sn a

instance (Algorithm a) => Delete (HashRefSet a v) where
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

type instance Row (Sized c) = Row c
type instance UniqueKey (Sized c) = UniqueKey c
type instance MultiKey (Sized c) = MultiKey c
type instance Value (Sized c) = Value c

instance (Collection c) => Collection (Sized c) where
  new = Sized <$> new <*> newIORef 0
  foldM (Sized c _) = foldM c

instance (Lookup c) => Lookup (Sized c) where
  lookup (Sized c _) a = lookup c a

instance (LookupMulti c) => LookupMulti (Sized c) where
  lookupMulti (Sized c _) k = lookupMulti c k

instance (Elem c) => Elem (Sized c) where
  elem (Sized c _) a = elem c a

instance (Insert c) => Insert (Sized c) where
  insert (Sized c size) a = do
    ok <- insert c a  
    when ok $ modifyIORef size succ
    return ok

instance (Delete c) => Delete (Sized c) where
  delete (Sized c size) a = do
    ok <- delete c a
    when ok $ modifyIORef size pred
    return ok

instance (Collection c) => Size (Sized c) where
  size (Sized _ s) = readIORef s

instance (Collection c) => Null (Sized c)



-- * MultiTable
-------------------------

-- |
-- A multitable (or multimap) with an underlying 'Algorithm' @a@, a key @k@ and 
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
newtype MultiTable a k s = MultiTable (T.IOHashTable a k s)

type instance Row (MultiTable a k s) = (k, Row s)
type instance UniqueKey (MultiTable a k s) = (k, UniqueKey s)
type instance MultiKey (MultiTable a k s) = k
type instance Value (MultiTable a k s) = Value s

instance (Algorithm a, Key k, Collection s) => 
         Collection (MultiTable a k s) where
  new = MultiTable <$> T.new
  foldM (MultiTable t) z f = T.foldM f' z t where
    f' z (k, s) = foldM s z f'' where
      f'' z v = f z (k, v)

instance (Algorithm a, Key k, Collection s, Value s ~ Row s) => 
         LookupMulti (MultiTable a k s) where
  lookupMulti (MultiTable t) k = do
    T.lookup t k >>= \case
      Nothing -> return []
      Just s -> toList s

instance (Algorithm a, Key k, Elem s) => 
         Elem (MultiTable a k s) where
  elem (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return False
      Just s -> elem s v

instance (Algorithm a, Key k, Insert s) => 
         Insert (MultiTable a k s) where
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

instance (Algorithm a, Key k, Delete s) => Delete (MultiTable a k s) where
  delete (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return False
      Just s -> delete s v
  deleteFast (MultiTable t) (k, v) = do
    T.lookup t k >>= \case
      Nothing -> return ()
      Just s -> deleteFast s v
      
instance (Algorithm a, Key k, Delete s) => Delete (MultiTable a k (Sized s)) where
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
      
