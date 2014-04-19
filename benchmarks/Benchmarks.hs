
import Benchmarks.Prelude.Basic hiding (group)
import Benchmarks.Prelude.Strings
import Benchmarks.Prelude.Transformers
import qualified CriterionPlus as C
import qualified Benchmarks.Random as R
import qualified HashtablesPlus as H
import qualified Data.HashTable.IO as T

main = do
  gen <- R.newGen
  C.benchmark $ do
    C.standoff "Table" $ do
      rows <- liftIO $ replicateM 10000 $ 
        (,) <$> R.generateName gen <*> R.generateVariate gen
      liftIO $ evaluate $! rnf rows
      C.group "Hashtables" $ do
        C.subject "10000" $ do
          liftIO $ do
            t :: T.LinearHashTable StrictText Int <- T.new
            forM_ rows $ uncurry $ T.insert t
      C.group "HashtablesPlus" $ do
        C.subject "10000" $ do
          liftIO $ do
            t :: H.Table H.Linear StrictText Int <- H.new
            forM_ rows $ H.insertFast t


    -- C.standoff "Set/Inserting" $ do
    --   let 
    --     subject :: 
    --       forall c. (H.Insert c, H.Row c ~ StrictText) => 
    --       c -> Int -> C.Standoff ()
    --     subject t n = do
    --       values <- liftIO $ replicateM n $ R.generateName gen
    --       C.subject (cs $ show n) $ do
    --         liftIO $ do
    --           c :: c <- H.new
    --           forM_ values $ H.insert c

    --   C.group "Sized" $ do
    --     C.group "Linear" $ do
    --       mapM_ (subject (undefined :: H.Set H.Linear StrictText)) 
    --             [31000, 26000, 21000, 16000, 11000, 6000, 1000]
    --   C.group "Linear" $ do
    --     mapM_ (subject (undefined :: H.Set H.Linear StrictText)) 
    --           [31000, 26000, 21000, 16000, 11000, 6000, 1000]
    --   C.group "Cuckoo" $ do
    --     mapM_ (subject (undefined :: H.Set H.Cuckoo StrictText)) 
    --           [31000, 26000, 21000, 16000, 11000, 6000, 1000]
    --   C.group "Basic" $ do
    --     mapM_ (subject (undefined :: H.Set H.Basic StrictText)) 
    --           [31000, 26000, 21000, 16000, 11000, 6000, 1000]
