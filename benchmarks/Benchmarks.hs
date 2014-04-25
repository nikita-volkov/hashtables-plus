
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

    C.standoff "VS Hashtables" $ do
      rows <- liftIO $ replicateM 10000 $ 
        (,) <$> R.generateName gen <*> R.generateVariate gen
      liftIO $ evaluate $! rnf rows
      C.subject "HashtablesPlus" $ do
        liftIO $ do
          t :: H.Table H.Linear StrictText Int <- H.new
          forM_ rows $ H.insertFast t
      C.subject "Hashtables" $ do
        liftIO $ do
          t :: T.LinearHashTable StrictText Int <- T.new
          forM_ rows $ uncurry $ T.insert t

    C.standoff "Set/Inserting" $ do
      let 
        subject :: 
          forall c. (H.Insert c, H.Row c ~ StrictText) => 
          c -> Int -> C.Standoff ()
        subject t n = do
          values <- liftIO $ replicateM n $ R.generateName gen
          C.subject (cs $ show n) $ do
            liftIO $ do
              c :: c <- H.new
              forM_ values $ H.insertFast c

      C.group "Sized" $ do
        C.group "Linear" $ do
          mapM_ (subject (undefined :: H.Set H.Linear StrictText)) 
                [10000, 9000, 8000]
      C.group "Linear" $ do
        mapM_ (subject (undefined :: H.Set H.Linear StrictText)) 
              [10000, 9000, 8000]
      C.group "Cuckoo" $ do
        mapM_ (subject (undefined :: H.Set H.Cuckoo StrictText)) 
              [10000, 9000, 8000]
      C.group "Basic" $ do
        mapM_ (subject (undefined :: H.Set H.Basic StrictText)) 
              [10000, 9000, 8000]

    C.standoff "Multitable/Inserts" $ do
      rows <- 
        liftIO $ forM [10000, 9000, 8000] $ \n -> do
          replicateM n $ (,) <$> R.generateName gen <*> R.generateVariate gen
      liftIO $! evaluate $! rnf $! rows
      C.group "Multitable" $ do
        C.group "Linear" $ do
          forM_ rows (rowsInsertionSubject (undefined :: H.MultiTable H.Linear StrictText (H.Set H.Linear Int)))
      C.group "Table" $ do
        C.group "Linear" $ do
          forM_ rows (rowsInsertionSubject (undefined :: H.Table H.Linear StrictText Int))



rowsInsertionSubject :: 
  forall c. (H.Insert c, H.Row c ~ (StrictText, Int)) => 
  c -> [H.Row c] -> C.Standoff ()
rowsInsertionSubject _ rows = do
  C.subject (cs $ show $ length $ rows) $ do
    liftIO $ do
      c :: c <- H.new
      forM_ rows $ H.insertFast c
