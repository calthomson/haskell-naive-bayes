{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text


--   1. Number of times pregnant
--   2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
--   3. Diastolic blood pressure (mm Hg)
--   4. Triceps skin fold thickness (mm)
--   5. 2-Hour serum insulin (mu U/ml)
--   6. Body mass index (weight in kg/(height in m)^2)
--   7. Diabetes pedigree function
--   8. Age (years)
--   9. Class variable (0 or 1)

-- Original attempt at reading in file
-- loadCsv :: IO ()
-- loadCsv = do
--   csvData <- BL.readFile "data.csv"
--   case decode NoHeader csvData of
--       Left err -> putStrLn err
--       Right V.forM_ v $ \ (pregnancies, plasmaGlucose, bloodPressure, tricepFolds, serumInsulin, bmi, diabetesPedigree, age, diabetic) ->
 --           putStrLn $ pregnancies ++ ", " ++ plasmaGlucose ++ ", " ++ bloodPressure ++ ", " ++ tricepFolds ++ ", " ++ serumInsulin ++ ", " ++ bmi ++ ", " ++ diabetesPedigree ++ ", " ++ age ++ ", " ++ diabetic

-- THE BELOW CODE IS FROM HERE: https://howistart.org/posts/haskell/1
-- a simple type alias for data
type Patient = (Int, Int, Int, Int, Int, Float, Float, Int, Int)

--type mytype =  Either String (V.Vector Patient)
main :: IO ()
main = do
  csvData <- BL.readFile "data.csv" 
  case decode NoHeader csvData of
    Left err -> putStrLn err
    Right v -> print (separateByClass v)
    
  --let v = decode NoHeader csvData :: Either String (V.Vector Patient)
  -- let summed = fmap (V.foldr summer 0) v
  -- putStrLn $ "Total atBats was: " ++ (show summed)
  --print (separateByClass v)
  -- where summer (name, year, team, atBats) n = n + atBats


separateByClass :: (V.Vector Patient) -> ((V.Vector Patient), (V.Vector Patient))
separateByClass vec = ((V.filter (\(_,_,_,_,_,_,_,_,x) -> x==1) vec) , (V.filter (\(_,_,_,_,_,_,_,_,x) -> x==0) vec))

