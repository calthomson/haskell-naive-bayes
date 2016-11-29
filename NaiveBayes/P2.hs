{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

--   1. Number of times pregnant
--   2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
--   3. Diastolic blood pressure (mm Hg)
--   4. Triceps skin fold thickness (mm)
--   5. 2-Hour serum insulin (mu U/ml)
--   6. Body mass index (weight in kg/(height in m)^2)
--   7. Diabetes pedigree function
--   8. Age (years)
--   9. Class variable (0 or 1)

main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (pregnancies, plasmaGlucose, bloodPressure, tricepFolds, serumInsulin, bmi, diabetesPedigree, age, diabetic) ->
            putStrLn $ pregnancies ++ ", " ++ plasmaGlucose ++ ", " ++ bloodPressure ++ ", " ++ tricepFolds ++ ", " ++ serumInsulin ++ ", " ++ bmi ++ ", " ++ diabetesPedigree ++ ", " ++ age ++ ", " ++ diabetic