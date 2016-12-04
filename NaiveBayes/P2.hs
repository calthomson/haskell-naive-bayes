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
--type Patient = (Int, Int, Int, Int, Int, Float, Float, Int, Int)
type Patient = (Float, Float, Float, Float, Float, Float, Float, Float, Float)

--type mytype =  Either String (V.Vector Patient)
main :: IO ()
main = do
  csvData <- BL.readFile "data.csv" 
  case decode NoHeader csvData of
    Left err -> putStrLn err
    Right v -> do
        let (testVector, train) = V.splitAt (div (V.length v) 3) v
        let test = processTestingData testVector
        let sep = (separateByClass train)
        let summaries = summarizeTuple sep 
        putStrLn "Number of testing rows"
        print (length (test))
        putStrLn "Number of training rows"
        print (length (train))
    
  

-- changes a vector of tuples into a list of lists. [T1..] -> [L1..] with Li having the same values as Ti
-- Note that this will keep the final 'class marker' variable, used for checking accuracy of predictions later on
processTestingData :: (V.Vector Patient) -> [[Float]]
processTestingData v = V.toList(V.map (\(pregnancies, plasmaGlucose, bloodPressure, tricepFolds, serumInsulin, bmi, diabetesPedigree, age, diabetes)-> pregnancies:plasmaGlucose:bloodPressure: tricepFolds:serumInsulin:bmi:diabetesPedigree:age:diabetes:[]) v)

-- Partitions vector a tuple of two vectors, one containing all diabetes positive cases, the other with the negatives
separateByClass :: (V.Vector Patient) -> ((V.Vector Patient), (V.Vector Patient))
separateByClass vec = (V.partition (\(_,_,_,_,_,_,_,_,x) -> x==1) vec) 

-- Summarizes the diabetes positive and negative vectors
summarizeTuple (v1, v2) = (summarize v1, summarize v2) 

-- returns the means and standard devations for each 'column' of our vectors
summarize :: (V.Vector Patient) -> [(Float,Float)]
summarize v =
    let 
        (v1, v2, v3, v4 ,v5 ,v6, v7, v8) = (splitVars v)
        list = v1:v2:v3:v4:v5:v6:v7:v8:[]
        means = map mean list
        stdevs = map stdev list
    in (zip means stdevs)
    

-- Splits a vector of tuples into a tuple of list [(v1..v9)..] -> (l1, .., l8) where li is the list of vi's and we ignore the final variable because we already split based on that being 1 or 0 
splitVars v = V.foldr myFun ([],[],[],[],[],[],[],[]) v
myFun (pregnancies, plasmaGlucose, bloodPressure, tricepFolds, serumInsulin, bmi, diabetesPedigree, age, diabetes) (lst1, lst2, lst3, lst4, lst5, lst6, lst7, lst8) = ((pregnancies:lst1), (plasmaGlucose:lst2), (bloodPressure:lst3), (tricepFolds:lst4), (serumInsulin:lst5), (bmi:lst6), (diabetesPedigree:lst7), (age:lst8))      

-- Calculates  mean and stardard devation of a list of floats
mean numbers = (sum numbers)  / fromIntegral(length numbers)
stdev :: [Float] -> Float
stdev numbers = 
    let 
        avg = mean numbers
        varience = (sum [(x-avg)^(fromIntegral(2)) | x <- numbers])/ fromIntegral(length(numbers)-1)
    in (sqrt varience)