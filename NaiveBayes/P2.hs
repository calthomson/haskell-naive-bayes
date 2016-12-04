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
        let dataSet = splitVars v
        let sep = (separateByClass v)
        -- Calculate mean and standard deviation of each attribute
        -- summaries is a ([(Float,Float)], [(Float,Float)]), 2 lists of 8 2-tuples (mean, stdev).
        -- One list is for POSITIVE, the other for NEGATIVE
        -- summaries will be:
        --([(4.8656716,3.7412379),(141.25746,31.939623),(70.82462,21.49181),(22.164179,17.679703),(100.33582,138.68918),(35.142532,7.262968),(0.5504999,0.37235445),(37.067165,10.968255)],[(3.298,3.0171828),(109.98,26.141197),(68.184,18.063078),(19.664,14.889947),(68.792,98.86524),(30.304186,7.6898556),(0.42973423,0.29908526),(31.19,11.667651)])
        let summaries = summarizeTuple sep 
        let classProbs = calcClassProb summaries v
        print classProbs
    
  

-- Partitions vector a tuple of two vectors, one containing all diabetes positive cases, the other with the negatives
separateByClass :: (V.Vector Patient) -> ((V.Vector Patient), (V.Vector Patient))
separateByClass vec = (V.partition (\(_,_,_,_,_,_,_,_,x) -> x==1) vec) 

-- Summarizes the diabetes positive and negative vectors
-- summarizeByClass
summarizeTuple :: ((V.Vector Patient), (V.Vector Patient)) -> ([(Float,Float)], [(Float,Float)])
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
    
-- vectorToList :: (V.Vector Patient) -> [Float]
-- vectorToList v = [v1, v2, v3, v4, v5, v6, v7, v8]

vectorToList :: (V.Vector Patient) -> [(Float,Float,Float,Float,Float,Float,Float,Float)]
vectorToList v =
    let list = v1:v2:v3:v4:v5:v6:v7:v8:[]
    list

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


-- Calculate Probability of Attribute Value --
-- Use Gaussian probability density function to estimate the probability of a given
-- value, given the mean and standard deviation for the attribute.
-- Output: Conditional probability of a given attribute value given a class value
calcProb :: Float -> Float -> Float -> Float
calcProb val mean stdev = do
  let prob = (1 / sqrt (2 * pi * (stdev ** 2))) * (calcExp val mean stdev)
  prob

-- Calculate exponent given an attribute value, and attribute mean & standard deviation 
calcExp :: Float-> Float -> Float -> Float
calcExp val mean stdev = exp ( - (((val - mean) ** 2) / (2 * (stdev **  2))))

-- Calculate Class Probabilities --
-- Combine the probabilities of all the attribute values
-- Input: 
--     summaries: A tuple of 2 lists (negative & positive, each containing 8 tuples (attributes) of mean & stdev
--     inputVector: The testing data, as a list of tuples of 8 Floats: [(Float, Float, Float, Float, Float, Float, Float, Float, Float)]
-- Output: Probability of entire data instance belonging to class // TODO
calcClassProb :: ([(Float,Float)], [(Float,Float)]) -> V.Vector Patient -> [Float]
calcClassProb summaries inputVector = do
  let funcs = [calcProb x| x <- (vectorToList inputVector)]
  let goodvals = zip funcs (fst summaries)
  -- let badvals = zip funcs (snd inputVector)
  let goodPreds = [ f v | (f,v) <- goodvals]
  -- let badPreds = [f v | (f,v) <- badvals]
  goodPreds

--Code for the calculateClassProbabilities() function

--def calculateClassProbabilities(summaries, inputVector):
--  probabilities = {}
--  for classValue, classSummaries in summaries.iteritems():
--    probabilities[classValue] = 1
--    for i in range(len(classSummaries)):
--      mean, stdev = classSummaries[i]
--      x = inputVector[i]
--      probabilities[classValue] *= calculateProbability(x, mean, stdev)
--  return probabilities
