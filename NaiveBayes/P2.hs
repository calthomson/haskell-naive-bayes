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
        putStrLn "Summarizing training data"
        let summaries = summarizeTuple sep 
        putStrLn "Number of testing rows"
        print (length (test))
        putStrLn "Number of training rows"
        print (length (train))    
        putStrLn "Calculating example probability"
        let classProbs = calcClassProb summaries (head test)
        putStrLn "Chance of Positive for Diabetes:"
        print (fst classProbs)
        putStrLn "Chance of Negative for Diabetes:"
        print (snd classProbs)
        let pred = predict summaries (head test)
        putStrLn "Test prediction:"
        print pred
        putStrLn "Calculating all predictions"
        let allPred = getPredictions summaries test
        putStrLn "First 10 predictions"
        print (take 10 allPred)
        putStrLn "We were"
        print (getAccuracy allPred test)
        putStrLn "percent accurate"

-- changes a vector of tuples into a list of lists. [T1..] -> [L1..] with Li having the same values as Ti
-- Note that this will keep the final 'class marker' variable, used for checking accuracy of predictions later on
processTestingData :: (V.Vector Patient) -> [[Float]]
processTestingData v = V.toList(V.map (\(pregnancies, plasmaGlucose, bloodPressure, tricepFolds, serumInsulin, bmi, diabetesPedigree, age, diabetes)-> pregnancies:plasmaGlucose:bloodPressure: tricepFolds:serumInsulin:bmi:diabetesPedigree:age:diabetes:[]) v)

-- Partitions vector a tuple of two vectors, one containing all diabetes positive cases, the other with the negatives
separateByClass :: (V.Vector Patient) -> ((V.Vector Patient), (V.Vector Patient))
separateByClass vec = (V.partition (\(_,_,_,_,_,_,_,_,x) -> x==1) vec) 

-- Summarizes the diabetes positive and negative vectors
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

-- Calculate Class Probabilities for One Data Instance (Patient) --
-- Combine the probabilities of all the attribute values for a patient to find the liklihood of that patient
-- being positive or negative for diabese
-- Input: 
--     summaries: A tuple of 2 lists (negative & positive, each containing 8 tuples (attributes) of mean & stdev
--     inputVector: The testing data, as a list of tuples of 8 Floats: [(Float, Float, Float, Float, Float, Float, Float, Float, Float)]
---- Output: Probability of this data instance (patient) belonging to each class (POS or NEG for diabete)
calcClassProb :: ([(Float,Float)], [(Float,Float)]) -> [Float] -> (Float, Float)
calcClassProb summaries inputVector = do
  let funcs = [calcProb x| x <- inputVector]
  let goodvals = zip funcs (fst summaries)
  let badvals = zip funcs (snd summaries)
  let goodPreds = [ f v1 v2 | (f,(v1,v2)) <- goodvals]
  let badPreds = [f v1 v2 | (f,(v1,v2)) <- badvals]
  ((product goodPreds),(product badPreds))

-- Predict Class --
-- Predict probabilities of instance belonging to either class, then return class with highest probability
predict :: ([(Float,Float)], [(Float,Float)]) -> [Float] -> Float
predict summaries inputVector
  | x > y = 1
  | otherwise = 0
  where
    probabilities = calcClassProb summaries inputVector
    x = (fst probabilities)
    y = (snd probabilities)

-- Predict Every Instance in Test Data --
-- Predict class for every instance in test data
-- Output: List of result for each instance, 0 for negative prediction, 1 for positive prediction
getPredictions :: ([(Float,Float)], [(Float,Float)]) -> [[Float]] -> [Float]
getPredictions summaries testData = do
  let predictions = [predict summaries x | x <- testData]
  predictions


-- Find the accuracy of the predictions. 
getAccuracy :: [Float] -> [[Float]]-> Float
getAccuracy predictions test = 
    let 
        correct = foldr (\(p, t)-> \i-> (if p == (last t) then (i+1) else i) ) 0 (zip predictions test)
    in (fromIntegral(correct)/ fromIntegral(length test))*100