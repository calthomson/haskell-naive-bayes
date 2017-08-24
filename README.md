# HaskellNaiveBayes

Haskell program by Authors: Callum Campbell (i2t8, 12881124), Gulipek Candan (d4c9, 53906137), & Caledonia Thomson (b9b9, 17711136)

**What is the problem?**
	Machine Learning algorithms are one of the main sub-categories for AI and there are many different algorithms of varying levels of difficulty. We wanted to implement the Naive Bayes Algorithm in Haskell to see how suitable this language is for building a machine learning algorithm. It is not just in programming but in other areas of life that machine learning can be useful, such as providing medical diagnostic services. Programs that make the diagnostic process faster for doctors could provide better and faster health care for the public. In our project, we implement Naive Bayes to diagnose female diabetic patients older than 21. We found a dataset that has 8 different attributes about each patient, the 9th being the diagnosis (0 for no diabetes, 1 for diabetes). 
We used this patient data for training and testing our algorithm.
We modelled our implementation on this Python Naive Bayes implementation.
Our Naive Bayes assumes each attribute’s probability to belong to an outcome (0 or 1 in this case) is independent from all other attributes. Thus, for each attribute, the algorithm calculates the conditional probability of the attribute given the outcome, and then multiplies the conditional probabilities to find the probability of a data point having that particular outcome (0 or 1). 
	At last, the prediction part of the algorithm calculates the probabilities of each attribute for a given Patient data, chooses the highest probability among the attributes and then uses that probability to make a prediction. It is a simple calculation with assumptions that might not always be true, but given the simplicity the prediction accuracy is high. 

**What is the something extra?**
	Given the scope of the project and our idea of implementing an ML algorithm, our first extra is to actually implement the algorithm without errors and have it working. At the end, we managed to fully implement the gaussian distribution, which is what the algorithm does the probability calculation with, yet we had difficulties in implementing the random distribution of the data points into test and training data. Moreover, we used casava package to parse the csv file, instead of having it as a list of lists written by us. It added something extra, since the parsing result was of type vector and it was more difficult to work with vectors instead of a list of lists. 

**What did we learn from doing this?**
Math functions are much easier to implement in Haskell than in Prolog
The way Haskell functions have to return tuples is frustrating to work with. Because there are no side effects, if you want to change more than one thing, both must be returned explicitly. 
Functions as first order objects is very useful. For example we stored the Gaussian probability density functions for each individual value, and then applied those functions to the appropriate mean and standard deviations. This is a lot cleaner than iterating through multiple lists simultaneously. 
Compared to the Python code (http://machinelearningmastery.com/naive-bayes-classifier-scratch-python/) we have implemented the same algorithm in a shorter and more efficient way with Haskell. 
Because Haskell is strongly typed, defining a function correctly was difficult on a project that was relatively big in scale, especially compared to the functions we wrote in assignments. 
The parser for csv returned type Vector and instead of working with lists we had to work with vectors, which was another challenge due to certain differences between type list and vector. 
It is easy to adapt this implementation to another problem and dataset by changing only a few features, making this an efficient way of writing machine learning algorithms. 
Haskell was at times confusing because of the packages that needed to be downloaded to implement a new library.


**Limitations**
Implementing a randomized selection for the test and training data was hard, since the raw data after parsing was of type vector. Thus, we are aware that the way we divided the data is just by ratio but not by randomization. 

