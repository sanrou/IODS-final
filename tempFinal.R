# Temp for the final assignment.

# Initialize required libraries
library(dplyr)

# Read learning2014.csv
learning2014 <- read.csv("learning2014.csv",row.names = 1)

# Check how many did not take the exam. Answer is 17.
sum(learning2014$points == 0)

# Split the learning2014 to exam takers and non-takers.
examY <- filter(learning2014, points > 0)
examN <- filter(learning2014, points == 0)

# Lets hope that the observations are not sorted in some way. Use the first 75 % as the training set and the latter 25 % as the testing set.
yTrainN <- round(nrow(examY)*0.75)
nTrainN <- round(nrow(examN)*0.75)

trainSet <- rbind(examY[1:yTrainN ,], examN[1:nTrainN ,])
testSet  <- rbind(examY[-(1:yTrainN) ,], examN[-(1:nTrainN) ,])


## Select variables for the hand picked variables for the hopefully better performing model.
# First list the columns
colnames(learning2014)

# Hand pick promising variables
subVariables <- c("su_lp", "st_os", "st_tm", "stat_conf", "value", "interest", "math_conf", "affect")

# Change points to be boolean of taking exam
trainSet$takeExam <- trainSet$points > 0

# Remove the points variable from trainSet, so that the full model is not able to use it as a variable.
trainSet <- dplyr::select(trainSet, -points)

# Split a subset of variables for the hand picked model.
handSet  <- trainSet[c(subVariables, 'takeExam')]


## I'll use a linear model. Logistic regression model (glm).
fullModel <- glm(takeExam ~ ., data = trainSet, family = "binomial")
handModel <- glm(takeExam ~ ., data = handSet,  family = "binomial")



# Predict the probability of taking exam
probabilitiesFull <- predict(fullModel, type = "response")
probabilitiesHand <- predict(handModel, type = "response")

# Add the predicted probabilities to the dataframes
trainSet <- mutate(trainSet, probability = probabilitiesFull)
handSet  <- mutate(handSet,  probability = probabilitiesHand)

# Use the probabilities to make a prediction of taking exam
trainSet <- mutate(trainSet, prediction = probability > 0.5)
handSet  <- mutate(handSet,  prediction = probability > 0.5)

# tabulate the target variable versus the predictions
table(takeExam = trainSet$takeExam, prediction = trainSet$prediction)
table(takeExam = handSet$takeExam,  prediction = handSet$prediction)

# Well that didn't go too well. The full set performed better, but poorly (2 true negatives and 1 false negative, 11 false positives). The hand picked model had 1 false negative and 13 false positives, and 0 true negatives.

# Search for a threshold in prediction which will give 13 predictions. So just sort the probabilities and find the 13th and 14th values, take average. 
fullProbabilitySorted <- sort(trainSet$probability)
thresholdFull <- mean(fullProbabilitySorted[c(13,14)])

handProbabilitySorted <- sort(handSet$probability)
thresholdHand <- mean(handProbabilitySorted[c(13,14)])

# The new thresholds are 0.6919 (full) and 0.7941 (hand picked).

# Stir the pot and do the predictions again.
trainSet <- mutate(trainSet, prediction = probability > thresholdFull)
handSet  <- mutate(handSet,  prediction = probability > thresholdHand)

# tabulate the target variable versus the predictions
table(takeExam = trainSet$takeExam, prediction = trainSet$prediction)
table(takeExam = handSet$takeExam,  prediction = handSet$prediction)

# Well, that seems a bit better, but not much. Now there are many more false negatives but at least there are more true negatives. 


## Time to look at the testing data for actual performance.
predFull <- predict(fullModel, newdata = testSet)
predHand <- predict(handModel, newdata = testSet)

table(correct = points>0, predicted = testSet$)

