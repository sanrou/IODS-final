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
testSet$takeExam <- testSet$points > 0

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
# Fit the model and compute accuracy.
fitFull <- predict(fullModel, newdata = testSet)
fitFull <- ifelse(fitFull > thresholdFull, 1, 0)
classificationErrorFull <- mean(fitFull != testSet$takeExam)
print(paste('Accuracy of the full model', 1 - classificationErrorFull))

fitHand <- predict(handModel, newdata = testSet)
fitHand <- ifelse(fitHand > thresholdHand, 1, 0)
classificationErrorFull <- mean(fitHand != testSet$takeExam)
print(paste('Accuracy of the hand picked model', 1 - classificationErrorFull))

# The accuracies seem pretty decent if one didn't know better. Guessing that all will take the exam would have a better accuracy (1 - nDidNotTakeExam/All = 1 - 4/46 = 0.91). Using ROC to compute the area under curve might be a better idea. It is a commonly used performance measurement for binary classifier performance.

library(ROCR)

# ROC full model
p <- predict(fullModel, newdata=testSet, type="response")
pr <- prediction(p, testSet$takeExam)
# TPR = sensitivity (true positive rate), FPR=specificity (false positive rate)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# The plot is pretty jagged and close to the diagonal of the plot (which means chance level). There are as many steps as there are those that did not take the exam (very few, thus jagged ROC graph).

# AUC full model
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(paste('AUC of the full model', auc))

# ROC hand picked model
pH <- predict(handModel, newdata=testSet, type="response")
prH <- prediction(pH, testSet$takeExam)
# TPR = sensitivity (true positive rate), FPR=specificity (false positive rate)
prfH <- performance(prH, measure = "tpr", x.measure = "fpr")
plot(prfH)

# A bit different from the full model. Still poor performance.

aucH <- performance(prH, measure = "auc")
aucH <- aucH@y.values[[1]]
print(paste('AUC of the hand picked model', aucH))
aucH

# Bleugh. Both models' predictive power suck. The full model might be a bit better (AUC 0.55 vs 0.51) but they are very close. Thus my own prediction of the the models performing about the same holds true. Too bad that they perform rather as poorly so it's hard to say if there is actually any sense in the models.

# Check the summaries of the models out of curiosity.
summary(fullModel)

# There is only one significant variable, confidence in math. 

summary(handModel)

# Same thing with the hand picked variables. I find it a bit peculiar that confidence in statistics is not even close significant. I would have assumed the variables to correlate strongly. In the hand picked model the intercept becomes highly significant instead of merely marginally significant. 