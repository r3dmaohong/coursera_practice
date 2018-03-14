library(caret)
library(kernlab)
data(spam)

train_indx = createDataPartition(y = spam$type, p = 0.75, list = FALSE)
train = spam[train_indx, ]
test = spam[-train_indx, ]

hist(train$capitalAve, main = '', 
     xlab = 'ave. Capital run length')
mean(train$capitalAve)
sd(train$capitalAve) # too big

trainCapAve = train$capitalAve
trainCapAveS = (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve = test$capitalAve
testCapAveS = (testCapAve - mean(testCapAve))/sd(testCapAve)
mean(testCapAveS)
sd(testCapAveS)

preObj = preProcess(train[, names(train)!='type'], method = c('center', 'scale'))
trainCapAveS = predict(preObj, train[, names(train)!='type'])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS = predict(preObj, test[, names(test)!='type'])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(1234)
glm_model = train(type~., data = train, preProcess=c('center', 'scale'), method = 'glm')
glm_model


preObj = preProcess(train[, -58], method = 'BoxCox') # normal distribution
trainCapAveS = predict(preObj, train[,-58])$capitalAve
par(mfrow = c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

## na values
train$capAve = train$capitalAve
selectNA = rbinom(nrow(train), size = 1, prob = 0.05)==1
train$capAve[selectNA] = NA

preObj = preProcess(train[,-58], method = 'knnImpute')
capAve = predict(preObj, train[,-58])$capAve

capAveTruth = train$capitalAve
capAveTruth = (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])
