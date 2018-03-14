library(caret)
library(kernlab)

data(spam)
train_index = createDataPartition(y = spam$type, p = 0.75, list = FALSE)

train = spam[train_index, ]
test  = spam[-train_index, ]
dim(train)
dim(test)

head(train)

## model
set.seed(1)
# names(getModelInfo())
glm_model = train(type ~., data = train, method = 'glm')
glm_model
glm_model$finalModel

( pred = predict(glm_model, newdata = test) )

confusionMatrix(data = pred, reference = test$type)

# k-folds
set.seed(1)
#train
folds = createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)

folds[[1]]
folds[[10]]

# test
folds = createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)
sapply(folds, length)

# resampling
folds = createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds, length)

folds[[1]][1:10] # replace

# time slices
tme = 1:1000
folds = createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
folds$train[[1]] #20
folds$train[[2]]

folds$test[[1]] #10

