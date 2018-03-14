# pca

library(caret)
library(kernlab)
data(spam)

train_indx = createDataPartition(y=spam$type, 
                                 p=0.75,
                                 list=FALSE)
train = spam[train_indx,]
test = spam[-train_indx,]

M = abs(cor(train[,-58]))
diag(M) <- 0
which(M>0.8, arr.ind = TRUE)
names(spam)[c(34, 40)]
plot(spam[,34], spam[,40])

smallSpam = spam[, c(34, 40)]
(prComp = prcomp(smallSpam))
plot(prComp$x[,1], prComp$x[,2])
prComp$rotation
head( prComp$x )

## 
typeColor = ((spam$type=='spam')*1+1)
prComp = prcomp(spam[, -58])
dim(prComp$x)
plot(prComp$x[,1], prComp$x[,2], col=typeColor,
     xlab='PC1', ylab='PC2')

prComp = prcomp(log10(spam[, -58]+1))
plot(prComp$x[,1], prComp$x[,2], col=typeColor,
     xlab='PC1', ylab='PC2')


##
##
preProc = preProcess(log10(train[,-58]+1), method='pca',
                     pcaComp = 2)
trainPC = predict(preProc, log10(train[,-58]+1))
dim(trainPC)
plot(trainPC[,1], trainPC[,2], col=typeColor)

library(dplyr)
trainPC = cbind(trainPC, train$type) %>% `colnames<-`(c('PC1', 'PC2', 'type'))
glm_model = train(type ~ ., method='glm',
                  data=trainPC)

testPC = predict(preProc, log10(test[,-58]+1))
confusionMatrix(test$type, predict(glm_model, testPC))


##
glm_model = train(type~., method='glm', preProcess='pca', data=train)
confusionMatrix(test$type, predict(glm_model, test))

