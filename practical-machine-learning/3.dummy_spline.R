library(kernlab)
data(spam)

spam$capitalAveSq = spam$capitalAve^2

##
##
##

library(ISLR)
library(caret)
data("Wage")

train_indx = createDataPartition(y = Wage$wage,
                                 p=0.7,
                                 list=FALSE)
train = Wage[train_indx, ]
test = Wage[-train_indx, ]

table(train$jobclass)
dummies = dummyVars(wage~jobclass, data = train)
head(predict(dummies, newdata=train))

# removing zero covariates
( nsv = nearZeroVar(train, saveMetrics = TRUE) )
( nsv = nearZeroVar(train, saveMetrics = FALSE) )
table(train[, nsv])

##
##
##
library(splines)
( bsBasis = bs(train$age, df=3) )

lm1 = lm(wage~bsBasis, data=train)
par(mfrow = c(1,1))
plot(train$age, train$wage, pch=4, cex=0.5)
points(train$age, predict(lm1, newdata=train), col='red', pch=19, cex=0.75)
# on test set
predict(bsBasis, age = test$age)
