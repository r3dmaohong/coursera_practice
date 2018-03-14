library(caret)
library(ElemStatLearn)
data(ozone, package='ElemStatLearn')

ozone = ozone[order(ozone$ozone),]
head(ozone)

predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag = bag(predictors, temperature, B=10,
              bagControl = bagControl(fit=ctreeBag$fit,
                                      predict=ctreeBag$pred,
                                      aggregate=ctreeBag$aggregate))

plot(ozone$ozone, temperature, col='lightgrey', pch=19)
# single regression tree
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch=19, col='red')
points(ozone$ozone, predict(treebag, predictors), pch=19, col='blue')

