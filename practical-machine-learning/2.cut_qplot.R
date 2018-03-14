library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
summary(Wage)

train_indx = createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
train = Wage[train_indx,]
test = Wage[-train_indx,]
dim(train)
dim(test)

featurePlot(x = train[, c('age', 'education', 'jobclass')],
            y = train$wage, plot = 'pairs')

qplot(age, wage, data = train)
qplot(age, wage, color = jobclass, data = train)

qplot(age, wage, color = education, data = train) + 
  geom_smooth(method = 'lm', formula = y~x)


# cut continous value to discrete value 
cutWage = Hmisc::cut2(train$wage, g = 3)
table(cutWage)

( p1 = qplot(cutWage, age, data = train, fill = cutWage, geom = 'boxplot') )
qplot(cutWage, age, data = train, fill = cutWage, geom = 'jitter')
( p2 = qplot(cutWage, age, data = train, fill = cutWage, geom = c('boxplot', 'jitter')) )

gridExtra::grid.arrange(p1, p2, ncol = 2)

#
( t1 = table(cutWage, train$jobclass))
prop.table(t1, margin = 1) # col or row base

qplot(wage, color = education, data = train, geom = 'density')
