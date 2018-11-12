
#Ques.1. Use the below given data set

View(weight_lifting_exercises)
str(weight_lifting_exercises)
weight_lifting_exercises<-data.frame(weight_lifting_exercises[,-c(11:35,49:58,68:82,86:100,102:111,124:138,140:149)])

str(weight_lifting_exercises)
summary(weight_lifting_exercises)

weightTrain<-weight_lifting_exercises[1:2012,]
weightTest<-weight_lifting_exercises[2013:4024,]
summary(weightTrain)
names(weightTrain)


#Ques.2. Perform the below given activities:

# a. Create classification model using different random forest.
install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.weight_lifting_exercises <- randomForest(classe~.,weight_lifting_exercises,
                                         subset = weightTrain, mtry = 3,importance = TRUE)
dim(bag.weight_lifting_exercises)

#e plot importance of variables
importance(bag.weight_lifting_exercises)

varImpPlot(bag.weight_lifting_exercises,col = 'blue',pch = 10, cex = 1.25)

bag.weight_lifting_exercises

# b. Verify model goodness of fit.
#........for pred.....

test.pred.bag<-predict(bag.weight_lifting_exercises, newdata = weight_lifting_exercises[-weightTrain, ],type = 'class')
table(test.pred.rf,weightTest)

# c. Apply all the model validation techniques.

set.seed(3)
install.packages('tree')
library(tree)
tree.weight_lifting_exercises1<-tree(classe~. , weight_lifting_exercises, subset = weightTrain)
cv.weight_lifting_exercises<-cv.tree(tree.weight_lifting_exercises1,FUN = prune.misclass)  #cv->cross validation
names(cv.weightlifting_exercises)
cv.weightlifting_exercises

par(mfrow = c(1,2))
plot(cv.weight$size,cv.weight$dev,type = 'b',col = 'red')

prune.weight<-prune.misclass(tree,best = 9)
plot(prune.weight)
text(prune.weight,pretty = 0)

weightTrain$cvtd_timestamp<-as.integer(weightTrain$cvtd_timestamp)
weightTrain$new_window<-as.integer(weightTrain$new_window)
tree.pred1<-predict(prune.weight,weightTrain,type = 'class')
table(tree.pred1,weightTest)


#.........adaboost..........

install.packages(adabag)
library(adabag)
set.seed(300)
weight_lifting_exercises$classe<-as.character(weight_lifting_exercises$classe)
weight_adaboost<-boosting(classe~., data = weight_lifting_exercises)

p.weight_adaboost<-predict(weight_adaboost,weight_lifting_exercises)
head(p.weight_adaboost)
head(p.weight_adaboost$class)
p.weight_adaboost$confusion
set.seed(300)
car_adaboost_cv<-boosting.cv(classe,data = weight_lifting_exercises)
car_adaboost_cv$confusion
















