# Fitting Classification Trees

library(tree)
library(ISLR)
attach(Carseats)

#####################
#CLASSIFICATION TREES
#####################

#1.  preparing data
High=ifelse(Sales<=8,"No","Yes")   #making sales binary
Carseats=data.frame(Carseats,High)

#2.  fit class tree - predict High using all variables but Sale
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

#3. evaluate test error rate

##a)split data into training & testing
set.seed(2)
train=sample(1:nrow(Carseats), 200) #training obs indexes
Carseats.test=Carseats[-train,] #df with only testing
High.test=High[-train]  #lebels for testing data

##b)refit tree using only the training data
tree.carseats=tree(High~.-Sales,Carseats,subset=train)

##c)predict for testing observations using trained model 
tree.pred=predict(tree.carseats,Carseats.test,type="class")

##d)calculate the % of time we correctly predict
conf.matr<-table(tree.pred,High.test)
(conf.matr[1,1] + conf.matr[2,2])/sum(conf.matr)

#4.  Does pruning the tree lead to better results?

#a) CV to determine optimum level of complexity (alpha) <- "cost complexity tuning"
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
###Note: use FUN= to specify CER as criteria instead of default = deviance
names(cv.carseats)  #dev = CER.
cv.carseats

#b) plot the error rate as a function of size and k(alpha)
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

#c)  prune the tree to our best size
prune.carseats=prune.misclass(tree.carseats,best=9)
par(mfrow=c(1,1))
plot(prune.carseats)
text(prune.carseats,pretty=0)

#d)compute test error rate
tree.pred=predict(prune.carseats,Carseats.test,type="class")
conf.matr<-table(tree.pred,High.test)
(conf.matr[1,1] + conf.matr[2,2])/sum(conf.matr)

#########################
# Fitting Regression Trees
#########################
#1.  Preparing data
library(MASS)
set.seed(1)
attach(Boston)
hist(medv)
train = sample(1:nrow(Boston), nrow(Boston)/2)
Boston.test = Boston[-train]

#2. Fit regression tree on training data
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

#3.  Does pruning the tree improve performanc?
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')

#it does not, but if we wanted to prune we could with
#prune.boston=prune.tree(tree.boston,best=5)
#plot(prune.boston)
#text(prune.boston,pretty=0)
#

#4.  Use CV tree to predict on the training set & calculate Test error
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)  
#intepretation:
#MSE = 25.05 --> SE = 5.005
#model leads to test predictions that are within around $5,005
#of the true median home value for the suburb

###########
# Bagging 
###########

#1.Fit a bagged regression tree on training data
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

#2 Test error rate
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)   
#intepretation:
#MSE = 13.47349 --> SE = 3.671
#model leads to test predictions that are within around $3,671
#of the true median home value for the suburb

#Change the number of trees grown by the random forest (default = 500)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

#######################
# Random Forest Growing
#######################

#1.Grow a random forest with m = 6
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)  #11.48

importance(rf.boston)
varImpPlot(rf.boston)

###########
# Boosting
###########
library(gbm)
set.seed(1)

##a)  Fit boosted regression trees to the Boston data
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

##b) predict testing data & calculate MSE
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)  #11.71

#boost with lambda = .2 instead of default =.001
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)