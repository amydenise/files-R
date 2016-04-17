library(neuralnet)
library(nnet)
library(car)
library(caret)
library(ISLR)

plot(Grad.Rate~.,data=College)

#Standardize the variables
Colleges <- College[,-1]
Colleges = as.data.frame(scale(Colleges))
Colleges$Private <- as.factor(College[,1])

# Split into training and test sets 
set.seed(1)
train = sample(1:dim(Colleges)[1],as.integer((2/3)*dim(Colleges)[1]))
Colleges.train = Colleges[train,]
Colleges.test = Colleges[-train,]
gradrates.test = Colleges.test$Grad.Rate

# Fit nn with no hidden layer (equivalent to linear regression)
mod1 = nnet(Grad.Rate ~ ., College, size=0, skip=TRUE, linout=TRUE)
summary(mod1)

# Confirm that they are the same
mod2 <- lm(Grad.Rate ~ ., College)

# One hidden node
mod3 <- nnet(Grad.Rate ~ ., College, size=1, linout=TRUE)
# SSE changes wildly on different iterations, scale data to fix this
Colleges <- College.train[,-1]
Colleges = as.data.frame(scale(Colleges))
Colleges$Private <- as.factor(College.train[,1])

mod4 = nnet(Grad.Rate~., Colleges, size=1, linout=TRUE) 
summary(mod4)

#import the plot function from Github
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
par(mfrow=c(1,1))
plot.nnet(mod4)

# Try with test data
Colleges.test <- College.test[,-1]
Colleges.test = as.data.frame(scale(Colleges.test))
Colleges.test$Private <- as.factor(College.test[,1])

pred = predict(mod4, Colleges.test)
gradrates.test <-scale(gradrate.test)
mean((pred-gradrates.test)^2) # MSE = .54
plot(pred,gradrates.test)  #for 1 node

#2 hidden nodes
mod5 = nnet(Grad.Rate~., Colleges, size=2, linout=TRUE) 
summary(mod5)

pred2 = predict(mod5, Colleges.test)
mean((pred2-gradrates.test)^2) # MSE = .615
plot(pred,gradrates.test)  #for 2 notes

#10 hidden nodes
mod6 = nnet(Grad.Rate~., Colleges, size=10, linout=TRUE) 
summary(mod6)

pred3 = predict(mod6, Colleges.test)
mean((pred3-gradrates.test)^2) # MSE = 1.01
plot(pred3,gradrates.test)  #for 10 nodes

#####CV for decay and size
my.grid = expand.grid(.decay =  seq(0.0001, 2, length.out=10), .size = seq(1,10))
nn.fit = train(Grad.Rate~., data = Colleges, 
               method = "nnet",metric="RMSE",maxit = 1000, tuneGrid = my.grid)  
plot(nn.fit)
nn.fit$bestTune
summary(nn.fit)
#plot.nnet(nn.fit)
plot(nn.fit$finalModel)


