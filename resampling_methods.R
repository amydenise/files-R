##################################################
library(ISLR)
library(boot)
attach(Default)
set.seed(1)

#exploring the data
head(Default)

#Fit a logistic regression model that uses income and balnce to predict default
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)

#Using the validation set approach, estimate the test error of this model
#i. split dataset
dim(Default)
train=sample(10000,5000)

#fit LM using training
glm.fit = glm(default ~ income + balance, data = Default, family = binomial, subset = train) 

#Obtain a prediction of default status for each individual in the validation set
#by computing the posterior probability of default for that individual, and classify that individual
#to the default category if the posterior probability >.5
glm.pred = factor(c(rep("No", 5000)), levels = c("No", "Yes"))
glm.probs <- predict(glm.fit, newdata = Default[-train,], type = "response")
glm.pred[glm.probs > 0.5] <- "Yes"  

#Compute the validation set error, which is the fraction of the observations
#in the validation set that are misclassified
mean(glm.pred != Default[-train, ]$default)

#Repeat the process in (b) three times, using three different splits
train=sample(10000,5000)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial, subset = train) 
glm.pred = factor(c(rep("No", 5000)), levels = c("No", "Yes"))
glm.probs <- predict(glm.fit, newdata = Default[-train,], type = "response")
glm.pred[glm.probs > 0.5] <- "Yes"  
mean(glm.pred != Default[-train, ]$default)
#0.0286, 0.0236, .028: close but different

#d
train=sample(10000,5000)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, subset = train) 
glm.pred = factor(c(rep("No", 5000)), levels = c("No", "Yes"))
glm.probs <- predict(glm.fit, newdata = Default[-train,], type = "response")
glm.pred[glm.probs > 0.5] <- "Yes"  
mean(glm.pred != Default[-train, ]$default)
#TER = .0246: not much change

##################################################
set.seed(1)

#determine estimated SEs from glm model
glm.fit = glm(default ~ income + balance, data = Default, family = binomial) 
summary(glm.fit)

#write a function boot.fn(data, index) that outputs coefficient estimates
#for income and balance
boot.fn = function(data, index){
  glm(default ~ income + balance, data = Default[index,], family = binomial)$coefficients  
}

#Use boot() and boot.fn() to estimate the SE of the income and balance coefficients
boot(Default,boot.fn,1000)

#comment on the different estimates of SE 

#Bootstrap Statistics :
#  original        bias           std. error
#t1* -1.154047e+01 -8.008379e-03 4.239273e-01
#t2*  2.080898e-05  5.870933e-08 4.582525e-06
#t3*  5.647103e-03  2.299970e-06 2.267955e-04

# GLM Coefficients:
#               Estimate    Std. Error 
#(Intercept)  -1.154e+01     4.348e-01     #~same
# income       2.081e-05     4.985e-06     #greater
# balance      5.647e-03     2.274e-04     #~same

##################################################


#generate a simulated data set as follows
set.seed(1)
y = rnorm(100)
x=rnorm(100)
y = x-2*x^2+rnorm(100)
data <- data.frame(x,y)
#n = 100 p = 2   y = X - X^2 + E

#Create a scatterplot of x against y
plot(x, y)

#Compute the LOOCV errors that result from fitting the following 4 models using LS:
cv.error <- rep(1:4)
for (i in 1:4){
  glm.fit=glm(y~poly(x,i),data=data)  
  cv.error[i]=cv.glm(data,glm.fit)$delta[1]   
}
cv.error  #5.890979 1.086596 1.102585 1.114772

#Repeat c using another random seed, do your estimates change?
set.seed(28)
cv.error <- rep(1:4)
for (i in 1:4){
  glm.fit=glm(y~poly(x,i),data=data)  
  cv.error[i]=cv.glm(data,glm.fit)$delta[1]   
}
cv.error  #5.890979 1.086596 1.102585 1.114772


for (i in 1:4){
  glm.fit=glm(y~poly(x,i),data=data)  
  print(summary(glm.fit))
}

#Adding the first and second order terms to the model decrease error, according to the
#CV results.  This is consistent with what is seen using LS estimation, since these
#same terms are deemed "significant"

##################################################
library(MASS)
attach(Boston)

#provide an estimate for the population mean of medv
mu_hat <- mean(medv)
mu_hat

#provide an estimate of the se of mu_hat
SE_hat <- sd(medv)/sqrt(506)
SE_hat

#now estimate se using bootstrap
boot.fn = function(data, index) return(mean(data[index]))
boot1 <- boot(medv,boot.fn,1000)
#Bootstrap Statistics :
#original      bias    std. error
#t1* 22.53281 -0.01018004   0.4185706


#95% CI using bootstrap compared to t.test
t.test(Boston$medv)
#95 percent confidence interval:
#21.72953 23.33608

lconf <- mu_hat - 2* 0.4185706
uconf <- mu_hat + 2* 0.4185706
lconf
uconf
#21.69567 23.36995 #bootstrap higher SE (3rd sig digit)

#provide an estimate of the median
med_hat <- median(medv)
med_hat

#Estimate the SD of median using bootstrap
boot.fn = function(data, index) return(median(data[index]))
boot2 <- boot(medv,boot.fn,1000)

#Bootstrap Statistics :
#original  bias         std. error
#t1*     21.2 -0.0451   0.3848169

#g -- estimate the 10th percerntile
medv_hat_10 = quantile(medv, c(0.1))
medv_hat_10

#h -- estimate (bootstrap) the SE of the estimate of the 10th quantile
boot.fn = function(data, index) return(quantile(data[index], c(.1)))
boot3 <- boot(medv,boot.fn,1000)

#Bootstrap Statistics :
#  original  bias    std. error
#t1* 12.75 0.03245   0.4774489
