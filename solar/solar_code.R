
solar <- read.table('solar.csv', head = T, sep= ',')

attach(solar)

#########################
#2.0 Exploratory Plots
#########################

table(storeClosing)

table(nClosed)


####################
#3.1 Large grade
################s

####
#Peak Plots
####
index<-1:(dim(solar)[1])
solar.close <- solar[solar$storeClosing==1,]
solar.open <- solar[solar$storeClosing==0,]
close.index<-index[solar$storeClosing==1]
open.index<-index[solar$storeClosing==0]

par(mfrow=c(2,1))
plot(open.index,solar.open$N, col='red', main = "North against Time", 
     ylab='North', pch = 16,
     xlab = "Time Index")
points(close.index,solar.close$N, col='green')

plot(open.index,solar.open$S, col='red', main = "South against Time", 
     sub="Note: Green points highlight 10-day intervals with >= 1 closed store(s)", ylab='South', pch = 16,
     xlab = "Time Index")
points(close.index,solar.close$S, col='green')


####
#3.1 Logistic Regression
####

mod1 <- glm(storeClosing ~ N, family = "binomial")
summary(mod1)

mod2 <- glm(storeClosing ~ S, family = "binomial")
summary(mod2)

mod3 <- glm(storeClosing ~ abs(N), family = "binomial")
summary(mod3)

mod4 <- glm(storeClosing ~ abs(S), family = "binomial")
summary(mod4)

mod5 <- glm(storeClosing ~ abs(N-S), family = "binomial")
summary(mod5)


####
#3.2 Peak Plots, Medium grade
####

## I choose time indices from 1200 to 1300, you may change it to any range you want
par(mfrow=c(2,1))
low.b<-1200
upper.b<-1300
index.part<- low.b:upper.b

## north
plot(index.part, solar$N[index.part], main = "North against Time", ylab="North",
     xlab = "Time Index")
closeindex.part<- which(solar$storeClosing[index.part] == 1) + low.b - 1
points(closeindex.part, solar$N[closeindex.part], col=2, pch=3)


## south
plot(index.part, solar$S[index.part], main = "South against Time", ylab="South",
     xlab = "Time Index", sub = "Note: Red points highlight 10-day intervals with >= 1 closed store(s)")
closeindex.part<- which(solar$storeClosing[index.part] == 1) + low.b - 1
points(closeindex.part, solar$S[closeindex.part], col=2, pch=3)


####
#3.2.1 Creating a new factor covariate
####

#North

##Identifying local max/min
x<-rep(0, 1375)
for(i in 6:1370){
  if(solar$N[i]>=solar$N[i-1] && solar$N[i]>=solar$N[i-2]
     && solar$N[i]>=solar$N[i-3] && solar$N[i]>=solar$N[i-4]
     && solar$N[i]>=solar$N[i-5] && solar$N[i]>=solar$N[i+1] && solar$N[i]>=solar$N[i+2]
     && solar$N[i]>=solar$N[i+3] && solar$N[i]>=solar$N[i+4]
     && solar$N[i]>=solar$N[i+5] )
    x[i]=1
  else if (solar$N[i]<=solar$N[i-1] && solar$N[i]<=solar$N[i-2]
           && solar$N[i]<=solar$N[i-3] && solar$N[i]<=solar$N[i-4]
           && solar$N[i]<=solar$N[i-5] && solar$N[i]<=solar$N[i+1]
           && solar$N[i]<=solar$N[i+2] && solar$N[i]<=solar$N[i+3]
           && solar$N[i]<=solar$N[i+4] && solar$N[i]<=solar$N[i+5] )
    x[i]=2
} 

## code the time indices that are near the local min/max, a and b are tuning parameters
a <- 10 ## tune for the neighborhood on the x axis (time)
b <- 10 ## tune for the neighborhood on the y axis (north)
index.max <- which(x==1)
index.min <- which(x==2)

## new covariate indictor.min.max
indictor.min.max<- rep(0, 1375)

## neighborhood wrt x axis and y axis, find corresponding time indices, 
## denoted as 1 if it's near a local max, 2 if it's near a local min, ow 0
for (i in index.max){
  max.value <- solar$N[i]
  index.near.max <- which( (solar$N >= (max.value - b)) & (solar$N <= max.value + b) )
  index.near.max <- index.near.max[(index.near.max >= max(c(i-a, 1))) & 
                                     (index.near.max <= min(c(i+a, 1375)))] 
  indictor.min.max[index.near.max] <- 1
}
for (i in index.min){
  min.value <- solar$N[i]
  index.near.min <- which( (solar$N >= min.value - b) & (solar$N <= (min.value + b)) )
  index.near.min <- index.near.min[(index.near.min >= max(c(i-a, 1))) & 
                                     (index.near.min <= min(c(i+a, 1375)))] 
  indictor.min.max[index.near.min] <- 2
}

## plots to see effect of selection
## Again, I choose the time range from 1200 to 1300
par(mfrow = c(1,1))
low.b<-1200
upper.b<-1300

index.part<- low.b:upper.b
plot(index.part, solar$N[index.part], main="Near Peak and Valley Points", 
     xlab="Time index", ylab="North", )
closeindex.part<- which(solar$storeClosing[index.part] == 1) + low.b - 1

index.max.part<- which(indictor.min.max[index.part] == 1) + low.b - 1
index.min.part <- which(indictor.min.max[index.part] == 2) + low.b -1
points(closeindex.part, solar$N[closeindex.part], col=2, pch=3) 
## near min points
points(index.min.part, solar$N[index.min.part], col=3, pch=2)
## near max points
points(index.max.part, solar$N[index.max.part], col=3, pch=2)
legend(1270, -50, c("Have Store Closing", "Near Peak or Valley"), pch=c(3,2), col=c(2,3), cex=0.7)


## new factor covariate based on the value of North

solar$min.max.effect_N <- as.factor(ifelse(indictor.min.max == 0, 0, 1) )




#South

##Identifying local max/min
x<-rep(0, 1375)

for(i in 6:1370){
  if(solar$S[i]>=solar$S[i-1] && solar$S[i]>=solar$S[i-2]
     && solar$S[i]>=solar$S[i-3] && solar$S[i]>=solar$S[i-4]
     && solar$S[i]>=solar$S[i-5] && solar$S[i]>=solar$S[i+1] && solar$S[i]>=solar$S[i+2]
     && solar$S[i]>=solar$S[i+3] && solar$S[i]>=solar$S[i+4]
     && solar$S[i]>=solar$S[i+5] )
    x[i]=1
  else if (solar$S[i]<=solar$S[i-1] && solar$S[i]<=solar$S[i-2]
           && solar$S[i]<=solar$S[i-3] && solar$S[i]<=solar$S[i-4]
           && solar$S[i]<=solar$S[i-5] && solar$S[i]<=solar$S[i+1]
           && solar$S[i]<=solar$S[i+2] && solar$S[i]<=solar$S[i+3]
           && solar$S[i]<=solar$S[i+4] && solar$S[i]<=solar$S[i+5] )
    x[i]=2
}

## code the time indices that are near the local min/max, a and b are tuning parameters
a <- 10 ## tune for the neighborhood on the x axis (time)
b <- 10 ## tune for the neighborhood on the y axis (north)
index.max <- which(x==1)
index.min <- which(x==2)

## new covariate indictor.min.max
indictor.min.max<- rep(0, 1375)

## neighborhood wrt x axis and y axis, find corresponding time indices, 
## denoted as 1 if it's near a local max, 2 if it's near a local min, ow 0
for (i in index.max){
  max.value <- solar$S[i]
  index.near.max <- which( (solar$S >= (max.value - b)) & (solar$S <= max.value + b) )
  index.near.max <- index.near.max[(index.near.max >= max(c(i-a, 1))) & 
                                     (index.near.max <= min(c(i+a, 1375)))] 
  indictor.min.max[index.near.max] <- 1
}
for (i in index.min){
  min.value <- solar$S[i]
  index.near.min <- which( (solar$S >= min.value - b) & (solar$S <= (min.value + b)) )
  index.near.min <- index.near.min[(index.near.min >= max(c(i-a, 1))) & 
                                     (index.near.min <= min(c(i+a, 1375)))] 
  indictor.min.max[index.near.min] <- 2
}

## plots to see effect of selection
## Again, I choose the time range from 1200 to 1300
low.b<-1200
upper.b<-1300

index.part<- low.b:upper.b
plot(index.part, solar$S[index.part], main="Near Peak and Valley Points", 
     xlab="Time index", ylab="Sorth", )
closeindex.part<- which(solar$storeClosing[index.part] == 1) + low.b - 1

index.max.part<- which(indictor.min.max[index.part] == 1) + low.b - 1
index.min.part <- which(indictor.min.max[index.part] == 2) + low.b -1
points(closeindex.part, solar$S[closeindex.part], col=2, pch=3) 
## near min points
points(index.min.part, solar$S[index.min.part], col=3, pch=2)
## near max points
points(index.max.part, solar$S[index.max.part], col=3, pch=2)
legend(1275, 90, c("Have Store Closing", "Near Peak or Valley"), pch=c(3,2), col=c(2,3), cex=0.7)

# new factor covariate based on the value of South.
solar$min.max.effect_S <- as.factor(ifelse(indictor.min.max == 0, 0, 1) )




####
#3.2.2 Logistic Regressions
####

Peak_Valley_North <- solar$min.max.effect_N
Peak_Valley_South <- solar$min.max.effect_S

#Predictor: Indicator of Peak/Valley for North Variable
mod1<-glm(formula = storeClosing ~ Peak_Valley_North, family = binomial
          , data= solar)
summary(mod1)

#Predictor: Indicator of Peak/Valley for South Variable
mod2<-glm(formula = storeClosing ~ Peak_Valley_South, family = binomial
          , data= solar)
summary(mod2)


