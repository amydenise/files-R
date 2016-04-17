#Read in designs gerated in JMP
designs <- read.csv("~/Desktop/CExp/designs.csv")

#FUNCTION:
#calculates the correlation matrix for some X and corresponding thetas
Rcalc <-function(x1,x2,theta1,theta2){
  dim = length(x1)
  R <- matrix(nrow = dim, ncol = dim)
  for (i in 1:dim){
    for (j in 1:dim){
      R[i,j] = exp(-theta1*(x1[i]-x1[j])^2)*
        exp(-theta2*(x2[i]-x2[j])^2)
    }  
  }  
  return(R)
}

#FUNCTION:
#generates observations (Y's) from Gaussian process with corr = R
obsgen <-function(R){
  #Square root R
  Rsr <- chol(R)     #t(Rsr) %*% Rsr = R
  S <- t(Rsr)        #SX ~ N(O, R)
  
  #Generate random sample from iid Normal(0,1)
  samples <- rnorm(nrow(R),0,1)
  
  #Generate observations 
  obs <- S%*%samples
  return(obs)
}

#FUNCTION:
#Generates plot comparing theta estimates
plottheta <- function(lhtheta, maxenttheta, truetheta, theta){
  g_range <- range(0, lhtheta, maxenttheta, na.rm = TRUE)
  
  plot(lhtheta, type="o", col="blue", ylim=g_range, 
       axes=FALSE, ann=FALSE)
  
  axis(1, at=1:4, lab=c("10","20","50","100"))
  
  axis(2, las=1, at=4*0:g_range[2])
  
  box()
  
  lines(maxenttheta, type="o", pch=22, lty=2, col="red")
  
  if(theta == 1){
    title(main=bquote("Theta1" == .(truetheta)))
  }
  if(theta == 2){
    title(main=bquote("Theta2" == .(truetheta)))
  }
  
  title(xlab="Runs")
  title(ylab="Estimated Theta")
  
  legend(1, g_range[2], c("Latin Hypercube","Maximum Entropy"), cex=0.8, 
         col=c("blue","red"), pch=21:22, lty=1:2, bty = "n");
  
  abline(truetheta,0)
}

######################################
#Generate newx1, newx2
####################################
set.seed(13)
newx1 <- runif(25,-1,1)
set.seed(24)
newx2 <- runif(25, -1, 1)


###############################
#Analysis for theta1 = 3, theta2 = 3, n = 10
###############################

#
#True Y (theta1 = 3, theta2 = 3, n = 10)
#

x1lh <- designs$x1_10L[1:10]
x2lh <- designs$x2_10L[1:10]
x1me <- designs$x1_3_3_10[1:10]
x2me <- designs$x2_3_3_10[1:10]

x1 <- c(x1lh, x1me, newx1)
x2 <- c(x2lh, x2me, newx2)

#Calculate R
R <- Rcalc(x1 = x1, 
           x2 = x2,theta1 = 3, theta2 = 3)

#Square root R
Rsr <- chol(R)     #t(Rsr) %*% Rsr = R
S <- t(Rsr)        #SX ~ N(O, R)

#Generate random sample from iid Normal(0,1)
set.seed(1)
samples <- rnorm(nrow(R),0,1)

#Generate Y  from GP 
y <- S%*%samples

#Combine X1, X2, and Y into one data sort and export it as a csv
obs <-as.vector(y)
data <- cbind(x1, x2, y)
colnames(data, do.NULL = TRUE, prefix = "col")
colnames(data) <- c("X1", "X2", "Y")

write.csv(data[1:10,1:3], file = "/Users/amydenise/Desktop/CExp/33/33_10LHmod.csv",row.names = FALSE)
write.csv(data[11:20,1:3], file = "/Users/amydenise/Desktop/CExp/33/33_10MEmod.csv",row.names = FALSE)
write.csv(data, file = "/Users/amydenise/Desktop/CExp/33/33_10true.csv",row.names = FALSE)

#
#read in predictions
#
predLH3310 <- read.csv("~/Desktop/CExp/33/33_10LHpred.csv", header=FALSE)
predME3310 <- read.csv("~/Desktop/CExp/33/33_10MEpred.csv", header=FALSE)

#
#calculate mspe for LH 33 n = 10
#
mspe_LH3310 <- sum((predLH3310$V4[21:35] - predLH3310$V3[21:35])^2)/25
mspe_LH3310
#
#calculate mspe for LH 33 n = 10
#
mspe_ME3310 <- sum((predME3310$V4[21:35] - predME3310$V3[21:35])^2)/25
mspe_ME3310

###############################
#Analysis for theta1 = 3, theta2 = 3, n = 20
###############################

#
#True Y (theta1 = 3, theta2 = 3, n = 20)
#
n=20
x1lh <- designs$x1_20L[1:n]
x2lh <- designs$x2_20L[1:n]
x1me <- designs$x1_3_3_20[1:n]
x2me <- designs$x2_3_3_20[1:n]

x1 <- c(x1lh, x1me, newx1)
x2 <- c(x2lh, x2me, newx2)

#Calculate R
R <- Rcalc(x1 = x1, 
           x2 = x2,theta1 = 3, theta2 = 3)

#Square root R
Rsr <- chol(R)     #t(Rsr) %*% Rsr = R
S <- t(Rsr)        #SX ~ N(O, R)

#Generate random sample from iid Normal(0,1)
set.seed(2)
samples <- rnorm(nrow(R),0,1)

#Generate Y  from GP 
y <- S%*%samples

#Combine X1, X2, and Y into one data sort and export it as a csv
obs <-as.vector(y)
data <- cbind(x1, x2, y)
colnames(data, do.NULL = TRUE, prefix = "col")
colnames(data) <- c("X1", "X2", "Y")

write.csv(data[1:n,1:3], file = "/Users/amydenise/Desktop/CExp/33/33_20LHmod.csv",row.names = FALSE)
write.csv(data[(n+1):(2*n),1:3], file = "/Users/amydenise/Desktop/CExp/33/33_20MEmod.csv",row.names = FALSE)
write.csv(data, file = "/Users/amydenise/Desktop/CExp/33/33_20true.csv",row.names = FALSE)

#
#read in predictions
#
predLH3320 <- read.csv("~/Desktop/CExp/33/33_20LHpred.csv", header=FALSE)
predME3320 <- read.csv("~/Desktop/CExp/33/33_20MEpred.csv", header=FALSE)

#
#calculate mspe for LH 33 n = 20
#
mspe_LH3320 <- sum((predLH3320$V4[(n + 1):(n + 25)] - predLH3320$V3[(n + 1):(n + 25)])^2)/25
mspe_LH3320
#
#calculate mspe for ME 33 n = 20
#
mspe_ME3320 <- sum((predME3320$V4[(n + 1):(n + 25)] - predME3320$V3[(n + 1):(n + 25)])^2)/25
mspe_ME3320

###############################
#Analysis for theta1 = 3, theta2 = 3, n = 50
###############################

#
#True Y 
#
n=50
x1lh <- designs$x1_50L[1:n]
x2lh <- designs$x2_50L[1:n]
x1me <- designs$x1_3_3_50[1:n]
x2me <- designs$x2_3_3_50[1:n]

x1 <- c(x1lh, x1me, newx1)
x2 <- c(x2lh, x2me, newx2)

#Calculate R
R <- Rcalc(x1 = x1, 
           x2 = x2,theta1 = 3, theta2 = 3)

#Square root R
Rsr <- chol(R)     #t(Rsr) %*% Rsr = R
S <- t(Rsr)        #SX ~ N(O, R)

#Generate random sample from iid Normal(0,1)
set.seed(3)
samples <- rnorm(nrow(R),0,1)

#Generate Y  from GP 
y <- S%*%samples

#Combine X1, X2, and Y into one data sort and export it as a csv
obs <-as.vector(y)
data <- cbind(x1, x2, y)
colnames(data, do.NULL = TRUE, prefix = "col")
colnames(data) <- c("X1", "X2", "Y")

write.csv(data[1:n,1:3], file = "/Users/amydenise/Desktop/CExp/33/33_50LHmod.csv",row.names = FALSE)
write.csv(data[(n+1):(2*n),1:3], file = "/Users/amydenise/Desktop/CExp/33/33_50MEmod.csv",row.names = FALSE)
write.csv(data, file = "/Users/amydenise/Desktop/CExp/33/33_50true.csv",row.names = FALSE)

#
#read in predictions
#
predLH3350 <- read.csv("~/Desktop/CExp/33/33_50LHpred.csv", header=FALSE)
predME3350 <- read.csv("~/Desktop/CExp/33/33_50MEpred.csv", header=FALSE)

#
#calculate mspe for LH 33 n = 50
#
mspe_LH3350 <- sum((predLH3350$V4[(n + 1):(n + 25)] - predLH3350$V3[(n + 1):(n + 25)])^2)/25
mspe_LH3350
#
#calculate mspe for ME 33 n = 50
#
mspe_ME3350 <- sum((predME3350$V4[(n + 1):(n + 25)] - predME3350$V3[(n + 1):(n + 25)])^2)/25
mspe_ME3350

###############################
#Analysis for theta1 = 2, theta2 = 10, n = 10
###############################

#
#True Y 
#
n=10
x1lh <- designs$x1_10L[1:n]
x2lh <- designs$x2_10L[1:n]
x1me <- designs$x1_2_10_10[1:n]
x2me <- designs$x2_2_10_10[1:n]

x1 <- c(x1lh, x1me, newx1)
x2 <- c(x2lh, x2me, newx2)

#Calculate R
R <- Rcalc(x1 = x1, 
           x2 = x2,theta1 = 2, theta2 = 10)

#Square root R
Rsr <- chol(R)     #t(Rsr) %*% Rsr = R
S <- t(Rsr)        #SX ~ N(O, R)

#Generate random sample from iid Normal(0,1)
set.seed(5)
samples <- rnorm(nrow(R),0,1)

#Generate Y  from GP 
y <- S%*%samples

#Combine X1, X2, and Y into one data sort and export it as a csv
obs <-as.vector(y)
data <- cbind(x1, x2, y)
colnames(data, do.NULL = TRUE, prefix = "col")
colnames(data) <- c("X1", "X2", "Y")

write.csv(data[1:n,1:3], file = "/Users/amydenise/Desktop/CExp/210/210_10LHmod.csv",row.names = FALSE)
write.csv(data[(n+1):(2*n),1:3], file = "/Users/amydenise/Desktop/CExp/210/210_10MEmod.csv",row.names = FALSE)
write.csv(data, file = "/Users/amydenise/Desktop/CExp/210/210_10true.csv",row.names = FALSE)

#
#read in predictions
#
predLH21010 <- read.csv("~/Desktop/CExp/210/210_10LHpred.csv", header=FALSE)
predME21010 <- read.csv("~/Desktop/CExp/210/210_10MEpred.csv", header=FALSE)

#
#calculate mspe for LH 210 n = 10
#
mspe_LH21010 <- sum((predLH21010$V4[(n + 1):(n + 25)] - predLH21010$V3[(n + 1):(n + 25)])^2)/25
mspe_LH21010
#
#calculate mspe for ME 210 n = 10
#
mspe_ME21010 <- sum((predME21010$V4[(n + 1):(n + 25)] - predME21010$V3[(n + 1):(n + 25)])^2)/25
mspe_ME21010


###############################
#Analysis for theta1 = 2, theta2 = 10, n = 20
###############################

#
#True Y 
#
n=20
x1lh <- designs$x1_20L[1:n]
x2lh <- designs$x2_20L[1:n]
x1me <- designs$x1_2_10_20[1:n]
x2me <- designs$x2_2_10_20[1:n]

x1 <- c(x1lh, x1me, newx1)
x2 <- c(x2lh, x2me, newx2)

#Calculate R
R <- Rcalc(x1 = x1, 
           x2 = x2,theta1 = 2, theta2 = 10)

#Square root R
Rsr <- chol(R)     #t(Rsr) %*% Rsr = R
S <- t(Rsr)        #SX ~ N(O, R)

#Generate random sample from iid Normal(0,1)
set.seed(4)
samples <- rnorm(nrow(R),0,1)

#Generate Y  from GP 
y <- S%*%samples

#Combine X1, X2, and Y into one data sort and export it as a csv
obs <-as.vector(y)
data <- cbind(x1, x2, y)
colnames(data, do.NULL = TRUE, prefix = "col")
colnames(data) <- c("X1", "X2", "Y")

write.csv(data[1:n,1:3], file = "/Users/amydenise/Desktop/CExp/210/210_20LHmod.csv",row.names = FALSE)
write.csv(data[(n+1):(2*n),1:3], file = "/Users/amydenise/Desktop/CExp/210/210_20MEmod.csv",row.names = FALSE)
write.csv(data, file = "/Users/amydenise/Desktop/CExp/210/210_20true.csv",row.names = FALSE)

#
#read in predictions
#
predLH21020 <- read.csv("~/Desktop/CExp/210/210_20LHpred.csv", header=FALSE)
predME21020 <- read.csv("~/Desktop/CExp/210/210_20MEpred.csv", header=FALSE)

#
#calculate mspe for LH 210 n = 20
#
mspe_LH21020 <- sum((predLH21020$V4[(n + 1):(n + 25)] - predLH21020$V3[(n + 1):(n + 25)])^2)/25
mspe_LH21020
#
#calculate mspe for ME 210 n = 20
#
mspe_ME21020 <- sum((predME21020$V4[(n + 1):(n + 25)] - predME21020$V3[(n + 1):(n + 25)])^2)/25
mspe_ME21020


###############################
#Analysis for theta1 = 2, theta2 = 10, n = 50
###############################

#
#True Y 
#
n=50
x1lh <- designs$x1_50L[1:n]
x2lh <- designs$x2_50L[1:n]
x1me <- designs$x1_2_10_50[1:n]
x2me <- designs$x2_2_10_50[1:n]

x1 <- c(x1lh, x1me, newx1)
x2 <- c(x2lh, x2me, newx2)

#Calculate R
R <- Rcalc(x1 = x1, 
           x2 = x2,theta1 = 2, theta2 = 10)

#Square root R
Rsr <- chol(R)     #t(Rsr) %*% Rsr = R
S <- t(Rsr)        #SX ~ N(O, R)

#Generate random sample from iid Normal(0,1)
set.seed(6)
samples <- rnorm(nrow(R),0,1)

#Generate Y  from GP 
y <- S%*%samples

#Combine X1, X2, and Y into one data sort and export it as a csv
obs <-as.vector(y)
data <- cbind(x1, x2, y)
colnames(data, do.NULL = TRUE, prefix = "col")
colnames(data) <- c("X1", "X2", "Y")

write.csv(data[1:n,1:3], file = "/Users/amydenise/Desktop/CExp/210/210_50LHmod.csv",row.names = FALSE)
write.csv(data[(n+1):(2*n),1:3], file = "/Users/amydenise/Desktop/CExp/210/210_50MEmod.csv",row.names = FALSE)
write.csv(data, file = "/Users/amydenise/Desktop/CExp/210/210_50true.csv",row.names = FALSE)

#
#read in predictions
#
predLH21050 <- read.csv("~/Desktop/CExp/210/210_50LHpred.csv", header=FALSE)
predME21050 <- read.csv("~/Desktop/CExp/210/210_50MEpred.csv", header=FALSE)

#
#calculate mspe for LH 210 n = 50
#
mspe_LH21050 <- sum((predLH21050$V4[(n + 1):(n + 25)] - predLH21050$V3[(n + 1):(n + 25)])^2)/25
mspe_LH21050
#
#calculate mspe for ME 210 n = 50
#
mspe_ME21050 <- sum((predME21050$V4[(n + 1):(n + 25)] - predME21050$V3[(n + 1):(n + 25)])^2)/25
mspe_ME21050


###############
#Theta estimates
theta1_3_3_lh <- c(3.58,2.27,3.09)
theta1_3_3_me <- c(.85,2.25,3.36)
theta1_2_10_lh <-c(0,1.83,1.47)  
theta1_2_10_me <-c(.62,1.66,1.54)
theta2_3_3_lh <-c(1.06,5.03,3.34)
theta2_3_3_me <-c(1.48,5.84,2.65)
theta2_2_10_lh <-c(8.6,9.18,10.21) 
theta2_2_10_me <-c(2,7.51, 10.7)

thetas <-cbind(theta1_3_3_lh, theta1_3_3_me, 
               theta1_2_10_lh, theta1_2_10_me, 
               theta2_3_3_lh, theta2_3_3_me,
               theta2_2_10_lh, theta2_2_10_me)

##############################
#Plot comparing theta estimates 
########################
par(mfrow = c(1,2))

plottheta(lhtheta = theta1_3_3_lh, maxenttheta = theta1_3_3_me, truetheta = 3, theta = 1)
plottheta(lhtheta = theta2_3_3_lh, maxenttheta = theta2_3_3_me, truetheta = 3, theta = 2)

plottheta(lhtheta = theta1_2_10_lh, maxenttheta = theta1_2_10_me, truetheta = 2, theta = 1)
plottheta(lhtheta = theta2_2_10_lh, maxenttheta = theta2_2_10_me, truetheta = 10, theta = 2)


###############################
#Plot comparing prediction errors  (all 3 n's)
##############################

LH_3_3 <-c(.279, .115, .005)
ME_3_3 <-c(.101,.06,.0009)
LH_2_10 <- c(2.226, .143, .00477373)
ME_2_10 <- c(3.291, .2148, .004737813)

g_range <- range(0, LH_3_3, ME_3_3, LH_2_10, ME_2_10, na.rm = TRUE)
  
plot(LH_3_3, type="o", col="blue", ylim=g_range, 
       axes=FALSE, ann=FALSE)
  
axis(1, at=1:3, lab=c("10","20","50"))
  
axis(2, las=1)
  
box()
  
lines(ME_3_3, type="o", pch=22, lty=2, col="red")
lines(LH_2_10, type="o", pch=21, lty=3, col="purple")
lines(ME_2_10, type="o", pch=22, lty=4, col="orange")

title("MSPE by design")
 
title(xlab="Runs")
title(ylab="MSPE")
  
legend(2, g_range[2], c("Latin Hypercube (3,3)","Maximum Entropy (3,3)", 
                          "Latin Hypercube (2,10)", "Maximum Entropy (2,10)"), cex=0.8, 
                          col=c("blue","red", "purple", "orange"), pch=c(21,22,21,22), 
                          lty=c(1,2,3,4), bty = "n");
  



###############################
#Plot comparing prediction errors  (n=20, n = 50)
##############################

LH_3_3 <-c(.115, .005)
ME_3_3 <-c(.06,.0009)
LH_2_10 <- c(.143, .00477373)
ME_2_10 <- c(.2148, .004737813)

g_range <- range(0, LH_3_3, ME_3_3, LH_2_10, ME_2_10, na.rm = TRUE)

plot(LH_3_3, type="o", col="blue", ylim=g_range, 
     axes=FALSE, ann=FALSE)

axis(1, at=1:2, lab=c("20","50"))

axis(2, las=1)

box()

lines(ME_3_3, type="o", pch=22, lty=2, col="red")
lines(LH_2_10, type="o", pch=21, lty=3, col="purple")
lines(ME_2_10, type="o", pch=22, lty=4, col="orange")

title("MSPE by design")

title(xlab="Runs")
title(ylab="MSPE")

legend(1.5, g_range[2], c("Latin Hypercube (3,3)","Maximum Entropy (3,3)", 
                        "Latin Hypercube (2,10)", "Maximum Entropy (2,10)"), cex=0.8, 
       col=c("blue","red", "purple", "orange"), pch=c(21,22,21,22), 
       lty=c(1,2,3,4), bty = "n");


