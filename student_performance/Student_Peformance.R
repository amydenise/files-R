############Data importation, EDA, and dataset curation

######################
#Data importation and manipulation
####################
#Loading libraries and reading raw data
library(plyr)
library(MASS)
library(randomForest)
library(vcd)
library(caret)
library(Boruta)

#Importing data
setwd("~/Desktop/ML/project/data_original")
math <- read.csv("student-mat.csv", sep = ";")
por <- read.csv("student-por.csv", sep = ";")

#Removing "fatherd" from Portuguese dataset and "paid" from Math dataset for consistency
por <- por[,-18]
math <- math[,-18]

#Variables are divided for organization
nominalVars <- c("school", "sex", "address", "famsize", "Pstatus", "schoolsup", "famsup", "activities", "nursery", "higher", "internet", "romantic", "Mjob", "Fjob", "reason", "guardian")
ordinalVars <- c("Medu", "Fedu", "traveltime", "studytime", "failures","famrel", "freetime", "goout", "Dalc", "Walc", "health", "age", "absences", "G1", "G2", "G3")
Vars = c(nominalVars, ordinalVars)

##############################
#Exploration of Math G3 = 0
#############################
#Subsetting students with G3 = 0
zeros_m <- math[which(math$G3 == 0),]

#Analyzing absences
sum(zeros_m$absences)

#Comparing overall mean score for G1/G2, to the G1/G2 scores for G3 = 0 students
mean(math$G1)
mean(math$G2)
zeros_m$G1
zeros_m$G2

###########################
#Univariate, linear models
#######################
#Initialize lists to hold models
por_uni_mods <- vector(mode="list", length=31)
math_uni_mods <- vector(mode="list", length=31)

#Fit linear models
for (i in 1:31){
  math_uni_mods[[i]] <-summary(lm(math$G3 ~ math[,Vars[i]]))
}

#Initialize dataframe to hold AdjR2 and P values
rsp_math <- data.frame(matrix(ncol = 3, nrow = 31))
colnames(rsp_math) <- c("variable", "math_adjR2", "math_p")

#Extract AdjR2 and F-stats from models, caluculate P-values
for (i in 1:31){
  rsp_math[i, 1] <- Vars[i]
  rsp_math[i, 2] <- math_uni_mods[[i]]$adj.r.squared
  rsp_math[i, 3] <-round(pf(math_uni_mods[[i]]$fstatistic[1],math_uni_mods[[i]]$fstatistic[2],
                            math_uni_mods[[i]]$fstatistic[3], lower.tail = F), 6)
}

#Order variables by AdjR2
rsp_math_ordered <- rsp_math[order(rsp_math$math_adjR2, decreasing = T),]

#Print those with AdjR2 > .01
rsp_math_ordered[which(rsp_math_ordered$math_adjR2 > .01),]

##########################
#Remove G1 and G2
######################
por <- por[,-c(30,31)]
math <- math[,-c(30,31)]

##############################
#Identify Common Students
############################

#add .m/.p to math/Portuguese values of common (and distinct) variables
colnames(math)[c(15,29,30)] <- c("failures.m", "absences.m", "G3.m")
colnames(por)[c(15,29,30)] <- c("failures", "absences", "G3")

#merge
common_students <- merge(math, por, by = c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", 
                                           "Fedu", "Mjob", "Fjob", "reason", "guardian", "traveltime", "studytime", 
                                           "schoolsup", "famsup", "activities", "nursery", "higher", "internet", 
                                           "romantic", "famrel", "freetime", "goout", "Dalc", "Walc", "health"))

#################
#10-fold Cross-Validation for "mtry" parameter
#####################
#mtryGrid <- expand.grid(mtry = c(7:13))

#fitControl <- trainControl(
#  method = "repeatedcv",
#  number = 10,
#  repeats = 10)

#set.seed(2)
#tree.fit = train(por$G3 ~ ., data = por, method = "rf", 
 #                trControl = fitControl, metric = "RMSE", tuneGrid = mtryGrid)

#plot(tree.fit)

load("mtry_set.Rdata")
mtry_set <- as.numeric(tree.fit$bestTune)

################
#Run Boruta algorithm
###############
#set.seed(3)
#bor_por <- Boruta(por$G3 ~ ., data = por, doTrace = 2, 
 #                 ntree = 500, mtry = mtry_set, maxRuns = 200)

load("por_var_selection.Rdata")
#Extract attribute statistics and order variables by meanZ (i.e. Importance)
bor_por_stats <-attStats(bor_por)
bor_por_stats <- bor_por_stats[order(bor_por_stats$meanZ, decreasing = T), c(1,6)]
bor_por_stats

######################
#Random Forest fit to Boruta Selected Variables
######################
#Extract Boruta selected variables (including "tentatives")
var_select <- c(getSelectedAttributes(bor_por, withTentative = F), "G3")

#Create dataframes with only Boruta selected variables
por_sub <- cbind(por[,var_select])

#Fit a Random Forest to all Boruta selected variables
set.seed(5)
por_rf_sub <- randomForest(por_sub$G3 ~., data = por_sub, mtry = mtry_set, ntree = 500)

###############################
#Predict portuguese grades for common students
#######################################
common_students$predG3.p <- predict(por_rf_sub,newdata = common_students)

############################
#Creation of final dataset for use in modeling
#########################
#Add a variable of difference between true and predicted Portuguese G3
common_students$diff <- common_students$G3 - common_students$predG3.p

#remove Portuguese-specific variables used in predicting portuguese G3 grade
math_students <- common_students[,-c(31:33)]
colnames(math_students)[c(28:30)] <- c("failures", "absences", "G3")


############Math "G3" grade Modeling


###########################
#Create 1 dataset per model to be compared
###########################
#All original variables + predG3.p + diff (G3.p - Predicted G3.p)
M_full <- math_students

#All original variables + predG3.p 
M_pred <- math_students[,-c(32)]

#All original variables + Predicted G3.p 
M_diff <- math_students[,-c(31)]

#All original variables only
M_orig  <- math_students[,-c(31:32)]

####################################
#Fit many models, record RMSE estimates and variable importance ranks
##################################

#Define function that will predict Math "G3" many times per model (i.e. subset of variables)
#Function returns an object of class S4 that contains one list per run holding the rank of each variable from the Boruta algorithm
# as well as the average RMSE and it's standard deviation across the runs (Ex. Model[[i]]@rmse.stats).  

#The Model_run function, defined below, iterates across the following steps for each model:  
#1.)  Splits the observations into training (.7) and testing (.3) sets 
#2.)  Runs Boruta algorithm on the selected observations using all variables present in the dataset
#3.)  Records the rank of all variables and extracts the "Boruta selected" variables
#4.)  Fits a random forest to all "Boruta selected" variables
#5.)  Calculates Root Mean Squared Error (RMSE) 

setClass(Class="Model",
         representation(
           Boruta.stats ="list",
           rmse.stats = "numeric"
         )
)

Model_run <- function(x, runs = 30, seed = 18){
  Boruta.mods <- vector(mode="list", length=runs)
  Boruta.stats <- vector(mode = "list", length = runs)
  RF.mods <- vector(mode = "list", length = runs)
  rmse <- c(1:runs)
  rmse.stats <- c(1:2)
  
  for (i in 1:runs) {
    #Break data into training and testing
    set.seed(seed + i)
    train.obs = createDataPartition(x$G3, times = 1, p = .7, list = F)
    
    train.set <<- x[c(train.obs),]
    test.set <<- x[-c(train.obs),]
    
    #Run Boruta algorithm
    set.seed(seed + i + 10)
    Boruta.mods[[i]] <- Boruta(train.set$G3 ~ ., data = train.set, doTrace = 2, 
                               ntree = 500, mtry = mtry_set, maxRuns = 100)
    
    #Extract attribute statistics and order variables by meanZ (i.e. Importance)
    Boruta.attstats <-attStats(Boruta.mods[[i]])
    Boruta.stats[[i]] <- Boruta.attstats[order(Boruta.attstats$meanZ, decreasing = T), c(1,6)]
    Boruta.stats[[i]]$rank <- c(1:(ncol(x)-1))
    
    #Extract selected variables
    var_select <- c(getSelectedAttributes(Boruta.mods[[i]], withTentative = F), "G3")
    
    #Create dataframes with only Boruta selected variables
    train.sub <<- cbind(train.set[,var_select])
    test.sub <<- cbind(test.set[,var_select])
    
    #Fit a Random Forest to all Boruta selected variables
    set.seed(seed + i + 20)
    mtry_plus <- (ncol(train.sub)/3) + 2  #chosen to avoid very low mtry
    RF.mods[[i]] <- randomForest(train.sub$G3 ~., data = train.sub, mtry = mtry_plus, ntree = 500)
    
    #Calculate RMSE
    yhat.test = predict(RF.mods[[i]],newdata=test.sub)
    rmse[i] <- sqrt(mean((yhat.test - test.sub$G3)^2))
  }
  
  rmse.stats[1] <-mean(rmse)
  rmse.stats[2] <-sd(rmse)
  
  return(new("Model",
             Boruta.stats=Boruta.stats,
             rmse.stats = rmse.stats))
}

load("four_models.Rdata")
#This fitting procedure is run 30 times on each of the 4 models.
#models <- vector(mode = "list", length = 4)
#models[[1]] <- Model_run(x = M_full)
#models[[2]] <- Model_run(x = M_pred)
#models[[3]] <- Model_run(x = M_diff)
#models[[4]] <- Model_run(x = M_orig)

#Record number of runs and number of mods
num_mods <- length(models)                    
num_runs <- length(models[[1]]@Boruta.stats) 

####Create a data frame summarizing the rank of each model--averaged across runs by model
#copy each of the 4 models into new list before modification
models_red <- models

#Remove excess columns and add variable name in preparation for merge by variable name 
for (k in 1: length(models)){
  for (j in 1:num_runs){
    models_red[[k]]@Boruta.stats[[j]]$Variable <- rownames(models_red[[k]]@Boruta.stats[[j]])
    models_red[[k]]@Boruta.stats[[j]] <- models_red[[k]]@Boruta.stats[[j]][,-c(1,2)]
  }
}

#merge all rankings (across runs) within each model, calculate av. ranking and order by av. ranking
for (k in 1: num_mods){
  models_red[[k]] <- Reduce(function(x, y) merge(x, y, by = "Variable"), models_red[[k]]@Boruta.stats)
  models_red[[k]]$mean_rank <- round(rowMeans(models_red[[k]][,c(2:(num_runs+1))]),2)
  models_red[[k]] <- models_red[[k]][order(models_red[[k]]$mean_rank),]
}

#copy each of the 8 models into new list before reordering
models_mean <- models_red

#order by "Variable" and remove by-run rankings
for (k in 1: num_mods){
  models_mean[[k]] <- models_mean[[k]][,-c(2:(num_runs + 1))]
  colnames(models_mean[[k]])[2] <- k
}

#Combine the average rank of each variable into one table across models
av.rank.table <- Reduce(function(x, y) merge(x, y, by = "Variable", all = T), models_mean)

av.rank.table <- av.rank.table[order(av.rank.table$"1", decreasing = F),]
colnames(av.rank.table)[2:5] <- c("Full Model", "Pred Model", "Diff Model", "Orig Model")
                               

#### Add the fraction of runs that selected each variable by model
#copy each of the 8 models into new structure
models_bin <- models

#Add binary indicator of "Confirmed", Remove excess columns, and Add variable name in preparation for merge
for (i in 1:num_mods){
  for(j in 1: length(models_bin[[i]]@Boruta.stats)){
    models_bin[[i]]@Boruta.stats[[j]]$Variable <- rownames(models_bin[[i]]@Boruta.stats[[j]])
    models_bin[[i]]@Boruta.stats[[j]]$conf <- ifelse(models_bin[[i]]@Boruta.stats[[j]]$decision ==   "Confirmed", 1, 0)
    models_bin[[i]]@Boruta.stats[[j]] <- models_bin[[i]]@Boruta.stats[[j]][,-c(1,2,3)]
  }
}

#merge all rankings for the runs within each model, calculate and order by average ranking
for (k in 1: num_mods){
  models_bin[[k]] <- Reduce(function(x, y) merge(x, y, by = "Variable"), models_bin[[k]]@Boruta.stats)
  models_bin[[k]]$frac_runs_selected <- round(rowMeans(models_bin[[k]][,c(2:(num_runs+1))]),3)*100
  models_bin[[k]] <- models_bin[[k]][order(models_bin[[k]]$frac_runs_selected, decreasing = T),]
  colnames(models_bin[[k]])[num_runs + 2] <- k
}

#copy each of the 4 models into new structure and remove by-run, binary indicator of "Confirmed"
models_conf <-models_bin

for (i in 1:num_mods){
  models_conf[[i]] <- models_conf[[i]][,-c(2:(num_runs + 1))]
}  

perc.time.not.rejected <- Reduce(function(x, y) merge(x, y, by = "Variable", all = T), models_conf)
ranks_per.selected <-merge(av.rank.table, perc.time.not.rejected, by = "Variable")

####Calculate an estimate of the fraction of times each variable is selected
#copy before modification
ranks_select <-ranks_per.selected

# Sum the fraction of times each variable selected across models 
ranks_select$Chosen <- rowSums(ranks_select[,c((num_mods + 2):(num_mods*2 + 1))], na.rm = T)

#Calculate the times (out of 4) that each variable was considered in a model
for (i in 1:nrow(ranks_select)){
  ranks_select$Chances[i] <- ifelse(sum(is.na(ranks_per.selected[i,])) == 0, 4, sum(is.na(ranks_per.selected[i,]))/2)
}

#Add percentage of time chosen
ranks_select$"%Chosen"<- round(ranks_select$Chosen/ranks_select$Chances,2)

#order by av. rank in full model
ranks_select <- ranks_select[order(ranks_select$"Full Model", decreasing = F),]

####Add RMSE, SD estimate
#remove variables chosen very infrequently
summary <- ranks_select[ranks_select$"%Chosen" > 5,]


#Add RMSE, STDV, and confidence intervals to table
RMSE <- c(rep(NA,num_mods))
STDV <- c(rep(NA,num_mods))
for (i in 1: num_mods  ){
  RMSE[i] <- round(models[[i]]@rmse.stats[1],3) 
  STDV[i] <- round(models[[i]]@rmse.stats[2],3)
  
}

RMSE <- c("RMSE", RMSE, "_", "_", "_", "_", "_", "_", "_")
STDV <- c("STDEV", STDV, "_", "_", "_", "_", "_", "_", "_")

summary <- do.call(rbind, list(summary, RMSE, STDV))

# rearrange columns, add column names
summary <-summary[c(1,2,6,3,7,4,8,5,9,12)]
colnames(summary)[c(3,5,7,9)] <- c("%Chosen" , "%Chosen" , "%Chosen" , "%Chosen")


#Prepare plots of Important Predictors
cat_Vars <- c("schoolsup","higher","Medu","goout","Mjob","guardian", "failures")
cat_Vars_labs <-c("School Support", "Desires Higher Education", "Mother's Education",
                  "Going out with Friends", "Mother's Job", "Guardian", "Number of Past Class Failures")
int_Vars <- c("predG3.p", "diff", "absences",  "age" )
int_Vars_labs <- c("Predicted Portuguese G3", "Diff (True - Predicted Portuguese G3)",
                   "Number of Absences", "Age")

#Boxplots for categorical variables
for (i in 1:length(cat_Vars)){
  boxplot(common_students$G3 ~ common_students[,cat_Vars[i]], 
          data = common_students, main = cat_Vars_labs[i], ylab = "G3", xlab = cat_Vars[i])
}

for (i in 1:length(int_Vars)){
  plot(common_students[,int_Vars[i]], common_students$G3, col="#00000020",pch=16, 
       main = int_Vars_labs[i], xlab = int_Vars[i], ylab = "G3")
}