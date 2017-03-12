library(MASS)
library(FNN)
library(tidyverse)
library(pls)
library(glmnet)

set.seed(03082017)
setwd("....")
# Load in the sample using the load function
load(file="Decathlons.Rdata")

# Read external functions
source("AdaptedStepwiseSelection.R")
head(london)
str(london)
names(london)

# Remove variables that are not needed because the scenario specifies that we have only completed
# day 1 of the competition.
decathlon110 <- dplyr::select(london, -score, -discus, -pole, -javelin, -x1500m, -Rank, -Medal, -Athlete, -NOC, -Team)
str(decathlon110)
names(decathlon110)

# Remove variables that are not needed because the scenario specifies that we have only completed
# day 1 of the competition.
decathlon1500 <- dplyr::select(london, -score, -discus, -pole, -javelin, -x110m, -Rank, -Medal, -Athlete, -NOC, -Team)
str(decathlon1500)
names(decathlon1500)

# show rows of data that have missing values 
decathlon110[!complete.cases(decathlon110),]
# list rows of data that have missing values 
decathlon1500[!complete.cases(decathlon1500),]

# remove data with missing values
dat110<- decathlon110[complete.cases(decathlon110),]
names(dat110)
str(dat110)

# remove data with missing values
dat1500<- decathlon1500[complete.cases(decathlon1500),]
names(dat1500)
str(dat1500)

# normalize the data set for regression
newDat1500 <-as.data.frame.matrix(scale(dat1500)) 

# normalize the data set for regression
newDat110 <-as.data.frame.matrix(scale(dat110))

#-----------------------------------------STEP-WISE-REGRESSION---------------------------------------------------#

# step-wise regression for x110
input_list110 <- make_step_regmods(newDat110, direction="backward", yname="x110m", output="input_list") 
input_list110
cvmse110 <- stepwise_cvmse(cv_K=27, input_list110, newDat110, yname="x110m")
cvmse110
which.min(cvmse110)
input_list110[[which.min(cvmse110)]]
# MSE from LOOCV
regCVmse110 <- min(cvmse110)
regCVmse110

# step-wise regression for x1500
input_list1500 <- make_step_regmods(newDat1500, direction="backward", yname="x1500m", output="input_list") 
input_list1500
cvmse1500 <- stepwise_cvmse(cv_K=26, input_list1500, newDat1500, yname="x1500m")
cvmse1500
which.min(cvmse1500)
input_list1500[[which.min(cvmse1500)]]
# MSE from LOOCV
regCVmse1500 <- min(cvmse1500)
regCVmse1500

#-----------------------------------------RIDGE-REGRESSION--------------------------------------------------#

# Ridge regression fitting for x110 (find min lambda range)
ridge110<-cv.glmnet(as.matrix(newDat110[,-7]), newDat110$x110m,
                    alpha = 0, nfolds = 27, group = FALSE, lambda = c(0.01,0.1,1,10,100))
ridge110
# Ridge regression fitting for x110
ridge110<-cv.glmnet(as.matrix(newDat110[, -7]), newDat110$x110m,
                    alpha = 0, nfolds = 27, group = FALSE, lambda = seq(0,1,0.1))
ridge110
# plot the possible lambdas vs resulting cvmse values
plot(ridge110$lambda, ridge110$cvm)
# select lambda for final model based on lowest cvmse
ridgeLambda110 <- ridge110$lambda[which.min(ridge110$cvm)]
ridgeLambda110
# MSE from LOOCV
ridge110MSE <-ridge110$cvm[which.min(ridge110$cvm)]
ridge110MSE


# Ridge regression fitting for x1500 (find min lambda range)
ridge1500<-cv.glmnet(as.matrix(newDat1500[,-7]), newDat1500$x1500m,
                    alpha = 0, nfolds = 26, group = FALSE, lambda = c(0.01,0.1,1,10,100))
ridge1500
# Ridge regression fitting for x1500
ridge1500<-cv.glmnet(as.matrix(newDat1500[,-7]), newDat1500$x1500m,
                     alpha = 0, nfolds = 26, group = FALSE, lambda = seq(0,1,0.1))
ridge1500
# plot the possible lambdas vs resulting cvmse values
plot(ridge1500$lambda, ridge1500$cvm)
# select lambda for final model based on lowest cvmse
ridgeLambda1500 <- ridge1500$lambda[which.min(ridge1500$cvm)]
ridgeLambda1500
# MSE from LOOCV
ridge1500MSE <-ridge1500$cvm[which.min(ridge1500$cvm)]
ridge1500MSE

#-----------------------------------------LASSO-REGRESSION---------------------------------------------------#

# Lasso regression fitting for x110 (find min lambda range)
lasso110<-cv.glmnet(as.matrix(newDat110[,-7]), newDat110$x110m,
                    alpha = 1, nfolds = 27, group = FALSE, lambda = c(0.01,0.1,1,10,100))
lasso110
# Lasso regression fitting x110
lasso110<-cv.glmnet(as.matrix(newDat110[,-7]), newDat110$x110m,
                    alpha = 1, nfolds = 27, group = FALSE, lambda = seq(0,1,0.1))
lasso110
# plot the possible lambdas vs resulting cvmse values
plot(lasso110$lambda, lasso110$cvm)
# select lambda for final model based on lowest cvmse
lassoLambda110 <- lasso110$lambda[which.min(lasso110$cvm)]
lassoLambda110
# MSE from LOOCV
lasso110MSE <-lasso110$cvm[which.min(lasso110$cvm)]
lasso110MSE

# Lasso regression fitting for x1500 (find min lambda range)
lasso1500<-cv.glmnet(as.matrix(newDat1500[,-7]), newDat1500$x1500m,
                    alpha = 1, nfolds = 26, group = FALSE, lambda = c(0.01,0.1,1,10,100))
lasso1500
# Lasso regression fitting for x1500
lasso1500<-cv.glmnet(as.matrix(newDat1500[,-7]), newDat1500$x1500m,
                     alpha = 1, nfolds = 26, grouped = FALSE, lambda = seq(0,1,0.1))
lasso1500
# plot the possible lambdas vs resulting cvmse values
plot(lasso1500$lambda, lasso1500$cvm)
# select lambda for final model based on lowest cvmse
lassoLambda1500 <- lasso1500$lambda[which.min(lasso1500$cvm)]
lassoLambda1500
# MSE from LOOCV
lasso1500MSE <-lasso1500$cvm[which.min(lasso1500$cvm)]
lasso1500MSE


#-----------------------------------------PCR---------------------------------------------------#

# Perform PCR using LOOCV for x110
pcr110.fit <- pcr(x110m ~ ., data=newDat110, validation="LOO", scale=FALSE)
# Examine the resulting fit and select comps w/ min RMSE
# LOOCV MSE is min RMSE squared
summary(pcr110.fit)
# What is the best number of components 
validationplot(pcr110.fit,val.type = "RMSEP")
# Plot of cross-validation MSE
validationplot(pcr110.fit,val.type = "MSEP")


# Perform PCR using LOOCV for x1500
pcr1500.fit <- pcr(x1500m ~ ., data=newDat1500, validation="LOO", scale=FALSE)
# Examine the resulting fit and select comps w/ min RMSE
# LOOCV MSE is min RMSE squared
summary(pcr1500.fit)
# What is the best number of components 
validationplot(pcr1500.fit,val.type = "RMSEP")
# Plot of cross-validation MSE
validationplot(pcr1500.fit,val.type = "MSEP")


#-----------------------------------------PLSR---------------------------------------------------#

# Perform PLSR using LOOCV for x110
plsr110.fit <- plsr(x110m ~ ., data=newDat110, validation="LOO", scale=FALSE)
# Examine the resulting fit and select comps w/ min RMSE
# LOOCV MSE is min RMSE squared
summary(plsr110.fit)
# What is the best number of components 
validationplot(plsr110.fit,val.type = "RMSEP")
# Plot of cross-validation MSE
validationplot(plsr110.fit,val.type = "MSEP")

# Perform PLSR using LOOCV for x1500
plsr1500.fit <- plsr(x1500m ~ ., data=newDat1500, validation="LOO", scale=FALSE)
# Examine the resulting fit and select comps w/ min RMSE
# LOOCV MSE is min RMSE squared
summary(plsr1500.fit)
# What is the best number of components 
validationplot(plsr1500.fit,val.type = "RMSEP")
# Plot of cross-validation MSE
validationplot(plsr1500.fit,val.type = "MSEP")





