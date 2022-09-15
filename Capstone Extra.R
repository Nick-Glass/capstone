# Capstone Extras

# Create a MLR model that contains a subset of desired predictors 
FOM_Reduced <- lm(Future_Goals ~ Goals + First_Assists + Second_Assists 
                  +  ixG  + iFF + iSCF + iHDCF, data = train.data.FO)

# Summary
summary(FOM_Reduced)

# Calculate the error
library(forecast)
accuracy(FOM_Reduced)

# AIC
AIC(FOM_Reduced)

# BIC
BIC(FOM_Reduced)

# Out-of-sample Prediction
pred_FOM_Reduced <- predict(FOM_Reduced, newdata = valid.data.FO)

# Find the error of the validation data
accuracy(pred_FOM_Reduced, valid.data.FO$Future_Goals)

-----------------------------------------------------------------------
# Forward/Backward/Stepwise Regression
FOM_null <- lm(Future_Goals ~ 1, data = train.data.FO)
FOM_full <- lm(Future_Goals ~ Goals + First_Assists + Second_Assists 
               + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
               + Rebounds_Created, data = train.data.FO)

# Forward Selection
FOM_fwd <- step(FOM_null, scope = list(lower = FOM_null, upper = FOM_full), 
                direction = "forward")

# Print the summary of the forward selection model
summary(FOM_fwd)

# Backward Elimination 
FOM_back <- step(FOM_full, direction = "backward")

# Print the summary of the backward elimination model
summary(FOM_back)

# Stepwise Regression
FOM_step <- step(FOM_null, scope = list(lower = FOM_null, upper = FOM_full),
                 direction = "both")
summary(FOM_step)

view(train.data.FO)

# Find the missing values by column
Missing_values_train <- sort(colSums(is.na(train.data.FO)), decreasing = TRUE)
Missing_values_train <- Missing_values_train[Missing_values_train > 0]
print(sort(Missing_values_train), decreasing = TRUE)
------------------------------------------------------------------------
library(leaps)
# Run an exhaustive search
FOM_search <- regsubsets(Future_Goals ~ Goals + First_Assists + Second_Assists 
                         + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
                         + Rebounds_Created + Takeaways + On_The_Fly_Starts + 
                           Off_Zone_Starts + Neu_Zone_Starts
                         + Def_Zone_Starts, data = train.data.FO, nbest = 1, 
                         nvmax = ncol(train.data.FO))

sum <- summary(FOM_search)

sum$which

# a useful visualization of the models returned by exhaustive search
par(mfrow = c(1, 1))
plot(FOM_search, scale = "Cp")
t(t(sum$cp))

# Create a MLR model that contains only variables from  the exhaustive search 
FOM_ES <- lm(Future_Goals ~ Goals + First_Assists + iSCF + Takeaways + 
               On_The_Fly_Starts + Off_Zone_Starts + Neu_Zone_Starts, data = train.data.FO)
# Summary
summary(FOM_ES)

# Calculate the error
library(forecast)
accuracy(FOM_ES)

# AIC
AIC(FOM_ES)

# BIC
BIC(FOM_ES)

# Out-of-sample Prediction
pred_FOM_ES <- predict(FOM_ES, newdata = valid.data.FO)

# Find the error of the validation data
accuracy(pred_FOM_ES, valid.data.FO$Future_Goals)

# Ridge regression model
# Make the new training data
set.seed(1)
n=nrow(Defense_Offensive)

x = as.matrix(Defense_Offensive[,6:20]) 
y = as.matrix(Defense_Offensive[,21:21])  
train_rows = sample(1:n, round(n)/2) 
x.train = x[train_rows,] 
x.valid = x[-train_rows,] 
y.train = y[train_rows] 
y.valid = y[-train_rows] 

# load the package
library(glmnet)
cv.out <- cv.glmnet(x.train, y.train, alpha=0)

# Change the selected variables to rates
Forward_Defensive <- Forward_Defensive %>%
  mutate(Giveaways60 = round((Giveaways*60/TOI),2)) %>%
  mutate(Takeaways60 = round((Takeaways*60/TOI),2)) %>%
  mutate(CA60 = round((CA*60/TOI),2)) %>%
  mutate(xGA60 = round((xGA*60/TOI),2)) %>%
  mutate(SCA60 = round((SCA*60/TOI),2)) %>%
  mutate(HDCA60 = round((HDCA*60/TOI),2)) %>%
  mutate(HDGA60 = round((HDGA*60/TOI),2)) %>%
  mutate(On_The_Fly_Starts60 = round((On_The_Fly_Starts*60/TOI),2)) %>%
  mutate(Off_Zone_Starts60 = round((Off_Zone_Starts*60/TOI),2)) %>%
  mutate(Neu_Zone_Starts60 = round((Neu_Zone_Starts*60/TOI),2)) %>%
  mutate(Def_Zone_Starts60 = round((Def_Zone_Starts*60/TOI),2)) %>%
  mutate(Future_Goals_Against60 = round((Future_Goals_Against*60/TOI),2)) %>%
  dplyr::select(Player:TOI,Giveaways60:Future_Goals_Against60)

view(Forward_Defensive)

# Create a MLR model that contains the full desired predictors 
FDM_Full <- lm(Future_Goals_Against60 ~ Giveaways60 + Takeaways60 + CA60 + xGA60 + SCA60 + 
                 HDCA60 + On_The_Fly_Starts60 + Off_Zone_Starts60 + Neu_Zone_Starts60
               + Def_Zone_Starts60, data = train.data.FD)
# Summary
summary(FDM_Full) 

# Applying Box-Cox transformation to lm object
library(MASS)
library(tidyverse)
bc.results=boxcox(FDM_Full) #Applying Box-Cox transformation to lm object
lambda=bc.results$x[which.max(bc.results$y)] #extract the best lambda
lambda

lambda_FDM_Full <-lm(Future_Goals_Against ~ Giveaways + CA + xGA + SCA + 
                       HDCA + On_The_Fly_Starts + Off_Zone_Starts + Neu_Zone_Starts
                     + Def_Zone_Starts, data=train.data.FD)
# Summary
summary(lambda_FDM_Full) 

# Get list of residuals 
lambda_Res_FDM_Full <- resid(lambda_FDM_Full)

# Produce residual vs. fitted plot
plot(fitted(lambda_FDM_Full), lambda_Res_FDM_Full)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(lambda_Res_FOM_Full)

# Add a straight diagonal line to the plot
qqline(lambda_Res_FDM_Full) 

# Create density plot of residuals
plot(density(lambda_Res_FDM_Full))


# k-Nearest Neighbors
library(FNN)  
FOM_knn_reg <- knn.reg(train = train.data.FO[ , c(6:16)], 
                       test = valid.data.FO[ , c(6:16)], 
                       y = train.data.FO$Future_Goals, 
                       k = 1)

# compile the actual and predicted values and view the first 20 records
FOM_knn_results <- data.frame(cbind(pred = FOM_knn_reg$pred, actual = valid.data.FO$Future_Goals))
head(FOM_knn_results, 20)


