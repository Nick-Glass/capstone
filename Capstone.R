# Capstone Project
# Load NHL individual stats 2013-14
rm(list=ls())
library(readr)
library(tidyverse)

## 2013-14 ##
IND_5V5_13_14 <- read_csv("Player_Season_Totals_13_14_IND_5V5.csv",
                          col_types = "cccciniiiiininniiiiiiiiiiiiiiiiiiin")
head(IND_5V5_13_14)

# Delete unnecessary columns
IND_5V5_13_14 <- IND_5V5_13_14[,-1]
head(IND_5V5_13_14)

# Load NHL on-ice stats 2013-14
OI_5V5_13_14 <- read_csv("Player_Season_Totals_13_14_OI_5V5.csv",
                         col_types = "cccciniiniiniiniinnnniiniiniiniiniiniiniinnnniiiiniiin")
head(OI_5V5_13_14)

# Delete unnecessary columns
OI_5V5_13_14 <- OI_5V5_13_14[, -c(1,3,4,5,6)]
head(OI_5V5_13_14)

# Join the two tables for 2013-14
Data_13_14 <- merge(x=IND_5V5_13_14,y=OI_5V5_13_14,by="Player",all=TRUE)
head(Data_13_14)



## 2014-15 ##
# Load NHL individual stats 2014-15
IND_5V5_14_15 <- read_csv("Player_Season_Totals_14_15_IND_5V5.csv",
                          col_types = "cccciniiiiininniiiiiiiiiiiiiiiiiiin")
head(IND_5V5_14_15)

# Delete unnecessary columns
IND_5V5_14_15 <- IND_5V5_14_15[,-1]
head(IND_5V5_14_15)

# Load NHL on-ice stats 2014-15
OI_5V5_14_15 <- read_csv("Player_Season_Totals_14_15_OI_5V5.csv",
                         col_types = "cccciniiniiniiniinnnniiniiniiniiniiniiniinnnniiiiniiin")
head(OI_5V5_14_15)

# Delete unnecessary columns
OI_5V5_14_15 <- OI_5V5_14_15[, -c(1,3,4,5,6)]
head(OI_5V5_14_15)

# Join the two tables for 2014-15
Data_14_15 <- merge(x=IND_5V5_14_15,y=OI_5V5_14_15,by="Player",all=TRUE)
head(Data_14_15)


## 2015-16 ##
# Load NHL individual stats 2015-16
IND_5V5_15_16 <- read_csv("Player_Season_Totals_15_16_IND_5V5.csv",
                          col_types = "cccciniiiiininniiiiiiiiiiiiiiiiiiin")
head(IND_5V5_15_16)

# Delete unnecessary columns
IND_5V5_15_16 <- IND_5V5_15_16[,-1]
head(IND_5V5_15_16)

# Load NHL on-ice stats 2015-16
OI_5V5_15_16 <- read_csv("Player_Season_Totals_15_16_OI_5V5.csv",
                         col_types = "cccciniiniiniiniinnnniiniiniiniiniiniiniinnnniiiiniiin")
head(OI_5V5_15_16)

# Delete unnecessary columns
OI_5V5_15_16 <- OI_5V5_15_16[, -c(1,3,4,5,6)]
head(OI_5V5_15_16)

# Join the two tables for 2015-16
Data_15_16 <- merge(x=IND_5V5_15_16,y=OI_5V5_15_16,by="Player",all=TRUE)
head(Data_15_16)


## 2016-17 ##
# Load NHL individual stats for 2016-17
IND_5V5_16_17 <- read_csv("Player_Season_Totals_16_17_IND_5V5.csv",
                          col_types = "cccciniiiiininniiiiiiiiiiiiiiiiiiin")
head(IND_5V5_16_17)

# Delete unnecessary columns
IND_5V5_16_17 <- IND_5V5_16_17[,-1]
head(IND_5V5_16_17)

# Load NHL on-ice stats 2016-17
OI_5V5_16_17 <- read_csv("Player_Season_Totals_16_17_OI_5V5.csv",
                         col_types = "cccciniiniiniiniinnnniiniiniiniiniiniiniinnnniiiiniiin")
head(OI_5V5_16_17)

# Delete unnecessary columns
OI_5V5_16_17 <- OI_5V5_16_17[, -c(1,3,4,5,6)]
head(OI_5V5_16_17)

# Join the two tables for 2016-17
Data_16_17 <- merge(x=IND_5V5_16_17,y=OI_5V5_16_17,by="Player",all=TRUE)
head(Data_16_17)


## 2017-18 ##
# Load NHL individual stats 2017-18
IND_5V5_17_18 <- read_csv("Player_Season_Totals_17_18_IND_5V5.csv",
                          col_types = "cccciniiiiininniiiiiiiiiiiiiiiiiiin")
head(IND_5V5_17_18)

# Delete unnecessary columns
IND_5V5_17_18 <- IND_5V5_17_18[,-1]
head(IND_5V5_17_18)

# Load NHL on-ice stats 2017-18
OI_5V5_17_18 <- read_csv("Player_Season_Totals_17_18_OI_5V5.csv",
                         col_types = "cccciniiniiniiniinnnniiniiniiniiniiniiniinnnniiiiniiin")
head(OI_5V5_17_18)

# Delete unnecessary columns
OI_5V5_17_18 <- OI_5V5_17_18[, -c(1,3,4,5,6)]
head(OI_5V5_17_18)

# Join the two tables for 2017-18
Data_17_18 <- merge(x=IND_5V5_17_18,y=OI_5V5_17_18,by="Player",all=TRUE)
head(Data_17_18)


## 2018-19 ##
# Load NHL individual stats 2018-19
IND_5V5_18_19 <- read_csv("Player_Season_Totals_18_19_IND_5V5.csv",
                          col_types = "cccciniiiiininniiiiiiiiiiiiiiiiiiin")
head(IND_5V5_18_19)

# Delete unnecessary columns
IND_5V5_18_19 <- IND_5V5_18_19[,-1]
head(IND_5V5_18_19)

# Load NHL on-ice stats 2018-19
OI_5V5_18_19 <- read_csv("Player_Season_Totals_18_19_OI_5V5.csv",
                         col_types = "cccciniiniiniiniinnnniiniiniiniiniiniiniinnnniiiiniiin")
head(OI_5V5_18_19)

# Delete unnecessary columns
OI_5V5_18_19 <- OI_5V5_18_19[, -c(1,3,4,5,6)]
head(OI_5V5_18_19)

# Join the two tables for 2018-19
Data_18_19 <- merge(x=IND_5V5_18_19,y=OI_5V5_18_19,by="Player",all=TRUE)
head(Data_18_19)

-----------------------------------------------------------------------------
# Combine statistics with the next years actual goals 
# Find the goals of 2014_15
goals14_15 <- Data_14_15 %>%
  dplyr::select(Player, Goals)

head(goals14_15)
dim(goals14_15)

# Join goals14_15 with the 2013-14 data
clean_data13_14 <- inner_join(Data_13_14,
                              goals14_15,
                              by="Player")
head(clean_data13_14)

# Find the goals of 2015_16
goals15_16 <- Data_15_16 %>%
  dplyr::select(Player, Goals)

head(goals15_16)
dim(goals15_16)

# Join goals15_16 with the 2014-15 data
clean_data14_15 <- inner_join(Data_14_15,
                              goals15_16,
                              by="Player")
head(clean_data14_15)

# Find the goals of 2016_17
goals16_17 <- Data_16_17 %>%
  dplyr::select(Player, Goals)

head(goals16_17)
dim(goals16_17)

# Join goals16_17 with the 2015-16 data
clean_data15_16 <- inner_join(Data_15_16,
                              goals16_17,
                              by="Player")
head(clean_data15_16)


# Find the goals of 2017_18
goals17_18 <- Data_17_18 %>%
  dplyr::select(Player, Goals)

head(goals17_18)
dim(goals17_18)

# Join goals17_18 with the 2016-17 data
clean_data16_17 <- inner_join(Data_16_17,
                              goals17_18,
                              by="Player")
head(clean_data16_17)


# Find the goals of 2018_19
goals18_19 <- Data_18_19 %>%
  dplyr::select(Player, Goals)

head(goals18_19)
dim(goals18_19)

# Join goals18_19 with the 2017-18 data
clean_data17_18 <- inner_join(Data_17_18,
                              goals18_19,
                              by="Player")
head(clean_data17_18)

# Combine the five seasons worth of data
hockey_data <- rbind(clean_data13_14, clean_data14_15, clean_data15_16, 
                     clean_data16_17, clean_data17_18)

view(hockey_data)

# Rename the Goals variables
hockey_data <- rename(hockey_data, Goals = Goals.x)
hockey_data <- rename(hockey_data, Future_Goals = Goals.y)
hockey_data <- rename(hockey_data, First_Assists = `First Assists`)
hockey_data <- rename(hockey_data, Second_Assists = `Second Assists`)
hockey_data <- rename(hockey_data, Rush_Attempts = `Rush Attempts`)
hockey_data <- rename(hockey_data, Rebounds_Created = `Rebounds Created`)
hockey_data <- rename(hockey_data, Off_Zone_Starts = `Off. Zone Starts`)
hockey_data <- rename(hockey_data, Neu_Zone_Starts = `Neu. Zone Starts`)
hockey_data <- rename(hockey_data, Def_Zone_Starts = `Def. Zone Starts`)
hockey_data <- rename(hockey_data, On_The_Fly_Starts = `On The Fly Starts`)

view(hockey_data)
head(hockey_data)

# Find the missing values by column
Missing_values <- sort(colSums(is.na(hockey_data)), decreasing = TRUE)
Missing_values <- Missing_values[Missing_values > 0]
print(sort(Missing_values), decreasing = TRUE)

-----------------------------------------------------------------------
# Prepare the data for the forwards 5v5 offense model
Forward_Offense <- hockey_data %>%
  filter(Position == "R"|Position == "L"|Position == "C") %>%
  dplyr::select(Player:Rebounds_Created, Takeaways, On_The_Fly_Starts, Off_Zone_Starts:Def_Zone_Starts, Future_Goals)

# Insert zero for the missing IPP values
Forward_Offense$IPP[is.na(Forward_Offense$IPP)] = 0

# Find the missing values by column
Missing_values <- sort(colSums(is.na(Forward_Offense)), decreasing = TRUE)
Missing_values <- Missing_values[Missing_values > 0]
print(sort(Missing_values), decreasing = TRUE)

# Find the dimensions of the data
dim(Forward_Offense)

# Filtering for games played and teams played
Forward_Offense <- hockey_data %>%
  filter(Position == "R"|Position == "L"|Position == "C", GP > 10) %>%
  filter(str_detect(Team, pattern=",", negate=TRUE)) %>%
  dplyr::select(Player:Rebounds_Created, Takeaways, On_The_Fly_Starts, Off_Zone_Starts:Def_Zone_Starts, Future_Goals)

# Find the dimensions of the data after filtering by GP and Team
dim(Forward_Offense)

view(Forward_Offense)
head(Forward_Offense)

# Create a correlation matrix for the variables
Forward_Offense_Matrix <- Forward_Offense %>%
  dplyr::select(Goals:Def_Zone_Starts)

library("GGally")
ggpairs(Forward_Offense_Matrix)

# Add one goal to the future goals 
Forward_Offense <- Forward_Offense %>%
  mutate(Future_Goals = Future_Goals + 1)

view(Forward_Offense)

# Drop unwanted columns
Forward_Offense <- Forward_Offense[, -c(7,10,12,13,15)]

view(Forward_Offense)
head(Forward_Offense)

---------------------------------------------------------------------
# Regression analysis 
# Splitting data into training and validation sets
set.seed(1)
train.rows.FO <- sample(rownames(Forward_Offense), nrow(Forward_Offense) * 0.7)
train.data.FO <- Forward_Offense[train.rows.FO, ]
valid.rows.FO <- setdiff(rownames(Forward_Offense), train.rows.FO)
valid.data.FO <- Forward_Offense[valid.rows.FO, ]

# Create a MLR model that contains the full desired predictors 
FOM_Full <- lm(Future_Goals ~ Goals + First_Assists + Second_Assists +
                            + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
                            + Rebounds_Created + Takeaways + On_The_Fly_Starts + 
                              Off_Zone_Starts + Neu_Zone_Starts
                            + Def_Zone_Starts, data = train.data.FO)
# Summary
summary(FOM_Full) 

# Get list of residuals 
Res_FOM_Full <- resid(FOM_Full)

# Produce residual vs. fitted plot
plot(fitted(FOM_Full), Res_FOM_Full)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FOM_Full)

# Add a straight diagonal line to the plot
qqline(Res_FOM_Full) 

# Create density plot of residuals
plot(density(Res_FOM_Full))

# Calculate the error
library(forecast)
accuracy(FOM_Full)

# Out-of-sample Prediction
pred_FOM_Full <- predict(FOM_Full, newdata=valid.data.FO)
pred_FOM_Full

# Find the error of the validation data
accuracy(pred_FOM_Full, valid.data.FO$Future_Goals)

# Applying Box-Cox transformation to lm object
library(MASS)
library(tidyverse)
bc.results=boxcox(FOM_Full) #Applying Box-Cox transformation to lm object
lambda=bc.results$x[which.max(bc.results$y)] #extract the best lambda
lambda

lambda_FOM_Full <-lm(sqrt(Future_Goals) ~ Goals + First_Assists + Second_Assists 
                     + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
                     + Rebounds_Created + Takeaways + On_The_Fly_Starts + 
                       Off_Zone_Starts + Neu_Zone_Starts
                     + Def_Zone_Starts, data = train.data.FO)
# Summary
summary(lambda_FOM_Full) 

# Get list of residuals 
lambda_Res_FOM_Full <- resid(lambda_FOM_Full)

# Produce residual vs. fitted plot
plot(fitted(lambda_FOM_Full), lambda_Res_FOM_Full)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(lambda_Res_FOM_Full)

# Add a straight diagonal line to the plot
qqline(lambda_Res_FOM_Full) 

# Create density plot of residuals
plot(density(lambda_Res_FOM_Full))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FOM_Full)

# Compute VIF
vif(lambda_FOM_Full)

# Calculate the error
library(forecast)
accuracy(lambda_FOM_Full)

# Out-of-sample Prediction
pred_lambda_FOM_Full <- predict(lambda_FOM_Full, newdata=valid.data.FO)

# Undo the previous transformations
Real_pred_lambda_FOM_Full <- pred_lambda_FOM_Full^2 - 1
Real_pred_lambda_FOM_Full

# Find the error of the validation data
accuracy(Real_pred_lambda_FOM_Full, valid.data.FO$Future_Goals)

# Best subset selection
library(leaps)
best_subset_FOM=regsubsets(sqrt(Future_Goals) ~ Goals + First_Assists + Second_Assists
                       + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
                       + Rebounds_Created + Takeaways + On_The_Fly_Starts + 
                         Off_Zone_Starts + Neu_Zone_Starts
                       + Def_Zone_Starts, data=train.data.FO, nvmax=15)

# Summarize the result
best_summary_FOM=summary(best_subset_FOM)
best_summary_FOM 

# Find the best Cp
p=2:16
Cp=cbind(p,best_summary_FOM$cp) #Pair p and Cp
colnames(Cp)=c('p','Cp') #Define column names 
print(Cp)

# Find BIC
p=2:16
BIC=cbind(p,best_summary_FOM$bic) #Pair p and BIC
colnames(BIC)=c('p','BIC') #Define column names 
print(BIC) 
plot(BIC,cex=2,pch=19) # Plot of BIC

# Find AIC
n=nrow(train.data.FO)
p=BIC[,1]
aic=BIC[,2]-p*log(n)+2*p #Need to compute AIC using BIC
AIC=cbind(2:16,aic) #Pair p and AIC
colnames(AIC)=c('p','AIC') #Define column names 
print(AIC) 
plot(AIC,cex=2,pch=19) #Plot of AIC


# Best Cp Model
FOM_BestCp <- lm(sqrt(Future_Goals) ~ Goals + First_Assists + iSCF + Takeaways + 
                   On_The_Fly_Starts + Off_Zone_Starts + Neu_Zone_Starts,data=train.data.FO)
# Summary
summary(FOM_BestCp)

# Get list of residuals 
Res_FOM_BestCp <- resid(FOM_BestCp)

# Produce residual vs. fitted plot
plot(fitted(FOM_BestCp), Res_FOM_BestCp)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FOM_BestCp)

# Add a straight diagonal line to the plot
qqline(Res_FOM_BestCp) 

# Create density plot of residuals
plot(density(Res_FOM_BestCp))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FOM_BestCp)

# Calculate the error
accuracy(FOM_BestCp)

# Out-of-sample Prediction
pred_FOM_BestCp <- predict(FOM_BestCp, newdata=valid.data.FO)

# Undo the previous transformations
Real_pred_FOM_BestCp <- pred_FOM_BestCp^2 - 1
Real_pred_FOM_BestCp

# Find the error of the validation data
accuracy(Real_pred_FOM_BestCp, valid.data.FO$Future_Goals)

# Best BIC Model
FOM_BestBIC <- lm(sqrt(Future_Goals) ~ Goals + iSCF + Takeaways + 
                    On_The_Fly_Starts + Off_Zone_Starts,data=train.data.FO)
# Summary
summary(FOM_BestBIC)

# Get list of residuals 
Res_FOM_BestBIC <- resid(FOM_BestBIC)

# Produce residual vs. fitted plot
plot(fitted(FOM_BestBIC), Res_FOM_BestBIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FOM_BestBIC)

# Add a straight diagonal line to the plot
qqline(Res_FOM_BestBIC) 

# Create density plot of residuals
plot(density(Res_FOM_BestBIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FOM_BestBIC)

# Calculate the error
accuracy(FOM_BestBIC)

# Out-of-sample Prediction
pred_FOM_BestBIC <- predict(FOM_BestBIC, newdata = valid.data.FO)

# Undo the previous transformations
Real_pred_FOM_BestBIC <- pred_FOM_BestBIC^2 - 1
Real_pred_FOM_BestBIC

# Find the error of the validation data
accuracy(Real_pred_FOM_BestBIC, valid.data.FO$Future_Goals)

# Best AIC Model
FOM_BestAIC <- lm(sqrt(Future_Goals) ~ Goals + First_Assists + iSCF + Takeaways + 
                    On_The_Fly_Starts + Off_Zone_Starts + Neu_Zone_Starts,data=train.data.FO)
# Summary
summary(FOM_BestAIC)

# Get list of residuals 
Res_FOM_BestAIC <- resid(FOM_BestAIC)

# Produce residual vs. fitted plot
plot(fitted(FOM_BestAIC), Res_FOM_BestAIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FOM_BestAIC)

# Add a straight diagonal line to the plot
qqline(Res_FOM_BestAIC) 

# Create density plot of residuals
plot(density(Res_FOM_BestAIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FOM_BestAIC)

# Calculate the error
accuracy(FOM_BestAIC)

# Out-of-sample Prediction
pred_FOM_BestAIC <- predict(FOM_BestAIC, newdata = valid.data.FO)

# Undo the previous transformations
Real_pred_FOM_BestAIC <- pred_FOM_BestAIC^2 - 1
Real_pred_FOM_BestAIC

# Find the error of the validation data
accuracy(Real_pred_FOM_BestAIC, valid.data.FO$Future_Goals)

----------------------------------------------------------------
# Defense Offensive Model
# Combine the five seasons worth of data
hockey_data2 <- rbind(clean_data13_14, clean_data14_15, clean_data15_16, 
                     clean_data16_17, clean_data17_18)

view(hockey_data2)

# Rename the Goals variables
hockey_data2 <- rename(hockey_data2, Goals = Goals.x)
hockey_data2 <- rename(hockey_data2, Future_Goals = Goals.y)
hockey_data2 <- rename(hockey_data2, First_Assists = `First Assists`)
hockey_data2 <- rename(hockey_data2, Second_Assists = `Second Assists`)
hockey_data2 <- rename(hockey_data2, Rush_Attempts = `Rush Attempts`)
hockey_data2 <- rename(hockey_data2, Rebounds_Created = `Rebounds Created`)
hockey_data2 <- rename(hockey_data2, Off_Zone_Starts = `Off. Zone Starts`)
hockey_data2 <- rename(hockey_data2, Neu_Zone_Starts = `Neu. Zone Starts`)
hockey_data2 <- rename(hockey_data2, Def_Zone_Starts = `Def. Zone Starts`)
hockey_data2 <- rename(hockey_data2, On_The_Fly_Starts = `On The Fly Starts`)

view(hockey_data2)
head(hockey_data2)

# Find the missing values by column
Missing_values2 <- sort(colSums(is.na(hockey_data2)), decreasing = TRUE)
Missing_values2 <- Missing_values2[Missing_values2 > 0]
print(sort(Missing_values2), decreasing = TRUE)

library(tidyverse)
# Prepare the data for the forwards 5v5 offense model
Defense_Offensive <- hockey_data2 %>%
  filter(Position == "D") %>%
  dplyr::select(Player:Rebounds_Created, Takeaways, On_The_Fly_Starts, Off_Zone_Starts:Def_Zone_Starts, Future_Goals)

# Insert zero for the missing IPP values
Defense_Offensive$IPP[is.na(Defense_Offensive$IPP)] = 0

# Find the missing values by column
Missing_values <- sort(colSums(is.na(Defense_Offensive)), decreasing = TRUE)
Missing_values <- Missing_values[Missing_values > 0]
print(sort(Missing_values), decreasing = TRUE)

# Find the dimensions of the data
dim(Defense_Offensive)

# Filtering for games played and teams played
Defense_Offensive <- hockey_data %>%
  filter(Position == "D", GP > 10) %>%
  filter(str_detect(Team, pattern=",", negate=TRUE)) %>%
  dplyr::select(Player:Rebounds_Created, Takeaways, On_The_Fly_Starts, Off_Zone_Starts:Def_Zone_Starts, Future_Goals)

# Find the dimensions of the data after filtering by GP and Team
dim(Defense_Offensive)

view(Defense_Offensive)
head(Defense_Offensive)

# Create a correlation matrix for the variables
Defense_Offensive_Matrix <- Defense_Offensive %>%
  dplyr::select(Goals:Def_Zone_Starts)

library("GGally")
ggpairs(Defense_Ofensive_Matrix)

# Add one goal to the future goals 
Defense_Offensive <- Defense_Offensive %>%
  mutate(Future_Goals = Future_Goals + 1)

view(Defense_Offensive)

# Drop unwanted columns
Defense_Offensive <- Defense_Offensive[, -c(7,10,12,13,15)]

view(Defense_Offensive)
head(Defense_Offensive)

# Regression analysis 
# Splitting data into training and validation sets
set.seed(1)
train.rows.DO <- sample(rownames(Defense_Offensive), nrow(Defense_Offensive) * 0.7)
train.data.DO <- Defense_Offensive[train.rows.DO, ]
valid.rows.DO <- setdiff(rownames(Defense_Offensive), train.rows.DO)
valid.data.DO <- Defense_Offensive[valid.rows.DO, ]

# Create a MLR model that contains the full desired predictors 
DOM_Full <- lm(Future_Goals ~ Goals + First_Assists + Second_Assists 
               + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
               + Rebounds_Created + Takeaways + On_The_Fly_Starts + 
                 Off_Zone_Starts + Neu_Zone_Starts
               + Def_Zone_Starts, data = train.data.DO)
# Summary
summary(DOM_Full) 

# Get list of residuals 
Res_DOM_Full <- resid(DOM_Full)

# Produce residual vs. fitted plot
plot(fitted(DOM_Full), Res_DOM_Full)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DOM_Full)

# Add a straight diagonal line to the plot
qqline(Res_DOM_Full) 

# Create density plot of residuals
plot(density(Res_DOM_Full))

# Calculate the error
accuracy(DOM_Full)

# Out-of-sample Prediction
pred_DOM_Full <- predict(DOM_Full, newdata=valid.data.DO)
pred_DOM_Full

# Find the error of the validation data
accuracy(pred_DOM_Full, valid.data.DO$Future_Goals)

# Applying Box-Cox transformation to lm object
library(MASS)
bc.results=boxcox(DOM_Full) #Applying Box-Cox transformation to lm object
lambda=bc.results$x[which.max(bc.results$y)] #extract the best lambda
lambda

lambda_DOM_Full <-lm(log(Future_Goals) ~ Goals + First_Assists + Second_Assists 
                     + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
                     + Rebounds_Created + Takeaways + On_The_Fly_Starts + 
                       Off_Zone_Starts + Neu_Zone_Starts
                     + Def_Zone_Starts, data = train.data.DO)
# Summary
summary(lambda_DOM_Full) 

# Get list of residuals 
lambda_Res_DOM_Full <- resid(lambda_DOM_Full)

# Produce residual vs. fitted plot
plot(fitted(lambda_DOM_Full), lambda_Res_DOM_Full)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(lambda_Res_DOM_Full)

# Add a straight diagonal line to the plot
qqline(lambda_Res_DOM_Full) 

# Create density plot of residuals
plot(density(lambda_Res_DOM_Full))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DOM_Full)

# Compute VIF
vif(lambda_DOM_Full)

# Calculate the error
accuracy(lambda_DOM_Full)

# Out-of-sample Prediction
pred_lambda_DOM_Full <- predict(lambda_DOM_Full, newdata=valid.data.DO)

# Undo the previous transformations
MSE_Lambda_DOM_Full <- 0.5873^2
Real_pred_lambda_DOM_Full <- exp(pred_lambda_DOM_Full + 1/2*MSE_Lambda_DOM_Full) - 1
Real_pred_lambda_DOM_Full

# Find the error of the validation data
accuracy(Real_pred_lambda_DOM_Full, valid.data.DO$Future_Goals)

# Best subset selection
library(leaps)
best_subset_DOM=regsubsets(log(Future_Goals) ~ Goals + First_Assists + Second_Assists 
                       + IPP + ixG + iFF + iSCF + iHDCF + Rush_Attempts 
                       + Rebounds_Created + Takeaways + On_The_Fly_Starts + 
                         Off_Zone_Starts + Neu_Zone_Starts
                       + Def_Zone_Starts, data = train.data.DO, nvmax=15 )

# Summarize the result
best_summary_DOM=summary(best_subset_DOM)
best_summary_DOM 

# Find the best Cp
p=2:16
Cp=cbind(p,best_summary_DOM$cp) #Pair p and Cp
colnames(Cp)=c('p','Cp') #Define column names 
print(Cp)

# Find BIC
p=2:16
BIC=cbind(p,best_summary_DOM$bic) #Pair p and BIC
colnames(BIC)=c('p','BIC') #Define column names 
print(BIC) 
plot(BIC,cex=2,pch=19) # Plot of BIC

# Find AIC
n=nrow(train.data.DO)
p=BIC[,1]
aic=BIC[,2]-p*log(n)+2*p #Need to compute AIC using BIC
AIC=cbind(2:16,aic) #Pair p and AIC
colnames(AIC)=c('p','AIC') #Define column names 
print(AIC) 
plot(AIC,cex=2,pch=19) #Plot of AIC


# Best Cp Model
DOM_BestCp <- lm(log(Future_Goals) ~ Goals + First_Assists + IPP + ixG + iSCF + iHDCF 
                  + Rush_Attempts + Rebounds_Created + On_The_Fly_Starts + Off_Zone_Starts + 
                   Neu_Zone_Starts + Def_Zone_Starts, data = train.data.DO)
# Summary
summary(DOM_BestCp)

# Get list of residuals 
Res_DOM_BestCp <- resid(DOM_BestCp)

# Produce residual vs. fitted plot
plot(fitted(DOM_BestCp), Res_DOM_BestCp)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DOM_BestCp)

# Add a straight diagonal line to the plot
qqline(Res_DOM_BestCp) 

# Create density plot of residuals
plot(density(Res_DOM_BestCp))

# Calculate the error
accuracy(DOM_BestCp)

# Out-of-sample Prediction
pred_DOM_BestCp <- predict(DOM_BestCp, newdata=valid.data.DO)

# Undo the previous transformations
MSE_DOM_BestCp <- 0.5861^2
Real_pred_DOM_BestCp <- exp(pred_DOM_BestCp + 1/2*MSE_DOM_BestCp) - 1
Real_pred_DOM_BestCp

# Find the error of the validation data
accuracy(Real_pred_DOM_BestCp, valid.data.DO$Future_Goals)

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DOM_BestCp)

# Best BIC Model
DOM_BestBIC <- lm(log(Future_Goals) ~ ixG + iHDCF, data = train.data.DO)

# Summary
summary(DOM_BestBIC)

# Get list of residuals 
Res_DOM_BestBIC <- resid(DOM_BestBIC)

# Produce residual vs. fitted plot
plot(fitted(DOM_BestBIC), Res_DOM_BestBIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DOM_BestBIC)

# Add a straight diagonal line to the plot
qqline(Res_DOM_BestBIC) 

# Create density plot of residuals
plot(density(Res_DOM_BestBIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DOM_BestBIC)

# Calculate the error
accuracy(DOM_BestBIC)

# Out-of-sample Prediction
pred_DOM_BestBIC <- predict(DOM_BestBIC, newdata=valid.data.DO)

# Undo the previous transformations
MSE_DOM_BestBIC <- 0.5863^2
Real_pred_DOM_BestBIC <- exp(pred_DOM_BestBIC + 1/2*MSE_DOM_BestBIC) - 1
Real_pred_DOM_BestBIC

# Find the error of the validation data
accuracy(Real_pred_DOM_BestBIC, valid.data.DO$Future_Goals)

# Best AIC Model
DOM_BestAIC <- lm(log(Future_Goals) ~ ixG + iHDCF + On_The_Fly_Starts + Neu_Zone_Starts, data = train.data.DO)

# Summary
summary(DOM_BestAIC)

# Get list of residuals 
Res_DOM_BestAIC <- resid(DOM_BestAIC)

# Produce residual vs. fitted plot
plot(fitted(DOM_BestAIC), Res_DOM_BestAIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DOM_BestAIC)

# Add a straight diagonal line to the plot
qqline(Res_DOM_BestAIC) 

# Create density plot of residuals
plot(density(Res_DOM_BestAIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DOM_BestAIC)

# Calculate the error
accuracy(DOM_BestAIC)

# Out-of-sample Prediction
pred_DOM_BestAIC <- predict(DOM_BestAIC, newdata=valid.data.DO)

# Undo the previous transformations
MSE_DOM_BestAIC <- 0.5842^2
Real_pred_DOM_BestAIC <- exp(pred_DOM_BestAIC + 1/2*MSE_DOM_BestAIC) - 1
Real_pred_DOM_BestAIC

# Find the error of the validation data
accuracy(Real_pred_DOM_BestAIC, valid.data.DO$Future_Goals)

-------------------------------------------------------------------
# Defensive performance at 5v5
# Combine statistics with the next years actual goals against
# Find the goals of 2014_15
Goals_Against_14_15 <- Data_14_15 %>%
  dplyr::select(Player, GA)

head(Goals_Against_14_15)
dim(Goals_Against_14_15)

# Join goals14_15 with the 2013-14 data
clean_data13_14_Def <- inner_join(Data_13_14,
                              Goals_Against_14_15,
                              by="Player")
head(clean_data13_14_Def)

# Find the goals of 2015_16
Goals_Against_15_16 <- Data_15_16 %>%
  dplyr::select(Player, GA)

head(Goals_Against_15_16)
dim(Goals_Against_15_16)

# Join goals15_16 with the 2014-15 data
clean_data14_15_Def <- inner_join(Data_14_15,
                              Goals_Against_15_16,
                              by="Player")
head(clean_data14_15_Def)

# Find the goals of 2016_17
Goals_Against_16_17 <- Data_16_17 %>%
  dplyr::select(Player, GA)

head(Goals_Against_16_17)
dim(Goals_Against_16_17)

# Join goals16_17 with the 2015-16 data
clean_data15_16_Def <- inner_join(Data_15_16,
                              Goals_Against_16_17,
                              by="Player")
head(clean_data15_16)


# Find the goals of 2017_18
Goals_Against_17_18 <- Data_17_18 %>%
  dplyr::select(Player, GA)

head(Goals_Against_17_18)
dim(Goals_Against_17_18)

# Join goals17_18 with the 2016-17 data
clean_data16_17_Def <- inner_join(Data_16_17,
                              Goals_Against_17_18,
                              by="Player")
head(clean_data16_17_Def)


# Find the goals of 2018_19
Goals_Against_18_19 <- Data_18_19 %>%
  dplyr::select(Player, GA)

head(Goals_Against_18_19)
dim(Goals_Against_18_19)

# Join goals18_19 with the 2017-18 data
clean_data17_18_Def <- inner_join(Data_17_18,
                              Goals_Against_18_19,
                              by="Player")
head(clean_data17_18_Def)

# Combine the five seasons worth of data
hockey_data3 <- rbind(clean_data13_14_Def, clean_data14_15_Def, clean_data15_16_Def, 
                     clean_data16_17_Def, clean_data17_18_Def)

view(hockey_data3)

# Rename the Goals variables
hockey_data3 <- rename(hockey_data3, Goals_Against = GA.x)
hockey_data3 <- rename(hockey_data3, Future_Goals_Against = GA.y)
hockey_data3 <- rename(hockey_data3, First_Assists = `First Assists`)
hockey_data3 <- rename(hockey_data3, Second_Assists = `Second Assists`)
hockey_data3 <- rename(hockey_data3, Rush_Attempts = `Rush Attempts`)
hockey_data3 <- rename(hockey_data3, Rebounds_Created = `Rebounds Created`)
hockey_data3 <- rename(hockey_data3, Off_Zone_Starts = `Off. Zone Starts`)
hockey_data3 <- rename(hockey_data3, Neu_Zone_Starts = `Neu. Zone Starts`)
hockey_data3 <- rename(hockey_data3, Def_Zone_Starts = `Def. Zone Starts`)
hockey_data3 <- rename(hockey_data3, On_The_Fly_Starts = `On The Fly Starts`)

view(hockey_data3)
head(hockey_data3)

# Find the missing values by column
Missing_values3 <- sort(colSums(is.na(hockey_data3)), decreasing = TRUE)
Missing_values3 <- Missing_values3[Missing_values3 > 0]
print(sort(Missing_values3), decreasing = TRUE)

# Prepare the data for the forwards 5v5 defensive model
Forward_Defensive <- hockey_data3 %>%
  filter(Position == "R"|Position == "L"|Position == "C") %>%
  dplyr::select(Player:TOI,Giveaways:Takeaways,CA,xGA,SCA,HDCA,HDGA,PDO,On_The_Fly_Starts,
                Off_Zone_Starts:Def_Zone_Starts,Future_Goals_Against)

# Find the missing values by column
Missing_values3 <- sort(colSums(is.na(Forward_Defensive)), decreasing = TRUE)
Missing_values3 <- Missing_values3[Missing_values3 > 0]
print(sort(Missing_values3), decreasing = TRUE)

# Find the dimensions of the data
dim(Forward_Defensive)

# Filtering for games played and teams played
Forward_Defensive <- hockey_data3 %>%
  filter(Position == "R"|Position == "L"|Position == "C", GP > 10) %>%
  filter(str_detect(Team, pattern=",", negate=TRUE)) %>%
  dplyr::select(Player:TOI,Giveaways:Takeaways,CA,xGA,SCA,HDCA,HDGA,PDO,On_The_Fly_Starts,
                Off_Zone_Starts:Def_Zone_Starts,Future_Goals_Against)

# Find the dimensions of the data after filtering by GP and Team
dim(Forward_Defensive)

view(Forward_Defensive)
head(Forward_Defensive)

# Regression analysis 
# Splitting data into training and validation sets
set.seed(1)
train.rows.FD <- sample(rownames(Forward_Defensive), nrow(Forward_Defensive) * 0.7)
train.data.FD <- Forward_Defensive[train.rows.FD, ]
valid.rows.FD <- setdiff(rownames(Forward_Defensive), train.rows.FD)
valid.data.FD <- Forward_Defensive[valid.rows.FD, ]

# Create a MLR model that contains the full desired predictors 
FDM_Full <- lm(Future_Goals_Against ~ Giveaways + CA + xGA + SCA + 
                 HDCA + On_The_Fly_Starts + Neu_Zone_Starts
               + Def_Zone_Starts, data=train.data.FD)
# Summary
summary(FDM_Full) 

# Get list of residuals 
Res_FDM_Full <- resid(FDM_Full)

# Produce residual vs. fitted plot
plot(fitted(FDM_Full), Res_FDM_Full)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FDM_Full)

# Add a straight diagonal line to the plot
qqline(Res_FDM_Full) 

# Create density plot of residuals
plot(density(Res_FDM_Full))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FDM_Full)

# Calculate the error
library(forecast)
accuracy(FDM_Full)

# Out-of-sample Prediction
pred_FDM_Full <- predict(FDM_Full, newdata=valid.data.FD)
pred_FDM_Full

# Find the error of the validation data
accuracy(pred_FDM_Full, valid.data.FD$Future_Goals_Against)

# Best subset selection
library(leaps)
best_subset_FDM=regsubsets(Future_Goals_Against ~ Giveaways + CA + xGA + 
                             SCA + HDCA + On_The_Fly_Starts + Neu_Zone_Starts
                           + Def_Zone_Starts, data=train.data.FD, nvmax=8)

# Summarize the result
best_summary_FDM=summary(best_subset_FDM)
best_summary_FDM 

# Find the best Cp
p=2:9
Cp=cbind(p,best_summary_FDM$cp) #Pair p and Cp
colnames(Cp)=c('p','Cp') #Define column names 
print(Cp)

# Find BIC
p=2:9
BIC=cbind(p,best_summary_FDM$bic) #Pair p and BIC
colnames(BIC)=c('p','BIC') #Define column names 
print(BIC) 
plot(BIC,cex=2,pch=19) # Plot of BIC

# Find AIC
n=nrow(train.data.FD)
p=BIC[,1]
aic=BIC[,2]-p*log(n)+2*p #Need to compute AIC using BIC
AIC=cbind(2:9,aic) #Pair p and AIC
colnames(AIC)=c('p','AIC') #Define column names 
print(AIC) 
plot(AIC,cex=2,pch=19) #Plot of AIC


# Best Cp Model
FDM_BestCp <- lm(Future_Goals_Against ~ Giveaways + CA + HDCA + On_The_Fly_Starts +
                   Neu_Zone_Starts + Def_Zone_Starts,data=train.data.FD)
# Summary
summary(FDM_BestCp)

# Get list of residuals 
Res_FDM_BestCp <- resid(FDM_BestCp)

# Produce residual vs. fitted plot
plot(fitted(FDM_BestCp), Res_FDM_BestCp)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FDM_BestCp)

# Add a straight diagonal line to the plot
qqline(Res_FDM_BestCp) 

# Create density plot of residuals
plot(density(Res_FDM_BestCp))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FDM_BestCp)

# Calculate the error
accuracy(FDM_BestCp)

# Out-of-sample Prediction
pred_FDM_BestCp <- predict(FDM_BestCp, newdata = valid.data.FD)
pred_FDM_BestCp

# Find the error of the validation data
accuracy(pred_FDM_BestCp, valid.data.FD$Future_Goals_Against)

# Best BIC Model
FDM_BestBIC <- lm(Future_Goals_Against ~ Giveaways + xGA + On_The_Fly_Starts,data=train.data.FD)

# Summary
summary(FDM_BestBIC)

# Get list of residuals 
Res_FDM_BestBIC <- resid(FDM_BestBIC)

# Produce residual vs. fitted plot
plot(fitted(FDM_BestBIC), Res_FDM_BestBIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FDM_BestBIC)

# Add a straight diagonal line to the plot
qqline(Res_FDM_BestBIC) 

# Create density plot of residuals
plot(density(Res_FDM_BestBIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FDM_BestBIC)

# Calculate the error
accuracy(FDM_BestBIC)

# Out-of-sample Prediction
pred_FDM_BestBIC <- predict(FDM_BestBIC, newdata=valid.data.FD)
pred_FDM_BestBIC

# Find the error of the validation data
accuracy(pred_FDM_BestBIC, valid.data.FD$Future_Goals_Against)

# Best AIC Model
FDM_BestAIC <- lm(Future_Goals_Against ~ Giveaways + CA + HDCA + On_The_Fly_Starts + 
                    Neu_Zone_Starts + Def_Zone_Starts, data=train.data.FD)

# Summary
summary(FDM_BestAIC)

# Get list of residuals 
Res_FDM_BestAIC <- resid(FDM_BestAIC)

# Produce residual vs. fitted plot
plot(fitted(FDM_BestAIC), Res_FDM_BestAIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_FDM_BestAIC)

# Add a straight diagonal line to the plot
qqline(Res_FDM_BestAIC) 

# Create density plot of residuals
plot(density(Res_FDM_BestAIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(FDM_BestAIC)

# Calculate the error
accuracy(FDM_BestAIC)

# Out-of-sample Prediction
pred_FDM_BestAIC <- predict(FDM_BestAIC, newdata = valid.data.FD)
pred_FDM_BestAIC

# Find the error of the validation data
accuracy(pred_FDM_BestAIC, valid.data.FD$Future_Goals_Against)

# Combine the five seasons worth of data
hockey_data4 <- rbind(clean_data13_14_Def, clean_data14_15_Def, clean_data15_16_Def, 
                      clean_data16_17_Def, clean_data17_18_Def)

view(hockey_data4)

# Rename the Goals variables
hockey_data4 <- rename(hockey_data4, Goals_Against = GA.x)
hockey_data4 <- rename(hockey_data4, Future_Goals_Against = GA.y)
hockey_data4 <- rename(hockey_data4, First_Assists = `First Assists`)
hockey_data4 <- rename(hockey_data4, Second_Assists = `Second Assists`)
hockey_data4 <- rename(hockey_data4, Rush_Attempts = `Rush Attempts`)
hockey_data4 <- rename(hockey_data4, Rebounds_Created = `Rebounds Created`)
hockey_data4 <- rename(hockey_data4, Off_Zone_Starts = `Off. Zone Starts`)
hockey_data4 <- rename(hockey_data4, Neu_Zone_Starts = `Neu. Zone Starts`)
hockey_data4 <- rename(hockey_data4, Def_Zone_Starts = `Def. Zone Starts`)
hockey_data4 <- rename(hockey_data4, On_The_Fly_Starts = `On The Fly Starts`)

view(hockey_data4)
head(hockey_data4)

# Find the missing values by column
Missing_values4 <- sort(colSums(is.na(hockey_data4)), decreasing = TRUE)
Missing_values4 <- Missing_values4[Missing_values4 > 0]
print(sort(Missing_values4), decreasing = TRUE)

# Prepare the data for the forwards 5v5 defensive model
Defense_Defensive <- hockey_data4 %>%
  filter(Position == "D") %>%
  dplyr::select(Player:TOI,Giveaways:Takeaways,CA,xGA,SCA,HDCA,HDGA,PDO,On_The_Fly_Starts,
                Off_Zone_Starts:Def_Zone_Starts,Future_Goals_Against)

# Find the missing values by column
Missing_values4 <- sort(colSums(is.na(Defense_Defensive)), decreasing = TRUE)
Missing_values4 <- Missing_values4[Missing_values4 > 0]
print(sort(Missing_values4), decreasing = TRUE)

# Find the dimensions of the data
dim(Defense_Defensive)

# Filtering for games played and teams played
Defense_Defensive <- hockey_data4 %>%
  filter(Position == "D", GP > 10) %>%
  filter(str_detect(Team, pattern=",", negate=TRUE)) %>%
  dplyr::select(Player:TOI,Giveaways:Takeaways,CA,xGA,SCA,HDCA,HDGA,PDO,On_The_Fly_Starts,
                Off_Zone_Starts:Def_Zone_Starts,Future_Goals_Against)

# Find the dimensions of the data after filtering by GP and Team
dim(Defense_Defensive)

view(Defense_Defensive)
head(Defense_Defensive)

# Regression analysis 
# Splitting data into training and validation sets
set.seed(1)
train.rows.DD <- sample(rownames(Defense_Defensive), nrow(Defense_Defensive) * 0.7)
train.data.DD <- Defense_Defensive[train.rows.DD, ]
valid.rows.DD <- setdiff(rownames(Defense_Defensive), train.rows.DD)
valid.data.DD <- Defense_Defensive[valid.rows.DD, ]

# Create a MLR model that contains the full desired predictors 
DDM_Full <- lm(Future_Goals_Against ~ Giveaways + CA + xGA + SCA + 
                 HDCA + On_The_Fly_Starts + Neu_Zone_Starts
               + Def_Zone_Starts, data=train.data.DD)
# Summary
summary(DDM_Full) 

# Get list of residuals 
Res_DDM_Full <- resid(DDM_Full)

# Produce residual vs. fitted plot
plot(fitted(DDM_Full), Res_DDM_Full)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DDM_Full)

# Add a straight diagonal line to the plot
qqline(Res_DDM_Full) 

# Create density plot of residuals
plot(density(Res_DDM_Full))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DDM_Full)

# Calculate the error
library(forecast)
accuracy(DDM_Full)

# Out-of-sample Prediction
pred_DDM_Full <- predict(DDM_Full, newdata=valid.data.DD)
pred_DDM_Full

# Find the error of the validation data
accuracy(pred_DDM_Full, valid.data.DD$Future_Goals_Against)

# Best subset selection
library(leaps)
best_subset_DDM=regsubsets(Future_Goals_Against ~ Giveaways + CA + xGA + 
                             SCA + HDCA + On_The_Fly_Starts + Neu_Zone_Starts
                           + Def_Zone_Starts, data=train.data.DD, nvmax=8)

# Summarize the result
best_summary_DDM=summary(best_subset_DDM)
best_summary_DDM 

# Find the best Cp
p=2:9
Cp=cbind(p,best_summary_DDM$cp) #Pair p and Cp
colnames(Cp)=c('p','Cp') #Define column names 
print(Cp)

# Find BIC
p=2:9
BIC=cbind(p,best_summary_DDM$bic) #Pair p and BIC
colnames(BIC)=c('p','BIC') #Define column names 
print(BIC) 
plot(BIC,cex=2,pch=19) # Plot of BIC

# Find AIC
n=nrow(train.data.DD)
p=BIC[,1]
aic=BIC[,2]-p*log(n)+2*p #Need to compute AIC using BIC
AIC=cbind(2:9,aic) #Pair p and AIC
colnames(AIC)=c('p','AIC') #Define column names 
print(AIC) 
plot(AIC,cex=2,pch=19) #Plot of AIC


# Best Cp Model
DDM_BestCp <- lm(Future_Goals_Against ~ Giveaways + CA + xGA + SCA + 
                   On_The_Fly_Starts + Neu_Zone_Starts,data=train.data.DD)

# Summary
summary(DDM_BestCp)

# Get list of residuals 
Res_DDM_BestCp <- resid(DDM_BestCp)

# Produce residual vs. fitted plot
plot(fitted(DDM_BestCp), Res_DDM_BestCp)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DDM_BestCp)

# Add a straight diagonal line to the plot
qqline(Res_DDM_BestCp) 

# Create density plot of residuals
plot(density(Res_DDM_BestCp))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DDM_BestCp)

# Calculate the error
accuracy(DDM_BestCp)

# Out-of-sample Prediction
pred_DDM_BestCp <- predict(DDM_BestCp, newdata=valid.data.DD)
pred_DDM_BestCp

# Find the error of the validation data
accuracy(pred_DDM_BestCp, valid.data.DD$Future_Goals_Against)

# Best BIC Model
DDM_BestBIC <- lm(Future_Goals_Against ~ Giveaways + xGA + On_The_Fly_Starts + 
                  Neu_Zone_Starts,data=train.data.DD)

# Summary
summary(DDM_BestBIC)

# Get list of residuals 
Res_DDM_BestBIC <- resid(DDM_BestBIC)

# Produce residual vs. fitted plot
plot(fitted(DDM_BestBIC), Res_DDM_BestBIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DDM_BestBIC)

# Add a straight diagonal line to the plot
qqline(Res_DDM_BestBIC) 

# Create density plot of residuals
plot(density(Res_DDM_BestBIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DDM_BestBIC)

# Calculate the error
accuracy(DDM_BestBIC)

# Out-of-sample Prediction
pred_DDM_BestBIC <- predict(DDM_BestBIC, newdata=valid.data.DD)
pred_DDM_BestBIC

# Find the error of the validation data
accuracy(pred_DDM_BestBIC, valid.data.DD$Future_Goals_Against)

# Best AIC Model
DDM_BestAIC <- lm(Future_Goals_Against ~ Giveaways + xGA + SCA + On_The_Fly_Starts + 
                    Neu_Zone_Starts, data=train.data.DD)

# Summary
summary(DDM_BestAIC)

# Get list of residuals 
Res_DDM_BestAIC <- resid(DDM_BestAIC)

# Produce residual vs. fitted plot
plot(fitted(DDM_BestAIC), Res_DDM_BestAIC)

# Add a horizontal line at 0 
abline(0,0)

# Create Q-Q plot for residuals
qqnorm(Res_DDM_BestAIC)

# Add a straight diagonal line to the plot
qqline(Res_DDM_BestAIC) 

# Create density plot of residuals
plot(density(Res_DDM_BestAIC))

# Find VIF's
# Load the package
library(HH)

# Compute VIF
vif(DDM_BestAIC)

# Calculate the error
accuracy(DDM_BestAIC)

# Out-of-sample Prediction
pred_DDM_BestAIC <- predict(DDM_BestAIC, newdata=valid.data.DD)
pred_DDM_BestAIC

# Find the error of the validation data
accuracy(pred_DDM_BestAIC, valid.data.DD$Future_Goals_Against)

# Selecting the models
# Forward Offensive






















