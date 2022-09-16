# capstone

Nick Glass
12/3/2021

Overview:

This was my capstone project for my undergrad where I predicted NHL goals for players in the next season. For this project I decided to only focus on 5 on 5 offense and 5 on 5 defense broken down by forwards and defensemen since this is the most common state of the game. Also, I decided to focus on only regression models to predict the amount of goals the players will score the next season not the amount of goals the player will create. For the models that predict how the player will prevent goals I decided to predict how many goals are scored against the players team when that player is on the ice. After making these changes my new goal for the project was to become more familiar with hockey analytics models and regression methods so I can build more complex models in the future.

The data for this project is from Natural Stat Trick.

## Load NHL individual stats 2013-14
### 2013-14
IND_5V5_13_14 <- read_csv("Player_Season_Totals_13_14_IND_5V5.csv",
                          col_types = "cccciniiiiininniiiiiiiiiiiiiiiiiiin")
head(IND_5V5_13_14)

Delete unnecessary columns
IND_5V5_13_14 <- IND_5V5_13_14[,-1]

Load NHL on-ice stats 2013-14
OI_5V5_13_14 <- read_csv("Player_Season_Totals_13_14_OI_5V5.csv",
                         col_types = "cccciniiniiniiniinnnniiniiniiniiniiniiniinnnniiiiniiin")
head(OI_5V5_13_14)

Delete unnecessary columns
OI_5V5_13_14 <- OI_5V5_13_14[, -c(1,3,4,5,6)]

Join the two tables for 2013-14
Data_13_14 <- merge(x=IND_5V5_13_14,y=OI_5V5_13_14,by="Player",all=TRUE)
head(Data_13_14)

Join the two tables for 2018-19
Data_18_19 <- merge(x=IND_5V5_18_19,y=OI_5V5_18_19,by="Player",all=TRUE)

The data selected is of NHL players from 2013-2019. Each year of data was loaded separately with two data files per season. The above code shows the steps taken to load the data. Each year of data was loaded the same way. One of the types of data includes individual statistics and the other includes counts of statistics while the player was on the ice. These data sets were merged together so there would be only one set of data per year. 

Create the response variables column:

### Combine statistics with the next years actual goals 
Find the goals of 2014_15
goals14_15 <- Data_14_15 %>%
  dplyr::select(Player, Goals)

head(goals14_15)

### Combine the five seasons worth of data
hockey_data <- rbind(clean_data13_14, clean_data14_15, clean_data15_16,clean_data16_17, clean_data17_18)

For the models that predict a players offense the data was joined with the count of goals the players scored in the following year. This new column was named future goals and is the response variable for the offensive models. Each of the years were then combined to form a single new data frame that contains all of the necessary information called hockey_data. This data set includes 3646 observations or players and 83 columns or variables.

Preparing the data for the forward offensive model:

### Rename the Goals variables
hockey_data <- rename(hockey_data, Goals = Goals.x)
hockey_data <- rename(hockey_data, Future_Goals = Goals.y)
hockey_data <- rename(hockey_data, First_Assists = `First Assists`)
hockey_data <- rename(hockey_data, Second_Assists = `Second Assists`)
hockey_data <- rename(hockey_data, Rush_Attempts = `Rush Attempts`)
hockey_data <- rename(hockey_data, Rebounds_Created = `Rebounds Created`)
hockey_data <- rename(hockey_data, Off_Zone_Starts = `Off. Zone Starts`)
hockey_data <- rename(hockey_data, Neu_Zone_Starts = `Neu. Zone Starts`)
hockey_data <- rename(hockey_data, Def_Zone_Starts = `Def. Zone Starts`)
hockey_data <- rename(hockey_data, On_The_Fly_Starts = `On The Fly Starts`)

### Find the missing values by column
Missing_values <- sort(colSums(is.na(hockey_data)), decreasing = TRUE)
Missing_values <- Missing_values[Missing_values > 0]
print(sort(Missing_values), decreasing = TRUE)

### Prepare the data for the forwards 5v5 offense model
Forward_Offense <- hockey_data %>%
  filter(Position == "R"|Position == "L"|Position == "C") %>%
  dplyr::select(Player:Rebounds_Created,Takeaways,On_The_Fly_Starts,Off_Zone_Starts:Def_Zone_Starts,Future_Goals)

### Insert zero for the missing IPP values
Forward_Offense$IPP[is.na(Forward_Offense$IPP)] = 0

Find the dimensions of the data
dim(Forward_Offense)

### Filtering for games played and teams played
Forward_Offense <- hockey_data %>%
  filter(Position == "R"|Position == "L"|Position == "C", GP > 10) %>%
  filter(str_detect(Team, pattern=",", negate=TRUE)) %>%
  dplyr::select(Player:Rebounds_Created,Takeaways,On_The_Fly_Starts,Off_Zone_Starts:Def_Zone_Starts,Future_Goals)

#### Find the dimensions of the data after filtering by GP and Team
dim(Forward_Offense)

### Add one goal to the future goals 
Forward_Offense <- Forward_Offense %>%
  mutate(Future_Goals = Future_Goals + 1)

#### Drop unwanted columns
Forward_Offense <- Forward_Offense[, -c(7,10,12,13,15)]
head(Forward_Offense)

Before the regression models could be made there needed to be some preprocessing. First, some variables were renamed to make them more convenient to work with. Second, the number of missing variables were identified and dealt with. For IPP which is the individual points percentage of the player, some values were NA when they should have been 0. In this case the value of 0 was imputed for the NA values. It turns out that all of the other variables that had missing values were not used anyway in the analysis. Third, the data set was filtered to only included centers, left wings, and right wings, otherwise known as forwards. The full data set for forwards includes 2291 observations or players and 26 columns or variables. In the subset of the data after filtering out players who played with more than one team in a season or played in less then 10 games, there is 1856 observations and 26 variables. After dropping unwanted variables the data set includes Player, Team, Position, GP, TOI, Goals, First Assists, Second Assists, IPP, ixG, iFF, iSCF, iHDCF, Rush_Attempts, Rebounds_Created, Takeaways, On_The_Fly_Starts, Neu_Zone_Starts, Def_Zone_Starts, and Future_Goals. Finally, one goal was added to the Future goals column in order to run the box cox transformation in the future.

Creating the models to predict the forwards offense:

## Regression analysis 
Splitting data into training and validation sets
set.seed(1)
train.rows.FO <- sample(rownames(Forward_Offense), nrow(Forward_Offense) * 0.7)
train.data.FO <- Forward_Offense[train.rows.FO, ]
valid.rows.FO <- setdiff(rownames(Forward_Offense), train.rows.FO)
valid.data.FO <- Forward_Offense[valid.rows.FO, ]

Create a MLR model that contains the full desired predictors 
FOM_Full <- lm(Future_Goals ~ Goals + First_Assists + Second_Assists +
                            + IPP + ixG + iFF + iSCF + iHDCF +       
                            Rush_Attempts + Rebounds_Created + 
                            Takeaways + On_The_Fly_Starts + 
                            Off_Zone_Starts + Neu_Zone_Starts + 
                            Def_Zone_Starts, data=train.data.FO)

summary(FOM_Full) 

### Get list of residuals 
Res_FOM_Full <- resid(FOM_Full)

### Produce residual vs. fitted plot
plot(fitted(FOM_Full), Res_FOM_Full)

#### Add a horizontal line at 0 
abline(0,0)

![image](https://user-images.githubusercontent.com/113626253/190727314-b1bc6030-4f34-4ce7-bef8-bb52d348446d.png)

### Create Q-Q plot for residuals
qqnorm(Res_FOM_Full)

#### Add a straight diagonal line to the plot
qqline(Res_FOM_Full) 

![image](https://user-images.githubusercontent.com/113626253/190727381-7b65d2ed-e743-4606-bc9b-0b31f2a5f1ec.png)

### Create density plot of residuals
plot(density(Res_FOM_Full))

![image](https://user-images.githubusercontent.com/113626253/190727492-06b2cd53-ec5e-40a8-9667-9df0520871eb.png)

#### Calculate the error
accuracy(FOM_Full)

## Out-of-sample Prediction
pred_FOM_Full <- predict(FOM_Full, newdata=valid.data.FO)
pred_FOM_Full

#### Find the error of the validation data
accuracy(pred_FOM_Full, valid.data.FO$Future_Goals)

### Applying Box-Cox transformation to lm object
bc.results=boxcox(FOM_Full) # Applying Box-Cox transformation to lm object

![image](https://user-images.githubusercontent.com/113626253/190727713-2b17967c-e196-4036-b447-2aa5051d3ba6.png)

lambda=bc.results$x[which.max(bc.results$y)] #extract the best lambda
lambda

lambda_FOM_Full <-lm(sqrt(Future_Goals) ~ Goals + First_Assists +  
                      Second_Assists + IPP + ixG + iFF + iSCF + iHDCF +
                      Rush_Attempts + Rebounds_Created + Takeaways + 
                      On_The_Fly_Starts + Off_Zone_Starts +
                      Neu_Zone_Starts +     
                      Def_Zone_Starts,data=train.data.FO)

summary(lambda_FOM_Full) 

#### Get list of residuals 
lambda_Res_FOM_Full <- resid(lambda_FOM_Full)

### Produce residual vs. fitted plot
plot(fitted(lambda_FOM_Full), lambda_Res_FOM_Full)

#### Add a horizontal line at 0 
abline(0,0)

![image](https://user-images.githubusercontent.com/113626253/190727897-bc220948-84b1-47bb-811b-60cd6b1222a0.png)

### Create Q-Q plot for residuals
qqnorm(lambda_Res_FOM_Full)

Add a straight diagonal line to the plot
qqline(lambda_Res_FOM_Full) 

![image](https://user-images.githubusercontent.com/113626253/190727941-aeaa247c-1dea-46c1-b32a-8ef0c3637419.png)

### Create density plot of residuals
plot(density(lambda_Res_FOM_Full))

![image](https://user-images.githubusercontent.com/113626253/190727994-3cc5ccf5-cea0-4f12-8fa7-41b6ba9acb8f.png)

#### Compute VIF
vif(FOM_Full)

vif(lambda_FOM_Full)

#### Calculate the error
accuracy(lambda_FOM_Full)

## Out-of-sample Prediction
pred_lambda_FOM_Full <- predict(lambda_FOM_Full, newdata=valid.data.FO)

#### Undo the previous transformations
Real_pred_lambda_FOM_Full <- pred_lambda_FOM_Full^2 - 1
Real_pred_lambda_FOM_Full

#### Find the error of the validation data
accuracy(Real_pred_lambda_FOM_Full, valid.data.FO$Future_Goals)

## Best subset selection
best_subset_FOM=regsubsets(sqrt(Future_Goals) ~ Goals + First_Assists +
                           Second_Assists + IPP + ixG + iFF + iSCF + 
                            iHDCF + Rush_Attempts + Rebounds_Created +
                            Takeaways + On_The_Fly_Starts + 
                            Off_Zone_Starts + Neu_Zone_Starts +
                          Def_Zone_Starts, data=train.data.FO,nvmax=15)

#### Summarize the result
best_summary_FOM=summary(best_subset_FOM)
best_summary_FOM 

### Find the best Cp
p=2:16
Cp=cbind(p,best_summary_FOM$cp) #Pair p and Cp
colnames(Cp)=c('p','Cp') #Define column names 
print(Cp)

### Find BIC
p=2:16
BIC=cbind(p,best_summary_FOM$bic) #Pair p and BIC
colnames(BIC)=c('p','BIC') #Define column names 
print(BIC) 

plot(BIC,cex=2,pch=19) # Plot of BIC

![image](https://user-images.githubusercontent.com/113626253/190728350-0e0a94f1-640c-4e08-bd21-9e82c312cc5d.png)

### Find AIC
n=nrow(train.data.FO)
p=BIC[,1]
aic=BIC[,2]-p*log(n)+2*p #Need to compute AIC using BIC
AIC=cbind(2:16,aic) #Pair p and AIC
colnames(AIC)=c('p','AIC') #Define column names 
print(AIC) 

![image](https://user-images.githubusercontent.com/113626253/190728417-ec17794d-6630-4f3a-8607-03ffcf77aa7e.png)


## Best Cp Model
FOM_BestCp <- lm(sqrt(Future_Goals) ~ Goals + First_Assists + iSCF + 
                   Takeaways + On_The_Fly_Starts + Off_Zone_Starts + 
                   Neu_Zone_Starts,data=train.data.FO)
                   
summary(FOM_BestCp)

vif(FOM_BestCp)

#### Calculate the error
accuracy(FOM_BestCp)

### Out-of-sample Prediction
pred_FOM_BestCp <- predict(FOM_BestCp, newdata=valid.data.FO)

#### Undo the previous transformations
Real_pred_FOM_BestCp <- pred_FOM_BestCp^2 - 1
Real_pred_FOM_BestCp

#### Find the error of the validation data
accuracy(Real_pred_FOM_BestCp, valid.data.FO$Future_Goals)

## Best BIC Model
FOM_BestBIC <- lm(sqrt(Future_Goals) ~ Goals + iSCF + Takeaways + 
                    On_The_Fly_Starts + 
                    Off_Zone_Starts,data=train.data.FO)
                    
summary(FOM_BestBIC)

vif(FOM_BestBIC)

### Calculate the error
accuracy(FOM_BestBIC)

### Out-of-sample Prediction
pred_FOM_BestBIC <- predict(FOM_BestBIC, newdata = valid.data.FO)

### Undo the previous transformations
Real_pred_FOM_BestBIC <- pred_FOM_BestBIC^2 - 1
Real_pred_FOM_BestBIC

### Find the error of the validation data
accuracy(Real_pred_FOM_BestBIC, valid.data.FO$Future_Goals)

## Best AIC Model
FOM_BestAIC <- lm(sqrt(Future_Goals) ~ Goals + First_Assists + iSCF + 
                    Takeaways + On_The_Fly_Starts + Off_Zone_Starts + 
                    Neu_Zone_Starts,data=train.data.FO)

summary(FOM_BestAIC)

vif(FOM_BestAIC)

### Calculate the error
accuracy(FOM_BestAIC)

### Out-of-sample Prediction
pred_FOM_BestAIC <- predict(FOM_BestAIC, newdata = valid.data.FO)

### Undo the previous transformations
Real_pred_FOM_BestAIC <- pred_FOM_BestAIC^2 - 1
Real_pred_FOM_BestAIC

### Find the error of the validation data
accuracy(Real_pred_FOM_BestAIC, valid.data.FO$Future_Goals)

I created five different regression models to predict the future goals for the forwards in the NHL. First, I separated the data into the training and validation sets with 70% of the data in the training set and 30% in the later. I then ran the full regression model with the variables being Goals, First_Assists, Second_Assists, IPP, ixG, iFF, iSCF, iHDCF, Rush_Attempts, Rebounds_Created, Takeaways, On_The_Fly_Starts, Off_Zone_Starts, Neu_Zone_Starts, and Def_Zone_Starts. This model had a solid adjusted r-square value but the residuals were not normally distributed. To fix this I ran the box cox transformation to find the ideal lambda and its corresponding transformation which happened to be the square root of the response variable. Next I re-ran the full model with the transformation and got a slightly worse adjusted r-square value but the residuals were fixed. I then ran the best subset model with the transformation to find the Cp, BIC, and AIC models. The Cp model and the AIC model turned out to be the same with 7 predictors which were all significant. The BIC model had 5 predictors with them all significant. After finding the VIF values it can be seen that all three of the sub models did not have multicollinearity but the full models did have issues with it. The last step was to undo the transformation and subtract the extra goal before predicting the future values and their error.

## Picking the model:

![image](https://user-images.githubusercontent.com/113626253/190728988-2c7f9e19-8638-46cd-8156-e578a281efeb.png)

For the forward offensive model it was tough to chose between the AIC/Cp model and the BIC but I decided to chose the BIC model as the best for a few reasons. First, both the RMSE and the MAE were the lowest even if they were close. Second, I know the BIC model had the lower of the adjusted r-square values but it is close enough to not make a huge difference. The deciding factor for me was that the BIC model is the simplest of the three. The predictors used for this model are Goals, iSCF, Takeaways, On_The_Fly_Starts, and Off_Zone_Starts. Each of these variables makes sense in a logical sense. I expected the number of goals, scoring chances, takeaways, and offensive zone starts to lead to future goals. I find it interesting that as the number of on the fly starts increases we expect the number of future goals to decrease. I feel this model does a good job predicting future goals without having extra variables like the AIC or Cp. This makes a difference when explaining and using the model in a practical setting like a hockey teams front office.

## Preparing the data for the defense offensive model:

The same process was used in the preprocessing for the defense offensive model as the forward offensive model with the exception that I filtered for defensemen instead of forwards. In this data set there are 1047 observations. Refer to the original steps for the process if needed.

Like the forward offensive model I created five different regression models to predict the future goals for the defensemen in the NHL. First, I separated the data into the training and validation sets with 70% of the data in the training set and 30% in the later. I then ran the full regression model with the same variables as before since I am still looking to predict players goals. These variables are Goals, First_Assists, Second_Assists, IPP, ixG, iFF, iSCF, iHDCF, Rush_Attempts, Rebounds_Created, Takeaways, On_The_Fly_Starts, Off_Zone_Starts, Neu_Zone_Starts, and Def_Zone_Starts. This model had a worse adjusted r-square value then the forward model but that can be expected since the model is for defensemen. Defensemen are known more for setting up goals then scoring them. Furthermore, the defensemen that do score a lot tend to do it on the power play not 5 on 5 meaning this model might not capture the full picture. like before the residuals were not normally distributed. To fix this I ran the box cox transformation to find the ideal lambda and its corresponding transformation which happened to be the log of the response variable. Next I re-ran the full model with the transformation and got a slightly worse adjusted r-square value but the residuals were much better. I then ran the best subset model with the transformation to find the Cp, BIC, and AIC models. After looking at the VIF values the Cp model and the full models had multicollinearity but the BIC and AIC models did not. The BIC model had 2 predictors and the AIC model had 4 predictors with them all being significant. The last part was undoing the log transformation and subtracting the goal that was added before predicting the future goals for defensemen. One value being observation 24 seemed to be extremely high for the data. I decided to keep the value since the error is really high and the player did score a high amount of goals the next season.

![image](https://user-images.githubusercontent.com/113626253/190729305-2f2a4ea4-5eda-468c-b186-b5a16d77ee24.png)

I choose the AIC model as the best model because it has a higher adjusted r-square value than the BIC model and has a lower RMSE and MAE. The hardest part for me in this decision was the fact that the BIC model only has 2 predictors while the AIC model has 4. I ended up going with the more complex model because it is better across the board. Furthermore, the AIC models predictors are the same as the BIC but with On_The_Fly_Starts and Neu_Zone_Starts added. I think it makes perfect sense that the model included ixG otherwise known as individual expected goals. This is a stat that measures the probability of a shot being a goal based on location and angle. I found it interesting that iHDCF or individual high danger chances was negative. I thought that having more dangerous chances would lead to more future goals. This is something I want to look into further in the future.

### Create the response variable column:

The preprocessing for the defensive models is almost the exact same as it was for the offensive models. The only difference is that for the defensive models the data for each year was joined with the amount of goals against that occurred while the player was on the ice the following season. This will be our response variable.

![image](https://user-images.githubusercontent.com/113626253/190729632-510975f4-8266-409c-a11f-93b25e6b4f26.png)

I really did not have much of a choice when picking the model because the BIC model is the only one that does not have multicollinearity. This model has the highest RMSE and MAE but also has the smallest number of predictors. This is definitely not ideal but the good news is the variables in the model make sense from a logical perspective. There are only three predictors in this model which are giveaways, XGA otherwise known as the probability of an opponents shot becoming a goal, and on the fly starts. It makes sense that the number of giveaways and the better the quality chances would be good predictors of future goals against. As for the on the fly starts it appears to have the same effect as previous models where it is negatively correlated. Overall this model seems to be okay given the amount of uncertainty that comes with a players defense.

## Preparing the data for the defense defensive model:

The preprocessing for the defensemen defensive model was almost the exact same as for the forward defensive model with the only difference being the data was filtered for defensemen. Like the other defensemen models there were 1047 observations.

![image](https://user-images.githubusercontent.com/113626253/190729748-bec5ed60-156b-48d1-aa9b-04310f21df71.png)

Like the forward defensive model I really did not have much of a choice when picking the model because the BIC model is the only one that does not have multicollinearity. This model has the lowest RMSE and the second lowest MAE but also has the smallest number of predictors. It is definitely not ideal that the error is so high but the good news is most of the variables in the model make sense from a logical perspective. There are only four predictors in this model which are giveaways, XGA otherwise known as the probability of an opponents shot becoming a goal, on the fly starts, and neutral zone starts. It makes sense that the number of giveaways and the better the quality chances would be good predictors of future goals against. As for the on the fly starts it appears to have the same effect as previous models where it is negatively correlated. The one variable that does not make sense is the neutral zone starts. I would have thought that having defensive zone starts would make more sense then neutral zone starts because that gives the opponent a better chance to score off the faceoff. One way to explain this is that coaches like to put there best defensive players on the ice for defensive zone starts which could lead to the team clearing the zone thus not getting scored on. Overall this model seems to be okay given the amount of uncertainty that comes with a players defense.

In conclusion I think the models created did a relatively good job at predicting a players future goals and goals against. I think in the future more models should be developed to further study this topic. I could possibly use ridge regression or lasso regression to deal with the multicollinearity. Furthermore, I could try using random forests or neural nets to get a better model. Another idea to improve the model is to use rates. I did try this idea briefly but the model had a really small adjusted r-square value so I abandon the idea. In the future I would like to add more models for different phases of the game such as power play and penalty kill. The last idea to improve the model is to find better data. The full report for this project can be found in my capstone repository. 



