library(tidyverse)
library(factoextra)
library(lubridate)
library(glmnet)
library(tree)
library(randomForest)

df <- read.csv("marketing_campaign.csv", sep = "\t", header = T)
# -------------------------------------------------------------------------------
# Preliminary process
df <- na.omit(df)
colSums(is.na(df)) # check if there is NA left
summary(df) # Summary Statistics
colnames(df)

# Combine/Calculate 
df['Age']= 2022 - df$Year_Birth
df['total_spent'] = df$MntMeatProducts + df$MntFishProducts + df$MntWines + 
  df$MntFruits + df$MntSweetProducts + df$MntGoldProds
df['Kid'] = df$Kidhome + df$Teenhome
df['accepted'] = df$AcceptedCmp1 + df$AcceptedCmp2 + df$AcceptedCmp3 + 
  df$AcceptedCmp4 + df$AcceptedCmp5

# ------------------------------------------------------------------#
education.type <- factor(df$Education, order=FALSE)
levels(education.type)
marital.type <- factor(df$Marital_Status, order=FALSE)
levels(marital.type)
df$Education <- as.numeric(factor(df$Education, order=FALSE)) - 1
df$Marital_Status <- as.numeric(factor(df$Marital_Status, order=FALSE)) - 1
df$Dt_Customer <- as.numeric(as.POSIXct(df$Dt_Customer, format="%d-%m-%Y"))

colnames(df)

set.seed(362)
index <- sample(nrow(df), nrow(df) * 0.5)


#Regression dataset
#response = MntFruits
df_response <- df[, c(11,3, 4, 5, 8, 9, 16:20, 26, 30,32)]
colnames(df_response)

ridge_x <- df_response[,-1]
ridge_y <- df_response[,1]

colnames(ridge_x) 


#linear regression: MntFruits
linear_fit <- lm(MntFruits~., data = df_response)
summary(linear_fit)
#Adjusted R-squared: 0.3638

#Prediction
ls_fit <- lm(MntFruits~., data = df_response[index, ])
summary(ls_fit)

predict(ls_fit, df_response[-index, ])
cor(predict(ls_fit, df_response[-index, ]), 
    df_response[-index, ]$MntFruits, use = "complete.obs")
#correlation: 0.5804942


#Ridge regression: MntFruits
ridge_reg_x <- model.matrix(MntFruits~., data = df_response)[, -1] 
ridge_reg_y <- df_response$MntFruits

ridge_cv <- cv.glmnet(x=ridge_reg_x[index, ],
                      y=ridge_reg_y[index], alpha = 0)
plot(ridge_cv)
ridge_best_lambda <- ridge_cv$lambda.min

# Refit the data with the best lambda
ridge_best <- glmnet(ridge_reg_x, ridge_reg_y, 
                     lambda = ridge_best_lambda, alpha = 0)

# form predictions
ridge_pred <- predict(ridge_best, 
                      s = ridge_best_lambda, newx = ridge_reg_x[-index, ])

#test error
mean((ridge_pred - ridge_reg_y[-index])^2)
#MSE = 1017.789

# Estimated regression coefficients
predict(ridge_best, type = "coefficients")[, 1]

#Random Forest
library(randomForest)

rf_fit <- randomForest(formula = MntFruits ~.,
                       data = df_response, 
                       mtry = (ncol(df_response[index, ]) - 1)/ 3, 
                       ntree = 1000, importance = TRUE)

rf_pred <- predict(rf_fit, df_response[-index, ])
mean((df_response[-index, ]$MntFruits - rf_pred)^2)
#[1] 165.4985

importance(rf_fit)
varImpPlot(rf_fit, 
           main ="Random Forest Variables Importance for Amount Spent on Fruits")


#------decision tree------#


colnames(df_response)
head(df_response)

# Fit regression tree
reg_tree <- tree(MntFruits ~., df_response[index, ])
summary(reg_tree)

# MSE in test data
mean((df_response[-index, ]$MntFruits - predict(reg_tree, df_response[-index, ]))^2)
#mse = 996.5893

plot(reg_tree)
text(reg_tree)

#Use cross validation to determine 
#the optimal choice of size (the number of terminal nodes)
set.seed(362)
cv <- cv.tree(reg_tree)
plot(cv$size, cv$dev, type = "b")

# best size
cv$size[which.min(cv$dev)] 
## [1] 4

prune_reg_tree <- prune.tree(reg_tree, best = cv$size[which.min(cv$dev)])

# MSE in test data
mean((df_response[-index, ]$MntFruits - predict(prune_reg_tree, df_response[-index, ]))^2)
#MSE = 1003.492
plot(prune_reg_tree)
text(prune_reg_tree)

