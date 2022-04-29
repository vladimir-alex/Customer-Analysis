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
#response = MntWine
df_response <- df[, c(10,3, 4, 5, 8, 9, 16:20, 26, 30,32)]
colnames(df_response)

ridge_x <- df_response[,-1]
ridge_y <- df_response[,1]

colnames(ridge_x) 


#linear regression: MntWines
linear_fit <- lm(MntWines~., data = df_response)
summary(linear_fit)
#Adjusted R-squared: 0.6247

#Prediction
ls_fit <- lm(MntWines~., data = df_response[index, ])
summary(ls_fit)

predict(ls_fit, df_response[-index, ])
cor(predict(ls_fit, df_response[-index, ]), 
    df_response[-index, ]$MntWines, use = "complete.obs")
#correlation: 0.7459006

#Ridge regression: MntWine
ridge_reg_x <- model.matrix(MntWines~., data = df_response)[, -1] 
ridge_reg_y <- df_response$MntWines

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
#MSE = 43541.63

# Estimated regression coefficients
predict(ridge_best, type = "coefficients")[, 1]

#Random Forest
library(randomForest)

rf_fit <- randomForest(formula = MntWines ~.,
                            data = df_response, 
                            mtry = (ncol(df_response[index, ]) - 1)/ 3, 
                            ntree = 1000, importance = TRUE)

rf_pred <- predict(rf_fit, df_response[-index, ])
mean((df_response[-index, ]$MntWines - rf_pred)^2)
#[1] 4846.996

importance(rf_fit)
varImpPlot(rf_fit,
           main ="Random Forest Variables Importance for Amount Spent on Wine")

#------decision tree------#

colnames(df_response)
head(df_response)

# Fit regression tree
reg_tree <- tree(MntWines ~., df_response[index, ])
summary(reg_tree)

# MSE in test data
mean((df_response[-index, ]$MntWines - predict(reg_tree, df_response[-index, ]))^2)
#mse = 42030.38

plot(reg_tree)
text(reg_tree)

#Use cross validation to determine 
#the optimal choice of size (the number of terminal nodes)
set.seed(362)
cv <- cv.tree(reg_tree)
plot(cv$size, cv$dev, type = "b")

# best size
cv$size[which.min(cv$dev)] 
## [1] 9

prune_reg_tree <- prune.tree(reg_tree, best = cv$size[which.min(cv$dev)])

# MSE in test data
mean((df_response[-index, ]$MntWines - predict(prune_reg_tree, df_response[-index, ]))^2)
#MSE = 42030.38
plot(prune_reg_tree)
text(prune_reg_tree)

#--------------------------------------------#

#------decision tree------#

colnames(df_response)
head(df_response)

df_product <- df[, c(10, 3, 4, 5, 8, 9, 16:20, 26, 30, 32)]
colnames(df_product)
nrow(df_product)

set.seed(362)
index_1 <- sample(nrow(df), nrow(df) * 0.75)
length(index_1)
train_tv <- df_product[index_1, ] #train_tv = train + validation
test <- df_product[-index_1, ]

index_2 <- sample(nrow(train_tv), nrow(train_tv) * 2/3)

train <- train_tv[index_2,]
validation <- train_tv[-index_2,]

nrow(train)
nrow(validation)
nrow(test)

# Fit regression tree
reg_tree <- tree(MntWines ~., train)
summary(reg_tree)
#Number of terminal nodes:  9

# MSE in testing data without pruning the tree
mean((test$MntWines- predict(reg_tree, test))^2)
#mse = 44207.48

plot(reg_tree)
text(reg_tree)

#Use cross validation to determine 
#the optimal choice of size (the number of terminal nodes)
cv <- cv.tree(reg_tree)
plot(cv$size, cv$dev, type = "b")

# best size
cv$size[which.min(cv$dev)] 
## [1] 9
#the optimal size = 9


prune_reg_tree <- prune.tree(reg_tree, best = cv$size[which.min(cv$dev)])

#MSE in the validation data
mean((validation$MntWines - predict(prune_reg_tree, validation))^2)
#mse = 43585.29


# MSE in test data
mean((test$MntWines - predict(prune_reg_tree, test))^2)
#MSE = 44207.48
plot(prune_reg_tree)
text(prune_reg_tree)


