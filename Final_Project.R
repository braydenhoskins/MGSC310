#-----------------------------#
#       Library Calls         #
#-----------------------------#
library(forcats)
library(tidyverse)
library(summarytools)
library(corrplot)
library(ggplot2)
library(glmnet)
library(glmnetUtils)
library(randomForest)
library(randomForestExplainer)
library(caret)

#------------------------------------#
#       Importing Data Set           #
#------------------------------------#
steam <- read.csv("steam.csv")


#---------------------------------------------#
#        Exploratory Data Analysis            #
#---------------------------------------------#
nrow(steam[!complete.cases(steam),])
# There are no rows with missing values

#---------------------------------------------#
#       Variable Engineering/Cleaning         #
#---------------------------------------------#
steam$price <- steam$price *1.28
# Turns the units into dollars
steam<- subset(steam, select = -c(appid,english,steamspy_tags,
                                  name,release_date,
                                  platforms,publisher,developer))
# Removing variables that are not useful
steam$genres <- do.call('rbind',strsplit(as.character(steam$genres), ';', fixed=TRUE))[,1]
steam$categories <- do.call('rbind',strsplit(as.character(steam$categories), ';', fixed=TRUE))[,1]
# Getting rid of all the ';' delimited variables and instead assigned them a single value for that column
steam$categories <- as.factor(steam$categories)
steam$genres <- as.factor(steam$genres)
# Making categories and genres as factored variables
steam <- steam[steam$average_playtime < 100000,]
steam <- steam[steam$price <50,]
steam <- steam[steam$average_playtime < 40000,]
steam <- steam[steam$negative_ratings < 2e+05,]
steam <- steam[steam$positive_ratings < 1e+06,]
# Removing outliers
fct_count(steam$categories)
steam %>%
  mutate(categories = fct_lump(categories, n =5))%>%
  count(categories)
# Creating Groups of factored variables

steam$simple_categories <- fct_lump(steam$categories,n = 4)
# Creating a factored feature with only 4 options
steam$categories <- ifelse(steam$categories == "Single-player","SinglePlayer",
                           ifelse(steam$categories == "Multi-player","Multi-Player",
                                  ifelse(steam$categories == "Online Multi-Player","Multi-Player",
                                         ifelse(steam$categories == "Local Multi-Player","Multi-Player",
                                                ifelse(steam$categories == "MMO","MMO",
                                                       ifelse(steam$categories == "Co-op","Co-op",
                                                              ifelse(steam$categories == "Shared/Split Screen","Co-op",
                                                                     ifelse(steam$categories == "Local Co-op","Co-op",
                                                                            ifelse(steam$categories == "Online Co-op","Co-op",
                                                                                   ifelse(steam$categories == "Steam Cloud","Steam",
                                                                                          ifelse(steam$categories == "Steam Trading Cards","Steam",
                                                                                                 ifelse(steam$categories == "Steam Leaderboards","Steam",
                                                                                                        ifelse(steam$categories == "Steam Achievements","Steam","Other")))))))))))))
steam$categories <- as.factor(steam$categories)
unique(steam$categories)
# Another way of lumping together factors
steam$genres <- fct_lump(steam$genres,n = 5)
unique(steam$genres)
# Reducing the number of genres

#-----------------------------------------------#
#          Creating Predictor Variable          #
#-----------------------------------------------#
steam$successfulGame <- ifelse(steam$owners == "10000000-20000000",1,
                               ifelse(steam$owners == "20000000-50000000",1,
                                      ifelse(steam$owners == "50000000-100000000",1,
                                             ifelse(steam$owners == "100000000-200000000",1,
                                                    ifelse(steam$owners == "5000000-10000000",1,
                                                           ifelse(steam$owners == "2000000-5000000",1,
                                                                  ifelse(steam$owners == "1000000-2000000",1,0)))))))
steam$successfulGame <- as.factor(steam$successfulGame)
steam <- subset(steam, select = -c(owners,categories))
# Creating variable successful game, 1 = Successful with over 1 million games sold, 0 for everything less

#-------------------------------------#
#       Descriptive Statistics        #
#-------------------------------------#
descr(steam)
str(steam)

#---------------------------------------------#
#                Correlations                 #
#---------------------------------------------#
numeric_cols <- sapply(steam,is.numeric)
correlations <- cor(steam[,numeric_cols])
corrplot(correlations)

#---------------------------------------------#
#                Summary Plots                #
#---------------------------------------------#
ggplot(steam,aes(x = achievements,y = average_playtime)) + geom_point(aes(color = successfulGame))+
  geom_smooth() + labs(x = "Number of In-Game Achievements",y = "Average Playtime (hrs)",
                       title = "Plotted Achievements and Average Playtime")
ggplot(steam,aes(x = positive_ratings,y = negative_ratings)) + 
  geom_point(aes(color = successfulGame))+
  labs(x = "Number of Positive Ratings in the Steam Store",y = "Negative Ratings in the Steam Store",
       title = "Positive Ratings Plotted Against Negative Ratings")
ggplot(steam,aes(x = positive_ratings,y = average_playtime)) +geom_point(aes(color = successfulGame))+
  labs(x = "Number of Positive Ratings in the Steam Store",y = "Average Playtime (hrs)",
       title = "Average Playtime Plotted against Positive Ratings")
ggplot(steam,aes(x = price,y = average_playtime)) + 
  geom_point(aes(color = successfulGame)) + 
  labs(x = "Price of the Game (dollars)",y ="Average Playtime (hrs)",
       title = "Average Playtime Plotted Against Price of Game")
ggplot(steam,aes(x = average_playtime,y = successfulGame)) +
  geom_point()+
  labs(title = "Successful Game plotted Against Average Playtime",
       x = "Average Playtime (hrs)",y = "Successful Game (1 = Yes, 0 = No)")
boxplot(steam$average_playtime~steam$required_age,
        main = "Average Playtime by Required Age",xlab = "Required Age (years)",
        ylab = "Average Playtime")
ggplot(steam,aes(x = price,y = successfulGame)) +
  geom_point()+
  labs(title = "Successful Game plotted Against Average Playtime",
       x = "Price of a Game (dollars)",y = "Successful Game (1 = Yes, 0 = No)")

boxplot(steam$price~steam$categories,
        main = "Boxplot of Price By Category",
        xlab = "Categories",
        ylab = "Price (dollars)")
ggplot(steam,aes(x = genres,y = price)) + geom_boxplot() + 
  labs(title = "Boxplots of Price By Genre",x = "Genres",y = "Price (dollars)")

summary(steam)

#------------------------------------#
#         Random Forest Model        #
#------------------------------------#
set.seed(2019)
train_idx <- sample(1:nrow(steam), size = floor(.75 * nrow(steam)))
steam_train <- steam[train_idx,]
steam_test <- steam[-train_idx,]
# Creating Train and Test sets
rf_mods <- list()
oob_err <- NULL 
# Used to store out of bag error
test_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(successfulGame~.,
                         data = steam_train,
                         mtry = mtry,
                         ntrees = 500,
                         type = classification)
  oob_err[mtry] <- rf_fit$err.rate[500]
}
results_df <- data.frame(mtry = 1:9,
                         oob_err)
ggplot(results_df,aes(x = mtry,y = oob_err)) + geom_point() +geom_line()
# This helps us to find the best mtry for our Random Forest model
random_forest_steam <- randomForest(successfulGame~.,
                                    data = steam_train,
                                    mtry = 4,
                                    ntrees = 500,
                                    type = classification,
                                    importance = TRUE)
# Creating a model with the best m = 4
rf_preds <- predict(random_forest_steam, newdata = steam_test)
importance(random_forest_steam)
varImpPlot(random_forest_steam)
plot_min_depth_distribution(random_forest_steam)
plot_multi_way_importance(random_forest_steam)
# Plotting the results of the random forest model
explain_forest(random_forest_steam)
# Explaining the random forest model

#---------------------------------------#
#       Dimensionality Reduction        #
#---------------------------------------#
steam_subset <- subset(steam, select = -c(owners))
steam_lasso <- cv.glmnet(successfulGame~.,
                         data = steam,
                         alpha = 1,
                         family = "binomial")
# The lasso model would take very long before we reduced the dimensions of the categories and genres variables

#---------------------------------------#
#       K-fold Cross Validation         #
#---------------------------------------#

# creating folds (using createFolds from caret package)
steam_subset$folds <- createFolds(steam_subset$successfulGame, k = 10, list = FALSE)
steam_subset$row_num <- 1:nrow(steam)


### K-Fold Cross Validation
nfolds <- 10
preds_KFold_CV_DF <- data.frame(folds = steam_subset$folds,rownum = steam_subset$row_num,preds_KFold_CV = rep(NA,nrow(steam_subset)))

# for loop for K-Fold CV
for(i in 1:nfolds){
  mod <- lm(successfulGame ~ ., 
            data = steam_subset[steam_subset$folds != i,])
  preds <- predict(mod, 
                   newdata = steam_subset[steam_subset$folds == i,])
  preds_KFold_CV_DF[preds_KFold_CV_DF$folds == i,"preds_KFold_CV"]  <- preds
}

preds_DF <- data.frame(preds_KFold_CV = preds_KFold_CV_DF$preds_KFold_CV,true = steam$successfulGame)

RMSE <- function(t, p) {
  sqrt(sum(((t - p)^2)) * (1/length(t)))
}
RMSE(preds_DF$preds_KFold_CV,as.numeric(preds_DF$true))
