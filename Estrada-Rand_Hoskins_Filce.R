<<<<<<< HEAD


###Summary Statistics R code by Noah Estrada-Rand, Brady Hoskins, Charles Filce

setwd("C:\\Users\\noahe\\Desktop\\MGSC310")
steam <- read.csv("steam.csv")

#####exploratory data analysis

###there are no rows with missing values
nrow(steam[!complete.cases(steam),])



###variable engineering/cleaning
#steam$pos_rating_ratio <- steam$positive_ratings/steam$negative_ratings
##turns the unites into dollars
steam$price <- steam$price *1.28
####got rid of cariables that wont really help us
steam<- subset(steam, select = -c(appid,english,steamspy_tags,
                                  name,release_date,
                                  platforms,publisher,developer))

####I got rid of all the ';' delimited variables and instead assigned them a single value for that columne
steam$genres <- do.call('rbind',strsplit(as.character(steam$genres), ';', fixed=TRUE))[,1]
steam$categories <- do.call('rbind',strsplit(as.character(steam$categories), ';', fixed=TRUE))[,1]
###turn them all into factors
steam$categories <- as.factor(steam$categories)
steam$genres <- as.factor(steam$genres)


###removing outliers
steam <- steam[steam$average_playtime < 100000,]
steam <- steam[steam$price <50,]
steam <- steam[steam$average_playtime < 40000,]
steam <- steam[steam$negative_ratings < 2e+05,]
steam <- steam[steam$positive_ratings < 1e+06,]

# Creating Groups of factored variables
library(forcats)
library(tidyverse)
fct_count(steam$categories)
steam %>%
  mutate(categories = fct_lump(categories, n =5))%>%
  count(categories)

###one way to do it
steam$simple_categories <- fct_lump(steam$categories,n = 4)

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


unique(steam$categories)
###reducing the number of genres
steam$genres <- fct_lump(steam$genres,n = 5)
unique(steam$genres)


###creates variable successful game to help predict by (I was thinking we could 
#use this as our predicted variable)(1 is a successful game - over 1 million sold,0 is anything less)
steam$successfulGame <- ifelse(steam$owners == "10000000-20000000",1,
                               ifelse(steam$owners == "20000000-50000000",1,
                                      ifelse(steam$owners == "50000000-100000000",1,
                                             ifelse(steam$owners == "100000000-200000000",1,
                                                    ifelse(steam$owners == "5000000-10000000",1,
                                                           ifelse(steam$owners == "2000000-5000000",1,
                                                                  ifelse(steam$owners == "1000000-2000000",1,0)))))))
steam$successfulGame <- as.factor(steam$successfulGame)
steam <- subset(steam, select = -c(owners,categories))

library(summarytools)
descr(steam)


str(steam)

####correlations

numeric_cols <- sapply(steam,is.numeric)
library(corrplot)
correlations <- cor(steam[,numeric_cols])
corrplot(correlations)


####summary plots of random things, our data is pretty ininteresting to look at
library(ggplot2)
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




####regsubsets
library(leaps)
steam_fwd <- regsubsets(successfulGame~.,
                        data = steam,
                        nvmax = 9,
                        method = "forward")
summary(steam_fwd)
plot(steam_fwd,scaele = "adjr2")

which.max(summary(steam_fwd)$adjr2)

steam_reg_subsets <- regsubsets(successfulGame~.,
                                data = steam,
                                nvmax = 8)

summary(steam_reg_subsets)
plot(steam_reg_subsets,scale = "adjr2")
which.max(summary(steam_reg_subsets)$adjr2)




#####
set.seed(2019)
train_index <- sample(1:nrow(steam),.75*nrow(steam),replace = FALSE)
steam_train <- steam[train_index,]
steam_test <- steam[-train_index,]
steam_logit <- glm(successfulGame~.,
                   data = steam_train,
                   family = "binomial")

preds_LOOCV = NULL;
for(i in 1:nrow(steam_train)){
  mod <- glm(successfulGame~., family = binomial,
             data = steam_train[-i,])
  preds_LOOCV[i] <- predict(mod,newdata = steam_test[i,],type = "response")
}
head(preds_LOOCV)
preds_train <- data.frame(preds_train,loocvPreds = preds_LOOCV)



####dimensionality reduction
####Dont run this, it takes too long, we need to use other dimensionality reduction techniques
library(glmnet)
library(glmnetUtils)
steam_lasso <- cv.glmnet(successfulGame~.,
                         data = steam,
                         alpha = 1,
                         family = "binomial")

steam_lasso$lambda.min



####### finding the best mtry to use
set.seed(2019)
train_idx <- sample(1:nrow(steam), size = floor(.75 * nrow(steam)))
steam_train <- steam[train_idx,]
steam_test <- steam[-train_idx,]

library(randomForest)
rf_mods <- list()
oob_err <- NULL##to store out of bag error
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



library(randomForest)
random_forest_steam <- randomForest(successfulGame~.,
                                    data = steam_test,
                                    mtry = 4,
                                    ntrees = 500,
                                    type = classification,
                                    importance = TRUE)
####find that the best m is 4



importance(random_forest_steam)
plot(random_forest_steam)
text(random_forest_steam,pretty = 0)


####dimensionality reduction
####Dont run this, it takes too long, we need to use other dimensionality reduction techniques
library(glmnet)
library(glmnetUtils)
steam_lasso <- cv.glmnet(successfulGame~.,
                         data = steam,
                         alpha = 1,
                         family = "binomial")

plot(steam_lasso)
=======
setwd("C:\\Users\\noahe\\Desktop\\MGSC310")
steam <- read.csv("steam.csv")

#####exploratory data analysis

###there are no rows with missing values
nrow(steam[!complete.cases(steam),])



###variable engineering/cleaning
#steam$pos_rating_ratio <- steam$positive_ratings/steam$negative_ratings
##turns the unites into dollars
steam$price <- steam$price *1.28
####got rid of cariables that wont really help us
steam<- subset(steam, select = -c(appid,english,steamspy_tags,
                                  name,release_date,
                                  platforms))

####I got rid of all the ';' delimited variables and instead assigned them a single value for that columne
steam$genres <- do.call('rbind',strsplit(as.character(steam$genres), ';', fixed=TRUE))[,1]
steam$categories <- do.call('rbind',strsplit(as.character(steam$categories), ';', fixed=TRUE))[,1]
steam$publisher <- do.call('rbind',strsplit(as.character(steam$publisher), ';', fixed=TRUE))[,1]
steam$developer <- do.call('rbind',strsplit(as.character(steam$developer), ';', fixed=TRUE))[,1]
###turn them all into factors
steam$developer <- as.factor(steam$developer)
steam$categories <- as.factor(steam$categories)
steam$genres <- as.factor(steam$genres)
steam$publisher <- as.factor(steam$publisher)


###removingh outliers
steam <- steam[steam$average_playtime < 100000,]
steam <- steam[steam$price <100,]
steam <- steam[steam$average_playtime < 40000,]
steam <- steam[steam$negative_ratings < 2e+05,]
steam <- steam[steam$positive_ratings < 1e+06,]

###creates variable successful game to help predict by (I was thinking we could 
#use this as our predicted variable)(1 is a successful game - over 1 million sold,0 is anything less)
steam$successfulGame <- ifelse(steam$owners == "10000000-20000000",1,
                               ifelse(steam$owners == "20000000-50000000",1,
                                      ifelse(steam$owners == "50000000-100000000",1,
                                             ifelse(steam$owners == "100000000-200000000",1,
                                                    ifelse(steam$owners == "5000000-10000000",1,
                                                           ifelse(steam$owners == "2000000-5000000",1,
                                                                  ifelse(steam$owners == "1000000-2000000",1,0)))))))
steam$successfulGame <- as.factor(steam$successfulGame)
str(steam)

####correlations

numeric_cols <- sapply(steam,is.numeric)
library(corrplot)
correlations <- cor(steam[,numeric_cols])
corrplot(correlations)

####Summary Table
#install.packages("summarytools")
library(summarytools)
descr(steam)



####summary plots of random things, our data is pretty ininteresting to look at
library(ggplot2)
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
summary(steam)

####dimensionality reduction
####Dont run this, it takes too long, we need to use other dimensionality reduction techniques
library(glmnet)
library(glmnetUtils)
steam_lasso <- cv.glmnet(successfulGame~.,
                         data = steam,
                         alpha = 1,
                         family = "binomial")
>>>>>>> d38b5db4a55c6419f78f0de81751a476c368395a
