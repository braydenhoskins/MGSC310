setwd("C:\\Users\\noahe\\Desktop\\310_group")
steam <- read.csv("steam.csv")

#####exploratory data analysis

###there are no rows with missing values
nrow(steam[!complete.cases(steam),])



###variable engineering/cleaning
#steam$pos_rating_ratio <- steam$positive_ratings/steam$negative_ratings
##turns the unites into dollars
steam$price <- steam$price *1.28
steam<- subset(steam, select = -c(appid,english,steamspy_tags,
                                  name,release_date,
                                  platforms))
#steam$logprice <- log(steam$price)
steam$genres <- do.call('rbind',strsplit(as.character(steam$genres), ';', fixed=TRUE))[,1]
steam$categories <- do.call('rbind',strsplit(as.character(steam$categories), ';', fixed=TRUE))[,1]
steam$steamspy_tags <- do.call('rbind',strsplit(as.character(steam$steamspy_tags), ';', fixed=TRUE))[,1]
steam$publisher <- do.call('rbind',strsplit(as.character(steam$publisher), ';', fixed=TRUE))[,1]
steam$developer <- do.call('rbind',strsplit(as.character(steam$developer), ';', fixed=TRUE))[,1]
steam$developer <- as.factor(steam$developer)
steam$categories <- as.factor(steam$categories)
steam$genres <- as.factor(steam$genres)
steam$publisher <- as.factor(steam$publisher)
sapply(steam[,],is.finite)
###creates variable successful game to help predict by
steam$successfulGame <- ifelse(steam$owners == "10000000-20000000",1,
                               ifelse(steam$owners == "20000000-50000000",1,
                                      ifelse(steam$owners == "50000000-100000000",1,
                                             ifelse(steam$owners == "100000000-200000000",1,0))))
steam$successfulGame <- as.factor(steam$successfulGame)
steam[,c("owners","successfulGame")]
str(steam)

####correlations

numeric_cols <- sapply(steam,is.numeric)
library(corrplot)
correlations <- cor(steam[,numeric_cols])
corrplot(correlations)

####dimensionality reduction
library(glmnet)
library(glmnetUtils)
steam_lasso <- cv.glmnet(successfulGame~.,
                         data = steam,
                         alpha = 1,
                         family = "binomial")



####summary plots
library(ggplot2)
ggplot(steam,aes(x = achievements,y = average_playtime)) + geom_point(aes(color = successfulGame))+
  geom_smooth()
ggplot(steam,aes(x = positive_ratings,y = negative_ratings)) + 
  geom_point(aes(color = successfulGame))
ggplot(steam,aes(x = positive_ratings,y = average_playtime)) +geom_point(aes(color = successfulGame))
ggplot(steam,aes(x= categories,y =average_playtime)) + geom_boxplot()
ggplot(steam,aes(x = price,y = successfulGame)) + 
  geom_point()
summary(steam)
