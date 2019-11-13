
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
steam <- subset(steam, select = -c(owners,developer))

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

summary(steam)

####dimensionality reduction
####Dont run this, it takes too long, we need to use other dimensionality reduction techniques
library(glmnet)
library(glmnetUtils)
steam_lasso <- cv.glmnet(successfulGame~.,
                         data = steam,
                         alpha = 1,
                         family = "binomial")
