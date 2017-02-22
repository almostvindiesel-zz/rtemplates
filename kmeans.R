library(datasets)
head(iris)
View(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
iris$cluster <- as.character(irisCluster$cluster)

table(iris$Species, irisCluster$cluster)

#Plot Side by Side
library(gridExtra)
require(gridExtra)
plot1 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
plot2 <- ggplot(iris, aes(Petal.Length, Petal.Width, color = cluster)) + geom_point()
grid.arrange(plot1, plot2, ncol=2)



########################################################################
# Twitter Social Analysis

install.packages("RMySQL")
library(RMySQL)
library(gridExtra)
require(gridExtra)

sql = "select
          text,
        max(retweets) retweets,
        sum(followers) followers,
        max(sentiment) sentiment
       from tweet
       where followers > 0
       group by 1"

#Pull twitter data from db
mydb = dbConnect(MySQL(), user='mars', password='iloveb8con', dbname='twitter', host='localhost')
rs = dbSendQuery(mydb, sql)
df = fetch(rs, n=-1)

?View
#View(df)

#Create data frame with just two variables
columns <- c("followers", "sentiment")
df2 <- df[columns]

#Retweets v Followers - After Cluster Analysis
set.seed(20)
num_clusters = 6
dfCluster <- kmeans(df2, num_clusters, nstart = 20)
df2$cluster <- as.character(dfCluster$cluster)


plot1 <- ggplot(df2, aes(followers, sentiment, color = cluster)) + 
  geom_point(position = position_jitter(w = .1, h = 0.01),
             alpha=0.1, color="firebrick") +
  scale_x_log10()
plot2 <- ggplot(df2, aes(followers, sentiment, color = cluster)) + 
  geom_point(position = position_jitter(w = .1, h = 0.01),
             alpha=0.3) +
  scale_x_log10()
  #scale_x_continuous(limits = c(0, 10000)) 
grid.arrange(plot1, plot2, ncol=2)

plot2 <- ggplot(df2, aes(retweets, sentiment)) + 
  geom_point() +
  geom_jitter(alpha = 1/20) +
  scale_x_log10()
  #scale_x_continuous(limits = c(0, 10000)) 
grid.arrange(plot1, plot2, ncol=2)


