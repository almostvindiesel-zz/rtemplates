setwd("/Users/mars/code/rtemplates")

#Creating a dataframe from scratch
#Dataframe = list of vectors of equal length
    
#install.packages("ggplot2")
library (ggplot2)
#qplot(data = df, x = fb_likes, y=twitter_followers)

#summary(df)
#View(df)

# Runs regression against all numeric vectors in a dataframe
df <- read.csv('netflix-originals.csv')
is_df_numeric <- sapply(df, is.numeric)
for(i in 1:ncol(df)) {
  if (is_df_numeric[i]) {
    correlation = cor(df[i], df$fb_likes, method='pearson', use="pairwise.complete.obs")
    if (abs(correlation[1]) > .3 && abs(correlation[1]) != 1) {
      print (colnames(df)[i])
      print (correlation[1])
    }
  }
}

# Predict the fall enrollment (ROLL) using the unemployment rate (UNEM) and number of spring high school graduates (HGRAD).
regression <- lm(fb_likes ~ horror + influencer_pen + yt_pct_comments, df)


#Write model predictions to a df
df$fb_likes_pre <- df$yt_pct_comments * regression$coefficients['yt_pct_comments'] + 
                   df$influencer_pen  * regression$coefficients['influencer_pen'] + 
                   df$horror * regression$coefficients['horror'] + 
                   regression$coefficients['(Intercept)']

model_r = cor(df$fb_likes_pre, df$fb_likes, method='pearson', use="pairwise.complete.obs")

msg = paste0 ("Predicted\nr=", round(model_r,2) ,"0")

qplot(x=fb_likes_pre, y=fb_likes, data=df) + 
  geom_point( aes(y = fb_likes, colour = "Actual") ) + 
  geom_text( aes(label=title),hjust=-.1, vjust=0, size=2) +
  geom_line(  aes(y = fb_likes_pre, colour = msg)) + 
  xlim(0, 20000000) + ylim(0, 20000000)


  

rownames(df$title)
  



#Linear Regression w/Categorical Variables

#- Create a categorical 
for(i in 1:length(df$state.region)) {
  if (df$state.region[i] == '1') {
    df$state.regioncat[i] = 'aa'
  } else if (df$state.region[i] == '2') {
    df$state.regioncat[i] = 'bb'
  } else if (df$state.region[i] == '3') {
    df$state.regioncat[i] = 'cc'
  } else if (df$state.region[i] == '4') {
    df$state.regioncat[i] = 'dd'
  } else if (df$state.region[i] == '5') {
    df$state.regioncat[i] = 'ee'
  }
}

lm(life.exp ~ state.regioncat + highSchoolGrad, df)

?glm

?lm

hsb2 <- read.csv("http://www.ats.ucla.edu/stat/data/hsb2.csv")

View(df)

model.matrix(~ . + 0, data=df, contrasts.state.regioncat = lapply(df, contrasts, contrasts=FALSE))

test <- cbind(with(df, model.matrix(~ state.regioncat + 0)))

test3 <-cbind(test, df)

