setwd("/Users/mars/code/rtemplates")

#Creating a dataframe from scratch
#Dataframe = list of vectors of equal length
n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b)       

summary(df)
View(df)

df <- read.csv('netflix-originals.csv')
df <- read.csv('stateData.csv')

View(df)

df$fb_id <- NULL

#Number of rows + cols
nrow(df)
ncol(df)

#Filtering
subset(df, population > 1000)

#Linear Regression with a dataframe...

names <- lapply(df,class)
names[2]

typeof(df)
typeof(test3)

#print names[i]
#print (typeof(df[[paste(i)]])
# df[[paste(i, 'length', sep="_")]] <- str_length(df[[i]])   

View(df)


df <- test3

i = 5
for(i in 1:ncol(df)) {
  if (names[i] == 'numeric' || names[i] == 'integer') {
    correlation = cor(df[i], df$life.exp, method='pearson', use="pairwise.complete.obs")
    if (abs(correlation[1]) > .3 && abs(correlation[1]) != 1) {
      print (colnames(df)[i])
      print (correlation[1])
    }
  }
}

summary(aov(life.exp ~ state.regioncat, data=df))



# Predict the fall enrollment (ROLL) using the unemployment rate (UNEM) and number of spring high school graduates (HGRAD).
regression <- lm(life.exp ~ highSchoolGrad + murder + illiteracy, df)

cor(df$state.abb, df$life.exp, method='pearson', use="pairwise.complete.obs")


#For fun--view pearson's r for dependent variables
cor(df$highSchoolGrad, df$life.exp, method='pearson', use="pairwise.complete.obs")
cor(df$illiteracy, df$life.exp, method='pearson', use="pairwise.complete.obs")


qplot(x=murder, y=life.exp, data=df)
qplot(x=highSchoolGrad, y=life.exp, data=df)
qplot(x=illiteracy, y=life.exp, data=df)


#Write model predictions to a df
df$life.exp.pre <- df$murder * regression$coefficients['murder'] + 
  df$highSchoolGrad * regression$coefficients['highSchoolGrad'] + 
  df$illiteracy * regression$coefficients['illiteracy'] + 
  df$frost * regression$coefficients['frost'] + 
  regression$coefficients['(Intercept)']

model_r = cor(df$life.exp.pre, df$life.exp, method='pearson', use="pairwise.complete.obs")


msg = paste0 ("Predicted (r=", round(model_r,2) ,"0")

qplot(x=life.exp.pre, y=life.exp, data=df) + 
  geom_point(aes(y = life.exp, colour = "Actual")) + 
  geom_line(aes(y = life.exp.pre, colour = msg))
  

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

