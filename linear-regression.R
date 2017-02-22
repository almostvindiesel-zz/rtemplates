setwd("/Users/mars/code/rtemplates")

# Correlation 
# --------------------------------------------------------
# Read Dataset
reddit <- read.csv('reddit.csv')
#View(reddit)

#Convert age ranges and income ranges into a score
age_ranges <- c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above")
age_ranges_score <- c(15, 21, 30, 40, 50, 60,70)
age_ranges_df <- data.frame(age_ranges,age_ranges_score)
reddit$age.range.score <- age_ranges_df$age_ranges_score[match(reddit$age.range, age_ranges_df$age_ranges)]

income_ranges <- c("Under $20,000","$20,000 - $29,999","$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $69,999", "$70,000 - $99,999", "$100,000 - $149,999", "$150,000 or more")
income_ranges_score <- c(15000, 25000, 35000, 45000, 60000, 80000, 125000, 175000)
income_ranges_df <- data.frame(income_ranges,income_ranges_score)
reddit$income.range.score <- income_ranges_df$income_ranges_score[match(reddit$income.range, income_ranges_df$income_ranges)]

#unique(reddit$age.range.score)
#unique(reddit$income.range.score)
cor.test(reddit$age.range.score, reddit$income.range.score, method='pearson', use="pairwise.complete.obs")

#G
#ggplot(aes(x = age.range.score, y = income.range.score), data = reddit) + geom_jitter(alpha = 1/20)



# Linear Models
# --------------------------------------------------------

# From http://datasciencemaster.blogspot.com/2016/04/week11-multiple-regression-using-r.html


#read data into variable
df <- read.csv("dataset_multipleRegression.csv")
attach(df)

#View(df)

# Predict the fall enrollment (ROLL) using the unemployment rate (UNEM) and number of spring high school graduates (HGRAD).
threePredictorModel <- lm(ROLL ~ UNEM + HGRAD + INC, df)

#Write model predictions to a df
df$ROLL_PRE <- df$UNEM * threePredictorModel$coefficients['UNEM'] + 
  df$HGRAD * threePredictorModel$coefficients['HGRAD'] + 
  df$INC * threePredictorModel$coefficients['INC'] + 
  threePredictorModel$coefficients['(Intercept)']

#For fun--view pearson's r for dependent variables
cor(df$UNEM,     df$ROLL, method='pearson', use="pairwise.complete.obs")
cor(df$HGRAD,    df$ROLL, method='pearson', use="pairwise.complete.obs")
cor(df$INC,      df$ROLL, method='pearson', use="pairwise.complete.obs")
cor(df$ROLL_PRE, df$ROLL, method='pearson', use="pairwise.complete.obs")

#Plot of prediction vs actual
ggplot(df, aes(seq_along(df$ROLL))) + 
  geom_line(aes(y = ROLL_PRE, colour = "Predicted Enrollment")) + 
  geom_line(aes(y = ROLL, colour = "Actual Enrollment"))
  
