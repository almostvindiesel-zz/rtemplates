setwd("/Users/mars/code/rtemplates")

# --------------------------------------------------------
# Operations

statesInfo <- read.csv('stateData.csv')
reddit <- read.csv('reddit.csv')

str(reddit)
dim(reddit)
nrow(reddit)
table(reddit$employment.status)
summary(reddit)
by(pf$friend_count, pf$gender, summary)

unique(reddit[c("employment.status")])
unique(reddit[c("income.range")])
levels(reddit$income.range)

View(statesInfo)
head(statesInfo)ilove
tail(statesInfo)
mean(statePop$population)

#Filtering an
subset(statesInfo, state.region == 1)

stateSubset <- subset(statesInfo, state.region == 1)
OR
stateSubsetBracket <- statesInfo[statesInfo$state.region == 1,]

statePop <- subset(statesInfo, population > 10000)


# --------------------------------------------
# Graphing

install.packages("ggplot2")
library (ggplot2)

#Re order the age.range column using the factor function
# Factor variables are categorical variables that can be either numeric or string variables.
reddit$income.range <- ordered(reddit$income.range, levels = c("Under $20,000", "$20,000 - $29,999", "$30,000 - $39,999","$40,000 - $49,999", "$50,000 - $69,999", "$70,000 - $99,999", "$100,000 - $149,999", "$150,000 or more"
))
qplot(data = reddit, x = income.range)

reddit$age.range <- factor(reddit$age.range, levels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65 or Above"), ordered = T)
qplot(data = reddit, x = age.range)



#  ----------------------------------------------------------------------
# Lesson 3 - Facebook Data
read.delim('pseudo_facebook.tsv') 
pf <- read.csv('pseudo_facebook.tsv', sep ='\t')

# 4x3 grid bargraph
qplot(x = dob_day, data = pf) +
  facet_wrap(~dob_month, ncol = 4)

qplot(x = dob_day, data = subset(pf, dob_day > 10, dob_day < 20)) +
  facet_wrap(~dob_month, ncol = 4)


#Histogram
qplot(data = pf, x = friend_count)

#Histogram w/filters
qplot(pf$friend_count, geom="histogram", 
      binwidth = 50,  
      fill = I("blue"),
      xlim = c(0, 1000)
) 

#Histograph by male v female
qplot(x = friend_count, data = subset(pf, !is.na(gender)),  
      binwidth = 50,
      fill = I("blue"),) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~gender) 

#Histograph w/goodies + limits on x axis
qplot(x = tenure/365, data = pf,
#  binwidth = 50,
  fill = I("blue"),
  color = I("white"),
  xlab = "tenure (yrs)",
  ylab = "num users"
#,
#  xlim = c(0, 2000)
) + scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) 

#Histograph by male v female w/goodies + limits on x axis
qplot(x = age, data = subset(pf, !is.na(gender)),
  fill = I("#5760AB"),
  color = I("white"),
  xlab = "age",
  ylab = "num users"
) + scale_x_continuous(breaks = seq(10, 80, 10), limits = c(10, 80)) + facet_wrap(~gender) 


#Histogram w/more formatting
ggplot(aes(x = age), data = pf) + 
  geom_histogram(binwidth = 1, fill = '#5760AB', color = I("white")) + 
  scale_x_continuous(breaks = seq(10, 80, 10),) 


qplot(x = age, data = subset(pf, !is.na(gender)),
      fill = I("#5760AB"),
      color = I("white"),
      xlab = "age",
      ylab = "num users"
) + scale_x_continuous(breaks = seq(10, 80, 10), limits = c(10, 80)) 

install.packages("gridExtra")
library(gridExtra)


p1 = qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      fill = I("#5760AB"),
      color = I("white"),
      xlab = "# of friends",
      ylab = "users"
) +  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) 

p2 = qplot(x = friend_count, data = subset(pf, !is.na(gender)),
      fill = I("#5760AB"),
      color = I("white"),
      xlab = "# of friends",
      ylab = "users"
) + scale_x_log10()

p3 = qplot(x = friend_count, data = subset(pf, !is.na(gender)),
           fill = I("#5760AB"),
           color = I("white"),
           xlab = "# of friends",
           ylab = "users"
) + scale_x_sqrt()


grid.arrange(p1, p2, p3, ncol=1)


qplot(x = friend_count, y = ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      fill = I("#5760AB"),
      color = gender,
      geom = 'freqpoly',
      xlab = "# of friends",
      ylab = "users"
) + scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) 
  + facet_wrap(~gender) 


# -----------------------------------
by(pf$www_likes, pf$gender, sum)
by(pf$www_likes, pf$gender, summary)
by(pf$www_likes, pf$gender, mean)
by(pf$www_likes, pf$gender, sd)

by(pf$friendships_initiated, pf$gender, summary)


# Box Plot
# !!! Dont want to use the xlim/ylim parameter or the scale_y_continuous layer bc
# they'll remove data points and effect statistics. Need to use coord_cartesian instead


#!!! don't use

qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)),
   geom = 'boxplot',
   fill =I("#5760AB"),
   color = I("black"),
   xlab = "# of friends",
   ylab = "users",
   ylim = c(0, 1000) #wrong because it cuts off data in th edata
) 

# Box Plot
qplot(x = gender, y = friend_count, data = subset(pf, !is.na(gender)),
      geom = 'boxplot',
      fill =I("#5760AB"),
      color = I("black"),
      xlab = "# of friends",
      ylab = "users"
) + coord_cartesian(ylim = c(0, 300)) 


# Box Plot
mobile_checkin_in <- NA
pf$mobile_checkin_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_checkin_in <- factor(pf$mobile_check_in)
sum(pf$mobile_checkin_in)

round(sum(pf$mobile_checkin_in == 1) / length(pf$mobile_checkin_in),2)*100


sum(pf$mobile_checkin_in == 0) / sum(pf$mobile_checkin_in)


nrow(pf$mobile_checkin_in == 'TRUE')




# Tenure by Age Group
pf$age_group <- pf$age
pf$age_group <- ifelse(pf$age < '13' , '13-', pf$age_group)
pf$age_group <- ifelse(pf$age >= '13' & pf$age <= '17', '13-17', pf$age_group)
pf$age_group <- ifelse(pf$age >= '18' & pf$age <= '24', '18-24', pf$age_group)
pf$age_group <- ifelse(pf$age >= '25' & pf$age <= '34', '25-34', pf$age_group)
pf$age_group <- ifelse(pf$age >= '35' & pf$age <= '44', '35-44', pf$age_group)
pf$age_group <- ifelse(pf$age >= '45' & pf$age <= '54', '45-54', pf$age_group)
pf$age_group <- ifelse(pf$age >= '55' & pf$age <= '64', '55-64', pf$age_group)
pf$age_group <- ifelse(pf$age >= '65' , '65+', pf$age_group)

#Hisographs of friend count, factored by age_group
qplot(x = friend_count, data = pf,  
      binwidth = 50,
      fill = I("blue"),) + 
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50)) + 
  facet_wrap(~age_group, ncol = 1)

qplot(x = tenure, y =  ..count../sum(..count..),
      data = subset(pf, !is.na(gender)),
      fill = I("#5760AB"),
      color = age_group,
      geom = 'freqpoly',
      xlab = "tenure",
      ylab = "users"
) + scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) 
  + facet_wrap(~age_group) 


# --------------------------------------------
# Diamonds Data set


by(diamonds$price, diamonds$cut, summary)

#histogram of diamonds by price, factored by cut
qplot(x = price, 
  data = diamonds,
  fill =I("#5760AB"),
  color = I("white")
) + facet_wrap(~cut, ncol = 1)


qplot(x = price, y =  ..count../sum(..count..),
      data = diamonds,
      fill = I("#5760AB"),
      color = cut,
      geom = 'freqpoly',
      xlab = "price",
      ylab = "users"
) + facet_wrap(~cut) 


qplot(x = price/carat, y = ..count..,
      data = diamonds,
      fill = I("#5760AB"),
      color = cut,
      geom = 'freqpoly'
) + facet_wrap(~cut) 
  + scale_x_log10()





sum(diamonds$price > 0)
sum(diamonds$price < 500)
sum(diamonds$price >= 15000)


by(diamonds$price, diamonds$cut, summary)

subset(diamonds, price >= 18820)

by(diamonds$price, diamonds$cut, summary)


qplot(x = price, data = diamonds) + facet_wrap(~cut)

# --------------------------------------------
# Scatter Plots
# Good for using two continuous variables

qplot(x=age, y=friend_count, data=pf)

ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_point(alpha = 1/20) +
  xlim(13,90)

ggplot(aes(x = age, y = friend_count), data = pf) + 
  geom_jitter(alpha = 1/20) +
  xlim(13,90)

ggplot(aes(x = age, y = friendships_initiated), data = pf) + 
  geom_jitter(alpha = 1/20) +
  xlim(13,90) 


ggplot(aes(x = age, y = likes/likes_received), data = pf) + 
  geom_jitter(alpha = 1/20) +
  ylim(0,10) 
  coord_trans(y = "sqrt" )
  
#Scatter Plot with mean
ggplot(aes(x = age, y = friend_count), data = pf) + 
  xlim(13,90) +
  geom_point(alpha = 0.05,
      position = position_jitter(h = 0),
      color = 'orange') +
  coord_trans(y = 'sqrt' ) + 
  geom_line(stat = 'summary', fun.y = mean) + 
  geom_line(stat = 'summary', fun.y = quantile, 
            fun.args = list(probs = .5), linetype = 3, color=I("blue") ) + 
  geom_line(stat = 'summary', fun.y = quantile, 
            fun.args = list(probs = .1), linetype = 2, color=I("blue") ) + 
  geom_line(stat = 'summary', fun.y = quantile, 
            fun.args = list(probs = .9), linetype = 2, color=I("blue") )

# -----------------------------------------------------------  
install.packages('dplyr')
library(dplyr)

age_groups <- group_by(pf,age)

pf.fc_by_age <- summarise (age_groups,
   friend_count_mean = mean(friend_count),
   friend_count_median = median(friend_count),
   n = n()
)

#sort 
pf.fc_by_age <- arrange(pf.fc_by_age,age)

View(pf.fc_by_age)

#new plot
ggplot(aes(x = age, y = friend_count_median), 
   data = pf.fc_by_age
) + geom_point()

ggplot(aes(x = age, y = friend_count_mean), 
   data = pf.fc_by_age
) + geom_point()

ggplot(aes(x = age, y = friend_count_mean), 
   data = pf.fc_by_age
) + geom_line()



# Correlation
pf$age
cor.test(pf$age, pf$friend_count, method='pearson')
typeof()

typeof(pf$age)
typeof(pf$friend_count)
unique(pf$age)
unique(pf$friend_count)

with(subset(pf, age <=65), cor.test(age, friend_count), method='pearson')
with(subset(pf, age <=65), cor.test(age, friend_count), method='spearman')

with(pf, cor.test(www_likes_received, likes_received), method='pearson')


#
install.packages('alr3')
library('alr3')
data(Mitchell)
View(Mitchel)

ggplot(aes(x = Month %% 12, y = Temp), 
       data = Mitchell
) + geom_point()

with(Mitchell, cor.test(Month %% 12 , Temp), method='pearson')

with(subset(Mitchell, Month <=7), cor.test(Month %% 12 , Temp), method='pearson')

sort(unique(pf$dob_year))

qplot(x = dob_year, data = pf) 


# -----------------------------------------------------------  
ggplot(aes(x = gender, y = age),
       data = subset(pf, !is.na(gender))) + geom_boxplot() 

age_gender <- group_by(subset(pf,!is.na(gender)),age,gender)
                       
pf.fc_by_age_gender <- summarise (age_gender,
   mean_friend_count = mean(friend_count),
   median_friend_count = median(friend_count),
   n = n()
)


qplot(x = age, y = n,
      data = pf.fc_by_age_gender,
      color = gender,
) 

# -----------------------------------------------------------  
# Yogurt Analysis
yo <- read.csv('yogurt.csv')
View(yo)
str(yo)

yo$id <- factor(yo$id)

qplot(data = yo, x=price, fill= I('blue'))

str(yo)

yo <- transform(yo, all.purchases = strawberry + blueberry + plain + pina.colada + mixed.berry)

qplot(data = yo, x=all.purchases, fill= I('blue'))

#Scatter: price and purcahses
ggplot(aes(x = price, y = all.purchases), data = yo) + 
  geom_point() 

# Seeding
set.seed(4230)
sample.ids <-sample(levels(yo$id),16)
set.seed(4231)
sample.ids <-sample(levels(yo$id),16)


#Yogurt scatter: purchases overtime by sample household
ggplot(aes(x = time, y = price),
       data = subset(yo,id %in% sample.ids)) + 
  facet_wrap( ~ id) + 
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)


#GGally
install.packages('GGally')
library(GGally)

theme_set(theme_minimal(20))
set.seed(1836)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])


#Genomic Data
nci <- read.table('nci.tsv')



