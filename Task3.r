#Author: Patrick Glettig
#Date: 05.10.2018
#Title: Problemset 1 Empirical Methods, Task 3

library(readstata13)
library(Hmisc)
library(ggplot2)
#import the data
smoke <- read.dta13('smoke.dta')

#Data exploration
head(smoke)
summary(smoke)
describe(smoke) #there are 807 observations, 10 variables and zero missing values

#Task b) Provide a table of summary statistics

summary(smoke)

#Task c)ii
my_lm <- lm(cigs ~ educ, data = smoke)
my_lm$coefficients

#Task c)iii
y_new <- predict(my_lm)#Predict values
resid(my_lm)#gives a vector of residuals
#Create Scatterplot with regression line
ggplot(smoke, aes(x=educ, y=cigs)) +
  geom_point() +    # Scatters
  geom_smooth(method=lm, se=FALSE)

#Task c) v Regression without intercept
my_lm_no_constant <- lm(cigs ~ educ + 0, data = smoke)
my_lm_no_constant$coefficients
fitted(my_lm_no_constant)#calculates the fitted y values, does same as predict.

ggplot(smoke, aes(x=educ, y=cigs)) +
  geom_point() +    # Scatters
  geom_smooth(method=lm, se=FALSE)+
  geom_smooth(method=lm,
              formula = y ~ 0 + x, #remove constant
              se=FALSE, color='red')

#Task d)
third_lim <- lm(cigs ~ educ 
                        +age
                        +I(age^2)
                        +white
                        +restaurn,
                data = smoke)
summary(third_lim)#gets us all important information

#task d) ii
linear_coef <- summary(third_lim)$coefficients[3,1]#extract linear coefficient
squared_coef <- summary(third_lim)$coefficients[4,1]#extract squared coefficient

#Calculate marginal effect of age
#Take derivate of model with respect to age:
#y=beta0+beta1x+beta2x^2+beta3z+...
#y'(with respect to x)=beta1+2*beta2*x
me_age <- function(age=1){
  x <- linear_coef + 2*squared_coef*age
  return(x)
}
me_age(20)
me_age(40)
me_age(60)

#task d) iii
third_lim$residuals#calculates residuals
smoke$residi <- third_lim$residuals #add it to dataframe to plot
#A: Plot residuals vs. age.
ggplot(smoke,aes(x=age,y=residi))+
  geom_point()#residuals are mostly positive, no good for normal distribution!
#residuals have shape, do not look random.
#B: Calculate correlation
acf(smoke$residi)[1]#gives correlation with lag 1
cor(smoke$age,smoke$residi) #this correlation is really small, so we should be fine.

#C: Density plot residuals plus normal distribution
#add a "normal" series to benchmark:
smoke$benchmark <- rnorm(807)
ggplot(smoke)+
    geom_density( aes(residi))+
    geom_density(aes(benchmark)) 
#The residuals definitely do not have a normal distribution!       
       