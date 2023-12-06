#1----

#A)

library(MASS)
Boston
?Boston
#There are 14 columns.
#There are 506 rows.
#The columns represent different predictors in the Boston dataset.These predictors relate to housing.
#The rows represent different suburbs of Boston.

#B)
pairs(Boston)
#With a quick glance at the pairwise graphs, there are some that seem to have strong correlations and some that don't.
#Some pairs that are noticeably correlated are age and nox, dis and nox, and rm and medv.

#C)
#Dis (weighted mean of the distances to five Boston employment centers) seems to have an exponentially negative relationship with crime. The farther away, the less crime.
#Age seems to have a positive exponential relationship with crime.As age goes up, crime goes up
#Medv (median values of homes) appears to have an exponentially negative relationship.As home value goes up, crime goes down
#There also appears to be a quadratic relationship between it and the proportion of african americans. 

#D)
range(Boston$crim) 
range(Boston$tax)
range(Boston$ptratio)
#crim(.0063,88.976) tax(187,711) ptratio(12.6, 22)
#There are suburbs that have a high crime rate (88.98), high taxes (711 per 10000) and high pupil teacher ratio (22:1).The biggest difference in my opinion is the crime rate. Whatever suburb has 88.976 crime rate would be terrible to live in. 

#E)
sum(Boston$chas == 1)
#There are 35 suburbs that bound Charles River. 

#F)
median(Boston$ptratio)
#The median is 19.05.

#G)
min_medv = min(Boston$medv)
min_medv_suburb = Boston[Boston$medv == min_medv,]
print(min_medv_suburb)
#Suburbs 399 and 406 have the lowest median value of owner occupied homes. 

range(Boston$black)
range(Boston$crim)
range(Boston$lstat)
#Many of the other values make sense as to why these homes are valued so low. There are high crime rates, they are not near the river, they are not big lots, the tax is high, and a high percentage of the population is poor.

#H)
seven_or_more = sum(Boston$rm >= 7)
print(seven_or_more)
#64 suburbs average more than 7 rooms per dwelling
eight_or_more = sum(Boston$rm >= 8)
print(eight_or_more)
print(Boston[Boston$rm >=8,])
#When looking at the suburbs that average 8 or more rooms per house, we see that the medv is higher which makes since because it is more expensive to build a house with more rooms. 


#2----

#A)
set.seed(1)
x = rnorm(100,0,1)

#B)
eps = rnorm(100,0,.25)

#C)
y = -1+.5*x + eps
length(y)
#The length of vector y is 100 b0 is -1 and b1 is .5

#D)
plot(x,y, main="X vs Y", xlab = "x", ylab = "y")
# There appears to be a linear relationship between x and y. 

#E)
lm_xy = lm(y~x)
summary(lm_xy)
#The model is yhat=-1.009 + .499x
#As we can see, the b0hat and b1hat are very close to b0 and b1

#F)
plot(x,y, main="X vs Y", xlab = "x", ylab = "y")
abline(lm_xy, col = "red")
abline(a=-1, b=.5, col = "blue")
legend("topleft", legend = c("Least Squares Line", "Population Regression Line"), col = c("red","blue"), lty = 1)


#G)
lm_quad = lm(y~x+I(x*x))
summary(lm_quad)
# The pvalue of the x^2 term is fairly high. Given an alpha of .05, the pvalue of this term is well over, so it should be removed from the model. Therefore, the quadratic term does not improve the model fit.


#H)
set.seed(1)
x = rnorm(100,0,1)
eps = rnorm(100,0,.01)
y = -1+.5*x + eps
length(y)
#The length of vector y is 100 b0 is -1 and b1 is .5
plot(x,y, main="X vs Y", xlab = "x", ylab = "y")
# There appears to be a much stronger linear relationship between x and y compared to before.
lm_xy = lm(y~x)
summary(lm_xy)
#The model is yhat=-1.0003 + .49998x
#As we can see, the b0hat and b1hat are closer to b0 and b1 compared to before.
plot(x,y, main="X vs Y", xlab = "x", ylab = "y")
abline(lm_xy, col = "red")
abline(a=-1, b=.5, col = "blue")
legend("topleft", legend = c("Least Squares Line", "Population Regression Line"), col = c("red","blue"), lty = 1)
#The lines are ontop of eachother, meaning they are practically the same.
lm_quad = lm(y~x+I(x*x))
summary(lm_quad)
# The pvalue of the x^2 term is fairly high. Given an alpha of .05, the pvalue of this term is well over, so it should be removed from the model. Therefore, the quadratic term does not improve the model fit.

#3----
#A)
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+.3*x2+rnorm(100)
# The form is y = B0 + B1 * X1 + B2 * X2 + e
# The coeffiecents are B0 = 2, B1 = 2, B2 = .3

#B)
cor(x1,x2)
# The correlation between x1 and x2 is .84 which is high. 
plot(x1, x2, main = "X1 Vs X2", xlab = "X1,", ylab = "X2)")


#C)
library(car)

lm_mod = lm(y~x1+x2)
summary(lm_mod)
vif(lm_mod)
# x2 has a very high p value and should be dropped from the model if the level of significance is .95. The R2 is only .2 so the model is not explaining much of the varianace. 
# The regression coefficients are Bhat0 = 2.13 Bhat1 = 1.44 Bhat2 =  1.01 which are somewhat close to the true values. 
# We can reject the null hypothesis of x1 because the pvalue is lower than the alpha at the assumed level of sig of .95.
# We cannot reject the null hypothesis of x2 because the pvalue is higher than the alpha at the assumed level of sig of .95.
# The vif is not crazy high for either, but the values are still high and with the plot, we can see that multicolinearity is going to be a problem. 

#D)
lm_x1 = lm(y~x1)
summary(lm_x1)

#Yes, we can reject the null hypothesis that B1 = 0 because the p value of x1 is extremely small.

#E)
lm_x2 = lm(y~x2)
summary(lm_x2)
#Yes, we can reject the null hypothesis that B1 = 0 because the p value of x2 is extremely small.

#F)
# Since x1 is a strong predictor of y and x2 is a strong predictor of y, we would expect both of them together to create a very strong model for y. This is not the case though. 

#G)
x1=c(x1, 0.1)
x2 = c(x2, 0.8)
y=c(y,6)

lm_mod = lm(y~x1+x2)
summary(lm_mod)
#Now x1 has a very high pvalue and should not be in the model
plot(lm_mod)
#Data point 101 doesn't appear in the plots until the cooks distance plot. It has a value over 1, so it is an influential point.
hatvalues(lm_mod)
#Data point 101 is not a high leverage point.


lm_x1 = lm(y~x1)
summary(lm_x1)
#x1 is still a very strong predictor on its own. 
plot(lm_x1)
#We now see data point 101 show up in the sqrt Standardized Residuals vs fitted with a value of close to 2 meaning it is not technically an outlier but it is a point of concern.   
hatvalues(lm_x1)
#Data point 101 is not a high leverage point.

lm_x2 = lm(y~x2)
summary(lm_x2)
#x2 is still a very strong predictor on its own. 
plot(lm_x2)
#Datapoint 101 is not an influential point, outlier, or point of leverage in this model. I think that is why y~x2 has a lower pvalue now than y~x1.
hatvalues(lm_x2)
