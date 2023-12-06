# Midterm
#5)
library(ISLR2)
data("Weekly", package = "ISLR2")

#A
pairs(Weekly)

dim(Weekly)

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)

#B

glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Weekly$Direction)


#C
attach(Weekly)
training_data = (Year >= 1990 & Year <= 2008)
testing_data = Weekly[!training_data, ]
dim(testing_data)
glm_Lag123 = glm(Direction ~ Lag1 + Lag2 + Lag3, data = Weekly, family = binomial, subset = training_data)



glm.probs = predict(glm_Lag123, testing_data, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, testing_data$Direction)

mean(glm.pred == testing_data$Direction)

#D)
library(MASS)
training_data = Weekly$Year >= 1990 & Weekly$Year <= 2008
testing_data = Weekly[!training_data, ]
lda_Lag123 = lda(Direction ~ Lag1 + Lag2 + Lag3, data = Weekly, subset = training_data)

lda_pred = predict(lda_Lag123, testing_data)
lda_class = lda_pred$class
table(lda_class, testing_data$Direction)
mean(lda_class == testing_data$Direction)

#E)
library(class)
training_data = (Year >= 1990 & Year <= 2008)
testing_data = Weekly[!training_data, ]

# Create matrices for "Lag2" as predictors
train.X = cbind(Lag1, Lag2, Lag3)[training_data, ]
test.X = cbind(Lag1, Lag2, Lag3)[!training_data, ]
train.Direction = Direction[training_data]

set.seed(2016)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, testing_data$Direction)
mean(knn.pred == testing_data$Direction)




