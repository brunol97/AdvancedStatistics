#Libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(magrittr)
library(MASS)
library(corrplot)
#Dataset
dataset = read.csv("student-por.csv", sep = ";", header = TRUE)
dataset %<>% mutate(across(where(is.character), as.factor))

########Exploratory data analysis######

#Frequency of study time
ggplot(dataset, aes(studytime)) + 
  geom_histogram(binwidth = .25, color = "black", 
                 fill = "purple") + 
  xlab("Study time") +
  ylab("Frequency")


#Frequency of school
barplot(table(dataset$school),
        xlab = "School",
        ylab = "Frequency")

#Frequency of failures
ggplot(dataset, aes(failures)) + 
  geom_histogram(binwidth = .25, color = "black", 
                 fill = "white") + 
  xlab("Number of failures") +
  ylab("Frequency")

#Frequency of absences
boxplot(dataset$absences, main = "Boxplot of absences",col = "blue")
summary(dataset$absences)

#CONTINGENCY TABLES
prop.table(table(dataset$school,dataset$studytime), 1 )
prop.table(table(dataset$sex,dataset$failures), 1 )
prop.table(table(dataset$higher,dataset$sex), 1 )
prop.table(table(dataset$Medu,dataset$Fedu), 1 )

#Independence test
chisq.test(table(dataset$higher,dataset$sex)) 
chisq.test(table(dataset$Medu,dataset$Fedu))

#correlation between numeric variables
num_variables = select_if(X, is.numeric)
cor(num_variables, Y, method = "spearman")

correlarions <- cor(num_variables, method = "spearman")
correlarions

corrplot(correlarions, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

corrplot(correlarions, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, method = "number")

###Setting up target variables and predictors
Y = dataset$G3
X = dataset[,-c(31,32,33)]
###Removal of the outliers 
w = which(Y<2)
Y.nozeros = Y[-w]
X.nozeros = X[-w,]

mean(Y.nozeros)
var(Y.nozeros)

# Does it look like a Poisson?
ggplot(dataset, aes(G3)) + 
  geom_histogram(binwidth = .5, color = "black", 
                 fill = "blue") + 
  xlab("Final Grades") +
  ylab("Frequency")

prob <- dpois(dataset$G3, mean(dataset$G3))#sample mean of our data
prob

plot(dataset$G3, prob, pch = 16, col = "orange", 
     type = "h", 
     xlab = "Final grades")

points(dataset$G3, prob, pch = 16, col = "orange", 
       type = "p")

#Comparison
par(mfrow = c(1,3))

barplot(prop.table(table(dataset$G3)),
        ylim = c(0,.15),
        xlab = "Final Grades",
        main = "Actual probability distribution",
        col = "blue")
         
plot(dataset$G3, prob, pch = 16, col = "orange", 
     type = "h",
     ylim = c(0,.15),
     xlab = "Final Grades", 
     main = "Theoretical Distribution")
points(dataset$G3, prob, pch = 16, col = "orange", type = "p")

barplot(prop.table(table(Y.nozeros)),
        ylim = c(0,.15),
        xlab = "Final Grades",
        main = "Target probability distribution",
        col = "green")

par(mfrow = c(1,1))

#####POISSON REGRESSION MODEL

mod = glm(Y.nozeros ~ ., data=X.nozeros, family=poisson)
summary(mod)

best_mod = step(glm(Y.nozeros ~ ., data=X.nozeros, family=poisson))
summary(best_mod)

best_mod_improved = glm(formula = Y.nozeros ~ school + age + Medu + 
                        failures + schoolsup + higher + absences, 
                        family = poisson, 
                        data = X.nozeros)

summary(best_mod_improved)
exp((best_mod_improved$coefficients))

#residuals analysis
resid <- resid(best_mod_improved, type = "pearson")
fitted <- fitted(best_mod_improved)
hist(resid)
plot(best_mod_improved, which = 1) # Pearson residuals

#PREDICTION
set.seed(23)
tr = sample(nrow(X.nozeros), nrow(X.nozeros)*0.8)

train_model <- glm(Y.nozeros[tr] ~ school + age + Medu + 
                   failures + schoolsup + higher + absences, 
                   family = poisson, 
                   data = X.nozeros[tr,])

summary(train_model)

#RMSE on the train set
sqrt(mean((Y.nozeros[tr] - train_model$fitted.values)^2))

pred <- predict(train_model, newdata = X.nozeros[-tr,], type = "response")

# RMSE on the test set
sqrt(mean((Y.nozeros[-tr] - pred)^2))


#BENCHMARK REGRESSION
train_model_bench = glm(Y.nozeros[tr] ~ 1, data = X.nozeros, family = "poisson")
summary(train_model_bench)

#RMSE on the benchmark train set
sqrt(mean((Y.nozeros[tr] - train_model_bench$fitted.values)^2))

pred_bench <- predict(train_model_bench, newdata = X.nozeros[-tr,], type = "response")

# RMSE on the benchmark test set
sqrt(mean((Y.nozeros[-tr] - pred_bench)^2))


#############Regression with G1 and G2###########

Y1 = dataset$G3
X1 = dataset[,-c(33)]
k = which(Y1 < 2)
Y1_noz = Y1[-k]
X1_noz = X1[-k,]

mod_tot <- glm(Y1_noz~., data = X1_noz, family = poisson )
summary(mod_tot)

mod_aic <- stepAIC(mod_tot)
summary(mod_aic)
exp(coefficients(mod_aic))

#PREDICTION
set.seed(23)
tr = sample(nrow(X1_noz), nrow(X1_noz)*0.8)

train_model <- glm(Y1_noz[tr] ~ G1 + G2, 
                     family = poisson, data = X1_noz[tr,])
summary(train_model)

#RMSE on the train set
sqrt(mean((Y1_noz[tr] - train_model$fitted.values)^2))

pred <- predict(train_model, newdata = X1_noz[-tr,], type = "response")

# RMSE on the test set
sqrt(mean((Y1_noz[-tr] - pred)^2))
