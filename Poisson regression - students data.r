#Libraries
library(dplyr)
library(readr)
library(tidyverse)
library(magrittr)
library(MASS)
#Dataset
dataset = read.csv("student-por.csv", sep = ";", header = TRUE)
dataset %<>% mutate(across(where(is.character), as.factor))

Y = dataset$G3
X = dataset[,-c(31,32,33)]


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

#CONTINGENCY TABLES
prop.table(table(dataset$school,dataset$studytime), 1 )
prop.table(table(dataset$sex,dataset$failures), 1 )
prop.table(table(dataset$schoolsup,dataset$failures), 1 )
prop.table(table(dataset$activities,dataset$studytime), 1 )
prop.table(table(dataset$higher,dataset$sex), 1 )
#independence test
chisq.test(table(dataset$higher,dataset$sex))

#correlation between numeric variables
#Here we plot the correlation matrix of all numeric vaiables using the corrplot package
num_variables = select_if(X, is.numeric)
cor(num_variables, Y, method = "spearman")

cor_matr <- cor(num_variables, method = "spearman")
cor_matr
library(corrplot)
corrplot(cor_matr, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)



# Does it look like a Poisson?
# Here we create a plot to check whether the outcome variable is Poisson distributed
prob <- dpois(dataset$G3, mean(dataset$G3))
prob
plot(dataset$G3, prob, pch = 16, col = "orange", type = "h")
points(dataset$G3, prob, pch = 16, col = "orange", type = "p")
# From the graph we see it does look like a Poisson distribution

#####POISSON REGRESSION MODEL
# Here we regress the outcome variable G3 on all other variables except G1 and G2 using Poisson regression
mod = glm(Y ~ ., data=X, family=poisson)
summary(mod)

#H Here we perform stepwise variable selection to find the best model
# We finish with an AIC of 3313.1
best_mod = step(glm(Y ~ ., data=X, family=poisson))
summary(best_mod)

exp((best_mod$coefficients))


#deviance goodness of fit
1-pchisq(best_mod$deviance, best_mod$df.residual) #good fit

#residuals analysis
#Here we plot the residuals in a scatterplot and then a histogram showing how the residuals are distributed, first usig Pearson, then Deviance residuals
resid <- resid(best_mod, type = "pearson")
fitted <- fitted(best_mod)
plot(fitted, resid, main = "Pearson residuals")
hist(resid)

resid <- resid(best_mod, type = "deviance")
fitted <- fitted(best_mod)
plot(fitted, resid, main = "Deviance residuals")
hist(resid)


plot(best_mod, which = 1) # Pearson residuals

##### prediction
#Here we set a random seed for a random train/test split. Then we split up our dataset in 80 % training data and 20 % testing data.
set.seed(23)
tr = sample(nrow(X), nrow(X)*0.8)

#Here we train our model only using the training data
train_model <- glm(Y[tr] ~ school + sex + Fedu + studytime + failures + 
                     schoolsup + higher + Dalc + health, data = X[tr,])

summary(train_model)

#RMSE on the train set
sqrt(mean((Y[tr] - train_model$fitted.values)^2))


pred <- predict(train_model, newdata = X[-tr,], type = "response")
pred

# RMSE on the test set
sqrt(mean((Y[-tr] - pred)^2))

#benchmark
mod_bench = glm(Y ~ 1, data = X, family = "poisson")
summary(mod_bench)
pr_bench=predict(mod_bench,type="response")
mean((pr_bench - Y)^2)
rmse <- sqrt(mean((Y - pr_bench)^2))
rmse
