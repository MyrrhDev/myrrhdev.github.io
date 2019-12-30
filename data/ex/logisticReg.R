## Logistic regression
####################################################################

##  Task: classifying spam mail. When there is an interest in minimizing a particular source of errors ##  how do we change the 'cut point' for prediction?

library(MASS)
library(kernlab)  
data(spam)

## Pre-processing
spam[,55:57] <- as.matrix(log10(spam[,55:57]+1))
spam2 <- spam[spam$george==0,]
spam2 <- spam2[spam2$num650==0,]
spam2 <- spam2[spam2$hp==0,]
spam2 <- spam2[spam2$hpl==0,]
george.vars <- 25:28
spam2 <- spam2[,-george.vars]
moneys.vars <- c(16,17,20,24)
spam3 <- data.frame( spam2[,-moneys.vars], spam2[,16]+spam2[,17]+spam2[,20]+spam2[,24])
colnames(spam3)[51] <- "about.money"
dim(spam3)
set.seed (4321)
n <- nrow(spam3)                                                                                              
learn <- sample(1:n, round(0.67*n))
nlearn <- length(learn)
ntest <- n - nlearn

## Fit a GLM in the learning data
spamM1 <- glm (type ~ ., data=spam3[learn,], family=binomial)

## Simplify it using the AIC (this may take a while, since there are many variables)
spamM1.AIC <- step (spamM1)

#Fitted probabilities numerically very close to 0 or 1

# Probability at least P then we predict spam
spam.accs <- function (P=0.5)
{
  ## Compute accuracy in learning data
  spamM1.AICpred <- NULL
  spamM1.AICpred[spamM1.AIC$fitted.values<P] <- 0
  spamM1.AICpred[spamM1.AIC$fitted.values>=P] <- 1
  spamM1.AICpred <- factor(spamM1.AICpred, labels=c("nonspam","spam"))
  print(M1.TRtable <- table(Truth=spam3[learn,]$type,Pred=spamM1.AICpred))
  print(100*(1-sum(diag(M1.TRtable))/nlearn))
  
  ## Compute accuracy in test data
  gl1t <- predict(spamM1.AIC, newdata=spam3[-learn,],type="response")
  gl1predt <- NULL
  gl1predt[gl1t<P] <- 0
  gl1predt[gl1t>=P] <- 1
  gl1predt <- factor(gl1predt, labels=c("nonspam","spam"))
  print(M1.TEtable <- table(Truth=spam3[-learn,]$type,Pred=gl1predt))
  print(100*(1-sum(diag(M1.TEtable))/ntest))
}

spam.accs()
# 7.21% training error and 7.07% testing error

## Just like in python we can try to lower the probability of predicting spam when it is not
# We can do this (at the expense of increasing the converse probability) by:

spam.accs(0.7)

# gives 9.66% TRAINING ERROR and 10.3% TESTING ERROR

## The filter has a lower probability of predicting spam when it is not (which is the delicate case), of about 6.77% and is much better overall.
