##
# Author : Joydeep
# Date   : 22/03/2017
# Topic  : Implement logistic regression and compare with glm 
##

#Setting working directory location
setwd("~/Documents/Code/LogisticRegression")

#Load data
data <- read.csv("data.csv")

#create a plot 
plot(data$score.1,data$score.2, col=as.factor(data$label), xlab="Score-1", ylab="Score-2")

#from the plot we can say that  we need a non linear regression

#Predictor variables
X <- as.matrix(data[,c(1,2)])

#Add ones to X
X <- cbind(rep(1,nrow(X)),X)

#Response variable
Y <- as.matrix(data$label)

#Sigmoid function or link or logit
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

#Cost Function
cost <- function(theta)
{
  m <- nrow(X)
  g <- sigmoid(X%*%theta)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J)
}

#Intial theta
initial_theta <- rep(0,ncol(X))

#Cost at inital theta
cost(initial_theta)

# Derive theta using gradient descent using optim function
theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# probability of admission for student with 45 and 85 
prob <- sigmoid(t(c(1,45,85))%*%theta)

#Since there are chances of getting admission with probability of 0.774 
#We are now comparing with glm function in R 
data$label <- factor(data$label)

#Creating a model
myLogit <- glm(label ~ score.1 + score.2, data, family = "binomial")

#Model Summary
summary(myLogit)

#simulating Test data
test <- data.frame(45,85)
colnames(test,  do.NULL = TRUE, prefix = "col")
colnames(test) <- c("score.1","score.2")

#Predicting 
predict(myLogit, test, type="response")

#It predits it will go to Binary class 1 with probability 0.776 which is almost equal to what we predicted above.
