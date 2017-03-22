##
# Author : Joydeep
# Subject: Implementing linear regression using R
# Date: 24/02/2017
##

#importing Data 
setwd("/Users/Joydeep/Desktop")
myData <- read.csv("data.csv")

# Preparing the data 
names(myData)<- c("Population", "Profit")
y = myData$Profit
x = myData$Population

# Visualizing the data 
plot(x,y,xlab = "population in 10,000", ylab = "Profit in $10,000")

x = cbind(1,x)

#initialize theta 
theta = c(0,0)

m = nrow(x)

#calculate cost 
cost = sum(((x %*% theta ) - y)^2)/2*m

# Set Learning Parameter 
alpha = 0.001

# No of iterations 
iterations = 1500

for(i in 1:iterations){
  theta[1] = theta[1] - alpha *(1/m) * sum(((x%*%theta)- y))
  theta[2] = theta[2] - alpha *(1/m) * sum(((x%*%theta)- y) * x[,2])
}

#Predict for areas of the 35,000 and 70,000 people
predict1 <- c(1,3.5) %*% theta
predict2 <- c(1,7) %*% theta

predict1
predict2


