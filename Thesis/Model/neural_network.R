# The dataset
fname=file.choose()
data=read.csv(fname,header=T)


set.seed(500)

apply(data,2,function(x) sum(is.na(x)))
data$X <- NULL
data$shots_attempted.1 <- NULL
data$shots_attempted.2 <- NULL
data$shots_attempted.3 <- NULL

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(goals_all~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$goals_all)^2)/nrow(test)

# Preparing to fit the neural network
#maxs <- apply(data, 2, max) 
#mins <- apply(data, 2, min)

#scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

#train_ <- scaled[index,]
#test_ <- scaled[-index,]

train_<-train
test_<-test

# Parameters
library(neuralnet)

n <- names(train_)
f <- as.formula(paste("goals_all ~", paste(n[!n %in% "goals_all"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(9),linear.output=T)

plot(nn)

# Predicting medv using the neural network
pr.nn <- compute(nn,test_[,1:16])
test.r <- (test_$goals_all)

#pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
#test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
pr.nn_ <- pr.nn$net.result

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))


# Real vs predicted NN
par(mfrow=c(1,2))
plot(test$goals_all,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$goals_all,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


# A (fast) cross validation
library(boot)
set.seed(200)
lm.fit <- glm(goals_all~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]



set.seed(450)
cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}


mean(cv.error)



boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)
