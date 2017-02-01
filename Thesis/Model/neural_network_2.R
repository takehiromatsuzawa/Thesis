# The dataset
fname=file.choose()
data=read.csv(fname,header=T)


set.seed(500)
data=data[data$Position != "G", ]
apply(data,2,function(x) sum(is.na(x)))
data$X <- NULL
data$hits_all <- NULL
data$blocks_all <- NULL
data$fc_win<- NULL
data$fc_loss <- NULL

data$assists_pp <- NULL
data$assists_ev <- NULL
data$assists_sh <- NULL

data$date_game <- NULL

data$gm_loc_a[data$gm_loc=="Home"] <- 0
data$gm_loc_a[data$gm_loc=="Away"] <- 1
data$gm_loc_h[data$gm_loc=="Home"] <- 1
data$gm_loc_h[data$gm_loc=="Away"] <- 0
data$gm_loc <- NULL

data$gm_res_w[data$gm_result=="W"] <- 1
data$gm_res_w[data$gm_result=="L"] <- 0
data$gm_res_l[data$gm_result=="W"] <- 0
data$gm_res_l[data$gm_result=="L"] <- 1
data$gm_result <- NULL


data$Name <- NULL
data$Url <- NULL

data$gm_hand_r[data$ShotHand=="R"] <- 1
data$gm_hand_r[data$ShotHand=="L"] <- 0
data$gm_hand_l[data$ShotHand=="R"] <- 0
data$gm_hand_l[data$ShotHand=="L"] <- 1
data$ShotHand <- NULL

data$gm_pos_d[data$Position=="D"] <- 1
data$gm_pos_d[data$Position!="D"] <- 0
data$gm_pos_f[data$Position!="D"] <- 1
data$gm_pos_f[data$Position=="D"] <- 0

data$Position <- NULL
data$Year <- NULL
data$draft_years <- NULL
data$goals_all <- NULL

normalize <- function(x) {
  return ((x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)))
}


data$goals_ev<-normalize(data$goals_ev)
data$goals_pp<-normalize(data$goals_pp)
data$goals_sh<-normalize(data$goals_sh)

data$assists_all<-normalize(data$assists_all)

data$plus_minus<-normalize(data$plus_minus)
data$pen_min<-normalize(data$pen_min)

data$num_shifts<-normalize(data$num_shifts)
data$time_on_ice<-normalize(data$time_on_ice)

data$Height<-normalize(data$Height)
data$Weight<-normalize(data$Weight)
data$Drafts<-normalize(data$Drafts)






#'response_assists',
mycolumns<-c( 'assists_all','plus_minus','pen_min','goals_ev',
           'goals_sh','num_shifts','Height','Weight','Drafts','gm_loc_a'
           ,'gm_loc_h','gm_res_w','gm_res_l','gm_hand_r','gm_hand_l',
           'gm_pos_d','gm_pos_f','response_goal')

columns_lm<-c( 'assists_all','plus_minus','pen_min','goals_ev','goals_pp',
               'goals_sh','num_shifts','Height','Weight','Drafts','gm_loc_a'
               ,'gm_loc_h','gm_res_w','gm_hand_r',
               'gm_pos_f','response_goal')


data_lm <- data[columns_lm]
data_lm  <- data_lm[complete.cases(data_lm), ]
data_lm$response_goal<-data_lm$response_goal*5
apply(data_lm,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data_lm),round(0.8*nrow(data_lm)))
train_lm <- data_lm[index,]
test_lm  <- data_lm[-index,]
lm.fit <- glm(response_goal~., data=train_lm)
summary(lm.fit)
pr.lm <- predict(lm.fit,test_lm)
MSE.lm <- sum((pr.lm - test_lm$response_goal)^2)/nrow(test_lm)

# Preparing to fit the neural network
#maxs <- apply(data, 2, max) 
#mins <- apply(data, 2, min)

#scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

#train_ <- scaled[index,]
#test_ <- scaled[-index,]


columns_nn<-c( 'assists_all','plus_minus','pen_min','goals_ev','goals_pp',
              'goals_sh','num_shifts','Height','Weight','Drafts','gm_loc_a'
              ,'gm_loc_h','gm_res_w','gm_res_l','gm_hand_r','gm_hand_l',
              'gm_pos_d','gm_pos_f','response_goal')

columns_nn<-c( 'assists_all','plus_minus','pen_min','goals_ev','goals_pp',
               'goals_sh','num_shifts','Height','Weight','Drafts','response_goal')

columns_nn<-c( 'assists_all','plus_minus')

data_nn <- data[columns_nn]
data_nn  <- data_nn[complete.cases(data_nn), ]
data_nn.pca <- prcomp(data_nn,
                 center = TRUE,
                 scale. = TRUE) 
print(data_nn.pca)
plot(data_nn.pca, type = "l")

std_dev <- data_nn.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")


data_nn  <- data_nn[complete.cases(data_nn), ]
data_nn.pca_data <- data.frame(response_goal = data_nn$response_goal, data_nn.pca$x)
data_nn_pca <- data_nn.pca_data[,1:5]

index <- sample(1:nrow(data_nn_pca),round(0.8*nrow(data_nn_pca)))
train_nn <- data_nn_pca[index,]
test_nn  <- data_nn_pca[-index,]

train_<-train_nn
test_<-test_nn

# Parameters
library(neuralnet)
params <- c(5,7,9)
param<-3

#for (param in params) {
ptm <- proc.time()
n <- names(train_)
f <- as.formula(paste("response_goal ~", paste(n[!n %in% "response_goal"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(param),linear.output=T,stepmax=1e6)
  
# Predicting goals_all using the neural network
pr.nn <- compute(nn,test_[,1:4])
test.r <- (test_$response_goal)
pr.nn_ <- pr.nn$net.result
  
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(param)
print(MSE.nn)
print('')

print(proc.time() - ptm)
#}

# Real vs predicted NN
par(mfrow=c(1,2))
plot(test$goals_all,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$goals_all,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


# Poisson distribution







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
