# The dataset
fname=file.choose()
library(pscl)
require(ggplot2)
require(pscl)
require(boot)

data=read.csv(fname,header=T)
data <- data[sample(nrow(data)),]

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

#unfactor
data$Height<-as.numeric(data$Height)
data$Weight<-as.numeric(data$Weight)

data$Height<-normalize(data$Height)
data$Weight<-normalize(data$Weight)
data$Drafts<-normalize(data$Drafts)
data$gm_loc<-normalize(data$gm_loc)


columns_pos<-c( 'assists_all','plus_minus','pen_min','goals_ev','goals_pp',
               'goals_sh','num_shifts','Height','Weight','Drafts'
               ,'gm_loc','gm_result','gm_hand_r',
               'gm_pos_f','response_points')


data_pos <- data[columns_pos]
data_pos <- data_pos[complete.cases(data_pos), ]
apply(data_pos,2,function(x) sum(is.na(x)))


index <- sample(1:nrow(data_pos),round(0.8*nrow(data_pos)))
train_pos <- data_pos[index,]
test_pos  <- data_pos[-index,]

poisson.fit <- glm(response_points~.,data=train_pos, family = poisson)
summary(poisson.fit)
pos.pred <- predict(poisson.fit,test_pos)
MSE.pos <- sum((pos.pred  - test_pos$response_points)^2)/nrow(test_pos)


poisson.fit.best <- step(poisson.fit,trace=0)
summary(poisson.fit.best)
poisson.pred.best <- predict(poisson.fit.best,test_pos)
MSE.poisson.best <- sum((poisson.pred.best - test_pos$response_points)^2)/nrow(test_pos)


zero_pos.fit<-zeroinfl(response_points~.| 1, data=train_pos)
summary(zero_pos.fit)
zero_pos.pred <- predict(zero_pos.fit,test_pos)
MSE.zero_pos <- sum((zero_pos.pred - test_pos$response_points)^2)/nrow(test_pos)

zero_pos.fit.best <- step(zero_pos.fit,trace=0)
summary(zero_pos.fit.best)
zero_pos.pred.best <- predict(zero_pos.fit.best,test_pos)
MSE.zero_pos.best <- sum((zero_pos.pred.best - test_pos$response_points)^2)/nrow(test_pos)


# Real vs predicted NN
par(mfrow=c(1,1))
plot(test_pos$response_points,pos.pred,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
#points(test_pos$response_points,zero_pos.pred,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


columns_lm<-c( 'assists_all','plus_minus','pen_min','goals_ev','goals_pp',
               'goals_sh','num_shifts','Height','Weight','Drafts','gm_loc'
               ,'gm_result','gm_hand_r',
               'gm_pos_f','response_goal')


data_lm <- data[columns_lm]
data_lm  <- data_lm[complete.cases(data_lm), ]
data_lm$response_goal<-data_lm$response_goal*5
apply(data_lm,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data_lm),round(0.8*nrow(data_lm)))
train_lm <- data_lm[index,]
test_lm  <- data_lm[-index,]
lm.fit <- glm(response_goal~., data=train_lm)
pr.lm <- predict(lm.fit,test_lm)
MSE.lm <- sum((pr.lm - test_lm$response_goal)^2)/nrow(test_lm)

lm.fit.best <- step(lm.fit,trace=0)
summary(lm.fit.best)
pr.lm.best <- predict(lm.fit.best,test_lm)
MSE.lm.best <- sum((pr.lm.best - test_lm$response_goal)^2)/nrow(test_lm)
