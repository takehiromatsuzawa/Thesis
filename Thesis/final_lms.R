########################################
# a. Data Cleaning
#    1. Standardize quantatative variables 
#
# b. Linear Regressions
#    1. Ordinary Linear Regressions
#       Variance Inflation Factor
#       Cook's Distance
#    2. Ridge  Regressions
#    3. Lasso  Regressions
#    4. Elastic Net Regressions
#
########################################

fname=file.choose()
library(pscl)
require(ggplot2)
require(pscl)
require(boot)
library(FNN)
library(lars)
library(randomForest)
library(glmnet)
library(car)
library(MASS)
data=read.csv(fname,header=T)

set.seed(500)
#Remove Goalies
data=data[data$Position != "G", ]

#Remove Unnecessary columns
data$X <- NULL
data$hits_all <- NULL
data$blocks_all <- NULL
data$fc_win<- NULL
data$fc_loss <- NULL
data$assists_pp <- NULL
data$assists_ev <- NULL
data$assists_sh <- NULL
data$dates_game <- NULL
data$Name <- NULL
data$Url <- NULL

#right-handed
data$gm_hand_r[data$ShotHand=="R"] <- 1
data$gm_hand_r[data$ShotHand=="L"] <- 0

#left-handed
data$gm_hand_l[data$ShotHand=="R"] <- 0
data$gm_hand_l[data$ShotHand=="L"] <- 1
data$ShotHand <- NULL

#position
data$gm_pos_d[data$Position=="D"] <- 1
data$gm_pos_d[data$Position!="D"] <- 0
data$gm_pos_f[data$Position!="D"] <- 1
data$gm_pos_f[data$Position=="D"] <- 0

data$Position <- NULL
data$Year <- NULL
data$draft_years <- NULL
data$goals_all <- NULL
data$tm_agt <- NULL
data$tm_for <- NULL

#standalize all the numeric variables
data$goals_ev<-scale(data$goals_ev)
data$goals_pp<-scale(data$goals_pp)
data$goals_sh<-scale(data$goals_sh)

data$assists_all<-scale(data$assists_all)

data$plus_minus<-scale(data$plus_minus)
data$pen_min<-scale(data$pen_min)

data$num_shifts<-scale(data$num_shifts)
data$time_on_ice<-scale(data$time_on_ice)


data$Height<-as.numeric(data$Height)
data$Weight<-as.numeric(data$Weight)

data$Height<-scale(data$Height)
data$Weight<-scale(data$Weight)
data$Drafts<-scale(data$Drafts)
data$age<-scale(data$age)

data$tm_gm_loc_away<-scale(data$tm_gm_loc_away)
data$tm_num_games_away<-scale(data$tm_num_games_away)

data$tm_num_games_home<-scale(data$tm_num_games_home)

data$player_num_games<-scale(data$player_num_games)
data$player_gm_result<-scale(data$player_gm_result)

data$cum_wins_for_away<-scale(data$cum_wins_for_away)
data$cum_loss_for_away<-scale(data$cum_loss_for_away)

data$tm_shots_for_away<-scale(data$tm_shots_for_away)
data$tm_goals_sh_for_away<-scale(data$tm_goals_sh_for_away)
data$tm_goals_pp_for_away<-scale(data$tm_goals_pp_for_away)
data$tm_pen_min_away<-scale(data$tm_pen_min_away)

data$tm_shots_agt_away<-scale(data$tm_shots_agt_away)
data$opp_pen_min_away<-scale(data$opp_pen_min_away)
data$tm_goals_agt_pp_away<-scale(data$tm_goals_agt_pp_away)
data$tm_goals_agt_sh_away<-scale(data$tm_goals_agt_sh_away)

data$cum_wins_for_home<-scale(data$cum_wins_for_home)
data$cum_loss_for_home<-scale(data$cum_loss_for_home)

data$tm_shots_for_home<-scale(data$tm_shots_for_home)
data$tm_goals_sh_for_home<-scale(data$tm_goals_sh_for_home)
data$tm_goals_pp_for_home<-scale(data$tm_goals_pp_for_home)
data$tm_pen_min_home<-scale(data$tm_pen_min_home)

data$tm_shots_agt_home<-scale(data$tm_shots_agt_home)
data$opp_pen_min_home<-scale(data$opp_pen_min_home)
data$tm_goals_agt_pp_home<-scale(data$tm_goals_agt_pp_home)
data$tm_goals_agt_sh_home<-scale(data$tm_goals_agt_sh_home)
data$tm_goals_for_home<-scale(data$tm_goals_for_home)
data$tm_goals_for_away<-scale(data$tm_goals_for_away)



columns_lm <- c("player_gm_result",     "age",                  "player_num_games",    
                "player_gm_loc",        "assists_all",          "plus_minus",          
                "pen_min",              "goals_ev",             "goals_pp",            
                "goals_sh",             "num_shifts",           "time_on_ice",         
                "cum_loss_for_home",    "cum_loss_for_away",    "cum_wins_for_home",   
                "cum_wins_for_away",    "opp_pen_min_away",     "opp_pen_min_home",    
                "tm_goals_agt_pp_away", "tm_goals_agt_pp_home",
                "tm_goals_agt_sh_away", "tm_goals_agt_sh_home", "tm_goals_for_away",   
                "tm_goals_for_home",     "tm_loss_away",        
                "tm_loss_home",         "tm_num_games_away",    "tm_num_games_home",   
                "tm_shots_agt_away",   "tm_shots_agt_home",     
                "Height", "Weight", "Drafts", "response_points",     
                "gm_hand_r", "gm_pos_f" )

data_lm <- data[columns_lm]
data_lm  <- data_lm[complete.cases(data_lm), ]

#Check NA values
apply(data_lm,2,function(x) sum(is.na(x)))

#Ordinary Linear Model
#Bagging 10 time
MSE.lms <- c(0)
for (ith in 1:10){
  index <- sample(1:nrow(data_lm),round(0.8*nrow(data_lm)))
  train_lm <- data_lm[index,]
  test_lm  <- data_lm[-index,]
  lm.fit <- glm(response_points~., data=train_lm)
  pr.lm <- predict(lm.fit,test_lm)
  MSE.lm <- sum((pr.lm - test_lm$response_points)^2)/nrow(test_lm)
  MSE.lms[ith]<-MSE.lm
}
print(mean(MSE.lms))

#Variance Inflation Factor
vif(lm.fit)

#Remove highly correlated variables
lm.fit2 <- glm(response_points~. - tm_num_games_home- tm_num_games_away-cum_wins_for_away
               - cum_loss_for_away, data=train_lm)
summary(lm.fit2)
#Variance Inflation Factor 2
vif(lm.fit2)


pr.lm2 <- predict(lm.fit2,test_lm)
MSE.lm2 <- sum((pr.lm2 - test_lm$response_points)^2)/nrow(test_lm)

#Cook's Distance
cooks_dist<-cooks.distance(lm.fit2)

barplot(cooks_dist, xlab = "index", 
        ylab = "Cook's Distance",axisnames=FALSE)

#max of cook's distance
max(cooks_dist)
#histogram
hist(data_lm$response_points,xlab='Actual Average Number of Points',main = '',col = 'Red')

#histogram
hist(sqrt(data_lm$response_points),xlab='Actual Average Number of Points (Squared )',main = '',col = 'Blue')




lambdas <- c(0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001)
for (lambda in lambdas){
  mse.lassos<-c(0)
  for (ith in 1:10){
    index <- sample(1:nrow(data_lm),round(0.8*nrow(data_lm)))
    train_lm <- data_lm[index,]
    test_lm  <- data_lm[-index,]
  
    train_y <- as.matrix(train_lm$response_points)
    train_x <-as.matrix(train_lm[ , ! colnames(train_lm) %in% c("response_points") ])

    test_y <- as.matrix(test_lm$response_points)
    test_x <-as.matrix(test_lm[ , ! colnames(test_lm) %in% c("response_points") ])

    #Lasso Regression
    # fit model
    fit.lasso <- glmnet(train_x, train_y, family="gaussian", alpha=1, lambda=lambda)
  
    # make predictions
    pred.lasso <- predict(fit.lasso, test_x, type="link")
    # summarize accuracy
    mse.lasso <-sum((test_y - pred.lasso)^2)/nrow(test_y)
    mse.lassos[ith]<-mse.lasso
  }
  print(mse.lassos)
  print(paste("lambda", toString(lambda), sep=": "))
  print(mean(mse.lassos))
  print('')
}



#Ridge Regression
#change lambda
#Bagging 10 times
lambdas <- c(0.1, 0.05, 0.01, 0.005, 0.001, 0.0005, 0.0001, 0.00005, 0.00001)
for (lambda in lambdas){
  mse.ridges<-c(0)
  for (ith in 1:10){
    index <- sample(1:nrow(data_lm),round(0.8*nrow(data_lm)))
    train_lm <- data_lm[index,]
    test_lm  <- data_lm[-index,]
  
    train_y <- as.matrix(train_lm$response_points)
    train_x <-as.matrix(train_lm[ , ! colnames(train_lm) %in% c("response_points") ])
  
    test_y <- as.matrix(test_lm$response_points)
    test_x <-as.matrix(test_lm[ , ! colnames(test_lm) %in% c("response_points") ])
  
  
    fit.ridge <- glmnet(train_x, train_y, family="gaussian", alpha=0, lambda=lambda)

    # make predictions
    pred.ridge <- predict(fit.ridge, test_x, type="link")

    mse.ridge <- mean((test_y - pred.ridge)^2)
    mse.ridges[ith]<-mse.ridge
  }
  print(paste("lambda", toString(lambda), sep=": "))
  print(mean(mse.ridges))
  print('')
}





#Elastic Net
lambdas <- c(0.01, 0.005, 0.001)
alphas <- c(0.01, 0.05, 0.1)
mse.enets<-c(0)
for (lambda in lambdas){
  for (alpha in alphas){
    for (ith in 1:10){
      index <- sample(1:nrow(data_lm),round(0.8*nrow(data_lm)))
      train_lm <- data_lm[index,]
      test_lm  <- data_lm[-index,]
      
      train_y <- as.matrix(train_lm$response_points)
      train_x <-as.matrix(train_lm[ , ! colnames(train_lm) %in% c("response_points") ])
      
      test_y <- as.matrix(test_lm$response_points)
      test_x <-as.matrix(test_lm[ , ! colnames(test_lm) %in% c("response_points") ])
      
      
      fit.enet <- glmnet(train_x, train_y, family="gaussian", alpha=alpha, lambda=lambda)
      pred.enet <- predict(fit.enet, test_x, type="link")
  
      mse.enet <- mean((test_y - pred.enet)^2)
      mse.enets[ith] <- mse.enet
    }
    print(paste("lambda", toString(lambda), sep=": "))
    print(paste("alpha", toString(alpha), sep=": "))
    print(mean(mse.enets))
    print('')
  }
}
