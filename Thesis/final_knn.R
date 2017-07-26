########################################
# a. Data Cleaning
#    1. Standardize quantatative variables 
#
# b. Ordinary K-Nearest Neighborhood
#
# c.  Attribute Weighted K-Nearest Neighborhood 
#     Backward Elimination (1st Iteration)
#
# d.  Refit model after 1st Iteration
#
# e.  Attribute Weighted K-Nearest Neighborhood 
#     Backward Elimination (2nd Iteration)
#
# f.  Refit model after 2nd Iteration
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



columns_kk<-c("player_gm_result",     "age",                  "player_num_games",    
              "player_gm_loc",                    
              "goals_sh",             "num_shifts",           "time_on_ice",         
              "cum_loss_for_home",    "cum_loss_for_away",    "cum_wins_for_home",   
              "cum_wins_for_away",    "opp_pen_min_away",     "opp_pen_min_home",    
              "tm_goals_agt_pp_away", "tm_goals_agt_pp_home",
              "tm_goals_agt_sh_away", "tm_goals_agt_sh_home", "tm_goals_for_away",   
              "tm_goals_for_home",    "tm_goals_pp_for_away", "tm_goals_pp_for_home",
              "tm_goals_sh_for_away", "tm_goals_sh_for_home", "tm_loss_away",        
              "tm_loss_home",         "tm_num_games_away",    "tm_num_games_home",   
              "tm_pen_min_away",      "tm_pen_min_home",      "tm_shots_agt_away",   
              "tm_shots_agt_home",    "tm_shots_for_away",    "tm_shots_for_home",   
              "Height",               "Weight",               "Drafts",              
              "response_points",     
              "gm_hand_r",                    
              "gm_pos_f", "assists_all",          "plus_minus",          
              "pen_min",              "goals_ev",             "goals_pp")





#Ordinary K-Nearest Neighborhood
MSE.kks<-c(0)
#Bagging 10 times
for (ith in 1:10){

  index <- sample(1:nrow(data_kk),round(0.8*nrow(data_kk)))
  train_kk <- data_kk[index,]
  test_kk  <- data_kk[-index,]
  
  train_kk_y <- train_kk$response_points
  train_kk_x <-train_kk[ , ! colnames(train_kk) %in% c("response_points") ]
  
  test_kk_y <- test_kk$response_points
  test_kk_x <-test_kk[ , ! colnames(test_kk) %in% c("response_points") ]
  
  pdt_knn <- knn.reg(train_kk_x, y=train_kk_y, test=test_kk_x,k=10)
  MSE.kk <- sum((pdt_knn$pred- test_kk_y)^2)/nrow(test_kk)
  print(MSE.kk)
  MSE.kks[ith]<-MSE.kk
}
print(MSE.kks)
print(mean(MSE.kks))




#Attribute Weighted K-Nearest Neighborhood
#Backward Elimination
#average of errors after you bag 10 times
for (column_kk in columns_kk){
  MSE.kks<-c(0)
  for (ith in 1:10){
    data_kk <- data[columns_kk]
    num_sample<-as.integer(nrow(data_kk)*0.1)
    data_kk<-data_kk[sample(nrow(data_kk),num_sample ), ]
    data_kk <- data_kk[complete.cases(data_kk), ]

    index <- sample(1:nrow(data_kk),round(0.8*nrow(data_kk)))
    train_kk <- data_kk[index,]
    test_kk  <- data_kk[-index,]
    
    train_kk_y <- train_kk$response_points
    train_kk_x <-train_kk[ , ! colnames(train_kk) %in% c("response_points",column_kk) ]
    
    test_kk_y <- test_kk$response_points
    test_kk_x <-test_kk[ , ! colnames(test_kk) %in% c("response_points",column_kk) ]
    
    pdt_knn <- knn.reg(train_kk_x, y=train_kk_y, test=test_kk_x,k=10)
    MSE.kk <- sum((pdt_knn$pred- test_kk_y)^2)/nrow(test_kk)
    MSE.kks[ith]<-MSE.kk
  }
  if (mean(MSE.kks)<0.1251503){
    print('Remove This Variable')
  }
  print(column_kk)
  print(MSE.kks)
  print(mean(MSE.kks))
  print('')
}





#Refit model after 1st Iteration
data_kk <- data[columns_kk]
num_sample<-as.integer(nrow(data_kk))
data_kk<-data_kk[sample(nrow(data_kk),num_sample ), ]
data_kk <- data_kk[complete.cases(data_kk), ]
remove_variables<-c("response_points","player_gm_loc",
                    "plus_minus","opp_pen_min_away","tm_goals_agt_sh_away","tm_goals_agt_sh_home",
                    "tm_goals_for_away","tm_goals_sh_for_away","tm_goals_sh_for_home",
                    "tm_loss_away","tm_pen_min_home","tm_shots_agt_away",
                    "tm_shots_agt_home","player_gm_loc")

MSE.kks<-c(0)
for (ith in 1:10){
  index <- sample(1:nrow(data_kk),round(0.8*nrow(data_kk)))
  train_kk <- data_kk[index,]
  test_kk  <- data_kk[-index,]
  train_kk_y <- train_kk$response_points
  train_kk_x <-train_kk[ , ! colnames(train_kk) %in%  remove_variables ]
  
  test_kk_y <- test_kk$response_points
  test_kk_x <-test_kk[ , ! colnames(test_kk) %in%  remove_variables ]
  
  pdt_knn <- knn.reg(train_kk_x, y=train_kk_y, test=test_kk_x,k=10)
  MSE.kk <- sum((pdt_knn$pred- test_kk_y)^2)/nrow(test_kk)
  print(MSE.kk)
  MSE.kks[ith]<-MSE.kk
}
print(MSE.kks)
print(mean(MSE.kks))


#Get Base Case
data_kk <- data[columns_kk]
remove_variables<-c("response_points","player_gm_loc",
                    "plus_minus","opp_pen_min_away","tm_goals_agt_sh_away","tm_goals_agt_sh_home",
                    "tm_goals_for_away","tm_goals_sh_for_away","tm_goals_sh_for_home",
                    "tm_loss_away","tm_pen_min_home","tm_shots_agt_away",
                    "tm_shots_agt_home","player_gm_loc")

MSE.kks<-c(0)
data_kk <- data[columns_kk]
num_sample<-as.integer(nrow(data_kk))
data_kk<-data_kk[sample(nrow(data_kk),num_sample ), ]
data_kk <- data_kk[complete.cases(data_kk), ]

for (ith in 1:10){
  index <- sample(1:nrow(data_kk),round(0.8*nrow(data_kk)))
  train_kk <- data_kk[index,]
  test_kk  <- data_kk[-index,]
  train_kk_y <- train_kk$response_points
  train_kk_x <-train_kk[ , ! colnames(train_kk) %in%  remove_variables ]
  
  test_kk_y <- test_kk$response_points
  test_kk_x <-test_kk[ , ! colnames(test_kk) %in%  remove_variables ]
  
  pdt_knn <- knn.reg(train_kk_x, y=train_kk_y, test=test_kk_x,k=10)
  MSE.kk <- sum((pdt_knn$pred- test_kk_y)^2)/nrow(test_kk)
  print(MSE.kk)
  MSE.kks[ith]<-MSE.kk
}

print(MSE.kks)
print(mean(MSE.kks))


#Refit model after 2nd Iteration
data_kk <- data[columns_kk]
num_sample<-as.integer(nrow(data_kk))
data_kk<-data_kk[sample(nrow(data_kk),num_sample ), ]
data_kk <- data_kk[complete.cases(data_kk), ]
remove_variables<-c("response_points","player_gm_loc",
                    "plus_minus","opp_pen_min_away","tm_goals_agt_sh_away","tm_goals_agt_sh_home",
                    "tm_goals_for_away","tm_goals_sh_for_away","tm_goals_sh_for_home",
                    "tm_loss_away","tm_pen_min_home","tm_shots_agt_away",
                    "tm_shots_agt_home","player_gm_loc",
                    "goals_sh","num_shifts","cum_loss_for_home"
                    ,"cum_loss_for_away","cum_wins_for_home"
                    ,"cum_wins_for_away","tm_goals_for_home",
                    'tm_num_games_away','tm_num_games_home','tm_pen_min_away'
                    ,'gm_hand_r','weight')

MSE.kks<-c(0)
for (ith in 1:10){
  index <- sample(1:nrow(data_kk),round(0.8*nrow(data_kk)))
  train_kk <- data_kk[index,]
  test_kk  <- data_kk[-index,]
  train_kk_y <- train_kk$response_points
  train_kk_x <-train_kk[ , ! colnames(train_kk) %in%  remove_variables ]
  
  test_kk_y <- test_kk$response_points
  test_kk_x <-test_kk[ , ! colnames(test_kk) %in%  remove_variables ]
  
  pdt_knn <- knn.reg(train_kk_x, y=train_kk_y, test=test_kk_x,k=10)
  MSE.kk <- sum((pdt_knn$pred- test_kk_y)^2)/nrow(test_kk)
  print(MSE.kk)
  MSE.kks[ith]<-MSE.kk
}
print(MSE.kks)
print(mean(MSE.kks))



#Attribute Weighted K-Nearest Neighborhood
#Backward Elimination (3rd Iteration)
#average of errors after you bag 10 times
for (column_kk in columns_kk){
  MSE.kks<-c(0)
  if (! column_kk %in% remove_variables){
  for (ith in 1:10){
    data_kk <- data[columns_kk]
    num_sample<-as.integer(nrow(data_kk)*0.1)
    data_kk<-data_kk[sample(nrow(data_kk),num_sample ), ]
    data_kk <- data_kk[complete.cases(data_kk), ]
    index <- sample(1:nrow(data_kk),round(0.8*nrow(data_kk)))
    train_kk <- data_kk[index,]
    test_kk  <- data_kk[-index,]
    
    train_kk_y <- train_kk$response_points
    train_kk_x <-train_kk[ , ! colnames(train_kk) %in% c("response_points",column_kk, remove_variables) ]
    
    test_kk_y <- test_kk$response_points
    test_kk_x <-test_kk[ , ! colnames(test_kk) %in% c("response_points",column_kk, remove_variables) ]
    
    pdt_knn <- knn.reg(train_kk_x, y=train_kk_y, test=test_kk_x,k=10)
    MSE.kk <- sum((pdt_knn$pred- test_kk_y)^2)/nrow(test_kk)
    MSE.kks[ith]<-MSE.kk
  }
  if (mean(MSE.kks)<0.12229){
    print('Remove This Variable')
  }
  print(column_kk)
  print(MSE.kks)
  print(mean(MSE.kks))
  print('')
  }
}

#Refit model after 3rd Iteration
data_kk <- data[columns_kk]
num_sample<-as.integer(nrow(data_kk))
data_kk<-data_kk[sample(nrow(data_kk),num_sample ), ]
data_kk <- data_kk[complete.cases(data_kk), ]
remove_variables<-c("response_points","player_gm_loc",
                    "plus_minus","opp_pen_min_away","tm_goals_agt_sh_away","tm_goals_agt_sh_home",
                    "tm_goals_for_away","tm_goals_sh_for_away","tm_goals_sh_for_home",
                    "tm_loss_away","tm_pen_min_home","tm_shots_agt_away",
                    "tm_shots_agt_home","player_gm_loc",
                    "goals_sh","num_shifts","cum_loss_for_home"
                    ,"cum_loss_for_away","cum_wins_for_home"
                    ,"cum_wins_for_away","tm_goals_for_home",
                    'tm_num_games_away','tm_num_games_home','tm_pen_min_away'
                    ,'gm_hand_r','Weight',"opp_pen_min_home","tm_goals_agt_pp_away"
                    ,"tm_loss_home", 'Height')

MSE.kks<-c(0)
for (ith in 1:10){
  index <- sample(1:nrow(data_kk),round(0.8*nrow(data_kk)))
  train_kk <- data_kk[index,]
  test_kk  <- data_kk[-index,]
  train_kk_y <- train_kk$response_points
  train_kk_x <-train_kk[ , ! colnames(train_kk) %in%  remove_variables ]
  
  test_kk_y <- test_kk$response_points
  test_kk_x <-test_kk[ , ! colnames(test_kk) %in%  remove_variables ]
  
  pdt_knn <- knn.reg(train_kk_x, y=train_kk_y, test=test_kk_x,k=10)
  MSE.kk <- sum((pdt_knn$pred- test_kk_y)^2)/nrow(test_kk)
  print(MSE.kk)
  MSE.kks[ith]<-MSE.kk
}
mean(MSE.kks)
