########################################
# a. Data Cleaning
#    1. Standardize quantatative variables 
#
# b. GLM with Poisson Distribution
#
# c. GLM with Zero-Inflated Poisson Distribution
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
library(neuralnet)
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


columns_pos<-c("player_gm_result",     "age",                  "player_num_games",    
               "player_gm_loc",        "assists_all",          "plus_minus",          
               "pen_min",              "goals_ev",             "goals_pp",            
               "goals_sh",             "num_shifts",           "time_on_ice",         
               "cum_loss_for_home",    "cum_loss_for_away",    "cum_wins_for_home",   
               "cum_wins_for_away",    "opp_pen_min_away",     "opp_pen_min_home",    
               "tm_goals_agt_pp_away", "tm_goals_agt_pp_home",
               "tm_goals_agt_sh_away", "tm_goals_agt_sh_home", "tm_goals_for_away",   
               "tm_goals_for_home",    "tm_loss_away",        
               "tm_loss_home",         "tm_num_games_away",    "tm_num_games_home",   
               "tm_shots_agt_away",   "tm_shots_agt_home",    
               "Height",               "Weight",               "Drafts",              
               "response_points",     
               "gm_hand_r",                    
               "gm_pos_f" )


data_pos <- data[columns_pos]
data_pos <- data_pos[complete.cases(data_pos), ]

#GLM with Poisson Distribution as Link Function
#Bagging 10 time
MSE.poissons <- c(0)
for (ith in 1:10){
  

  index <- sample(1:nrow(data_pos),round(0.8*nrow(data_pos)))
  train_pos <- data_pos[index,]
  test_pos  <- data_pos[-index,]
  
  #Change to Integers because 
  #Poisson assumes the response value is an integer
  train_pos$response_points=train_pos$response_points*5
  test_pos$response_points=test_pos$response_points*5

  poisson.fit <- glm(response_points~.,data=train_pos, family=poisson(link=log))
  pos.pred <- predict(poisson.fit,test_pos)


  MSE.pos <- sum((pos.pred/5  - test_pos$response_points/5)^2)/nrow(test_pos)
  print(MSE.pos)
  MSE.poissons[ith] <- MSE.pos
}

print('Error: GLM with Poisson Distribution as Link Function')
print(mean(MSE.poissons))

MSE.zero_poissons <- c(0)
for (ith in 1:10){
  index <- sample(1:nrow(data_pos),round(0.8*nrow(data_pos)))
  train_pos <- data_pos[index,]
  test_pos  <- data_pos[-index,]
  
  #Change to Integers because 
  #Poisson assumes the response value is an integer
  train_pos$response_points=train_pos$response_points*5
  test_pos$response_points=test_pos$response_points*5
  
  zero_pos.fit<-zeroinfl(response_points~.| ., data=train_pos)
  zero_pos.pred <- predict(zero_pos.fit,test_pos)

  MSE.zero_pos <- sum((zero_pos.pred/5 - test_pos$response_points/5)^2)/nrow(test_pos)
  MSE.zero_poissons[ith] <- MSE.zero_pos 
}


