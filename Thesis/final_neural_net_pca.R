########################################
# a. Data Cleaning
#    1. Standardize quantatative variables 
#
# b. Cumulative Proportion of Variance
#
# c. Neural Networks (PCA)
#    1. Make Neural Networks
#    2. Fit PCA to the following variables
#         (Significant Variables from Random Forest Regression)
#         assists_all, time_on_ice, goals_pp, response_points,
#         gm_pos_f, goals_ev, num_shifts, Height, Weight, Drafts
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


#Important Variables from Random Forest
columns_nn<-c( 'assists_all','time_on_ice','goals_pp',"response_points",
               'gm_pos_f','goals_ev','num_shifts',"Height","Weight","Drafts")




data_nn <- data[columns_nn]
num_sample<-as.integer(nrow(data_nn)*1)
data_nn<-data_nn[sample(nrow(data_nn),num_sample ), ]
data_nn  <- data_nn[complete.cases(data_nn), ]
data_nn_x <-data_nn[ , ! colnames(data_nn) %in% c("response_points") ]

#Cumulative Proportion of Variance
data_nn.pca <- prcomp(data_nn_x,
                      center = TRUE,
                      scale. = TRUE) 

std_dev <- data_nn.pca$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)


cdf_prop_varex=cumsum(prop_varex)
plot(cdf_prop_varex, xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

print(cumsum(prop_varex))
print(prop_varex)

data_nn.pca_data <- data.frame(response_points = data_nn$response_points, data_nn.pca$x)

#choose pca dimension
dimension <- 8
data_nn_pca <- data_nn.pca_data[,1:dimension]
index <- sample(1:nrow(data_nn_pca),round(0.8*nrow(data_nn_pca)))
train_nn_pca <- data_nn_pca[index,]
test_nn_pca  <- data_nn_pca[-index,]


test_nn_pca_y <- test_nn_pca$response_points
test_nn_pca_x <-test_nn_pca[ , ! colnames(test_nn_pca) %in% c("response_points") ]

param<-8
threshes<-c(0.1)
learningrates<-c(0.1)

for (thresh in threshes) {
  for (learningrate in learningrates) {
      
      print(paste("param", toString(param), sep=": "))
      print(paste("learningrate", toString(learningrate), sep=": "))
      print(paste("thresh", toString(thresh), sep=": "))

      
      ptm <- proc.time()
      n <- names(train_nn_pca)
      f <- as.formula(paste("response_points ~", paste(n[!n %in% "response_points"], collapse = " + ")))
      
      # Choose parameter
      nn <- neuralnet(f,data=train_nn_pca,hidden=c(param),linear.output=T,stepmax=1e8, threshold=thresh,learningrate=learningrate)
      plot(nn)
      pr.nn <- compute(nn,test_nn_pca_x)
      test.r <- (test_nn_pca_y)
      pr.nn_ <- pr.nn$net.result
      
      MSE.nn.pca <- sum((test.r - pr.nn_)^2)/nrow(test_nn_pca)
      print(MSE.nn.pca) 
  }
}
 
