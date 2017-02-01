# The dataset
fname=file.choose()
library(pscl)
require(ggplot2)
require(pscl)
require(boot)

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
data$gm_loc<-normalize(data$gm_loc)
data$gm_loc<-normalize(data$gm_loc)

columns_pos<-c( 'assists_all','plus_minus','pen_min','goals_ev','goals_pp',
               'goals_sh','num_shifts','Height','Weight','Drafts'
               ,'gm_loc','gm_result','gm_hand_r',
               'gm_pos_f','response_points')


data_pos <- data[columns_pos]
data_pos <- data_pos[complete.cases(data_pos), ]
apply(data_pos,2,function(x) sum(is.na(x)))

cdi_model <- glm(response_points~.,data=data_pos, family = poisson)
summary(cdi_model)


index <- sample(1:nrow(data_pos),round(0.8*nrow(data_pos)))
train_pos <- data_pos[index,]
test_pos  <- data_pos[-index,]
zero_pos.fit<-zeroinfl(response_points~.| 1, data=train_pos)
summary(zero_pos.fit)
pr.pos <- predict(zero_pos.fit,test_pos)
MSE.pos <- sum((pr.pos - test_pos$response_points)^2)/nrow(test_pos)

