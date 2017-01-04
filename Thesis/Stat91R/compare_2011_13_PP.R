#2012-2014

fname=file.choose()
data20112012PP=read.csv(fname,header=T)

fname=file.choose()
data20122013PP=read.csv(fname,header=T)

fname=file.choose()
pTOIPP20112012=read.csv(fname,header=T)

fname=file.choose()
pTOIPP20122013=read.csv(fname,header=T)

data20112012PP$Name<-str_replace_all(data20112012PP$Name," ",".")
data20122013PP$Name<-str_replace_all(data20122013PP$Name," ",".")

data20112012PP <-ddply(data20112012PP, "Name", numcolwise(sum))
data20122013PP <-ddply(data20122013PP, "Name", numcolwise(sum))
player20112012PP <- merge(data20112012PP,pTOIPP20112012,by="Name")

player20122013PP <- merge(data20122013PP,pTOIPP20122013,by="Name")


player20112013PP <- merge(player20112012PP,player20122013PP,by="Name")
#player20112013PP
#player20112012PP=player20112012PP[player20112012PP$Year.x==20112012, ]
player20112013PPD=player20112013PP[player20112013PP$pos.x=='D',]
player20112013PPF=player20112013PP[player20112013PP$pos.x!='D',]



player20112013PPF100<-player20112013PPF[player20112013PPF$TOI.y>58.5,]
player20112013PPF100<-player20112013PPF100[player20112013PPF100$TOI.y>58.5,]

player20112013PPF100$GoalonIce20142015<-(
  player20112013PPF100$NonAssistGoal.y+player20112013PPF100$X1stAssist.y+player20112013PPF100$X2ndAssisted1stAssist.y+
    player20112013PPF100$X2ndAssist.y+player20112013PPF100$NonAssistPlayers.y+player20112013PPF100$X1stAssistPlayers.y+
    player20112013PPF100$X2ndAssistPlayers.y+
    player20112013PPF100$X1stAssistGoal.y+
    player20112013PPF100$X2ndAssistGoal.y)



player20112013PPF100$NonAssistGoal.xPer60<-player20112013PPF100$NonAssistGoal.x*60/player20112013PPF100$TOI.x
player20112013PPF100$X1stAssist.xPer60<-player20112013PPF100$X1stAssist.x*60/player20112013PPF100$TOI.x
player20112013PPF100$X2ndAssisted1stAssist.xPer60<-player20112013PPF100$X2ndAssisted1stAssist.x*60/player20112013PPF100$TOI.x
#player20112013PPF100$X2ndAssisted2ndAssist.xPer60<-player20112013PPF100$X2ndAssisted2ndAssist.x/player20112013PPF100$TOI..x
player20112013PPF100$NonAssistPlayers.xPer60<-player20112013PPF100$NonAssistPlayers.x*60/player20112013PPF100$TOI.x
player20112013PPF100$X1stAssistPlayers.xPer60<-player20112013PPF100$X1stAssistPlayers.x*60/player20112013PPF100$TOI.x
player20112013PPF100$X2ndAssistPlayers.xPer60<-player20112013PPF100$X2ndAssistPlayers.x*60/player20112013PPF100$TOI.x
player20112013PPF100$X1stAssistGoal.xPer60<-player20112013PPF100$X1stAssistGoal.x*60/player20112013PPF100$TOI.x
player20112013PPF100$X2ndAssistGoal.xPer60<-player20112013PPF100$X2ndAssistGoal.x*60/player20112013PPF100$TOI.x
player20112013PPF100$X2ndAssist.xPer60<-player20112013PPF100$X2ndAssist.x*60/player20112013PPF100$TOI.x
player20112013PPF100$GoalonIce20142015Per60<-player20112013PPF100$GoalonIce20142015*60/player20112013PPF100$TOI.y




fitSQRTPer60F20112013PP<-lm(player20112013PPF100$GoalonIce20142015Per60~
                              sqrt(player20112013PPF100$NonAssistGoal.xPer60)+sqrt(player20112013PPF100$X1stAssist.xPer60)+sqrt(player20112013PPF100$X2ndAssisted1stAssist.xPer60)+
                              sqrt(player20112013PPF100$X2ndAssist.xPer60)+sqrt(player20112013PPF100$NonAssistPlayers.xPer60)+sqrt(player20112013PPF100$X1stAssistPlayers.xPer60)+
                              sqrt(player20112013PPF100$X2ndAssistPlayers.xPer60)+
                              sqrt(player20112013PPF100$X1stAssistGoal.xPer60)+
                              sqrt(player20112013PPF100$X2ndAssistGoal.xPer60),
                            weight=player20112013PPF100$TOI.y)

summary(fitSQRTPer60F20112013PP)
hist(player20122014PPF$TOI.y)



player20132015PPF100$GoalPredictWithout<-  predict(fitSQRTPer60F20132015PP, player20132015PPF100) -1.23597 *sqrt(player20132015PPF100$X2ndAssist.xPer60)
sortPreGoalplayer20132015PPF100=player20132015PPF100
sortPreGoalplayer20132015PPF100<-sortPreGoalplayer20132015PPF100[order(sortPreGoalplayer20132015PPF100$GoalPredictWithout),] 

player20122014PPF100$GoalPredictWithout<-  predict(fitSQRTPer60F20122014PP, player20122014PPF100) -1.0923*sqrt(player20122014PPF100$X2ndAssist.xPer60) 
sortPreGoalplayer20122014PPF100=player20122014PPF100
sortPreGoalplayer20122014PPF100<-sortPreGoalplayer20122014PPF100[order(sortPreGoalplayer20122014PPF100$GoalPredictWithout),] 

player20112013PPF100$GoalPredictWithout<-  predict(fitSQRTPer60F20112013PP, player20112013PPF100) -0.3176*sqrt(player20112013PPF100$X2ndAssist.xPer60)
sortPreGoalplayer20112013PPF100=player20112013PPF100
sortPreGoalplayer20112013PPF100<-sortPreGoalplayer20112013PPF100[order(sortPreGoalplayer20112013PPF100$GoalPredictWithout),] 


sortPreGoalplayer20122015PPF100<-rbind(sortPreGoalplayer20132015PPF100,sortPreGoalplayer20122014PPF100)
sortPreGoalplayer20112015FPP100<-rbind(sortPreGoalplayer20112013PPF100,sortPreGoalplayer20122015PPF100)


data=sortPreGoalplayer20112015FPP100
nrow(data)
set.seed(420); nsims=2000; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)

model5PP=lm(GoalonIce20142015Per60~
            sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
            sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
            sqrt(X2ndAssistPlayers.xPer60)+
            sqrt(X1stAssistGoal.xPer60)+
            sqrt(X2ndAssistGoal.xPer60),
          weight=TOI.y,data=data)
summary(model5PP)

model5PPDrop7=lm(GoalonIce20142015Per60~
              
              sqrt(X2ndAssist.xPer60)+
              sqrt(X2ndAssistPlayers.xPer60)+
              
              sqrt(X2ndAssistGoal.xPer60),
            weight=TOI.y,data=data)
summary(model5PPDrop7)

model5PPDrop5=lm(GoalonIce20142015Per60~
                   
                   sqrt(X2ndAssist.xPer60)+
                   sqrt(X2ndAssistPlayers.xPer60),
                 weight=TOI.y,data=data)
summary(model5PPDrop5)

modelStepBoth7<-step(model5PPDrop7,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)
modelStepBoth<-step(model5PPDrop5,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)

for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:350],]  
  test=data[reorder[350:n],]  
  #model5
  model5PP=lm(GoalonIce20142015Per60~
              sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
              sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
              sqrt(X2ndAssistPlayers.xPer60)+
              sqrt(X1stAssistGoal.xPer60)+
              sqrt(X2ndAssistGoal.xPer60),
            weight=TOI.y,data=train)
  
  modelPPDropP5=lm(GoalonIce20142015Per60~
                     
                     sqrt(X2ndAssist.xPer60)+
                     sqrt(X2ndAssistPlayers.xPer60),
                 weight=TOI.y,data=train)
  modelPPDropP7=lm(GoalonIce20142015Per60~
                   
                   sqrt(X2ndAssist.xPer60)+
                   sqrt(X2ndAssistPlayers.xPer60)+
                   
                   sqrt(X2ndAssistGoal.xPer60),
                 weight=TOI.y,data=train)
  
  
  sse1[i]=sum((test$GoalonIce20142015Per60-predict(model5PP,new=test))^2)
  sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
  sse3[i]=sum((test$GoalonIce20142015Per60-predict(modelPPDropP5,new=test))^2) 
  sse4[i]=sum((test$GoalonIce20142015Per60-predict(modelPPDropP7,new=test))^2) 
  sse5[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth7,new=test))^2)
}
c(mean(sse1),mean(sse2),mean(sse3),mean(sse4),mean(sse5))/(n-350)

