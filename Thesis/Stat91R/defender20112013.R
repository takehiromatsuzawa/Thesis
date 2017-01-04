player20112013D$NonAssistGoal.xPer60<-player20112013D$NonAssistGoal.x*60/player20112013D$TOI.x
player20112013D$X1stAssist.xPer60<-player20112013D$X1stAssist.x*60/player20112013D$TOI.x
player20112013D$X2ndAssisted1stAssist.xPer60<-player20112013D$X2ndAssisted1stAssist.x*60/player20112013D$TOI.x
#player20112013F$X2ndAssisted2ndAssist.xPer60<-player20112013F$X2ndAssisted2ndAssist.x/player20112013F$TOI..x
player20112013D$NonAssistPlayers.xPer60<-player20112013D$NonAssistPlayers.x*60/player20112013D$TOI.x
player20112013D$X1stAssistPlayers.xPer60<-player20112013D$X1stAssistPlayers.x*60/player20112013D$TOI.x
player20112013D$X2ndAssistPlayers.xPer60<-player20112013D$X2ndAssistPlayers.x*60/player20112013D$TOI.x
player20112013D$X1stAssistGoal.xPer60<-player20112013D$X1stAssistGoal.x*60/player20112013D$TOI.x
player20112013D$X2ndAssistGoal.xPer60<-player20112013D$X2ndAssistGoal.x*60/player20112013D$TOI.x
player20112013D$X2ndAssist.xPer60<-player20112013D$X2ndAssist.x*60/player20112013D$TOI.x

player20112013D$GoalonIce20142015<-(
  player20112013D$NonAssistGoal.y+player20112013D$X1stAssist.y+player20112013D$X2ndAssisted1stAssist.y+
    player20112013D$X2ndAssist.y+player20112013D$NonAssistPlayers.y+player20112013D$X1stAssistPlayers.y+
    player20112013D$X2ndAssistPlayers.y+
    player20112013D$X1stAssistGoal.y+
    player20112013D$X2ndAssistGoal.y)

player20112013D$GoalonIce20142015Per60<-player20112013D$GoalonIce20142015*60/player20112013D$TOI.y


player20112013D500<-player20112013D[player20112013D$TOI.y>292,]
player20112013D500<-player20112013D500[player20112013D500$TOI.x>500,]

player20122014D$NonAssistGoal.xPer60<-player20122014D$NonAssistGoal.x*60/player20122014D$TOI.x
player20122014D$X1stAssist.xPer60<-player20122014D$X1stAssist.x*60/player20122014D$TOI.x
player20122014D$X2ndAssisted1stAssist.xPer60<-player20122014D$X2ndAssisted1stAssist.x*60/player20122014D$TOI.x
#player20122014F$X2ndAssisted2ndAssist.xPer60<-player20122014F$X2ndAssisted2ndAssist.x/player20122014F$TOI..x
player20122014D$NonAssistPlayers.xPer60<-player20122014D$NonAssistPlayers.x*60/player20122014D$TOI.x
player20122014D$X1stAssistPlayers.xPer60<-player20122014D$X1stAssistPlayers.x*60/player20122014D$TOI.x
player20122014D$X2ndAssistPlayers.xPer60<-player20122014D$X2ndAssistPlayers.x*60/player20122014D$TOI.x
player20122014D$X1stAssistGoal.xPer60<-player20122014D$X1stAssistGoal.x*60/player20122014D$TOI.x
player20122014D$X2ndAssistGoal.xPer60<-player20122014D$X2ndAssistGoal.x*60/player20122014D$TOI.x
player20122014D$X2ndAssist.xPer60<-player20122014D$X2ndAssist.x*60/player20122014D$TOI.x

player20122014D$GoalonIce20142015<-(
  player20122014D$NonAssistGoal.y+player20122014D$X1stAssist.y+player20122014D$X2ndAssisted1stAssist.y+
    player20122014D$X2ndAssist.y+player20122014D$NonAssistPlayers.y+player20122014D$X1stAssistPlayers.y+
    player20122014D$X2ndAssistPlayers.y+
    player20122014D$X1stAssistGoal.y+
    player20122014D$X2ndAssistGoal.y)

player20122014D$GoalonIce20142015Per60<-player20122014D$GoalonIce20142015*60/player20122014D$TOI.y


player20122014D500<-player20122014D[player20122014D$TOI.y>292,]
player20122014D500<-player20122014D500[player20122014D500$TOI.x>500,]

player20122014D500$GoalPredictWithout<-  predict(fitSQRTPer60F20122014, player20122014D500) -0.792474 *sqrt(player20122014D500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20122014D500=player20122014D500
sortPreGoalplayer20122014D500<-sortPreGoalplayer20122014D500[order(sortPreGoalplayer20122014D500$GoalPredictWithout),] 




#fitSQRTPer60D20112013<-lm(player20112013D500$GoalonIce20142015Per60~
#                            sqrt(player20112013D500$NonAssistGoal.xPer60)+sqrt(player20112013D500$X1stAssist.xPer60)+sqrt(player20112013D500$X2ndAssisted1stAssist.xPer60)+
#                            sqrt(player20112013D500$X2ndAssist.xPer60)+sqrt(player20112013D500$NonAssistPlayers.xPer60)+sqrt(player20112013D500$X1stAssistPlayers.xPer60)+
#                            sqrt(player20112013D500$X2ndAssistPlayers.xPer60)+
#                            sqrt(player20112013D500$X1stAssistGoal.xPer60)+
#                            sqrt(player20112013D500$X2ndAssistGoal.xPer60),
#                          weight=player20112013D500$TOI.y)

#summary(fitSQRTPer60D20112013)

#player20112013D500$GoalPredictWithout<-  predict(fitSQRTPer60D20112013, player20112013D500) -0.78097 *sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60)
#sortPreGoalplayer20112013D500=player20112013D500
#sortPreGoalplayer20112013D500<-sortPreGoalplayer20112013D500[order(sortPreGoalplayer20112013D500$GoalPredictWithout),] 

player20132015D$NonAssistGoal.xPer60<-player20132015D$NonAssistGoal.x*60/player20132015D$TOI.x
player20132015D$X1stAssist.xPer60<-player20132015D$X1stAssist.x*60/player20132015D$TOI.x
player20132015D$X2ndAssisted1stAssist.xPer60<-player20132015D$X2ndAssisted1stAssist.x*60/player20132015D$TOI.x
#player20132015F$X2ndAssisted2ndAssist.xPer60<-player20132015F$X2ndAssisted2ndAssist.x/player20132015F$TOI..x
player20132015D$NonAssistPlayers.xPer60<-player20132015D$NonAssistPlayers.x*60/player20132015D$TOI.x
player20132015D$X1stAssistPlayers.xPer60<-player20132015D$X1stAssistPlayers.x*60/player20132015D$TOI.x
player20132015D$X2ndAssistPlayers.xPer60<-player20132015D$X2ndAssistPlayers.x*60/player20132015D$TOI.x
player20132015D$X1stAssistGoal.xPer60<-player20132015D$X1stAssistGoal.x*60/player20132015D$TOI.x
player20132015D$X2ndAssistGoal.xPer60<-player20132015D$X2ndAssistGoal.x*60/player20132015D$TOI.x
player20132015D$X2ndAssist.xPer60<-player20132015D$X2ndAssist.x*60/player20132015D$TOI.x

player20132015D$GoalonIce20142015<-(
  player20132015D$NonAssistGoal.y+player20132015D$X1stAssist.y+player20132015D$X2ndAssisted1stAssist.y+
    player20132015D$X2ndAssist.y+player20132015D$NonAssistPlayers.y+player20132015D$X1stAssistPlayers.y+
    player20132015D$X2ndAssistPlayers.y+
    player20132015D$X1stAssistGoal.y+
    player20132015D$X2ndAssistGoal.y)


player20132015D$GoalonIce20142015Per60<-player20132015D$GoalonIce20142015*60/player20132015D$TOI.y


player20132015D500<-player20132015D[player20132015D$TOI.y>292,]
player20132015D500<-player20132015D500[player20132015D500$TOI.x>500,]

player20132015D500$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, player20132015D500) -0.792474 *sqrt(player20132015D500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20132015D500=player20132015D500
sortPreGoalplayer20132015D500<-sortPreGoalplayer20132015D500[order(sortPreGoalplayer20132015D500$GoalPredictWithout),] 

sortPreGoalplayer20122015D500<-rbind(player20132015D500,player20122014D500)
sortPreGoalplayer20112015D500<-rbind(player20112013D500,sortPreGoalplayer20122015D500)


train=data

model5=lm(sortPreGoalplayer20112015D500$GoalonIce20142015Per60~
            sqrt(sortPreGoalplayer20112015D500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015D500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015D500$X2ndAssisted1stAssist.xPer60)+
            sqrt(sortPreGoalplayer20112015D500$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015D500$NonAssistPlayers.xPer60)+sqrt(sortPreGoalplayer20112015D500$X1stAssistPlayers.xPer60)+
            sqrt(sortPreGoalplayer20112015D500$X2ndAssistPlayers.xPer60)+
            sqrt(sortPreGoalplayer20112015D500$X1stAssistGoal.xPer60)+
            sqrt(sortPreGoalplayer20112015D500$X2ndAssistGoal.xPer60),
          weight=sortPreGoalplayer20112015D500$TOI.y,data=data)
summary(model5)
model5Akaike<-step(model5)
model5Akaike
modelDropP5=lm(sortPreGoalplayer20112015D500$GoalonIce20142015Per60~
                 
                 sqrt(sortPreGoalplayer20112015D500$X1stAssist.xPer60)+
                 sqrt(sortPreGoalplayer20112015D500$X2ndAssisted1stAssist.xPer60)+
                
                 sqrt(sortPreGoalplayer20112015D500$X2ndAssistPlayers.xPer60)+
                 
                 sqrt(sortPreGoalplayer20112015D500$X2ndAssistGoal.xPer60),
               weight=sortPreGoalplayer20112015D500$TOI.y,data=train)
summary(modelDropP5)
modelDropP5=lm(sortPreGoalplayer20112015D500$GoalonIce20142015Per60~
                 
                 sqrt(sortPreGoalplayer20112015D500$X1stAssist.xPer60)+
                 sqrt(sortPreGoalplayer20112015D500$X2ndAssisted1stAssist.xPer60)+
                 
                 sqrt(sortPreGoalplayer20112015D500$X2ndAssistPlayers.xPer60)+
                 
                 sqrt(sortPreGoalplayer20112015D500$X2ndAssistGoal.xPer60),
               weight=sortPreGoalplayer20112015D500$TOI.y,data=data)

summary(model5Akaike)
summary(modelDropP5)
data=sortPreGoalplayer20112015D500
nrow(data)
set.seed(420); nsims=3000; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
modelStepBoth<-step(modelDropP5,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)
summary(modelStepBoth)
for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:300],]  
  test=data[reorder[300:n],]  
  #model5
  
  model5=lm(train$GoalonIce20142015Per60~
              sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
              sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
              sqrt(X2ndAssistPlayers.xPer60)+
              sqrt(X1stAssistGoal.xPer60)+
              sqrt(X2ndAssistGoal.xPer60),
            weight=TOI.y,data=train)

  modelDropP5=lm(GoalonIce20142015Per60~
                   
                   sqrt(X1stAssist.xPer60)+
                   sqrt(X2ndAssisted1stAssist.xPer60)+
                   
                   sqrt(X2ndAssistPlayers.xPer60)+
                   
                   sqrt(X2ndAssistGoal.xPer60),
                 weight=TOI.y,data=train)


sse1[i]=sum((test$GoalonIce20142015Per60-predict(model5,new=test))^2)
sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
sse4[i]=sum((test$GoalonIce20142015Per60-predict(modelDropP5,new=test))^2) 

}
c(mean(sse1),mean(sse2),mean(sse4))/(n-300)
modelStepBoth

summary(modelStepBoth)


#the effect becomes stronger and more positive if the other variable is large
#Create a graph for defencemen as i did for forwards

