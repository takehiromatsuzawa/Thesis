#2012-2014

fname=file.choose()
data20122013PP=read.csv(fname,header=T)

fname=file.choose()
data20132014PP=read.csv(fname,header=T)

fname=file.choose()
pTOIPP20122013=read.csv(fname,header=T)

fname=file.choose()
pTOIPP20132014=read.csv(fname,header=T)

data20122013PP$Name<-str_replace_all(data20122013PP$Name," ",".")
data20132014PP$Name<-str_replace_all(data20132014PP$Name," ",".")

data20122013PP <-ddply(data20122013PP, "Name", numcolwise(sum))
data20132014PP <-ddply(data20132014PP, "Name", numcolwise(sum))

player20122013PP <- merge(data20122013PP,pTOIPP20122013,by="Name")
player20122013PPEV <- merge(player20122013PP,player20122013,by="Name")

player20132014PP <- merge(data20132014PP,pTOIPP20132014,by="Name")
newPlayer20132014PP<-player20132014PP

colnames(newPlayer20132014PP)[34] 
colnames(newPlayer20132014PP)[35] 
colnames(newPlayer20132014PP)[36] 


colnames(newPlayer20132014PP)[34]  <- "newTOI"
colnames(newPlayer20132014PP)[35]  <- "newTOIoff"
colnames(newPlayer20132014PP)[36]  <- "newTOI."

player20122014PPEV <- merge(player20122013PPEV,newPlayer20132014PP,by="Name")
player20122014PPEVD=player20122014PPEV[player20122014PPEV$pos.x=='D',]
player20122014PPEVF=player20122014PPEV[player20122014PPEV$pos.x!='D',]


100*48/82
player20122014PPEVF100<-player20122014PPEVF[player20122014PPEVF$newTOI>50,]
player20122014PPEVF100<-player20122014PPEVF100[player20122014PPEVF100$TOI.x>50,]
player20122014PPEVF100<-player20122014PPEVF100[player20122014PPEVF100$TOI.y>500,]

player20122014PPEVF100$GoalonIce20142015<-(
  player20122014PPEVF100$NonAssistGoal+player20122014PPEVF100$X1stAssist+player20122014PPEVF100$X2ndAssisted1stAssist+
    player20122014PPEVF100$X2ndAssist+player20122014PPEVF100$NonAssistPlayers+player20122014PPEVF100$X1stAssistPlayers+
    player20122014PPEVF100$X2ndAssistPlayers+
    player20122014PPEVF100$X1stAssistGoal+
    player20122014PPEVF100$X2ndAssistGoal)



player20122014PPEVF100$NonAssistGoal.xPer60<-player20122014PPEVF100$NonAssistGoal.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$X1stAssist.xPer60<-player20122014PPEVF100$X1stAssist.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$X2ndAssisted1stAssist.xPer60<-player20122014PPEVF100$X2ndAssisted1stAssist.x*60/player20122014PPEVF100$TOI.x
#player20122014PPEVF100$X2ndAssisted2ndAssist.xPer60<-player20122014PPEVF100$X2ndAssisted2ndAssist.x/player20122014PPEVF100$TOI..x
player20122014PPEVF100$NonAssistPlayers.xPer60<-player20122014PPEVF100$NonAssistPlayers.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$X1stAssistPlayers.xPer60<-player20122014PPEVF100$X1stAssistPlayers.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$X2ndAssistPlayers.xPer60<-player20122014PPEVF100$X2ndAssistPlayers.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$X1stAssistGoal.xPer60<-player20122014PPEVF100$X1stAssistGoal.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$X2ndAssistGoal.xPer60<-player20122014PPEVF100$X2ndAssistGoal.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$X2ndAssist.xPer60<-player20122014PPEVF100$X2ndAssist.x*60/player20122014PPEVF100$TOI.x
player20122014PPEVF100$GoalonIce20142015Per60<-player20122014PPEVF100$GoalonIce20142015*60/player20122014PPEVF100$newTOI


player20122014PPEVF100$NonAssistGoal.yPer60<-player20122014PPEVF100$NonAssistGoal.y*60/player20122014PPEVF100$TOI.y
player20122014PPEVF100$X1stAssist.yPer60<-player20122014PPEVF100$X1stAssist.y*60/player20122014PPEVF100$TOI.y
player20122014PPEVF100$X2ndAssisted1stAssist.yPer60<-player20122014PPEVF100$X2ndAssisted1stAssist.y*60/player20122014PPEVF100$TOI.y
#player20122014PPEVF100$X2ndAssisted2ndAssist.yPer60<-player20122014PPEVF100$X2ndAssisted2ndAssist.y/player20122014PPEVF100$TOI..y
player20122014PPEVF100$NonAssistPlayers.yPer60<-player20122014PPEVF100$NonAssistPlayers.y*60/player20122014PPEVF100$TOI.y
player20122014PPEVF100$X1stAssistPlayers.yPer60<-player20122014PPEVF100$X1stAssistPlayers.y*60/player20122014PPEVF100$TOI.y
player20122014PPEVF100$X2ndAssistPlayers.yPer60<-player20122014PPEVF100$X2ndAssistPlayers.y*60/player20122014PPEVF100$TOI.y
player20122014PPEVF100$X1stAssistGoal.yPer60<-player20122014PPEVF100$X1stAssistGoal.y*60/player20122014PPEVF100$TOI.y
player20122014PPEVF100$X2ndAssistGoal.yPer60<-player20122014PPEVF100$X2ndAssistGoal.y*60/player20122014PPEVF100$TOI.y
player20122014PPEVF100$X2ndAssist.yPer60<-player20122014PPEVF100$X2ndAssist.y*60/player20122014PPEVF100$TOI.y




fitSQRTPer60F20122014PPEV<-lm(player20122014PPEVF100$GoalonIce20142015Per60~
                                sqrt(player20122014PPEVF100$NonAssistGoal.xPer60)+sqrt(player20122014PPEVF100$X1stAssist.xPer60)+sqrt(player20122014PPEVF100$X2ndAssisted1stAssist.xPer60)+
                                sqrt(player20122014PPEVF100$X2ndAssist.xPer60)+sqrt(player20122014PPEVF100$NonAssistPlayers.xPer60)+sqrt(player20122014PPEVF100$X1stAssistPlayers.xPer60)+
                                sqrt(player20122014PPEVF100$X2ndAssistPlayers.xPer60)+
                                sqrt(player20122014PPEVF100$X1stAssistGoal.xPer60)+
                                sqrt(player20122014PPEVF100$X2ndAssistGoal.xPer60)+
                                sqrt(player20122014PPEVF100$NonAssistGoal.yPer60)+sqrt(player20122014PPEVF100$X1stAssist.yPer60)+sqrt(player20122014PPEVF100$X2ndAssisted1stAssist.yPer60)+
                                sqrt(player20122014PPEVF100$X2ndAssist.yPer60)+sqrt(player20122014PPEVF100$NonAssistPlayers.yPer60)+sqrt(player20122014PPEVF100$X1stAssistPlayers.yPer60)+
                                sqrt(player20122014PPEVF100$X2ndAssistPlayers.yPer60)+
                                sqrt(player20122014PPEVF100$X1stAssistGoal.yPer60)+
                                sqrt(player20122014PPEVF100$X2ndAssistGoal.yPer60),
                              weight=(player20122014PPEVF100$TOI.y)+(player20122014PPEVF100$TOI.x))

summary(fitSQRTPer60F20122014PPEV)




sortPreGoalplayer20122015FPP500<-rbind(sortPreGoalplayer20132015FPP500,sortPreGoalplayer20122014F500)
sortPreGoalplayer20112015FPP500<-rbind(sortPreGoalplayer20112013FPP500,sortPreGoalplayer20122015FPP500)



#player20122014PP
#player20122013PP=player20122013PP[player20122013PP$Year.x==20122013, ]
player20122014PPD=player20122014PP[player20122014PP$pos.x=='D',]
player20122014PPF=player20122014PP[player20122014PP$pos.x!='D',]



player20122014PPF100<-player20122014PPF[player20122014PPF$TOI.y>100,]
player20122014PPF100<-player20122014PPF100[player20122014PPF100$TOI.x>100,]

player20122014PPF100$GoalonIce20142015<-(
  player20122014PPF100$NonAssistGoal.y+player20122014PPF100$X1stAssist.y+player20122014PPF100$X2ndAssisted1stAssist.y+
    player20122014PPF100$X2ndAssist.y+player20122014PPF100$NonAssistPlayers.y+player20122014PPF100$X1stAssistPlayers.y+
    player20122014PPF100$X2ndAssistPlayers.y+
    player20122014PPF100$X1stAssistGoal.y+
    player20122014PPF100$X2ndAssistGoal.y)



player20122014PPF100$NonAssistGoal.xPer60<-player20122014PPF100$NonAssistGoal.x*60/player20122014PPF100$TOI.x
player20122014PPF100$X1stAssist.xPer60<-player20122014PPF100$X1stAssist.x*60/player20122014PPF100$TOI.x
player20122014PPF100$X2ndAssisted1stAssist.xPer60<-player20122014PPF100$X2ndAssisted1stAssist.x*60/player20122014PPF100$TOI.x
#player20122014PPF100$X2ndAssisted2ndAssist.xPer60<-player20122014PPF100$X2ndAssisted2ndAssist.x/player20122014PPF100$TOI..x
player20122014PPF100$NonAssistPlayers.xPer60<-player20122014PPF100$NonAssistPlayers.x*60/player20122014PPF100$TOI.x
player20122014PPF100$X1stAssistPlayers.xPer60<-player20122014PPF100$X1stAssistPlayers.x*60/player20122014PPF100$TOI.x
player20122014PPF100$X2ndAssistPlayers.xPer60<-player20122014PPF100$X2ndAssistPlayers.x*60/player20122014PPF100$TOI.x
player20122014PPF100$X1stAssistGoal.xPer60<-player20122014PPF100$X1stAssistGoal.x*60/player20122014PPF100$TOI.x
player20122014PPF100$X2ndAssistGoal.xPer60<-player20122014PPF100$X2ndAssistGoal.x*60/player20122014PPF100$TOI.x
player20122014PPF100$X2ndAssist.xPer60<-player20122014PPF100$X2ndAssist.x*60/player20122014PPF100$TOI.x
player20122014PPF100$GoalonIce20142015Per60<-player20122014PPF100$GoalonIce20142015*60/player20122014PPF100$TOI.y




fitSQRTPer60F20122014PP<-lm(player20122014PPF100$GoalonIce20142015Per60~
                              sqrt(player20122014PPF100$NonAssistGoal.xPer60)+sqrt(player20122014PPF100$X1stAssist.xPer60)+sqrt(player20122014PPF100$X2ndAssisted1stAssist.xPer60)+
                              sqrt(player20122014PPF100$X2ndAssist.xPer60)+sqrt(player20122014PPF100$NonAssistPlayers.xPer60)+sqrt(player20122014PPF100$X1stAssistPlayers.xPer60)+
                              sqrt(player20122014PPF100$X2ndAssistPlayers.xPer60)+
                              sqrt(player20122014PPF100$X1stAssistGoal.xPer60)+
                              sqrt(player20122014PPF100$X2ndAssistGoal.xPer60),
                            weight=player20122014PPF100$TOI.y)

summary(fitSQRTPer60F20122014PP)






data=player20122014PPF100
nrow(data)

player20132015F500$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, player20132015F500) -0.792474 *sqrt(player20132015F500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20132015F500=player20132015F500
sortPreGoalplayer20132015F500<-sortPreGoalplayer20132015F500[order(sortPreGoalplayer20132015F500$GoalPredictWithout),] 



sortPreGoalplayer20122015F500<-rbind(sortPreGoalplayer20132015F500,sortPreGoalplayer20122014F500)
sortPreGoalplayer20112015F500<-rbind(sortPreGoalplayer20112013F500,sortPreGoalplayer20122015F500)










sortPreGoalplayer20122015FPP500<-rbind(sortPreGoalplayer20132015FPP500,sortPreGoalplayer20122014F500)
sortPreGoalplayer20112015FPP500<-rbind(sortPreGoalplayer20112013FPP500,sortPreGoalplayer20122015FPP500)

model5PP=lm(sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60~
              sqrt(sortPreGoalplayer20112015FPP500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)+
              sqrt(sortPreGoalplayer20112015FPP500$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$NonAssistPlayers.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssistPlayers.xPer60)+
              sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistPlayers.xPer60)+
              sqrt(sortPreGoalplayer20112015FPP500$X1stAssistGoal.xPer60)+
              sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60),
            weight=sortPreGoalplayer20112015FPP500$TOI.y)

summary(model5PP)
modelDropPPP=lm(sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60~
                  sqrt(sortPreGoalplayer20112015FPP500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)+
                  sqrt(sortPreGoalplayer20112015FPP500$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssistPlayers.xPer60)+
                  sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistPlayers.xPer60)+
                  sqrt(sortPreGoalplayer20112015FPP500$X1stAssistGoal.xPer60)+
                  sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60),
                weight=sortPreGoalplayer20112015FPP500$TOI.y)
summary(modelDropPPP)
modelDropPPP7=lm(sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60~
                   sqrt(sortPreGoalplayer20112015FPP500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistPlayers.xPer60)+
                   
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015FPP500$TOI.y)

summary(modelDropPPP7)

modelDropPPP5=lm(sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60~
                   sqrt(sortPreGoalplayer20112015FPP500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssist.xPer60)+
                   
                   
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015FPP500$TOI.y)

summary(modelDropPPP5)


data=sortPreGoalplayer20112015FPP500
set.seed(420); nsims=10; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
#fitSQRTPer60F20112013<-lm(player20112013F500$GoalonIce20142015Per60~
#                           sqrt(player20112013F500$NonAssistGoal.xPer60)+sqrt(player20112013F500$X1stAssist.xPer60)+sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60)+
#                            sqrt(player20112013F500$X2ndAssist.xPer60)+sqrt(player20112013F500$NonAssistPlayers.xPer60)+sqrt(player20112013F500$X1stAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X1stAssistGoal.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistGoal.xPer60),
#                          weight=player20112013F500$TOI.y)
#modelStepBoth<-step(model5PP,scope=list(upper=~.^2,lower=~1),data=data,direction="both")
nrow(data)
set.seed(420); nsims=10; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],]  
  #model5PP
  model5PP=lm(sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60~
                sqrt(sortPreGoalplayer20112015FPP500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)+
                sqrt(sortPreGoalplayer20112015FPP500$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$NonAssistPlayers.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssistPlayers.xPer60)+
                sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistPlayers.xPer60)+
                sqrt(sortPreGoalplayer20112015FPP500$X1stAssistGoal.xPer60)+
                sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60),
              weight=sortPreGoalplayer20112015FPP500$TOI.y,data=train)
  # modelStepBoth<-step(model5PP,scope=list(upper=~.^2,lower=~1),data=train,direction="both",trace=0)
  #modelDropPPP5=lm(sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60~
  #                sqrt(sortPreGoalplayer20112015FPP500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)+
  #               sqrt(sortPreGoalplayer20112015FPP500$X2ndAssist.xPer60)+
  
  
  sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60),
  weight=sortPreGoalplayer20112015FPP500$TOI.y,data=train)
modelDropPPP7=lm(sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60~
                   sqrt(sortPreGoalplayer20112015FPP500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistPlayers.xPer60)+
                   
                   sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015FPP500$TOI.y,data=train)


sse1[i]=sum((sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60-predict(model5PP,new=test))^2)
sse2[i]=sum((sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
sse3[i]=sum((sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60-predict(modelDropPPP5,new=test))^2) 
sse4[i]=sum((sortPreGoalplayer20112015FPP500$GoalonIce20142015Per60-predict(modelDropPPP7,new=test))^2) 

}
c(mean(sse1),mean(sse2),mean(sse3))/(n-1200)
sse2

set.seed(420); nsims=2000; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)

model5PP=lm(GoalonIce20142015Per60~
              sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
              sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
              sqrt(X2ndAssistPlayers.xPer60)+
              sqrt(X1stAssistGoal.xPer60)+
              sqrt(X2ndAssistGoal.xPer60),
            weight=TOI.y,data=data)

modelStepBoth7<-step(modelDropPPP7,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)
modelStepBoth<-step(model5PP,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)

for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],]  
  #model5PP
  model5PP=lm(GoalonIce20142015Per60~
                sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
                sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
                sqrt(X2ndAssistPlayers.xPer60)+
                sqrt(X1stAssistGoal.xPer60)+
                sqrt(X2ndAssistGoal.xPer60),
              weight=TOI.y,data=train)
  
  modelDropPPP5=lm(GoalonIce20142015Per60~
                     sqrt(NonAssistGoal.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
                     sqrt(X2ndAssist.xPer60)+
                     
                     
                     sqrt(X2ndAssistGoal.xPer60),
                   weight=TOI.y,data=train)
  modelDropPPP7=lm(GoalonIce20142015Per60~
                     sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
                     sqrt(X2ndAssist.xPer60)+
                     sqrt(X2ndAssistPlayers.xPer60)+
                     
                     sqrt(X2ndAssistGoal.xPer60),
                   weight=TOI.y,data=train)
  
  
  sse1[i]=sum((test$GoalonIce20142015Per60-predict(model5PP,new=test))^2)
  sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
  sse3[i]=sum((test$GoalonIce20142015Per60-predict(modelDropPPP5,new=test))^2) 
  sse4[i]=sum((test$GoalonIce20142015Per60-predict(modelDropPPP7,new=test))^2) 
  sse5[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth7,new=test))^2)
}
c(mean(sse1),mean(sse2),mean(sse3),mean(sse4),mean(sse5))/(n-600)

n
for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],] 
  sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
}
model5PP=lm(GoalonIce20142015Per60~
              sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
              sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
              sqrt(X2ndAssistPlayers.xPer60)+
              sqrt(X1stAssistGoal.xPer60)+
              sqrt(X2ndAssistGoal.xPer60)+sqrt(X2ndAssist.xPer60):sqrt(X1stAssistPlayers.xPer60) +
              sqrt(NonAssistPlayers.xPer60):sqrt(X2ndAssistGoal.xPer60)+
              sqrt(X1stAssist.xPer60):sqrt(X2ndAssistPlayers.xPer60)          
            +sqrt(X2ndAssisted1stAssist.xPer60):sqrt(X1stAssistPlayers.xPer60)+
              +sqrt(X2ndAssisted1stAssist.xPer60):sqrt(X2ndAssist.xPer60) 
            +sqrt(NonAssistGoal.xPer60):sqrt(X1stAssistGoal.xPer60)   +
              +sqrt(X1stAssistPlayers.xPer60):sqrt(X1stAssistGoal.xPer60) 
            +sqrt(X2ndAssisted1stAssist.xPer60):sqrt(X1stAssistGoal.xPer60) 
            +sqrt(X2ndAssisted1stAssist.xPer60):sqrt(NonAssistPlayers.xPer60) 
            +sqrt(X2ndAssist.xPer60):sqrt(NonAssistPlayers.xPer60)     
            ,
            weight=TOI.y,data=data)
summary(model5PP)

model5PP=lm(GoalonIce20142015Per60~
              sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
              sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
              #sqrt(X2ndAssistPlayers.xPer60)+
              #sqrt(X1stAssistGoal.xPer60)+
              sqrt(X2ndAssistGoal.xPer60)+
              sqrt(X2ndAssist.xPer60):sqrt(X1stAssistPlayers.xPer60) +
              sqrt(NonAssistPlayers.xPer60):sqrt(X2ndAssistGoal.xPer60)
            #+    sqrt(X1stAssist.xPer60):sqrt(X2ndAssistPlayers.xPer60)          
            +sqrt(X2ndAssisted1stAssist.xPer60):sqrt(X1stAssistPlayers.xPer60)+
              +sqrt(X2ndAssisted1stAssist.xPer60):sqrt(X2ndAssist.xPer60) 
            
            ,
            weight=TOI.y,data=data)
summary(modelStepBoth)

sortPreGoalplayer20112015FPP500
player20132015FPP500<-player20132015F[player20132015F$TOI.y>500,]
player20132015FPP500<-player20132015FPP500[player20132015FPP500$TOI.x>500,]
fitSQRTPer60F20132015<-lm(GoalonIce20142015Per60~
                            sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)
                          +sqrt(X2ndAssisted1stAssist.xPer60)+
                            sqrt(X2ndAssist.xPer60)+
                            sqrt(X2ndAssistPlayers.xPer60)+
                            sqrt(X2ndAssistGoal.xPer60),
                          weight=TOI.y,data=sortPreGoalplayer20112015FPP500)

summary(fitSQRTPer60F20132015)

sortPreGoalplayer20112015FPP500$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, sortPreGoalplayer20112015FPP500) -0.8914 *sqrt(sortPreGoalplayer20112015FPP500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20112015FPP500<-sortPreGoalplayer20112015FPP500[order(sortPreGoalplayer20112015FPP500$GoalPredictWithout),] 
nrow(sortPreGoalplayer20112015FPP500)

sortPreGoalplayer20132015FPP500Top30 = sortPreGoalplayer20132015FPP500[1:30,] 
sortPreGoalplayer20132015FPP500Top30 <-sortPreGoalplayer20132015FPP500Top30[order(sortPreGoalplayer20132015FPP500Top30$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top60 =sortPreGoalplayer20132015FPP500[31:60,] 
sortPreGoalplayer20132015FPP500Top60 <-sortPreGoalplayer20132015FPP500Top60[order(sortPreGoalplayer20132015FPP500Top60$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top90 =sortPreGoalplayer20132015FPP500[61:90,] 
sortPreGoalplayer20132015FPP500Top90 <-sortPreGoalplayer20132015FPP500Top90[order(sortPreGoalplayer20132015FPP500Top90$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top120 =sortPreGoalplayer20132015FPP500[91:120,] 
sortPreGoalplayer20132015FPP500Top120 <-sortPreGoalplayer20132015FPP500Top120[order(sortPreGoalplayer20132015FPP500Top120$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top150 =sortPreGoalplayer20132015FPP500[121:150,] 
sortPreGoalplayer20132015FPP500Top150 <-sortPreGoalplayer20132015FPP500Top150[order(sortPreGoalplayer20132015FPP500Top150$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top180 =sortPreGoalplayer20132015FPP500[151:180,] 
sortPreGoalplayer20132015FPP500Top180 <-sortPreGoalplayer20132015FPP500Top180[order(sortPreGoalplayer20132015FPP500Top180$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top210 =sortPreGoalplayer20132015FPP500[181:210,] 
sortPreGoalplayer20132015FPP500Top210 <-sortPreGoalplayer20132015FPP500Top210[order(sortPreGoalplayer20132015FPP500Top210$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top240 =sortPreGoalplayer20132015FPP500[211:240,] 
sortPreGoalplayer20132015FPP500Top240 <-sortPreGoalplayer20132015FPP500Top240[order(sortPreGoalplayer20132015FPP500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top270 =sortPreGoalplayer20132015FPP500[241:270,] 
sortPreGoalplayer20132015FPP500Top270 <-sortPreGoalplayer20132015FPP500Top240[order(sortPreGoalplayer20132015FPP500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015FPP500Top30[30,83]-sortPreGoalplayer20132015FPP500Top30[1,83]+
  sortPreGoalplayer20132015FPP500Top60[30,83]-sortPreGoalplayer20132015FPP500Top60[1,83]+
  sortPreGoalplayer20132015FPP500Top90[30,83]-sortPreGoalplayer20132015FPP500Top90[1,83]+
  sortPreGoalplayer20132015FPP500Top120[30,83]-sortPreGoalplayer20132015FPP500Top120[1,83]+
  sortPreGoalplayer20132015FPP500Top150[30,83]-sortPreGoalplayer20132015FPP500Top150[1,83]+
  sortPreGoalplayer20132015FPP500Top180[30,83]-sortPreGoalplayer20132015FPP500Top180[1,83]+
  sortPreGoalplayer20132015FPP500Top210[30,83]-sortPreGoalplayer20132015FPP500Top210[1,83]+
  sortPreGoalplayer20132015FPP500Top240[30,83]-sortPreGoalplayer20132015FPP500Top240[1,83]+
  sortPreGoalplayer20132015FPP500Top270[30,83]-sortPreGoalplayer20132015FPP500Top270[1,83]

sortPreGoalplayer20112015FPP500Top30 =sortPreGoalplayer20112015FPP500[736:766,] 

sortPreGoalplayer20132015FPP500Top270 <-sortPreGoalplayer20132015FPP500Top240[order(sortPreGoalplayer20132015FPP500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112015FPP500$GoalPredictWithout<-  predict(model5PP, sortPreGoalplayer20112015FPP500) -1.1918 *sqrt(sortPreGoalplayer20112015FPP500$X2ndAssistGoal.xPer60)
sortPreGoalplayer20112015FPP500<-sortPreGoalplayer20112015FPP500[order(sortPreGoalplayer20112015FPP500$GoalPredictWithout),] 
sortPreGoalplayer20112015FPP500Top30 =sortPreGoalplayer20112015FPP500[736:766,] 
plot(sortPreGoalplayer20112015FPP500Top30$GoalonIce20142015Per60~sortPreGoalplayer20112015FPP500Top30$X2ndAssistGoal.xPer60,cex = .3,xlab="2ndGoal", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")
text(x=sortPreGoalplayer20112015FPP500Top30$X2ndAssistGoal.xPer60,y=sortPreGoalplayer20112015FPP500Top30$GoalonIce20142015Per60,
     sortPreGoalplayer20112015FPP500Top30$Name,cex=0.8)
plot(model5PP)

