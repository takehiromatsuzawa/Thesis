player20132015F500<-player20132015F[player20132015F$TOI.y>500,]
player20132015F500<-player20132015F500[player20132015F500$TOI.x>500,]
fitSQRTPer60F20132015<-lm(player20132015F500$GoalonIce20142015Per60~
                            sqrt(player20132015F500$NonAssistGoal.xPer60)+sqrt(player20132015F500$X1stAssist.xPer60)+sqrt(player20132015F500$X2ndAssisted1stAssist.xPer60)+
                            sqrt(player20132015F500$X2ndAssist.xPer60)+sqrt(player20132015F500$NonAssistPlayers.xPer60)+sqrt(player20132015F500$X1stAssistPlayers.xPer60)+
                            sqrt(player20132015F500$X2ndAssistPlayers.xPer60)+
                            sqrt(player20132015F500$X1stAssistGoal.xPer60)+
                            sqrt(player20132015F500$X2ndAssistGoal.xPer60),
                          weight=player20132015F500$TOI.y)

summary(fitSQRTPer60F20132015)

player20132015F500$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, player20132015F500) -0.792474 *sqrt(player20132015F500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20132015F500=player20132015F500
sortPreGoalplayer20132015F500<-sortPreGoalplayer20132015F500[order(sortPreGoalplayer20132015F500$GoalPredictWithout),] 

sortPreGoalplayer20132015F500Top30 = sortPreGoalplayer20132015F500[1:30,] 
sortPreGoalplayer20132015F500Top30 <-sortPreGoalplayer20132015F500Top30[order(sortPreGoalplayer20132015F500Top30$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top60 =sortPreGoalplayer20132015F500[31:60,] 
sortPreGoalplayer20132015F500Top60 <-sortPreGoalplayer20132015F500Top60[order(sortPreGoalplayer20132015F500Top60$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top90 =sortPreGoalplayer20132015F500[61:90,] 
sortPreGoalplayer20132015F500Top90 <-sortPreGoalplayer20132015F500Top90[order(sortPreGoalplayer20132015F500Top90$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top120 =sortPreGoalplayer20132015F500[91:120,] 
sortPreGoalplayer20132015F500Top120 <-sortPreGoalplayer20132015F500Top120[order(sortPreGoalplayer20132015F500Top120$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top150 =sortPreGoalplayer20132015F500[121:150,] 
sortPreGoalplayer20132015F500Top150 <-sortPreGoalplayer20132015F500Top150[order(sortPreGoalplayer20132015F500Top150$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top180 =sortPreGoalplayer20132015F500[151:180,] 
sortPreGoalplayer20132015F500Top180 <-sortPreGoalplayer20132015F500Top180[order(sortPreGoalplayer20132015F500Top180$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top210 =sortPreGoalplayer20132015F500[181:210,] 
sortPreGoalplayer20132015F500Top210 <-sortPreGoalplayer20132015F500Top210[order(sortPreGoalplayer20132015F500Top210$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top240 =sortPreGoalplayer20132015F500[211:240,] 
sortPreGoalplayer20132015F500Top240 <-sortPreGoalplayer20132015F500Top240[order(sortPreGoalplayer20132015F500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top270 =sortPreGoalplayer20132015F500[241:270,] 
sortPreGoalplayer20132015F500Top270 <-sortPreGoalplayer20132015F500Top240[order(sortPreGoalplayer20132015F500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top30[30,83]-sortPreGoalplayer20132015F500Top30[1,83]+
sortPreGoalplayer20132015F500Top60[30,83]-sortPreGoalplayer20132015F500Top60[1,83]+
sortPreGoalplayer20132015F500Top90[30,83]-sortPreGoalplayer20132015F500Top90[1,83]+
sortPreGoalplayer20132015F500Top120[30,83]-sortPreGoalplayer20132015F500Top120[1,83]+
sortPreGoalplayer20132015F500Top150[30,83]-sortPreGoalplayer20132015F500Top150[1,83]+
sortPreGoalplayer20132015F500Top180[30,83]-sortPreGoalplayer20132015F500Top180[1,83]+
sortPreGoalplayer20132015F500Top210[30,83]-sortPreGoalplayer20132015F500Top210[1,83]+
sortPreGoalplayer20132015F500Top240[30,83]-sortPreGoalplayer20132015F500Top240[1,83]+
sortPreGoalplayer20132015F500Top270[30,83]-sortPreGoalplayer20132015F500Top270[1,83]

0.7278208

plot(sortPreGoalplayer20132015F500Top270$GoalonIce20142015Per60~sortPreGoalplayer20132015F500Top270$X2ndAssisted1stAssist.xPer60,cex = .3,xlab="2013-2014 2nd1stAssist", ylab="2014-15 Goals",col="red")
#points(x, cex = .1, col = "dark red")
text(x=sortPreGoalplayer20132015F500Top270$X2ndAssisted1stAssist.xPer60,y=sortPreGoalplayer20132015F500Top270$GoalonIce20142015Per60,
     sortPreGoalplayer20132015F500Top270$Name,cex=0.8)

sortPreGoalplayer20122015F500<-rbind(sortPreGoalplayer20132015F500,sortPreGoalplayer20122014F500)
sortPreGoalplayer20112015F500<-rbind(sortPreGoalplayer20112013F500,sortPreGoalplayer20122015F500)
#sortPreGoalplayer20112013F500$GoalonIce20142015=sortPreGoalplayer20112013F500$GoalonIce20142015Per60*sortPreGoalplayer20112013F500$TOI.y
#sortPreGoalplayer20112013F500$GoalonIce20112013 <- NULL
#a <- c(colnames( sortPreGoalplayer20112013F500))
##a1 <- c(colnames( sortPreGoalplayer20122015F500))
#a-a1
#setdiff(a,a1)
#setdiff(a1,a)
#fitSQRTPer60F20112013<-lm(player20112013F500$GoalonIce20142015Per60~
 #                           sqrt(player20112013F500$NonAssistGoal.xPer60)+sqrt(player20112013F500$X1stAssist.xPer60)+sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60)+
#                            sqrt(player20112013F500$X2ndAssist.xPer60)+sqrt(player20112013F500$NonAssistPlayers.xPer60)+sqrt(player20112013F500$X1stAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X1stAssistGoal.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistGoal.xPer60),
#                          weight=player20112013F500$TOI.y)

#sortPreGoalplayer20112015F500
model5=lm(sortPreGoalplayer20112015F500$GoalonIce20142015Per60~
            sqrt(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$NonAssistPlayers.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssistPlayers.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X2ndAssistPlayers.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X1stAssistGoal.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60),
          weight=sortPreGoalplayer20112015F500$TOI.y)

summary(model5)
modelDropP=lm(sortPreGoalplayer20112015F500$GoalonIce20142015Per60~
            sqrt(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssistPlayers.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X2ndAssistPlayers.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X1stAssistGoal.xPer60)+
            sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60),
          weight=sortPreGoalplayer20112015F500$TOI.y)
summary(modelDropP)
modelDropP7=lm(sortPreGoalplayer20112015F500$GoalonIce20142015Per60~
                sqrt(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)+
                sqrt(sortPreGoalplayer20112015F500$X2ndAssist.xPer60)+
                sqrt(sortPreGoalplayer20112015F500$X2ndAssistPlayers.xPer60)+
                
                sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60),
              weight=sortPreGoalplayer20112015F500$TOI.y)

summary(modelDropP7)

modelDropP5=lm(sortPreGoalplayer20112015F500$GoalonIce20142015Per60~
                sqrt(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)+
                sqrt(sortPreGoalplayer20112015F500$X2ndAssist.xPer60)+
               
                
                sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60),
              weight=sortPreGoalplayer20112015F500$TOI.y)

summary(modelDropP5)


data=sortPreGoalplayer20112015F500
set.seed(420); nsims=10; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
#fitSQRTPer60F20112013<-lm(player20112013F500$GoalonIce20142015Per60~
#                           sqrt(player20112013F500$NonAssistGoal.xPer60)+sqrt(player20112013F500$X1stAssist.xPer60)+sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60)+
#                            sqrt(player20112013F500$X2ndAssist.xPer60)+sqrt(player20112013F500$NonAssistPlayers.xPer60)+sqrt(player20112013F500$X1stAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X1stAssistGoal.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistGoal.xPer60),
#                          weight=player20112013F500$TOI.y)
#modelStepBoth<-step(model5,scope=list(upper=~.^2,lower=~1),data=data,direction="both")
nrow(data)
set.seed(420); nsims=10; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],]  
  #model5
  model5=lm(sortPreGoalplayer20112015F500$GoalonIce20142015Per60~
              sqrt(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)+
              sqrt(sortPreGoalplayer20112015F500$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$NonAssistPlayers.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssistPlayers.xPer60)+
              sqrt(sortPreGoalplayer20112015F500$X2ndAssistPlayers.xPer60)+
              sqrt(sortPreGoalplayer20112015F500$X1stAssistGoal.xPer60)+
              sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60),
            weight=sortPreGoalplayer20112015F500$TOI.y,data=train)
 # modelStepBoth<-step(model5,scope=list(upper=~.^2,lower=~1),data=train,direction="both",trace=0)
  #modelDropP5=lm(sortPreGoalplayer20112015F500$GoalonIce20142015Per60~
   #                sqrt(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)+
    #               sqrt(sortPreGoalplayer20112015F500$X2ndAssist.xPer60)+
                   
                   
                   sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015F500$TOI.y,data=train)
  modelDropP7=lm(sortPreGoalplayer20112015F500$GoalonIce20142015Per60~
                   sqrt(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015F500$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015F500$X2ndAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015F500$X2ndAssistPlayers.xPer60)+
                   
                   sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015F500$TOI.y,data=train)
  
  
  sse1[i]=sum((sortPreGoalplayer20112015F500$GoalonIce20142015Per60-predict(model5,new=test))^2)
  sse2[i]=sum((sortPreGoalplayer20112015F500$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
  sse3[i]=sum((sortPreGoalplayer20112015F500$GoalonIce20142015Per60-predict(modelDropP5,new=test))^2) 
  sse4[i]=sum((sortPreGoalplayer20112015F500$GoalonIce20142015Per60-predict(modelDropP7,new=test))^2) 
  
}
c(mean(sse1),mean(sse2),mean(sse3))/(n-1200)
sse2

set.seed(420); nsims=2000; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)

model5=lm(GoalonIce20142015Per60~
            sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
            sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
            sqrt(X2ndAssistPlayers.xPer60)+
            sqrt(X1stAssistGoal.xPer60)+
            sqrt(X2ndAssistGoal.xPer60),
          weight=TOI.y,data=data)

modelStepBoth7<-step(modelDropP7,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)
modelStepBoth<-step(modelDropP5,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)
summary(modelStepBoth7)
summary(modelStepBoth)

sortPreGoalplayer20112015F500$GoalPredict<-  predict(fitSQRTPer60F20132015, sortPreGoalplayer20112015F500)
#sortPreGoalplayer20112015F500$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, sortPreGoalplayer20112015F500) -0.8914 *sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)
predictsortPreGoalplayer20112015F500<-sortPreGoalplayer20112015F500[order(sortPreGoalplayer20112015F500$GoalPredict),] 
nrow(predictsortPreGoalplayer20112015F500)

predictsortPreGoalplayer20142015F500<-predictsortPreGoalplayer20112015F500[predictsortPreGoalplayer20112015F500$Year.y==20142015,]

predictsortPreGoalplayer20142015F500Top30<-predictsortPreGoalplayer20142015F500[223:253,]
plot(predictsortPreGoalplayer20142015F500Top30$GoalPredict,cex = .4,main='Predicted Top30 Forwards (EH)',xlab="Ranking", ylab="Predicted Team Goals",col="red")
text(x=predictsortPreGoalplayer20142015F500Top30$ID,y=predictsortPreGoalplayer20142015F500Top30$GoalPredict,
     predictsortPreGoalplayer20142015F500Top30$Name,cex=0.7)
predictsortPreGoalplayer20142015F500Top30$ID<-seq.int(nrow(predictsortPreGoalplayer20142015F500Top30))
predictsortPreGoalplayer20142015F500Top30$ID

myvars <- c("Name", "GoalPredict")
newpredictsortPreGoalplayer20142015F500Top30 <- predictsortPreGoalplayer20142015F500Top30[myvars]
newpredictsortPreGoalplayer20142015F500Top30<-newpredictsortPreGoalplayer20142015F500Top30[newpredictsortPreGoalplayer20142015F500Top30$Name!='Steve.Ott',]
View(newpredictsortPreGoalplayer20142015F500Top30)

for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],]  
  #model5
  model5=lm(GoalonIce20142015Per60~
              sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
              sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
              sqrt(X2ndAssistPlayers.xPer60)+
              sqrt(X1stAssistGoal.xPer60)+
              sqrt(X2ndAssistGoal.xPer60),
            weight=TOI.y,data=train)
   
  modelDropP5=lm(GoalonIce20142015Per60~
                  sqrt(NonAssistGoal.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
                 sqrt(X2ndAssist.xPer60)+
  
  
  sqrt(X2ndAssistGoal.xPer60),
  weight=TOI.y,data=train)
modelDropP7=lm(GoalonIce20142015Per60~
                 sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
                 sqrt(X2ndAssist.xPer60)+
                 sqrt(X2ndAssistPlayers.xPer60)+
                 
                 sqrt(X2ndAssistGoal.xPer60),
               weight=TOI.y,data=train)


sse1[i]=sum((test$GoalonIce20142015Per60-predict(model5,new=test))^2)
sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
sse3[i]=sum((test$GoalonIce20142015Per60-predict(modelDropP5,new=test))^2) 
sse4[i]=sum((test$GoalonIce20142015Per60-predict(modelDropP7,new=test))^2) 
sse5[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth7,new=test))^2)
}
c(mean(sse1),mean(sse2),mean(sse3),mean(sse4),mean(sse5))/(n-600)
data=sortPreGoalplayer20112015D500
modelTest=lm(formula = GoalonIce20142015Per60 ~ sqrt(NonAssistGoal.xPer60) + 
     sqrt(X2ndAssisted1stAssist.xPer60) + sqrt(X2ndAssist.xPer60) + 
     sqrt(X2ndAssistPlayers.xPer60) + sqrt(X2ndAssistGoal.xPer60) + 
     sqrt(X2ndAssisted1stAssist.xPer60):sqrt(X2ndAssist.xPer60) 
     , 
   data = data, weights = TOI.y)
summary(modelTest)




sortPreGoalplayer20112015D500$GoalPredict<-  predict(modelTest, sortPreGoalplayer20112015D500)
#sortPreGoalplayer20112015D500$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, sortPreGoalplayer20112015D500) -0.8914 *sqrt(sortPreGoalplayer20112015D500$X2ndAssisted1stAssist.xPer60)
predictsortPreGoalplayer20112015D500<-sortPreGoalplayer20112015D500[order(sortPreGoalplayer20112015D500$GoalPredict),] 
nrow(predictsortPreGoalplayer20112015D500)
View(predictsortPreGoalplayer20112015D500)
predictsortPreGoalplayer20142015D500<-predictsortPreGoalplayer20112015D500[predictsortPreGoalplayer20112015D500$Year.y==20142015,]
nrow(predictsortPreGoalplayer20142015D500)

predictsortPreGoalplayer20142015D500Top30<-predictsortPreGoalplayer20142015D500[112:132,]

plot(predictsortPreGoalplayer20142015D500Top30$GoalPredict,cex = .4,main='Predicted Top30 Forwards (EH)',xlab="Ranking", ylab="Predicted Team Goals",col="red")
text(x=predictsortPreGoalplayer20142015D500Top30$ID,y=predictsortPreGoalplayer20142015D500Top30$GoalPredict,
     predictsortPreGoalplayer20142015D500Top30$Name,cex=0.7)
predictsortPreGoalplayer20142015D500Top30$ID<-seq.int(nrow(predictsortPreGoalplayer20142015D500Top30))
predictsortPreGoalplayer20142015D500Top30$ID

myvars <- c("Name", "GoalPredict")
newpredictsortPreGoalplayer20142015D500Top30 <- predictsortPreGoalplayer20142015D500Top30[myvars]
newpredictsortPreGoalplayer20142015D500Top30<-newpredictsortPreGoalplayer20142015D500Top30[newpredictsortPreGoalplayer20142015D500Top30$Name!='Andrej.Meszaros',]
newpredictsortPreGoalplayer20142015D500Top30<-newpredictsortPreGoalplayer20142015D500Top30[newpredictsortPreGoalplayer20142015D500Top30$Name!='Nick.Holden',]
View(newpredictsortPreGoalplayer20142015D500Top30)



for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],] 
  sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBoth,new=test))^2)
}
model5=lm(GoalonIce20142015Per60~
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
summary(model5)

model5=lm(GoalonIce20142015Per60~
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

sortPreGoalplayer20112015F500
player20132015F500<-player20132015F[player20132015F$TOI.y>500,]
player20132015F500<-player20132015F500[player20132015F500$TOI.x>500,]
fitSQRTPer60F20132015<-lm(GoalonIce20142015Per60~
                            sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)
                          +sqrt(X2ndAssisted1stAssist.xPer60)+
                            sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)
                          +sqrt(X1stAssistPlayers.xPer60)+
                            sqrt(X2ndAssistPlayers.xPer60)+  sqrt(X1stAssistGoal.xPer60)+
                            sqrt(X2ndAssistGoal.xPer60),
                          weight=TOI.y,data=sortPreGoalplayer20112015F500)

#fitSQRTPer60D20112015<-lm(GoalonIce20142015Per60~
#                            sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)
#                          +sqrt(X2ndAssisted1stAssist.xPer60)+
#                            sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)
#                          +sqrt(X1stAssistPlayers.xPer60)+
#                            sqrt(X2ndAssistPlayers.xPer60)+  sqrt(X1stAssistGoal.xPer60)+
#                            sqrt(X2ndAssistGoal.xPer60),
#                          weight=TOI.y,data=sortPreGoalplayer20112015D500)

summary(fitSQRTPer60F20132015)



sortPreGoalplayer20112015F500$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, sortPreGoalplayer20112015F500) -0.8914 *sqrt(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20112015F500<-sortPreGoalplayer20112015F500[order(sortPreGoalplayer20112015F500$GoalPredictWithout),] 
nrow(sortPreGoalplayer20112015F500)

sortPreGoalplayer20132015F500Top30 = sortPreGoalplayer20132015F500[1:30,] 
sortPreGoalplayer20132015F500Top30 <-sortPreGoalplayer20132015F500Top30[order(sortPreGoalplayer20132015F500Top30$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top60 =sortPreGoalplayer20132015F500[31:60,] 
sortPreGoalplayer20132015F500Top60 <-sortPreGoalplayer20132015F500Top60[order(sortPreGoalplayer20132015F500Top60$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top90 =sortPreGoalplayer20132015F500[61:90,] 
sortPreGoalplayer20132015F500Top90 <-sortPreGoalplayer20132015F500Top90[order(sortPreGoalplayer20132015F500Top90$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top120 =sortPreGoalplayer20132015F500[91:120,] 
sortPreGoalplayer20132015F500Top120 <-sortPreGoalplayer20132015F500Top120[order(sortPreGoalplayer20132015F500Top120$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top150 =sortPreGoalplayer20132015F500[121:150,] 
sortPreGoalplayer20132015F500Top150 <-sortPreGoalplayer20132015F500Top150[order(sortPreGoalplayer20132015F500Top150$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top180 =sortPreGoalplayer20132015F500[151:180,] 
sortPreGoalplayer20132015F500Top180 <-sortPreGoalplayer20132015F500Top180[order(sortPreGoalplayer20132015F500Top180$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top210 =sortPreGoalplayer20132015F500[181:210,] 
sortPreGoalplayer20132015F500Top210 <-sortPreGoalplayer20132015F500Top210[order(sortPreGoalplayer20132015F500Top210$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top240 =sortPreGoalplayer20132015F500[211:240,] 
sortPreGoalplayer20132015F500Top240 <-sortPreGoalplayer20132015F500Top240[order(sortPreGoalplayer20132015F500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top270 =sortPreGoalplayer20132015F500[241:270,] 
sortPreGoalplayer20132015F500Top270 <-sortPreGoalplayer20132015F500Top240[order(sortPreGoalplayer20132015F500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015F500Top30[30,83]-sortPreGoalplayer20132015F500Top30[1,83]+
  sortPreGoalplayer20132015F500Top60[30,83]-sortPreGoalplayer20132015F500Top60[1,83]+
  sortPreGoalplayer20132015F500Top90[30,83]-sortPreGoalplayer20132015F500Top90[1,83]+
  sortPreGoalplayer20132015F500Top120[30,83]-sortPreGoalplayer20132015F500Top120[1,83]+
  sortPreGoalplayer20132015F500Top150[30,83]-sortPreGoalplayer20132015F500Top150[1,83]+
  sortPreGoalplayer20132015F500Top180[30,83]-sortPreGoalplayer20132015F500Top180[1,83]+
  sortPreGoalplayer20132015F500Top210[30,83]-sortPreGoalplayer20132015F500Top210[1,83]+
  sortPreGoalplayer20132015F500Top240[30,83]-sortPreGoalplayer20132015F500Top240[1,83]+
  sortPreGoalplayer20132015F500Top270[30,83]-sortPreGoalplayer20132015F500Top270[1,83]
nrow(sortPreGoalplayer20112015F500)
sortPreGoalplayer20112015F500Top30 =sortPreGoalplayer20112015F500[736:766,] 

sortPreGoalplayer20132015F500Top270 <-sortPreGoalplayer20132015F500Top240[order(sortPreGoalplayer20132015F500Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112015F500$GoalPredictWithout<-  predict(model5, sortPreGoalplayer20112015F500) -1.1918 *sqrt(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60)
sortPreGoalplayer20112015F500<-sortPreGoalplayer20112015F500[order(sortPreGoalplayer20112015F500$GoalPredictWithout),] 
sortPreGoalplayer20112015F500Top30 =sortPreGoalplayer20112015F500[736:766,] 
plot(sortPreGoalplayer20112015F500Top30$GoalonIce20142015Per60~sortPreGoalplayer20112015F500Top30$X2ndAssistGoal.xPer60,cex = .3,xlab="2nd1stAssist", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")
text(x=sortPreGoalplayer20112015F500Top30$X2ndAssistGoal.xPer60,y=sortPreGoalplayer20112015F500Top30$GoalonIce20142015Per60,
     sortPreGoalplayer20112015F500Top30$Name,cex=0.8)
plot(model5)


#Defenders
fitSQRTPer60D20112015<-lm(GoalonIce20142015Per60~
                            sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)
                          +sqrt(X2ndAssisted1stAssist.xPer60)+
                            sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)
                          +sqrt(X1stAssistPlayers.xPer60)+
                            sqrt(X2ndAssistPlayers.xPer60)+  sqrt(X1stAssistGoal.xPer60)+
                            sqrt(X2ndAssistGoal.xPer60),
                          weight=TOI.y,data=sortPreGoalplayer20112015D500)


#sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
 # sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
 # sqrt(X2ndAssistPlayers.xPer60)+
 # sqrt(X1stAssistGoal.xPer60)+
#  sqrt(X2ndAssistGoal.xPer60)

summary(fitSQRTPer60D20112015)
sortPreGoalplayer20112015D500$GoalPredictWithout<-  predict(fitSQRTPer60D20112015, sortPreGoalplayer20112015D500) -0.50296 *sqrt(sortPreGoalplayer20112015D500$X2ndAssistGoal.xPer60)
sortPreGoalplayer20112015D500<-sortPreGoalplayer20112015D500[order(sortPreGoalplayer20112015D500$GoalPredictWithout),] 
nrow(sortPreGoalplayer20112015D500)
sortPreGoalplayer20112015D500Top30 =sortPreGoalplayer20112015D500[335:364,] 

plot(sortPreGoalplayer20112015D500Top30$GoalonIce20142015Per60~sortPreGoalplayer20112015D500Top30$X2ndAssistGoal.xPer60,cex = .3,xlab="2ndGoal", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")

text(x=sortPreGoalplayer20112015D500Top30$X2ndAssistGoal.xPer60,y=sortPreGoalplayer20112015D500Top30$GoalonIce20142015Per60,
     sortPreGoalplayer20112015D500Top30$Name,cex=0.8)
sortPreGoalplayer20112015F500$TotalGoals<-sortPreGoalplayer20112015F500$NonAssistGoal.xPer60+
  sortPreGoalplayer20112015F500$X1stAssistGoal.xPer60+
  sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60

sortPreGoalplayer20112015F500$TotalAssists<-sortPreGoalplayer20112015F500$X1stAssist.xPer60+
  sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60 +  sortPreGoalplayer20112015F500$X2ndAssist.xPer60



plot(sortPreGoalplayer20112015F500$TotalGoals~sortPreGoalplayer20112015F500$TotalAssists,cex = .3,main='Forward',xlab="Assists (EH)", ylab="Goals (EH)",col="red")
abline(0, 1)
#points(x, cex = .1, col = "dark red")
text(x=sortPreGoalplayer20112015F500$X2ndAssistPlayers.xPer60,y=sortPreGoalplayer20112015F500$GoalonIce20142015Per60,
     sortPreGoalplayer20112015F500$Name,cex=0.8)


