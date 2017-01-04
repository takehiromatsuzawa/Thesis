#2013-2015

fname=file.choose()
data20132014PP=read.csv(fname,header=T)

fname=file.choose()
data20142015PP=read.csv(fname,header=T)

fname=file.choose()
pTOIPP20132014=read.csv(fname,header=T)

fname=file.choose()
pTOIPP20142015=read.csv(fname,header=T)

data20132014PP$Name<-str_replace_all(data20132014PP$Name," ",".")
data20142015PP$Name<-str_replace_all(data20142015PP$Name," ",".")

data20132014PP <-ddply(data20132014PP, "Name", numcolwise(sum))
data20142015PP <-ddply(data20142015PP, "Name", numcolwise(sum))
player20132014PP <- merge(data20132014PP,pTOIPP20132014,by="Name")
head(pTOIPP20132014, n=10)
head(data20132014PP, n=10)
player20142015PP <- merge(data20142015PP,pTOIPP20142015,by="Name")
head(pTOIPP20142015, n=10)
head(data20142015PP, n=10)

head(player20132014PP, n=10)
head(player20142015PP, n=10)

player20132015PP <- merge(player20132014PP,player20142015PP,by="Name")
player20132015PP
#player20132014PP=player20132014PP[player20132014PP$Year.x==20132014, ]
player20132015PPD=player20132015PP[player20132015PP$pos.x=='D',]
player20132015PPF=player20132015PP[player20132015PP$pos.x!='D',]



player20132015PPF100<-player20132015PPF[player20132015PPF$TOI.y>100,]
player20132015PPF100<-player20132015PPF100[player20132015PPF100$TOI.x>100,]

player20132015PPF100$GoalonIce20142015<-(
  player20132015PPF100$NonAssistGoal.y+player20132015PPF100$X1stAssist.y+player20132015PPF100$X2ndAssisted1stAssist.y+
    player20132015PPF100$X2ndAssist.y+player20132015PPF100$NonAssistPlayers.y+player20132015PPF100$X1stAssistPlayers.y+
    player20132015PPF100$X2ndAssistPlayers.y+
    player20132015PPF100$X1stAssistGoal.y+
    player20132015PPF100$X2ndAssistGoal.y)



player20132015PPF100$NonAssistGoal.xPer60<-player20132015PPF100$NonAssistGoal.x*60/player20132015PPF100$TOI.x
player20132015PPF100$X1stAssist.xPer60<-player20132015PPF100$X1stAssist.x*60/player20132015PPF100$TOI.x
player20132015PPF100$X2ndAssisted1stAssist.xPer60<-player20132015PPF100$X2ndAssisted1stAssist.x*60/player20132015PPF100$TOI.x
#player20132015PPF100$X2ndAssisted2ndAssist.xPer60<-player20132015PPF100$X2ndAssisted2ndAssist.x/player20132015PPF100$TOI..x
player20132015PPF100$NonAssistPlayers.xPer60<-player20132015PPF100$NonAssistPlayers.x*60/player20132015PPF100$TOI.x
player20132015PPF100$X1stAssistPlayers.xPer60<-player20132015PPF100$X1stAssistPlayers.x*60/player20132015PPF100$TOI.x
player20132015PPF100$X2ndAssistPlayers.xPer60<-player20132015PPF100$X2ndAssistPlayers.x*60/player20132015PPF100$TOI.x
player20132015PPF100$X1stAssistGoal.xPer60<-player20132015PPF100$X1stAssistGoal.x*60/player20132015PPF100$TOI.x
player20132015PPF100$X2ndAssistGoal.xPer60<-player20132015PPF100$X2ndAssistGoal.x*60/player20132015PPF100$TOI.x
player20132015PPF100$X2ndAssist.xPer60<-player20132015PPF100$X2ndAssist.x*60/player20132015PPF100$TOI.x
player20132015PPF100$GoalonIce20142015Per60<-player20132015PPF100$GoalonIce20142015*60/player20132015PPF100$TOI.y


#fitSQRTPer60F20132015PP

player20122015PPF100<-rbind(player20122014PPF100,player20132015PPF100)
player20112015PPF100<-rbind(player20112013PPF100,player20122015PPF100)

fitSQRTPer60F20112015PP<-lm(player20112015PPF100$GoalonIce20142015Per60~
                              sqrt(player20112015PPF100$NonAssistGoal.xPer60)+sqrt(player20112015PPF100$X1stAssist.xPer60)+sqrt(player20112015PPF100$X2ndAssisted1stAssist.xPer60)+
                              sqrt(player20112015PPF100$X2ndAssist.xPer60)+sqrt(player20112015PPF100$NonAssistPlayers.xPer60)+sqrt(player20112015PPF100$X1stAssistPlayers.xPer60)+
                              sqrt(player20112015PPF100$X2ndAssistPlayers.xPer60)+
                              sqrt(player20112015PPF100$X1stAssistGoal.xPer60)+
                              sqrt(player20112015PPF100$X2ndAssistGoal.xPer60),
                            weight=player20112015PPF100$TOI.y)

summary(fitSQRTPer60F20112015PP)

player20112015PPF100$GoalPredictWithout<-predict(fitSQRTPer60F20112015PP, player20112015PPF100) - 0.81987  *sqrt(player20112015PPF100$X2ndAssist.xPer60)

sortPreGoalplayer20112015PPF100=player20112015PPF100
sortPreGoalplayer20112015PPF100<-sortPreGoalplayer20112015PPF100[order(sortPreGoalplayer20112015PPF100$GoalPredictWithout),] 
nrow(sortPreGoalplayer20112015PPF100)

sortPreGoalplayer20112015PPF100Top30 = sortPreGoalplayer20112015PPF100[372:402,] 
sortPreGoalplayer20112015PPF100Top30 <-sortPreGoalplayer20112015PPF100Top30[order(sortPreGoalplayer20112015PPF100Top30$X2ndAssisted1stAssist.xPer60),]


plot(sortPreGoalplayer20112015PPF100Top30$GoalonIce20142015Per60~sortPreGoalplayer20112015PPF100Top30$X2ndAssisted1stAssist.xPer60,cex = .3,xlab="2ndAssist", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")

text(x=sortPreGoalplayer20112015PPF100Top30$X2ndAssisted1stAssist.xPer60,y=sortPreGoalplayer20112015PPF100Top30$GoalonIce20142015Per60,
     sortPreGoalplayer20112015PPF100Top30$Name,cex=0.8)

player20112013PPD100<-player20112013PPD[player20112013PPD$TOI.y>58,]
player20112013PPD100<-player20112013PPD100[player20112013PPD100$TOI.x>100,]

player20132015PPD100<-player20132015PPD[player20132015PPD$TOI.y>100,]
player20132015PPD100<-player20132015PPD100[player20132015PPD100$TOI.x>100,]

player20122014PPD100<-player20122014PPD[player20122014PPD$TOI.y>100,]
player20122014PPD100<-player20122014PPD100[player20122014PPD100$TOI.x>58,]



player20122014PPD100$GoalonIce20142015<-(
  player20122014PPD100$NonAssistGoal.y+player20122014PPD100$X1stAssist.y+player20122014PPD100$X2ndAssisted1stAssist.y+
    player20122014PPD100$X2ndAssist.y+player20122014PPD100$NonAssistPlayers.y+player20122014PPD100$X1stAssistPlayers.y+
    player20122014PPD100$X2ndAssistPlayers.y+
    player20122014PPD100$X1stAssistGoal.y+
    player20122014PPD100$X2ndAssistGoal.y)

player20112013PPD100$GoalonIce20142015<-(
  player20112013PPD100$NonAssistGoal.y+player20112013PPD100$X1stAssist.y+player20112013PPD100$X2ndAssisted1stAssist.y+
    player20112013PPD100$X2ndAssist.y+player20112013PPD100$NonAssistPlayers.y+player20112013PPD100$X1stAssistPlayers.y+
    player20112013PPD100$X2ndAssistPlayers.y+
    player20112013PPD100$X1stAssistGoal.y+
    player20112013PPD100$X2ndAssistGoal.y)

player20132015PPD100$GoalonIce20142015<-(
  player20132015PPD100$NonAssistGoal.y+player20132015PPD100$X1stAssist.y+player20132015PPD100$X2ndAssisted1stAssist.y+
    player20132015PPD100$X2ndAssist.y+player20132015PPD100$NonAssistPlayers.y+player20132015PPD100$X1stAssistPlayers.y+
    player20132015PPD100$X2ndAssistPlayers.y+
    player20132015PPD100$X1stAssistGoal.y+
    player20132015PPD100$X2ndAssistGoal.y)

player20122015PPD100<-rbind(player20122014PPD100,player20132015PPD100)
player20112015PPD100<-rbind(player20112013PPD100,player20122015PPD100)

player20112015PPD100$NonAssistGoal.xPer60<-player20112015PPD100$NonAssistGoal.x*60/player20112015PPD100$TOI.x
player20112015PPD100$X1stAssist.xPer60<-player20112015PPD100$X1stAssist.x*60/player20112015PPD100$TOI.x
player20112015PPD100$X2ndAssisted1stAssist.xPer60<-player20112015PPD100$X2ndAssisted1stAssist.x*60/player20112015PPD100$TOI.x
#player20112015PPD100$X2ndAssisted2ndAssist.xPer60<-player20112015PPD100$X2ndAssisted2ndAssist.x/player20112015PPD100$TOI..x
player20112015PPD100$NonAssistPlayers.xPer60<-player20112015PPD100$NonAssistPlayers.x*60/player20112015PPD100$TOI.x
player20112015PPD100$X1stAssistPlayers.xPer60<-player20112015PPD100$X1stAssistPlayers.x*60/player20112015PPD100$TOI.x
player20112015PPD100$X2ndAssistPlayers.xPer60<-player20112015PPD100$X2ndAssistPlayers.x*60/player20112015PPD100$TOI.x
player20112015PPD100$X1stAssistGoal.xPer60<-player20112015PPD100$X1stAssistGoal.x*60/player20112015PPD100$TOI.x
player20112015PPD100$X2ndAssistGoal.xPer60<-player20112015PPD100$X2ndAssistGoal.x*60/player20112015PPD100$TOI.x
player20112015PPD100$X2ndAssist.xPer60<-player20112015PPD100$X2ndAssist.x*60/player20112015PPD100$TOI.x
player20112015PPD100$GoalonIce20142015Per60<-player20112015PPD100$GoalonIce20142015*60/player20112015PPD100$TOI.y

fitSQRTPer60D20112015PP<-lm(player20112015PPD100$GoalonIce20142015Per60~
                              sqrt(player20112015PPD100$NonAssistGoal.xPer60)+sqrt(player20112015PPD100$X1stAssist.xPer60)+sqrt(player20112015PPD100$X2ndAssisted1stAssist.xPer60)+
                              sqrt(player20112015PPD100$X2ndAssist.xPer60)+sqrt(player20112015PPD100$NonAssistPlayers.xPer60)+sqrt(player20112015PPD100$X1stAssistPlayers.xPer60)+
                              sqrt(player20112015PPD100$X2ndAssistPlayers.xPer60)+
                              sqrt(player20112015PPD100$X1stAssistGoal.xPer60)+
                              sqrt(player20112015PPD100$X2ndAssistGoal.xPer60),
                            weight=player20112015PPD100$TOI.y)

summary(fitSQRTPer60D20112015PP)

player20112015PPD100$GoalPredictWithout<-predict(fitSQRTPer60D20112015PP, player20112015PPD100) - 0.9314*sqrt(player20112015PPD100$X1stAssistPlayers.xPer60) 

sortPreGoalplayer20112015PPD100=player20112015PPD100
sortPreGoalplayer20112015PPD100<-sortPreGoalplayer20112015PPD100[order(sortPreGoalplayer20112015PPD100$GoalPredictWithout),] 
nrow(sortPreGoalplayer20112015PPD100)

sortPreGoalplayer20112015PPD100Top30 = sortPreGoalplayer20112015PPD100[113:143,] 
sortPreGoalplayer20112015PPD100Top30 <-sortPreGoalplayer20112015PPD100Top30[order(sortPreGoalplayer20112015PPD100Top30$X1stAssistPlayers.xPer60),]


plot(sortPreGoalplayer20112015PPD100Top30$GoalonIce20142015Per60~sortPreGoalplayer20112015PPD100Top30$X1stAssistPlayers.xPer60,cex = .3,xlab="X1stAssistPlayers", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")

text(x=sortPreGoalplayer20112015PPD100Top30$X1stAssistPlayers.xPer60,y=sortPreGoalplayer20112015PPD100Top30$GoalonIce20142015Per60,
     sortPreGoalplayer20112015PPD100Top30$Name,cex=0.8)




sortPreGoalplayer20132015PPF100Top30 = sortPreGoalplayer20132015PPF100[1:30,] 
sortPreGoalplayer20132015PPF100Top30 <-sortPreGoalplayer20132015PPF100Top30[order(sortPreGoalplayer20132015PPF100Top30$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top60 =sortPreGoalplayer20132015PPF100[31:60,] 
sortPreGoalplayer20132015PPF100Top60 <-sortPreGoalplayer20132015PPF100Top60[order(sortPreGoalplayer20132015PPF100Top60$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top90 =sortPreGoalplayer20132015PPF100[61:90,] 
sortPreGoalplayer20132015PPF100Top90 <-sortPreGoalplayer20132015PPF100Top90[order(sortPreGoalplayer20132015PPF100Top90$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top120 =sortPreGoalplayer20132015PPF100[91:120,] 
sortPreGoalplayer20132015PPF100Top120 <-sortPreGoalplayer20132015PPF100Top120[order(sortPreGoalplayer20132015PPF100Top120$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top150 =sortPreGoalplayer20132015PPF100[121:150,] 
sortPreGoalplayer20132015PPF100Top150 <-sortPreGoalplayer20132015PPF100Top150[order(sortPreGoalplayer20132015PPF100Top150$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top180 =sortPreGoalplayer20132015PPF100[151:180,] 
sortPreGoalplayer20132015PPF100Top180 <-sortPreGoalplayer20132015PPF100Top180[order(sortPreGoalplayer20132015PPF100Top180$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top210 =sortPreGoalplayer20132015PPF100[181:210,] 
sortPreGoalplayer20132015PPF100Top210 <-sortPreGoalplayer20132015PPF100Top210[order(sortPreGoalplayer20132015PPF100Top210$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top240 =sortPreGoalplayer20132015PPF100[211:240,] 
sortPreGoalplayer20132015PPF100Top240 <-sortPreGoalplayer20132015PPF100Top240[order(sortPreGoalplayer20132015PPF100Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top270 =sortPreGoalplayer20132015PPF100[241:270,] 
sortPreGoalplayer20132015PPF100Top270 <-sortPreGoalplayer20132015PPF100Top240[order(sortPreGoalplayer20132015PPF100Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top30[30,83]-sortPreGoalplayer20132015PPF100Top30[1,83]+
  sortPreGoalplayer20132015PPF100Top60[30,83]-sortPreGoalplayer20132015PPF100Top60[1,83]+
  sortPreGoalplayer20132015PPF100Top90[30,83]-sortPreGoalplayer20132015PPF100Top90[1,83]+
  sortPreGoalplayer20132015PPF100Top120[30,83]-sortPreGoalplayer20132015PPF100Top120[1,83]+
  sortPreGoalplayer20132015PPF100Top150[30,83]-sortPreGoalplayer20132015PPF100Top150[1,83]+
  sortPreGoalplayer20132015PPF100Top180[30,83]-sortPreGoalplayer20132015PPF100Top180[1,83]+
  sortPreGoalplayer20132015PPF100Top210[30,83]-sortPreGoalplayer20132015PPF100Top210[1,83]+
  sortPreGoalplayer20132015PPF100Top240[30,83]-sortPreGoalplayer20132015PPF100Top240[1,83]+
  sortPreGoalplayer20132015PPF100Top270[30,83]-sortPreGoalplayer20132015PPF100Top270[1,83]

0.7278208

plot(sortPreGoalplayer20132015PPF100Top270$GoalonIce20142015Per60~sortPreGoalplayer20132015PPF100Top270$X2ndAssisted1stAssist.xPer60,cex = .3,xlab="2013-2014 2nd1stAssist", ylab="2014-15 Goals",col="red")
#points(x, cex = .1, col = "dark red")
text(x=sortPreGoalplayer20132015PPF100Top270$X2ndAssisted1stAssist.xPer60,y=sortPreGoalplayer20132015PPF100Top270$GoalonIce20142015Per60,
     sortPreGoalplayer20132015PPF100Top270$Name,cex=0.8)

sortPreGoalplayer20122015PPF100<-rbind(sortPreGoalplayer20132015PPF100,sortPreGoalplayer20122014F100)
sortPreGoalplayer20112015PPF100<-rbind(sortPreGoalplayer20112013F100,sortPreGoalplayer20122015PPF100)
#sortPreGoalplayer20112013F100$GoalonIce20142015=sortPreGoalplayer20112013F100$GoalonIce20142015Per60*sortPreGoalplayer20112013F100$TOI.y
#sortPreGoalplayer20112013F100$GoalonIce20112013 <- NULL
#a <- c(colnames( sortPreGoalplayer20112013F100))
##a1 <- c(colnames( sortPreGoalplayer20122015PPF100))
#a-a1
#setdiff(a,a1)
#setdiff(a1,a)
#fitSQRTPer60F20112013<-lm(player20112013F100$GoalonIce20142015Per60~
#                           sqrt(player20112013F100$NonAssistGoal.xPer60)+sqrt(player20112013F100$X1stAssist.xPer60)+sqrt(player20112013F100$X2ndAssisted1stAssist.xPer60)+
#                            sqrt(player20112013F100$X2ndAssist.xPer60)+sqrt(player20112013F100$NonAssistPlayers.xPer60)+sqrt(player20112013F100$X1stAssistPlayers.xPer60)+
#                            sqrt(player20112013F100$X2ndAssistPlayers.xPer60)+
#                            sqrt(player20112013F100$X1stAssistGoal.xPer60)+
#                            sqrt(player20112013F100$X2ndAssistGoal.xPer60),
#                          weight=player20112013F100$TOI.y)

#sortPreGoalplayer20112015PPF100
model5PP=lm(sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60~
              sqrt(sortPreGoalplayer20112015PPF100$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)+
              sqrt(sortPreGoalplayer20112015PPF100$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$NonAssistPlayers.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssistPlayers.xPer60)+
              sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistPlayers.xPer60)+
              sqrt(sortPreGoalplayer20112015PPF100$X1stAssistGoal.xPer60)+
              sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60),
            weight=sortPreGoalplayer20112015PPF100$TOI.y)

summary(model5PP)
modelDropPPP=lm(sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60~
                  sqrt(sortPreGoalplayer20112015PPF100$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)+
                  sqrt(sortPreGoalplayer20112015PPF100$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssistPlayers.xPer60)+
                  sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistPlayers.xPer60)+
                  sqrt(sortPreGoalplayer20112015PPF100$X1stAssistGoal.xPer60)+
                  sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60),
                weight=sortPreGoalplayer20112015PPF100$TOI.y)
summary(modelDropPPP)
modelDropPPP7=lm(sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60~
                   sqrt(sortPreGoalplayer20112015PPF100$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistPlayers.xPer60)+
                   
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015PPF100$TOI.y)

summary(modelDropPPP7)

modelDropPPP5=lm(sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60~
                   sqrt(sortPreGoalplayer20112015PPF100$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssist.xPer60)+
                   
                   
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015PPF100$TOI.y)

summary(modelDropPPP5)


data=sortPreGoalplayer20112015PPF100
set.seed(420); nsims=10; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
#fitSQRTPer60F20112013<-lm(player20112013F100$GoalonIce20142015Per60~
#                           sqrt(player20112013F100$NonAssistGoal.xPer60)+sqrt(player20112013F100$X1stAssist.xPer60)+sqrt(player20112013F100$X2ndAssisted1stAssist.xPer60)+
#                            sqrt(player20112013F100$X2ndAssist.xPer60)+sqrt(player20112013F100$NonAssistPlayers.xPer60)+sqrt(player20112013F100$X1stAssistPlayers.xPer60)+
#                            sqrt(player20112013F100$X2ndAssistPlayers.xPer60)+
#                            sqrt(player20112013F100$X1stAssistGoal.xPer60)+
#                            sqrt(player20112013F100$X2ndAssistGoal.xPer60),
#                          weight=player20112013F100$TOI.y)
#modelStepBothPP<-step(model5PP,scope=list(upper=~.^2,lower=~1),data=data,direction="both")
nrow(data)
set.seed(420); nsims=10; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],]  
  #model5PP
  model5PP=lm(sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60~
                sqrt(sortPreGoalplayer20112015PPF100$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)+
                sqrt(sortPreGoalplayer20112015PPF100$X2ndAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$NonAssistPlayers.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssistPlayers.xPer60)+
                sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistPlayers.xPer60)+
                sqrt(sortPreGoalplayer20112015PPF100$X1stAssistGoal.xPer60)+
                sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60),
              weight=sortPreGoalplayer20112015PPF100$TOI.y,data=train)
  # modelStepBothPP<-step(model5PP,scope=list(upper=~.^2,lower=~1),data=train,direction="both",trace=0)
  #modelDropPPP5=lm(sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60~
  #                sqrt(sortPreGoalplayer20112015PPF100$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)+
  #               sqrt(sortPreGoalplayer20112015PPF100$X2ndAssist.xPer60)+
  
  
  sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60),
  weight=sortPreGoalplayer20112015PPF100$TOI.y,data=train)
modelDropPPP7=lm(sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60~
                   sqrt(sortPreGoalplayer20112015PPF100$NonAssistGoal.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X1stAssist.xPer60)+sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssist.xPer60)+
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistPlayers.xPer60)+
                   
                   sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60),
                 weight=sortPreGoalplayer20112015PPF100$TOI.y,data=train)


sse1[i]=sum((sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60-predict(model5PP,new=test))^2)
sse2[i]=sum((sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60-predict(modelStepBothPP,new=test))^2)
sse3[i]=sum((sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60-predict(modelDropPPP5,new=test))^2) 
sse4[i]=sum((sortPreGoalplayer20112015PPF100$GoalonIce20142015Per60-predict(modelDropPPP7,new=test))^2) 

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

modelStepBothPP7<-step(modelDropPPP7,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)
modelStepBothPP<-step(model5PP,scope=list(upper=~.^2,lower=~1),data=data,direction="both",trace=0)

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
  sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBothPP,new=test))^2)
  sse3[i]=sum((test$GoalonIce20142015Per60-predict(modelDropPPP5,new=test))^2) 
  sse4[i]=sum((test$GoalonIce20142015Per60-predict(modelDropPPP7,new=test))^2) 
  sse5[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBothPP7,new=test))^2)
}
c(mean(sse1),mean(sse2),mean(sse3),mean(sse4),mean(sse5))/(n-600)

n
for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:600],]  
  test=data[reorder[600:n],] 
  sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBothPP,new=test))^2)
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
summary(modelStepBothPP)

sortPreGoalplayer20112015PPF100
player20132015PPF100<-player20132015F[player20132015F$TOI.y>100,]
player20132015PPF100<-player20132015PPF100[player20132015PPF100$TOI.x>100,]
fitSQRTPer60F20132015<-lm(GoalonIce20142015Per60~
                            sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)
                          +sqrt(X2ndAssisted1stAssist.xPer60)+
                            sqrt(X2ndAssist.xPer60)+
                            sqrt(X2ndAssistPlayers.xPer60)+
                            sqrt(X2ndAssistGoal.xPer60),
                          weight=TOI.y,data=sortPreGoalplayer20112015PPF100)

summary(fitSQRTPer60F20132015)

sortPreGoalplayer20112015PPF100$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, sortPreGoalplayer20112015PPF100) -0.8914 *sqrt(sortPreGoalplayer20112015PPF100$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20112015PPF100<-sortPreGoalplayer20112015PPF100[order(sortPreGoalplayer20112015PPF100$GoalPredictWithout),] 
nrow(sortPreGoalplayer20112015PPF100)

sortPreGoalplayer20132015PPF100Top30 = sortPreGoalplayer20132015PPF100[1:30,] 
sortPreGoalplayer20132015PPF100Top30 <-sortPreGoalplayer20132015PPF100Top30[order(sortPreGoalplayer20132015PPF100Top30$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top60 =sortPreGoalplayer20132015PPF100[31:60,] 
sortPreGoalplayer20132015PPF100Top60 <-sortPreGoalplayer20132015PPF100Top60[order(sortPreGoalplayer20132015PPF100Top60$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top90 =sortPreGoalplayer20132015PPF100[61:90,] 
sortPreGoalplayer20132015PPF100Top90 <-sortPreGoalplayer20132015PPF100Top90[order(sortPreGoalplayer20132015PPF100Top90$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top120 =sortPreGoalplayer20132015PPF100[91:120,] 
sortPreGoalplayer20132015PPF100Top120 <-sortPreGoalplayer20132015PPF100Top120[order(sortPreGoalplayer20132015PPF100Top120$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top150 =sortPreGoalplayer20132015PPF100[121:150,] 
sortPreGoalplayer20132015PPF100Top150 <-sortPreGoalplayer20132015PPF100Top150[order(sortPreGoalplayer20132015PPF100Top150$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top180 =sortPreGoalplayer20132015PPF100[151:180,] 
sortPreGoalplayer20132015PPF100Top180 <-sortPreGoalplayer20132015PPF100Top180[order(sortPreGoalplayer20132015PPF100Top180$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top210 =sortPreGoalplayer20132015PPF100[181:210,] 
sortPreGoalplayer20132015PPF100Top210 <-sortPreGoalplayer20132015PPF100Top210[order(sortPreGoalplayer20132015PPF100Top210$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top240 =sortPreGoalplayer20132015PPF100[211:240,] 
sortPreGoalplayer20132015PPF100Top240 <-sortPreGoalplayer20132015PPF100Top240[order(sortPreGoalplayer20132015PPF100Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top270 =sortPreGoalplayer20132015PPF100[241:270,] 
sortPreGoalplayer20132015PPF100Top270 <-sortPreGoalplayer20132015PPF100Top240[order(sortPreGoalplayer20132015PPF100Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20132015PPF100Top30[30,83]-sortPreGoalplayer20132015PPF100Top30[1,83]+
  sortPreGoalplayer20132015PPF100Top60[30,83]-sortPreGoalplayer20132015PPF100Top60[1,83]+
  sortPreGoalplayer20132015PPF100Top90[30,83]-sortPreGoalplayer20132015PPF100Top90[1,83]+
  sortPreGoalplayer20132015PPF100Top120[30,83]-sortPreGoalplayer20132015PPF100Top120[1,83]+
  sortPreGoalplayer20132015PPF100Top150[30,83]-sortPreGoalplayer20132015PPF100Top150[1,83]+
  sortPreGoalplayer20132015PPF100Top180[30,83]-sortPreGoalplayer20132015PPF100Top180[1,83]+
  sortPreGoalplayer20132015PPF100Top210[30,83]-sortPreGoalplayer20132015PPF100Top210[1,83]+
  sortPreGoalplayer20132015PPF100Top240[30,83]-sortPreGoalplayer20132015PPF100Top240[1,83]+
  sortPreGoalplayer20132015PPF100Top270[30,83]-sortPreGoalplayer20132015PPF100Top270[1,83]

sortPreGoalplayer20112015PPF100Top30 =sortPreGoalplayer20112015PPF100[736:766,] 

sortPreGoalplayer20132015PPF100Top270 <-sortPreGoalplayer20132015PPF100Top240[order(sortPreGoalplayer20132015PPF100Top240$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112015PPF100$GoalPredictWithout<-  predict(model5PP, sortPreGoalplayer20112015PPF100) -1.1918 *sqrt(sortPreGoalplayer20112015PPF100$X2ndAssistGoal.xPer60)
sortPreGoalplayer20112015PPF100<-sortPreGoalplayer20112015PPF100[order(sortPreGoalplayer20112015PPF100$GoalPredictWithout),] 
sortPreGoalplayer20112015PPF100Top30 =sortPreGoalplayer20112015PPF100[736:766,] 
plot(sortPreGoalplayer20112015PPF100Top30$GoalonIce20142015Per60~sortPreGoalplayer20112015PPF100Top30$X2ndAssistGoal.xPer60,cex = .3,xlab="2ndGoal", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")
text(x=sortPreGoalplayer20112015PPF100Top30$X2ndAssistGoal.xPer60,y=sortPreGoalplayer20112015PPF100Top30$GoalonIce20142015Per60,
     sortPreGoalplayer20112015PPF100Top30$Name,cex=0.8)
plot(model5PP)

