#2011-2013

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
head(pTOIPP20112012, n=10)
head(data20112012PP, n=10)
player20122013PP <- merge(data20122013PP,pTOIPP20122013,by="Name")
head(pTOIPP20122013, n=10)
head(data20122013PP, n=10)

head(player20112012PP, n=10)
head(player20122013PP, n=10)

player20112013PP <- merge(player20112012PP,player20122013PP,by="Name")
player20112013PP
#player20112012PP=player20112012PP[player20112012PP$Year.x==20112012, ]
player20112013PPD=player20112013PP[player20112013PP$pos.x=='D',]
player20112013PPF=player20112013PP[player20112013PP$pos.x!='D',]

#EntirePlayers
player20112013PP$GoalonIce20142015<-(
  player20112013PP$NonAssistGoal.y+player20112013PP$X1stAssist.y+player20112013PP$X2ndAssisted1stAssist.y+
    player20112013PP$X2ndAssist.y+player20112013PP$NonAssistPlayers.y+player20112013PP$X1stAssistPlayers.y+
    player20112013PP$X2ndAssistPlayers.y+
    player20112013PP$X1stAssistGoal.y+
    player20112013PP$X2ndAssistGoal.y)

fitMeanTime20112013<-lm(player20112013PP$GoalonIce20142015~
                          player20112013PP$NonAssistGoal.x+player20112013PP$X1stAssist.x+player20112013PP$X2ndAssisted1stAssist.x+
                          player20112013PP$X2ndAssist.x+player20112013PP$NonAssistPlayers.x+player20112013PP$X1stAssistPlayers.x+
                          player20112013PP$X2ndAssistPlayers.x+
                          player20112013PP$X1stAssistGoal.x+
                          player20112013PP$X2ndAssistGoal.x,
                        weight=(player20112013PP$TOI..x+player20112013PP$TOI..y)/2
)
summary(fitMeanTime20112013)

fitOnlyY20112013<-lm(player20112013PP$GoalonIce20142015~
                       player20112013PP$NonAssistGoal.x+player20112013PP$X1stAssist.x+player20112013PP$X2ndAssisted1stAssist.x+
                       player20112013PP$X2ndAssist.x+player20112013PP$NonAssistPlayers.x+player20112013PP$X1stAssistPlayers.x+
                       player20112013PP$X2ndAssistPlayers.x+
                       player20112013PP$X1stAssistGoal.x+
                       player20112013PP$X2ndAssistGoal.x,
                     weight=player20112013PP$TOI..y
)
summary(fitOnlyY20112013)

player20112013PP$NonAssistGoal.xPer60<-player20112013PP$NonAssistGoal.x*60/player20112013PP$TOI.x
player20112013PP$X1stAssist.xPer60<-player20112013PP$X1stAssist.x*60/player20112013PP$TOI.x
player20112013PP$X2ndAssisted1stAssist.xPer60<-player20112013PP$X2ndAssisted1stAssist.x*60/player20112013PP$TOI.x
#player20112013PP$X2ndAssisted2ndAssist.xPer60<-player20112013PP$X2ndAssisted2ndAssist.x/player20112013PP$TOI..x
player20112013PP$NonAssistPlayers.xPer60<-player20112013PP$NonAssistPlayers.x*60/player20112013PP$TOI.x
player20112013PP$X1stAssistPlayers.xPer60<-player20112013PP$X1stAssistPlayers.x*60/player20112013PP$TOI.x
player20112013PP$X2ndAssistPlayers.xPer60<-player20112013PP$X2ndAssistPlayers.x*60/player20112013PP$TOI.x
player20112013PP$X1stAssistGoal.xPer60<-player20112013PP$X1stAssistGoal.x*60/player20112013PP$TOI.x
player20112013PP$X2ndAssistGoal.xPer60<-player20112013PP$X2ndAssistGoal.x*60/player20112013PP$TOI.x
player20112013PP$X2ndAssist.xPer60<-player20112013PP$X2ndAssist.x*60/player20112013PP$TOI.x
player20112013PP$GoalonIce20142015Per60<-player20112013PP$GoalonIce20142015*60/player20112013PP$TOI.y
fitPer6020112013<-lm(player20112013PP$GoalonIce20142015Per60~
                       player20112013PP$NonAssistGoal.xPer60+player20112013PP$X1stAssist.xPer60+player20112013PP$X2ndAssisted1stAssist.xPer60+
                       player20112013PP$X2ndAssist.xPer60+player20112013PP$NonAssistPlayers.xPer60+player20112013PP$X1stAssistPlayers.xPer60+
                       player20112013PP$X2ndAssistPlayers.xPer60+
                       player20112013PP$X1stAssistGoal.xPer60+
                       player20112013PP$X2ndAssistGoal.xPer60,
                     weight=player20112013PP$TOI.y,subset=(player20112013PP$TOI.y>200)
)
summary(fitPer6020112013)

#Only Forwards

player20112013PPF$GoalonIce20142015<-(
  player20112013PPF$NonAssistGoal.y+player20112013PPF$X1stAssist.y+player20112013PPF$X2ndAssisted1stAssist.y+
    player20112013PPF$X2ndAssist.y+player20112013PPF$NonAssistPlayers.y+player20112013PPF$X1stAssistPlayers.y+
    player20112013PPF$X2ndAssistPlayers.y+
    player20112013PPF$X1stAssistGoal.y+
    player20112013PPF$X2ndAssistGoal.y)


fitMeanTimeF20112013<-lm(player20112013PPF$GoalonIce20112013~
                           player20112013PPF$NonAssistGoal.x+player20112013PPF$X1stAssist.x+player20112013PPF$X2ndAssisted1stAssist.x+
                           player20112013PPF$X2ndAssist.x+player20112013PPF$NonAssistPlayers.x+player20112013PPF$X1stAssistPlayers.x+
                           player20112013PPF$X2ndAssistPlayers.x+
                           player20112013PPF$X1stAssistGoal.x+
                           player20112013PPF$X2ndAssistGoal.x,
                         weight=(player20112013PPF$TOI..x+player20112013PPF$TOI..y)/2
)
summary(fitMeanTimeF20112013)

fitOnlyYF20112013<-lm(player20112013PPF$GoalonIce20112013~
                        player20112013PPF$NonAssistGoal.x+player20112013PPF$X1stAssist.x+player20112013PPF$X2ndAssisted1stAssist.x+
                        player20112013PPF$X2ndAssist.x+player20112013PPF$NonAssistPlayers.x+player20112013PPF$X1stAssistPlayers.x+
                        player20112013PPF$X2ndAssistPlayers.x+
                        player20112013PPF$X1stAssistGoal.x+
                        player20112013PPF$X2ndAssistGoal.x,
                      weight=player20112013PPF$TOI..y
)
summary(fitOnlyYF20112013)

player20112013PPF$NonAssistGoal.xPer60<-player20112013PPF$NonAssistGoal.x*60/player20112013PPF$TOI.x
player20112013PPF$X1stAssist.xPer60<-player20112013PPF$X1stAssist.x*60/player20112013PPF$TOI.x
player20112013PPF$X2ndAssisted1stAssist.xPer60<-player20112013PPF$X2ndAssisted1stAssist.x*60/player20112013PPF$TOI.x
#player20112013PPF$X2ndAssisted2ndAssist.xPer60<-player20112013PPF$X2ndAssisted2ndAssist.x/player20112013PPF$TOI..x
player20112013PPF$NonAssistPlayers.xPer60<-player20112013PPF$NonAssistPlayers.x*60/player20112013PPF$TOI.x
player20112013PPF$X1stAssistPlayers.xPer60<-player20112013PPF$X1stAssistPlayers.x*60/player20112013PPF$TOI.x
player20112013PPF$X2ndAssistPlayers.xPer60<-player20112013PPF$X2ndAssistPlayers.x*60/player20112013PPF$TOI.x
player20112013PPF$X1stAssistGoal.xPer60<-player20112013PPF$X1stAssistGoal.x*60/player20112013PPF$TOI.x
player20112013PPF$X2ndAssistGoal.xPer60<-player20112013PPF$X2ndAssistGoal.x*60/player20112013PPF$TOI.x
player20112013PPF$X2ndAssist.xPer60<-player20112013PPF$X2ndAssist.x*60/player20112013PPF$TOI.x
player20112013PPF$GoalonIce20142015Per60<-player20112013PPF$GoalonIce20142015*60/player20112013PPF$TOI.y

fitPer60F20112013<-lm(player20112013PPF$GoalonIce20142015Per60~
                        player20112013PPF$NonAssistGoal.xPer60+player20112013PPF$X1stAssist.xPer60+player20112013PPF$X2ndAssisted1stAssist.xPer60+
                        player20112013PPF$X2ndAssist.xPer60+player20112013PPF$NonAssistPlayers.xPer60+player20112013PPF$X1stAssistPlayers.xPer60+
                        player20112013PPF$X2ndAssistPlayers.xPer60+
                        player20112013PPF$X1stAssistGoal.xPer60+
                        player20112013PPF$X2ndAssistGoal.xPer60,
                      weight=player20112013PPF$TOI.y,subset=(player20112013PPF$TOI.y>500 &player20112013PPF$Name!= "Mark.Stone")
)
500*48/82
player20112013PPF500<-player20112013PPF[player20112013PPF$TOI.y>292,]
player20112013PPF500<-player20112013PPF500[player20112013PPF500$TOI.x>500,]
fitSQRTPer60F20112013<-lm(player20112013PPF500$GoalonIce20142015Per60~
                            sqrt(player20112013PPF500$NonAssistGoal.xPer60)+sqrt(player20112013PPF500$X1stAssist.xPer60)+sqrt(player20112013PPF500$X2ndAssisted1stAssist.xPer60)+
                            sqrt(player20112013PPF500$X2ndAssist.xPer60)+sqrt(player20112013PPF500$NonAssistPlayers.xPer60)+sqrt(player20112013PPF500$X1stAssistPlayers.xPer60)+
                            sqrt(player20112013PPF500$X2ndAssistPlayers.xPer60)+
                            sqrt(player20112013PPF500$X1stAssistGoal.xPer60)+
                            sqrt(player20112013PPF500$X2ndAssistGoal.xPer60),
                          weight=player20112013PPF500$TOI.y)

summary(fitSQRTPer60F20112013)


player20112013PPF500$GoalPredictWithout<-  predict(fitSQRTPer60F20112013, player20112013PPF500) -0.78097 *sqrt(player20112013PPF500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20112013PPF500=player20112013PPF500
sortPreGoalplayer20112013PPF500<-sortPreGoalplayer20112013PPF500[order(sortPreGoalplayer20112013PPF500$GoalPredictWithout),] 

sortPreGoalplayer20112013PPF500Top30 = sortPreGoalplayer20112013PPF500[1:30,] 
sortPreGoalplayer20112013PPF500Top30 <-sortPreGoalplayer20112013PPF500Top30[order(sortPreGoalplayer20112013PPF500Top30$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500Top60 =sortPreGoalplayer20112013PPF500[31:60,] 
sortPreGoalplayer20112013PPF500Top60 <-sortPreGoalplayer20112013PPF500Top60[order(sortPreGoalplayer20112013PPF500Top60$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500Top90 =sortPreGoalplayer20112013PPF500[61:90,] 
sortPreGoalplayer20112013PPF500Top90 <-sortPreGoalplayer20112013PPF500Top90[order(sortPreGoalplayer20112013PPF500Top90$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500Top120 =sortPreGoalplayer20112013PPF500[91:120,] 
sortPreGoalplayer20112013PPF500Top120 <-sortPreGoalplayer20112013PPF500Top120[order(sortPreGoalplayer20112013PPF500Top120$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500Top150 =sortPreGoalplayer20112013PPF500[121:150,] 
sortPreGoalplayer20112013PPF500Top150 <-sortPreGoalplayer20112013PPF500Top150[order(sortPreGoalplayer20112013PPF500Top150$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500Top180 =sortPreGoalplayer20112013PPF500[151:180,] 
sortPreGoalplayer20112013PPF500Top180 <-sortPreGoalplayer20112013PPF500Top180[order(sortPreGoalplayer20112013PPF500Top180$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500Top210 =sortPreGoalplayer20112013PPF500[181:210,] 
sortPreGoalplayer20112013PPF500Top210 <-sortPreGoalplayer20112013PPF500Top210[order(sortPreGoalplayer20112013PPF500Top210$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500Top240 =sortPreGoalplayer20112013PPF500[211:240,] 
sortPreGoalplayer20112013PPF500Top240 <-sortPreGoalplayer20112013PPF500Top240[order(sortPreGoalplayer20112013PPF500Top240$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013PPF500[1:1,] 
sortPreGoalplayer20112013PPF500[30:30,] 
sortPreGoalplayer20112013PPF500[31:31,] 
sortPreGoalplayer20112013PPF500[60:60,] 
sortPreGoalplayer20112013PPF500[61:61,] 
sortPreGoalplayer20112013PPF500[90:90,] 
sortPreGoalplayer20112013PPF500[91:91,] 
sortPreGoalplayer20112013PPF500[120:120,] 
sortPreGoalplayer20112013PPF500[121:121,] 
sortPreGoalplayer20112013PPF500[150:150,] 
sortPreGoalplayer20112013PPF500[151:151,] 
sortPreGoalplayer20112013PPF500[180:180,] 
sortPreGoalplayer20112013PPF500[181:181,] 
sortPreGoalplayer20112013PPF500[210:210,] 
sortPreGoalplayer20112013PPF500[211:211,] 
sortPreGoalplayer20112013PPF500[240:240,] 
sortPreGoalplayer20112013PPF500[241:241,] 
sortPreGoalplayer20112013PPF500[270:270,] 
sortPreGoalplayer20112013PPF500[271:271,] 
sortPreGoalplayer20112013PPF500[285:285,] 

sortPreGoalplayer20112013PPF500[1:1,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[1:1,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[30:30,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[30:30,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[31:31,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[31:31,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[60:60,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[60:60,]$GoalPredictWithout


sortPreGoalplayer20112013PPF500[61:61,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[61:61,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[90:90,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[90:90,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[91:91,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[91:91,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[120:120,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[120:120,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[121:121,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[121:121,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[150:150,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[150:150,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[151:151,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[151:151,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[180:180,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[180:180,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[181:181,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[181:181,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[210:210,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[210:210,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[211:211,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[211:211,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[240:240,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[240:240,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[241:241,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[241:241,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[270:270,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[270:270,]$GoalPredictWithout

sortPreGoalplayer20112013PPF500[271:271,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[271:271,]$GoalPredictWithout
sortPreGoalplayer20112013PPF500[285:285,]$GoalonIce20142015Per60-sortPreGoalplayer20112013PPF500[285:285,]$GoalPredictWithout

boxplot(sqrt(player20112013PPF500$NonAssistGoal.xPer60),sqrt(player20112013PPF500$X1stAssist.xPer60),sqrt(player20112013PPF500$X2ndAssisted1stAssist.xPer60),
        sqrt(player20112013PPF500$X2ndAssist.xPer60),sqrt(player20112013PPF500$NonAssistPlayers.xPer60),sqrt(player20112013PPF500$X1stAssistPlayers.xPer60),
        sqrt(player20112013PPF500$X2ndAssistPlayers.xPer60),
        sqrt(player20112013PPF500$X1stAssistGoal.xPer60),
        sqrt(player20112013PPF500$X2ndAssistGoal.xPer60))
hist(player20112013PPF$NonAssistGoal.xPer60)
summary(fitPer60F20112013)
plot(fitPer60F20112013)

#only defenders
player20112013PPD$GoalonIce20142015<-(
  player20112013PPD$NonAssistGoal.y+player20112013PPD$X1stAssist.y+player20112013PPD$X2ndAssisted1stAssist.y+
    player20112013PPD$X2ndAssist.y+player20112013PPD$NonAssistPlayers.y+player20112013PPD$X1stAssistPlayers.y+
    player20112013PPD$X2ndAssistPlayers.y+
    player20112013PPD$X1stAssistGoal.y+
    player20112013PPD$X2ndAssistGoal.y)


fitMeanTimeD20112013<-lm(player20112013PPD$GoalonIce20142015~
                           player20112013PPD$NonAssistGoal.x+player20112013PPD$X1stAssist.x+player20112013PPD$X2ndAssisted1stAssist.x+
                           player20112013PPD$X2ndAssist.x+player20112013PPD$NonAssistPlayers.x+player20112013PPD$X1stAssistPlayers.x+
                           player20112013PPD$X2ndAssistPlayers.x+
                           player20112013PPD$X1stAssistGoal.x+
                           player20112013PPD$X2ndAssistGoal.x,
                         weight=(player20112013PPD$TOI..x+player20112013PPD$TOI..y)/2
)
summary(fitMeanTimeD20112013)

fitOnlyYD20112013<-lm(player20112013PPD$GoalonIce20142015~
                        player20112013PPD$NonAssistGoal.x+player20112013PPD$X1stAssist.x+player20112013PPD$X2ndAssisted1stAssist.x+
                        player20112013PPD$X2ndAssist.x+player20112013PPD$NonAssistPlayers.x+player20112013PPD$X1stAssistPlayers.x+
                        player20112013PPD$X2ndAssistPlayers.x+
                        player20112013PPD$X1stAssistGoal.x+
                        player20112013PPD$X2ndAssistGoal.x,
                      weight=player20112013PPD$TOI..y
)
summary(fitOnlyYD20112013)

player20112013PPD$NonAssistGoal.xPer60<-player20112013PPD$NonAssistGoal.x*60/player20112013PPD$TOI.x
player20112013PPD$X1stAssist.xPer60<-player20112013PPD$X1stAssist.x*60/player20112013PPD$TOI.x
player20112013PPD$X2ndAssisted1stAssist.xPer60<-player20112013PPD$X2ndAssisted1stAssist.x*60/player20112013PPD$TOI.x
#player20112013PPD$X2ndAssisted2ndAssist.xPer60<-player20112013PPD$X2ndAssisted2ndAssist.x/player20112013PPD$TOI..x
player20112013PPD$NonAssistPlayers.xPer60<-player20112013PPD$NonAssistPlayers.x*60/player20112013PPD$TOI.x
player20112013PPD$X1stAssistPlayers.xPer60<-player20112013PPD$X1stAssistPlayers.x*60/player20112013PPD$TOI.x
player20112013PPD$X2ndAssistPlayers.xPer60<-player20112013PPD$X2ndAssistPlayers.x*60/player20112013PPD$TOI.x
player20112013PPD$X1stAssistGoal.xPer60<-player20112013PPD$X1stAssistGoal.x*60/player20112013PPD$TOI.x
player20112013PPD$X2ndAssistGoal.xPer60<-player20112013PPD$X2ndAssistGoal.x*60/player20112013PPD$TOI.x
player20112013PPD$X2ndAssist.xPer60<-player20112013PPD$X2ndAssist.x*60/player20112013PPD$TOI.x
player20112013PPD$GoalonIce20142015Per60<-player20112013PPD$GoalonIce20142015*60/player20112013PPD$TOI.y
fitPer60D20112013<-lm(player20112013PPD$GoalonIce20142015Per60~
                        player20112013PPD$NonAssistGoal.xPer60+player20112013PPD$X1stAssist.xPer60+player20112013PPD$X2ndAssisted1stAssist.xPer60+
                        player20112013PPD$X2ndAssist.xPer60+player20112013PPD$NonAssistPlayers.xPer60+player20112013PPD$X1stAssistPlayers.xPer60+
                        player20112013PPD$X2ndAssistPlayers.xPer60+
                        player20112013PPD$X1stAssistGoal.xPer60+
                        player20112013PPD$X2ndAssistGoal.xPer60,
                      weight=player20112013PPD$TOI.y,subset=(player20112013PPD$TOI.y>200)
)
summary(fitPer60D20112013)
summary(fitPer60F20112013)
summary(fitPer6020112013)
plot(fitPer60F20112013)

plot(player20112013PPF$GoalonIce20142015Per60~player20112013PPF$X2ndAssisted1stAssist.xPer60,subset=(player20112013PPF$TOI.y>20))
fit1<-lm(player20112013PPF$GoalonIce20142015Per60~player20112013PPF$X2ndAssisted1stAssist.xPer60,subset=(player20112013PPF$TOI.y>20))
summary(fit1)





