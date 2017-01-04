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
head(pTOIPP20122013, n=10)
head(data20122013PP, n=10)
player20132014PP <- merge(data20132014PP,pTOIPP20132014,by="Name")
head(pTOIPP20132014, n=10)
head(data20132014PP, n=10)

head(player20122013PP, n=10)
head(player20132014PP, n=10)

player20122014PP <- merge(player20122013PP,player20132014PP,by="Name")
player20122014PP
#player20122013PP=player20122013PP[player20122013PP$Year.x==20122013, ]
player20122014PPD=player20122014PP[player20122014PP$pos.x=='D',]
player20122014PPF=player20122014PP[player20122014PP$pos.x!='D',]

#EntirePlayers
player20122014PP$GoalonIce20142015<-(
  player20122014PP$NonAssistGoal.y+player20122014PP$X1stAssist.y+player20122014PP$X2ndAssisted1stAssist.y+
    player20122014PP$X2ndAssist.y+player20122014PP$NonAssistPlayers.y+player20122014PP$X1stAssistPlayers.y+
    player20122014PP$X2ndAssistPlayers.y+
    player20122014PP$X1stAssistGoal.y+
    player20122014PP$X2ndAssistGoal.y)

fitMeanTime20122014<-lm(player20122014PP$GoalonIce20142015~
                          player20122014PP$NonAssistGoal.x+player20122014PP$X1stAssist.x+player20122014PP$X2ndAssisted1stAssist.x+
                          player20122014PP$X2ndAssist.x+player20122014PP$NonAssistPlayers.x+player20122014PP$X1stAssistPlayers.x+
                          player20122014PP$X2ndAssistPlayers.x+
                          player20122014PP$X1stAssistGoal.x+
                          player20122014PP$X2ndAssistGoal.x,
                        weight=(player20122014PP$TOI..x+player20122014PP$TOI..y)/2
)
summary(fitMeanTime20122014)

fitOnlyY20122014<-lm(player20122014PP$GoalonIce20142015~
                       player20122014PP$NonAssistGoal.x+player20122014PP$X1stAssist.x+player20122014PP$X2ndAssisted1stAssist.x+
                       player20122014PP$X2ndAssist.x+player20122014PP$NonAssistPlayers.x+player20122014PP$X1stAssistPlayers.x+
                       player20122014PP$X2ndAssistPlayers.x+
                       player20122014PP$X1stAssistGoal.x+
                       player20122014PP$X2ndAssistGoal.x,
                     weight=player20122014PP$TOI..y
)
summary(fitOnlyY20122014)

player20122014PP$NonAssistGoal.xPer60<-player20122014PP$NonAssistGoal.x*60/player20122014PP$TOI.x
player20122014PP$X1stAssist.xPer60<-player20122014PP$X1stAssist.x*60/player20122014PP$TOI.x
player20122014PP$X2ndAssisted1stAssist.xPer60<-player20122014PP$X2ndAssisted1stAssist.x*60/player20122014PP$TOI.x
#player20122014PP$X2ndAssisted2ndAssist.xPer60<-player20122014PP$X2ndAssisted2ndAssist.x/player20122014PP$TOI..x
player20122014PP$NonAssistPlayers.xPer60<-player20122014PP$NonAssistPlayers.x*60/player20122014PP$TOI.x
player20122014PP$X1stAssistPlayers.xPer60<-player20122014PP$X1stAssistPlayers.x*60/player20122014PP$TOI.x
player20122014PP$X2ndAssistPlayers.xPer60<-player20122014PP$X2ndAssistPlayers.x*60/player20122014PP$TOI.x
player20122014PP$X1stAssistGoal.xPer60<-player20122014PP$X1stAssistGoal.x*60/player20122014PP$TOI.x
player20122014PP$X2ndAssistGoal.xPer60<-player20122014PP$X2ndAssistGoal.x*60/player20122014PP$TOI.x
player20122014PP$X2ndAssist.xPer60<-player20122014PP$X2ndAssist.x*60/player20122014PP$TOI.x
player20122014PP$GoalonIce20142015Per60<-player20122014PP$GoalonIce20142015*60/player20122014PP$TOI.y
fitPer6020122014<-lm(player20122014PP$GoalonIce20142015Per60~
                       player20122014PP$NonAssistGoal.xPer60+player20122014PP$X1stAssist.xPer60+player20122014PP$X2ndAssisted1stAssist.xPer60+
                       player20122014PP$X2ndAssist.xPer60+player20122014PP$NonAssistPlayers.xPer60+player20122014PP$X1stAssistPlayers.xPer60+
                       player20122014PP$X2ndAssistPlayers.xPer60+
                       player20122014PP$X1stAssistGoal.xPer60+
                       player20122014PP$X2ndAssistGoal.xPer60,
                     weight=player20122014PP$TOI.y,subset=(player20122014PP$TOI.y>200)
)
summary(fitPer6020122014)

#Only Forwards

player20122014PPF$GoalonIce20142015<-(
  player20122014PPF$NonAssistGoal.y+player20122014PPF$X1stAssist.y+player20122014PPF$X2ndAssisted1stAssist.y+
    player20122014PPF$X2ndAssist.y+player20122014PPF$NonAssistPlayers.y+player20122014PPF$X1stAssistPlayers.y+
    player20122014PPF$X2ndAssistPlayers.y+
    player20122014PPF$X1stAssistGoal.y+
    player20122014PPF$X2ndAssistGoal.y)


fitMeanTimeF20122014<-lm(player20122014PPF$GoalonIce20142015~
                           player20122014PPF$NonAssistGoal.x+player20122014PPF$X1stAssist.x+player20122014PPF$X2ndAssisted1stAssist.x+
                           player20122014PPF$X2ndAssist.x+player20122014PPF$NonAssistPlayers.x+player20122014PPF$X1stAssistPlayers.x+
                           player20122014PPF$X2ndAssistPlayers.x+
                           player20122014PPF$X1stAssistGoal.x+
                           player20122014PPF$X2ndAssistGoal.x,
                         weight=(player20122014PPF$TOI..x+player20122014PPF$TOI..y)/2
)
summary(fitMeanTimeF20122014)

fitOnlyYF20122014<-lm(player20122014PPF$GoalonIce20142015~
                        player20122014PPF$NonAssistGoal.x+player20122014PPF$X1stAssist.x+player20122014PPF$X2ndAssisted1stAssist.x+
                        player20122014PPF$X2ndAssist.x+player20122014PPF$NonAssistPlayers.x+player20122014PPF$X1stAssistPlayers.x+
                        player20122014PPF$X2ndAssistPlayers.x+
                        player20122014PPF$X1stAssistGoal.x+
                        player20122014PPF$X2ndAssistGoal.x,
                      weight=player20122014PPF$TOI..y
)
summary(fitOnlyYF20122014)

player20122014PPF$NonAssistGoal.xPer60<-player20122014PPF$NonAssistGoal.x*60/player20122014PPF$TOI.x
player20122014PPF$X1stAssist.xPer60<-player20122014PPF$X1stAssist.x*60/player20122014PPF$TOI.x
player20122014PPF$X2ndAssisted1stAssist.xPer60<-player20122014PPF$X2ndAssisted1stAssist.x*60/player20122014PPF$TOI.x
#player20122014PPF$X2ndAssisted2ndAssist.xPer60<-player20122014PPF$X2ndAssisted2ndAssist.x/player20122014PPF$TOI..x
player20122014PPF$NonAssistPlayers.xPer60<-player20122014PPF$NonAssistPlayers.x*60/player20122014PPF$TOI.x
player20122014PPF$X1stAssistPlayers.xPer60<-player20122014PPF$X1stAssistPlayers.x*60/player20122014PPF$TOI.x
player20122014PPF$X2ndAssistPlayers.xPer60<-player20122014PPF$X2ndAssistPlayers.x*60/player20122014PPF$TOI.x
player20122014PPF$X1stAssistGoal.xPer60<-player20122014PPF$X1stAssistGoal.x*60/player20122014PPF$TOI.x
player20122014PPF$X2ndAssistGoal.xPer60<-player20122014PPF$X2ndAssistGoal.x*60/player20122014PPF$TOI.x
player20122014PPF$X2ndAssist.xPer60<-player20122014PPF$X2ndAssist.x*60/player20122014PPF$TOI.x
player20122014PPF$GoalonIce20142015Per60<-player20122014PPF$GoalonIce20142015*60/player20122014PPF$TOI.y
#fitPer60F20122014<-lm(player20122014PPF$GoalonIce20142015Per60~
 #                       player20122014PPF$NonAssistGoal.xPer60+player20122014PPF$X1stAssist.xPer60+player20122014PPF$X2ndAssisted1stAssist.xPer60+
#                        player20122014PPF$X2ndAssist.xPer60+player20122014PPF$NonAssistPlayers.xPer60+player20122014PPF$X1stAssistPlayers.xPer60+
#                        player20122014PPF$X2ndAssistPlayers.xPer60+
#                        player20122014PPF$X1stAssistGoal.xPer60+
#                        player20122014PPF$X2ndAssistGoal.xPer60,
#                      weight=player20122014PPF$TOI.y,subset=(player20122014PPF$TOI.y>500 &player20122014PPF$Name!= "Mark.Stone")
#)

player20122014PPF500<-player20122014PPF[player20122014PPF$TOI.y>100,]
hist(player20122014PPF$TOI.y)

fitSQRTPer60F20122014<-lm(player20122014PPF500$GoalonIce20142015Per60~
                            sqrt(player20122014PPF500$NonAssistGoal.xPer60)+sqrt(player20122014PPF500$X1stAssist.xPer60)+sqrt(player20122014PPF500$X2ndAssisted1stAssist.xPer60)+
                            sqrt(player20122014PPF500$X2ndAssist.xPer60)+sqrt(player20122014PPF500$NonAssistPlayers.xPer60)+sqrt(player20122014PPF500$X1stAssistPlayers.xPer60)+
                            sqrt(player20122014PPF500$X2ndAssistPlayers.xPer60)+
                            sqrt(player20122014PPF500$X1stAssistGoal.xPer60)+
                            sqrt(player20122014PPF500$X2ndAssistGoal.xPer60),
                          weight=player20122014PPF500$TOI.y)

summary(fitSQRTPer60F20122014)

boxplot(sqrt(player20122014PPF500$NonAssistGoal.xPer60),sqrt(player20122014PPF500$X1stAssist.xPer60),sqrt(player20122014PPF500$X2ndAssisted1stAssist.xPer60),
        sqrt(player20122014PPF500$X2ndAssist.xPer60),sqrt(player20122014PPF500$NonAssistPlayers.xPer60),sqrt(player20122014PPF500$X1stAssistPlayers.xPer60),
        sqrt(player20122014PPF500$X2ndAssistPlayers.xPer60),
        sqrt(player20122014PPF500$X1stAssistGoal.xPer60),
        sqrt(player20122014PPF500$X2ndAssistGoal.xPer60))
hist(player20122014PPF500$GoalonIce20142015Per60)
summary(fitPer60F20122014)
plot(fitPer60F20122014)

#only defenders
player20122014PPD$GoalonIce20142015<-(
  player20122014PPD$NonAssistGoal.y+player20122014PPD$X1stAssist.y+player20122014PPD$X2ndAssisted1stAssist.y+
    player20122014PPD$X2ndAssist.y+player20122014PPD$NonAssistPlayers.y+player20122014PPD$X1stAssistPlayers.y+
    player20122014PPD$X2ndAssistPlayers.y+
    player20122014PPD$X1stAssistGoal.y+
    player20122014PPD$X2ndAssistGoal.y)


fitMeanTimeD20122014<-lm(player20122014PPD$GoalonIce20142015~
                           player20122014PPD$NonAssistGoal.x+player20122014PPD$X1stAssist.x+player20122014PPD$X2ndAssisted1stAssist.x+
                           player20122014PPD$X2ndAssist.x+player20122014PPD$NonAssistPlayers.x+player20122014PPD$X1stAssistPlayers.x+
                           player20122014PPD$X2ndAssistPlayers.x+
                           player20122014PPD$X1stAssistGoal.x+
                           player20122014PPD$X2ndAssistGoal.x,
                         weight=(player20122014PPD$TOI..x+player20122014PPD$TOI..y)/2
)
summary(fitMeanTimeD20122014)

fitOnlyYD20122014<-lm(player20122014PPD$GoalonIce20142015~
                        player20122014PPD$NonAssistGoal.x+player20122014PPD$X1stAssist.x+player20122014PPD$X2ndAssisted1stAssist.x+
                        player20122014PPD$X2ndAssist.x+player20122014PPD$NonAssistPlayers.x+player20122014PPD$X1stAssistPlayers.x+
                        player20122014PPD$X2ndAssistPlayers.x+
                        player20122014PPD$X1stAssistGoal.x+
                        player20122014PPD$X2ndAssistGoal.x,
                      weight=player20122014PPD$TOI..y
)
summary(fitOnlyYD20122014)

player20122014PPD$NonAssistGoal.xPer60<-player20122014PPD$NonAssistGoal.x*60/player20122014PPD$TOI.x
player20122014PPD$X1stAssist.xPer60<-player20122014PPD$X1stAssist.x*60/player20122014PPD$TOI.x
player20122014PPD$X2ndAssisted1stAssist.xPer60<-player20122014PPD$X2ndAssisted1stAssist.x*60/player20122014PPD$TOI.x
#player20122014PPD$X2ndAssisted2ndAssist.xPer60<-player20122014PPD$X2ndAssisted2ndAssist.x/player20122014PPD$TOI..x
player20122014PPD$NonAssistPlayers.xPer60<-player20122014PPD$NonAssistPlayers.x*60/player20122014PPD$TOI.x
player20122014PPD$X1stAssistPlayers.xPer60<-player20122014PPD$X1stAssistPlayers.x*60/player20122014PPD$TOI.x
player20122014PPD$X2ndAssistPlayers.xPer60<-player20122014PPD$X2ndAssistPlayers.x*60/player20122014PPD$TOI.x
player20122014PPD$X1stAssistGoal.xPer60<-player20122014PPD$X1stAssistGoal.x*60/player20122014PPD$TOI.x
player20122014PPD$X2ndAssistGoal.xPer60<-player20122014PPD$X2ndAssistGoal.x*60/player20122014PPD$TOI.x
player20122014PPD$X2ndAssist.xPer60<-player20122014PPD$X2ndAssist.x*60/player20122014PPD$TOI.x
player20122014PPD$GoalonIce20142015Per60<-player20122014PPD$GoalonIce20142015*60/player20122014PPD$TOI.y
fitPer60D20122014<-lm(player20122014PPD$GoalonIce20142015Per60~
                        player20122014PPD$NonAssistGoal.xPer60+player20122014PPD$X1stAssist.xPer60+player20122014PPD$X2ndAssisted1stAssist.xPer60+
                        player20122014PPD$X2ndAssist.xPer60+player20122014PPD$NonAssistPlayers.xPer60+player20122014PPD$X1stAssistPlayers.xPer60+
                        player20122014PPD$X2ndAssistPlayers.xPer60+
                        player20122014PPD$X1stAssistGoal.xPer60+
                        player20122014PPD$X2ndAssistGoal.xPer60,
                      weight=player20122014PPD$TOI.y,subset=(player20122014PPD$TOI.y>200)
)
summary(fitPer60D20122014)
summary(fitPer60F20122014)
summary(fitPer6020122014)
plot(fitPer60F20122014)

plot(player20122014PPF$GoalonIce20142015Per60~player20122014PPF$X2ndAssisted1stAssist.xPer60,subset=(player20122014PPF$TOI.y>20))
fit1<-lm(player20122014PPF$GoalonIce20142015Per60~player20122014PPF$X2ndAssisted1stAssist.xPer60,subset=(player20122014PPF$TOI.y>20))
summary(fit1)
