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
#Number.y  NonAssistGoal.y  X1stAssistGoal.y  X2ndAssistGoal.y  X1stAssist.y  
#X2ndAssisted1stAssist.y  X2ndAssist.y  NonAssistPlayers.y  X1stAssistPlayers.y X2ndAssistPlayers.y

#EntirePlayers
player20132015PP$GoalonIce20142015<-(
  player20132015PP$NonAssistGoal.y+player20132015PP$X1stAssist.y+player20132015PP$X2ndAssisted1stAssist.y+
    player20132015PP$X2ndAssist.y+player20132015PP$NonAssistPlayers.y+player20132015PP$X1stAssistPlayers.y+
    player20132015PP$X2ndAssistPlayers.y+
    player20132015PP$X1stAssistGoal.y+
    player20132015PP$X2ndAssistGoal.y)

fitMeanTime<-lm(player20132015PP$GoalonIce20142015~
                  player20132015PP$NonAssistGoal.x+player20132015PP$X1stAssist.x+player20132015PP$X2ndAssisted1stAssist.x+
                  player20132015PP$X2ndAssist.x+player20132015PP$NonAssistPlayers.x+player20132015PP$X1stAssistPlayers.x+
                  player20132015PP$X2ndAssistPlayers.x+
                  player20132015PP$X1stAssistGoal.x+
                  player20132015PP$X2ndAssistGoal.x,
                weight=(player20132015PP$TOI..x+player20132015PP$TOI..y)/2
)
summary(fitMeanTime)

fitOnlyY<-lm(player20132015PP$GoalonIce20142015~
               player20132015PP$NonAssistGoal.x+player20132015PP$X1stAssist.x+player20132015PP$X2ndAssisted1stAssist.x+
               player20132015PP$X2ndAssist.x+player20132015PP$NonAssistPlayers.x+player20132015PP$X1stAssistPlayers.x+
               player20132015PP$X2ndAssistPlayers.x+
               player20132015PP$X1stAssistGoal.x+
               player20132015PP$X2ndAssistGoal.x,
             weight=player20132015PP$TOI..y
)
summary(fitOnlyY)

player20132015PP$NonAssistGoal.xPer60<-player20132015PP$NonAssistGoal.x*60/player20132015PP$TOI.x
player20132015PP$X1stAssist.xPer60<-player20132015PP$X1stAssist.x*60/player20132015PP$TOI.x
player20132015PP$X2ndAssisted1stAssist.xPer60<-player20132015PP$X2ndAssisted1stAssist.x*60/player20132015PP$TOI.x
#player20132015PP$X2ndAssisted2ndAssist.xPer60<-player20132015PP$X2ndAssisted2ndAssist.x/player20132015PP$TOI..x
player20132015PP$NonAssistPlayers.xPer60<-player20132015PP$NonAssistPlayers.x*60/player20132015PP$TOI.x
player20132015PP$X1stAssistPlayers.xPer60<-player20132015PP$X1stAssistPlayers.x*60/player20132015PP$TOI.x
player20132015PP$X2ndAssistPlayers.xPer60<-player20132015PP$X2ndAssistPlayers.x*60/player20132015PP$TOI.x
player20132015PP$X1stAssistGoal.xPer60<-player20132015PP$X1stAssistGoal.x*60/player20132015PP$TOI.x
player20132015PP$X2ndAssistGoal.xPer60<-player20132015PP$X2ndAssistGoal.x*60/player20132015PP$TOI.x
player20132015PP$X2ndAssist.xPer60<-player20132015PP$X2ndAssist.x*60/player20132015PP$TOI.x
player20132015PP$GoalonIce20142015Per60<-player20132015PP$GoalonIce20142015*60/player20132015PP$TOI.y
fitPer60<-lm(player20132015PP$GoalonIce20142015Per60~
               player20132015PP$NonAssistGoal.xPer60+player20132015PP$X1stAssist.xPer60+player20132015PP$X2ndAssisted1stAssist.xPer60+
               player20132015PP$X2ndAssist.xPer60+player20132015PP$NonAssistPlayers.xPer60+player20132015PP$X1stAssistPlayers.xPer60+
               player20132015PP$X2ndAssistPlayers.xPer60+
               player20132015PP$X1stAssistGoal.xPer60+
               player20132015PP$X2ndAssistGoal.xPer60,
             weight=player20132015PP$TOI.y,subset=(player20132015PP$TOI.y>200)
)
summary(fitPer60)

#Only Forwards
player20132015PPF$GoalonIce20142015<-(
  player20132015PPF$NonAssistGoal.y+player20132015PPF$X1stAssist.y+player20132015PPF$X2ndAssisted1stAssist.y+
    player20132015PPF$X2ndAssist.y+player20132015PPF$NonAssistPlayers.y+player20132015PPF$X1stAssistPlayers.y+
    player20132015PPF$X2ndAssistPlayers.y+
    player20132015PPF$X1stAssistGoal.y+
    player20132015PPF$X2ndAssistGoal.y)


fitMeanTimeF<-lm(player20132015PPF$GoalonIce20142015~
                   player20132015PPF$NonAssistGoal.x+player20132015PPF$X1stAssist.x+player20132015PPF$X2ndAssisted1stAssist.x+
                   player20132015PPF$X2ndAssist.x+player20132015PPF$NonAssistPlayers.x+player20132015PPF$X1stAssistPlayers.x+
                   player20132015PPF$X2ndAssistPlayers.x+
                   player20132015PPF$X1stAssistGoal.x+
                   player20132015PPF$X2ndAssistGoal.x,
                 weight=(player20132015PPF$TOI..x+player20132015PPF$TOI..y)/2
)
summary(fitMeanTimeF)
summary(fitMeanTime)
fitOnlyYF<-lm(player20132015PPF$GoalonIce20142015~
                player20132015PPF$NonAssistGoal.x+player20132015PPF$X1stAssist.x+player20132015PPF$X2ndAssisted1stAssist.x+
                player20132015PPF$X2ndAssist.x+player20132015PPF$NonAssistPlayers.x+player20132015PPF$X1stAssistPlayers.x+
                player20132015PPF$X2ndAssistPlayers.x+
                player20132015PPF$X1stAssistGoal.x+
                player20132015PPF$X2ndAssistGoal.x,
              weight=player20132015PPF$TOI..y
)
summary(fitOnlyYF)

player20132015PPF$NonAssistGoal.xPer60<-player20132015PPF$NonAssistGoal.x*60/player20132015PPF$TOI.x
player20132015PPF$X1stAssist.xPer60<-player20132015PPF$X1stAssist.x*60/player20132015PPF$TOI.x
player20132015PPF$X2ndAssisted1stAssist.xPer60<-player20132015PPF$X2ndAssisted1stAssist.x*60/player20132015PPF$TOI.x
#player20132015PPF$X2ndAssisted2ndAssist.xPer60<-player20132015PPF$X2ndAssisted2ndAssist.x/player20132015PPF$TOI..x
player20132015PPF$NonAssistPlayers.xPer60<-player20132015PPF$NonAssistPlayers.x*60/player20132015PPF$TOI.x
player20132015PPF$X1stAssistPlayers.xPer60<-player20132015PPF$X1stAssistPlayers.x*60/player20132015PPF$TOI.x
player20132015PPF$X2ndAssistPlayers.xPer60<-player20132015PPF$X2ndAssistPlayers.x*60/player20132015PPF$TOI.x
player20132015PPF$X1stAssistGoal.xPer60<-player20132015PPF$X1stAssistGoal.x*60/player20132015PPF$TOI.x
player20132015PPF$X2ndAssistGoal.xPer60<-player20132015PPF$X2ndAssistGoal.x*60/player20132015PPF$TOI.x
player20132015PPF$X2ndAssist.xPer60<-player20132015PPF$X2ndAssist.x*60/player20132015PPF$TOI.x
player20132015PPF$GoalonIce20142015Per60<-player20132015PPF$GoalonIce20142015*60/player20132015PPF$TOI.y
player20132015PPF=player20132015PPF[player20132015PPF$Name!="Dustin.Penner",]
fitPer60F<-lm(player20132015PPF$GoalonIce20142015Per60~
                player20132015PPF$NonAssistGoal.xPer60+player20132015PPF$X1stAssist.xPer60+player20132015PPF$X2ndAssisted1stAssist.xPer60+
                player20132015PPF$X2ndAssist.xPer60+player20132015PPF$NonAssistPlayers.xPer60+player20132015PPF$X1stAssistPlayers.xPer60+
                player20132015PPF$X2ndAssistPlayers.xPer60+
                player20132015PPF$X1stAssistGoal.xPer60+
                player20132015PPF$X2ndAssistGoal.xPer60,
              weight=player20132015PPF$TOI.y,subset=(player20132015PPF$TOI.y>200 &player20132015PPF$Name!="Jesper.Fast")
)
summary(fitPer60F)
plot(fitPer60F)


summary(fitPer60)
fitPer60F$resid[fitPer60F$resid< (-2)]


player20132015PPF500<-player20132015PPF[player20132015PPF$TOI.y>500,]
fitSQRTPer60F20132015<-lm(player20132015PPF500$GoalonIce20142015Per60~
                            sqrt(player20132015PPF500$NonAssistGoal.xPer60)+sqrt(player20132015PPF500$X1stAssist.xPer60)+sqrt(player20132015PPF500$X2ndAssisted1stAssist.xPer60)+
                            sqrt(player20132015PPF500$X2ndAssist.xPer60)+sqrt(player20132015PPF500$NonAssistPlayers.xPer60)+sqrt(player20132015PPF500$X1stAssistPlayers.xPer60)+
                            sqrt(player20132015PPF500$X2ndAssistPlayers.xPer60)+
                            sqrt(player20132015PPF500$X1stAssistGoal.xPer60)+
                            sqrt(player20132015PPF500$X2ndAssistGoal.xPer60),
                          weight=player20132015PPF500$TOI.y)




summary(fitSQRTPer60F20132015)
#only defenders
player20132015PPD$GoalonIce20142015<-(
  player20132015PPD$NonAssistGoal.y+player20132015PPD$X1stAssist.y+player20132015PPD$X2ndAssisted1stAssist.y+
    player20132015PPD$X2ndAssist.y+player20132015PPD$NonAssistPlayers.y+player20132015PPD$X1stAssistPlayers.y+
    player20132015PPD$X2ndAssistPlayers.y+
    player20132015PPD$X1stAssistGoal.y+
    player20132015PPD$X2ndAssistGoal.y)


fitMeanTimeD<-lm(player20132015PPD$GoalonIce20142015~
                   player20132015PPD$NonAssistGoal.x+player20132015PPD$X1stAssist.x+player20132015PPD$X2ndAssisted1stAssist.x+
                   player20132015PPD$X2ndAssist.x+player20132015PPD$NonAssistPlayers.x+player20132015PPD$X1stAssistPlayers.x+
                   player20132015PPD$X2ndAssistPlayers.x+
                   player20132015PPD$X1stAssistGoal.x+
                   player20132015PPD$X2ndAssistGoal.x,
                 weight=(player20132015PPD$TOI..x+player20132015PPD$TOI..y)/2
)
summary(fitMeanTimeD)

fitOnlyYD<-lm(player20132015PPD$GoalonIce20142015~
                player20132015PPD$NonAssistGoal.x+player20132015PPD$X1stAssist.x+player20132015PPD$X2ndAssisted1stAssist.x+
                player20132015PPD$X2ndAssist.x+player20132015PPD$NonAssistPlayers.x+player20132015PPD$X1stAssistPlayers.x+
                player20132015PPD$X2ndAssistPlayers.x+
                player20132015PPD$X1stAssistGoal.x+
                player20132015PPD$X2ndAssistGoal.x,
              weight=player20132015PPD$TOI..y
)
summary(fitOnlyYD)

player20132015PPD$NonAssistGoal.xPer60<-player20132015PPD$NonAssistGoal.x*60/player20132015PPD$TOI.x
player20132015PPD$X1stAssist.xPer60<-player20132015PPD$X1stAssist.x*60/player20132015PPD$TOI.x
player20132015PPD$X2ndAssisted1stAssist.xPer60<-player20132015PPD$X2ndAssisted1stAssist.x*60/player20132015PPD$TOI.x
#player20132015PPD$X2ndAssisted2ndAssist.xPer60<-player20132015PPD$X2ndAssisted2ndAssist.x/player20132015PPD$TOI..x
player20132015PPD$NonAssistPlayers.xPer60<-player20132015PPD$NonAssistPlayers.x*60/player20132015PPD$TOI.x
player20132015PPD$X1stAssistPlayers.xPer60<-player20132015PPD$X1stAssistPlayers.x*60/player20132015PPD$TOI.x
player20132015PPD$X2ndAssistPlayers.xPer60<-player20132015PPD$X2ndAssistPlayers.x*60/player20132015PPD$TOI.x
player20132015PPD$X1stAssistGoal.xPer60<-player20132015PPD$X1stAssistGoal.x*60/player20132015PPD$TOI.x
player20132015PPD$X2ndAssistGoal.xPer60<-player20132015PPD$X2ndAssistGoal.x*60/player20132015PPD$TOI.x
player20132015PPD$X2ndAssist.xPer60<-player20132015PPD$X2ndAssist.x*60/player20132015PPD$TOI.x
player20132015PPD$GoalonIce20142015Per60<-player20132015PPD$GoalonIce20142015*60/player20132015PPD$TOI.y

fitPer60D<-lm(player20132015PPD$GoalonIce20142015Per60~
                player20132015PPD$NonAssistGoal.xPer60+player20132015PPD$X1stAssist.xPer60+player20132015PPD$X2ndAssisted1stAssist.xPer60+
                player20132015PPD$X2ndAssist.xPer60+player20132015PPD$NonAssistPlayers.xPer60+player20132015PPD$X1stAssistPlayers.xPer60+
                player20132015PPD$X2ndAssistPlayers.xPer60+
                player20132015PPD$X1stAssistGoal.xPer60+
                player20132015PPD$X2ndAssistGoal.xPer60,
              weight=player20132015PPD$TOI.y,subset=(player20132015PPD$TOI.y>200)
)
summary(fitPer60D)
