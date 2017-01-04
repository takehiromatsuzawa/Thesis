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
player20132014PPEV <- merge(player20132014PP,player20132014,by="Name")

player20142015PP <- merge(data20142015PP,pTOIPP20142015,by="Name")
newPlayer20142015PP<-player20142015PP

colnames(newPlayer20142015PP)[34] 
colnames(newPlayer20142015PP)[35] 
colnames(newPlayer20142015PP)[36] 


colnames(newPlayer20142015PP)[34]  <- "newTOI"
colnames(newPlayer20142015PP)[35]  <- "newTOIoff"
colnames(newPlayer20142015PP)[36]  <- "newTOI."



hist(pTOIPP20142015[pTOIPP20142015$TOI>100,])
pTOIPP20142015F=pTOIPP20142015[pTOIPP20142015$pos!='D',]
pTOIPP20142015D=pTOIPP20142015[pTOIPP20142015$pos=='D',]
hist(pTOIPP20142015F$TOI , main="Power Play Time (Forward)", xlab='Time On Ice (Forward)')
hist(pTOIPP20142015D$TOI , main="Power Play Time (Defence)", xlab='Time On Ice (Defence)')

pTOIEH20142015F=pTOIEH20142015[pTOIEH20142015$pos!='D',]
pTOIEH20142015D=pTOIEH20142015[pTOIEH20142015$pos=='D',]
hist(pTOIEH20142015F$TOI , main="Equal Handed Time (Forward)", xlab='Time On Ice (Forward)')
hist(pTOIEH20142015D$TOI , main="Equal Handed Time (Defence)", xlab='Time On Ice (Defence)')



pTOIPP20132014F=pTOIPP20132014[pTOIPP20132014$pos!='D',]
pTOIPP20132014D=pTOIPP20132014[pTOIPP20132014$pos=='D',]
hist(pTOIPP20132014F$TOI , main="13-14 Power Play Time (Forward)", xlab='Time On Ice (Forward)')
hist(pTOIPP20132014D$TOI , main="13-14 Power Play Time (Defence)", xlab='Time On Ice (Defence)')

pTOIEH20132014F=pTOIEH20132014[pTOIEH20132014$pos!='D',]
pTOIEH20132014D=pTOIEH20132014[pTOIEH20132014$pos=='D',]
hist(pTOIEH20132014F$TOI , main="13-14 Equal Handed Time (Forward)", xlab='Time On Ice (Forward)')
hist(pTOIEH20132014D$TOI , main="13-14 Equal Handed Time (Defence)", xlab='Time On Ice (Defence)')


pTOIPP20122013F=pTOIPP20122013[pTOIPP20122013$pos!='D',]
pTOIPP20122013D=pTOIPP20122013[pTOIPP20122013$pos=='D',]
hist(pTOIPP20122013F$TOI , main="12-13 Power Play Time (Forward)", xlab='Time On Ice (Forward)')
hist(pTOIPP20122013D$TOI , main="12-13 Power Play Time (Defence)", xlab='Time On Ice (Defence)')

pTOIEH20122013F=pTOIEH20122013[pTOIEH20122013$pos!='D',]
pTOIEH20122013D=pTOIEH20122013[pTOIEH20122013$pos=='D',]
hist(pTOIEH20122013F$TOI , main="12-13 Equal Handed Time (Forward)", xlab='Time On Ice (Forward)')
hist(pTOIEH20122013D$TOI , main="12-13 Equal Handed Time (Defence)", xlab='Time On Ice (Defence)')




player20132015PPEV <- merge(player20132014PPEV,newPlayer20142015PP,by="Name")
player20132015PPEVD=player20132015PPEV[player20132015PPEV$pos.x=='D',]
player20132015PPEVF=player20132015PPEV[player20132015PPEV$pos.x!='D',]


100*48/82
player20132015PPEVF100<-player20132015PPEVF[player20132015PPEVF$newTOI>50,]
player20132015PPEVF100<-player20132015PPEVF100[player20132015PPEVF100$TOI.x>50,]
player20132015PPEVF100<-player20132015PPEVF100[player20132015PPEVF100$TOI.y>29,]

#player20132015PPEVF100$newTOI
player20132015PPEVF100$GoalonIce20142015<-(
  player20132015PPEVF100$NonAssistGoal+player20132015PPEVF100$X1stAssist+player20132015PPEVF100$X2ndAssisted1stAssist+
    player20132015PPEVF100$X2ndAssist+player20132015PPEVF100$NonAssistPlayers+player20132015PPEVF100$X1stAssistPlayers+
    player20132015PPEVF100$X2ndAssistPlayers+
    player20132015PPEVF100$X1stAssistGoal+
    player20132015PPEVF100$X2ndAssistGoal)



player20132015PPEVF100$NonAssistGoal.xPer60<-player20132015PPEVF100$NonAssistGoal.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$X1stAssist.xPer60<-player20132015PPEVF100$X1stAssist.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$X2ndAssisted1stAssist.xPer60<-player20132015PPEVF100$X2ndAssisted1stAssist.x*60/player20132015PPEVF100$TOI.x
#player20132015PPEVF100$X2ndAssisted2ndAssist.xPer60<-player20132015PPEVF100$X2ndAssisted2ndAssist.x/player20132015PPEVF100$TOI..x
player20132015PPEVF100$NonAssistPlayers.xPer60<-player20132015PPEVF100$NonAssistPlayers.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$X1stAssistPlayers.xPer60<-player20132015PPEVF100$X1stAssistPlayers.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$X2ndAssistPlayers.xPer60<-player20132015PPEVF100$X2ndAssistPlayers.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$X1stAssistGoal.xPer60<-player20132015PPEVF100$X1stAssistGoal.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$X2ndAssistGoal.xPer60<-player20132015PPEVF100$X2ndAssistGoal.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$X2ndAssist.xPer60<-player20132015PPEVF100$X2ndAssist.x*60/player20132015PPEVF100$TOI.x
player20132015PPEVF100$GoalonIce20142015Per60<-player20132015PPEVF100$GoalonIce20142015*60/player20132015PPEVF100$newTOI
player20122014PPEVF100$GoalonIce20142015Per60<-player20122014PPEVF100$GoalonIce20142015*60/player20122014PPEVF100$newTOI
player20112013PPEVF100$GoalonIce20142015Per60<-player20112013PPEVF100$GoalonIce20142015*60/player20112013PPEVF100$newTOI

player20132015PPEVF100$NonAssistGoal.yPer60<-player20132015PPEVF100$NonAssistGoal.y*60/player20132015PPEVF100$TOI.y
player20132015PPEVF100$X1stAssist.yPer60<-player20132015PPEVF100$X1stAssist.y*60/player20132015PPEVF100$TOI.y
player20132015PPEVF100$X2ndAssisted1stAssist.yPer60<-player20132015PPEVF100$X2ndAssisted1stAssist.y*60/player20132015PPEVF100$TOI.y
#player20132015PPEVF100$X2ndAssisted2ndAssist.yPer60<-player20132015PPEVF100$X2ndAssisted2ndAssist.y/player20132015PPEVF100$TOI..y
player20132015PPEVF100$NonAssistPlayers.yPer60<-player20132015PPEVF100$NonAssistPlayers.y*60/player20132015PPEVF100$TOI.y
player20132015PPEVF100$X1stAssistPlayers.yPer60<-player20132015PPEVF100$X1stAssistPlayers.y*60/player20132015PPEVF100$TOI.y
player20132015PPEVF100$X2ndAssistPlayers.yPer60<-player20132015PPEVF100$X2ndAssistPlayers.y*60/player20132015PPEVF100$TOI.y
player20132015PPEVF100$X1stAssistGoal.yPer60<-player20132015PPEVF100$X1stAssistGoal.y*60/player20132015PPEVF100$TOI.y
player20132015PPEVF100$X2ndAssistGoal.yPer60<-player20132015PPEVF100$X2ndAssistGoal.y*60/player20132015PPEVF100$TOI.y
player20132015PPEVF100$X2ndAssist.yPer60<-player20132015PPEVF100$X2ndAssist.y*60/player20132015PPEVF100$TOI.y




fitSQRTPer60F20132015PPEV<-lm(player20132015PPEVF100$GoalonIce20142015Per60~
                                sqrt(player20132015PPEVF100$NonAssistGoal.xPer60)+sqrt(player20132015PPEVF100$X1stAssist.xPer60)+sqrt(player20132015PPEVF100$X2ndAssisted1stAssist.xPer60)+
                                sqrt(player20132015PPEVF100$X2ndAssist.xPer60)+sqrt(player20132015PPEVF100$NonAssistPlayers.xPer60)+sqrt(player20132015PPEVF100$X1stAssistPlayers.xPer60)+
                                sqrt(player20132015PPEVF100$X2ndAssistPlayers.xPer60)+
                                sqrt(player20132015PPEVF100$X1stAssistGoal.xPer60)+
                                sqrt(player20132015PPEVF100$X2ndAssistGoal.xPer60)+
                                sqrt(player20132015PPEVF100$NonAssistGoal.yPer60)+sqrt(player20132015PPEVF100$X1stAssist.yPer60)+sqrt(player20132015PPEVF100$X2ndAssisted1stAssist.yPer60)+
                                sqrt(player20132015PPEVF100$X2ndAssist.yPer60)+sqrt(player20132015PPEVF100$NonAssistPlayers.yPer60)+sqrt(player20132015PPEVF100$X1stAssistPlayers.yPer60)+
                                sqrt(player20132015PPEVF100$X2ndAssistPlayers.yPer60)+
                                sqrt(player20132015PPEVF100$X1stAssistGoal.yPer60)+
                                sqrt(player20132015PPEVF100$X2ndAssistGoal.yPer60),
                              weight=(player20132015PPEVF100$TOI.y)+(player20132015PPEVF100$TOI.x))

summary(fitSQRTPer60F20132015PPEV)
#player20132015PPEVF100
View(player20132015PPEVF100)
player20122015PPEVF100<-rbind(player20132015PPEVF100,player20122014PPEVF100)

player20112015PPEVF100<-rbind(player20122015PPEVF100,player20112013PPEVF100)
View(player20112015PPEVF100)
hist(player20112015PPEVF100$GoalonIce20142015Per60)
plot.new()

player20112015PPEVF100$GoalonIce20142015<-(
  player20112015PPEVF100$NonAssistGoal+player20112015PPEVF100$X1stAssist+player20112015PPEVF100$X2ndAssisted1stAssist+
    player20112015PPEVF100$X2ndAssist+player20112015PPEVF100$NonAssistPlayers+player20112015PPEVF100$X1stAssistPlayers+
    player20112015PPEVF100$X2ndAssistPlayers+
    player20112015PPEVF100$X1stAssistGoal+
    player20112015PPEVF100$X2ndAssistGoal)

model5PPEV=lm(player20112015PPEVF100$GoalonIce20142015Per60~
              sqrt(player20112015PPEVF100$NonAssistGoal.xPer60)+sqrt(player20112015PPEVF100$X1stAssist.xPer60)+sqrt(player20112015PPEVF100$X2ndAssisted1stAssist.xPer60)+
              sqrt(player20112015PPEVF100$X2ndAssist.xPer60)+sqrt(player20112015PPEVF100$NonAssistPlayers.xPer60)+sqrt(player20112015PPEVF100$X1stAssistPlayers.xPer60)+
              sqrt(player20112015PPEVF100$X2ndAssistPlayers.xPer60)+
              sqrt(player20112015PPEVF100$X1stAssistGoal.xPer60)+
              sqrt(player20112015PPEVF100$X2ndAssistGoal.xPer60)+
              sqrt(player20112015PPEVF100$NonAssistGoal.yPer60)+sqrt(player20112015PPEVF100$X1stAssist.yPer60)+sqrt(player20112015PPEVF100$X2ndAssisted1stAssist.yPer60)+
              sqrt(player20112015PPEVF100$X2ndAssist.yPer60)+sqrt(player20112015PPEVF100$NonAssistPlayers.yPer60)+sqrt(player20112015PPEVF100$X1stAssistPlayers.yPer60)+
              sqrt(player20112015PPEVF100$X2ndAssistPlayers.yPer60)+
              sqrt(player20112015PPEVF100$X1stAssistGoal.yPer60)+
              sqrt(player20112015PPEVF100$X2ndAssistGoal.yPer60),
            weight=(player20112015PPEVF100$newTOI))
summary(model5PPEV)

player20112015PPEVF100$GoalPredictWithout<-  predict(model5PPEV, player20112015PPEVF100)-0.726710*sqrt(player20112015PPEVF100$X2ndAssist.xPer60)
sortplayer20112015PPEVF100<-player20112015PPEVF100
sortplayer20112015PPEVF100<-sortplayer20112015PPEVF100[order(sortplayer20112015PPEVF100$GoalPredictWithout),] 
nrow(sortplayer20112015PPEVF100)
sortplayer20112015PPEVF100Top30 =sortplayer20112015PPEVF100[460:488,] 
plot(sortplayer20112015PPEVF100Top30$GoalonIce20142015Per60~sortplayer20112015PPEVF100Top30$X2ndAssist.xPer60,cex = .3,xlab="2ndAssistPlayers (PP)", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")
text(x=sortplayer20112015PPEVF100Top30$X2ndAssist.xPer60,y=sortplayer20112015PPEVF100Top30$GoalonIce20142015Per60,
     sortplayer20112015PPEVF100Top30$Name,cex=0.8)

player20132015PPEVD100<-player20132015PPEVD[player20132015PPEVD$newTOI>50,]
player20132015PPEVD100<-player20132015PPEVD100[player20132015PPEVD100$TOI.x>50,]
player20132015PPEVD100<-player20132015PPEVD100[player20132015PPEVD100$TOI.y>500,]

player20122014PPEVD100<-player20122014PPEVD[player20122014PPEVD$newTOI>50,]
player20122014PPEVD100<-player20122014PPEVD100[player20122014PPEVD100$TOI.x>29,]
player20122014PPEVD100<-player20122014PPEVD100[player20122014PPEVD100$TOI.y>292,]


player20112013PPEVD100<-player20112013PPEVD[player20112013PPEVD$newTOI>29,]
player20112013PPEVD100<-player20112013PPEVD100[player20112013PPEVD100$TOI.x>50,]
player20112013PPEVD100<-player20112013PPEVD100[player20112013PPEVD100$TOI.y>500,]

player20122015PPEVD100<-rbind(player20132015PPEVD100,player20122014PPEVD100)
player20112015PPEVD100<-rbind(player20122015PPEVD100,player20112013PPEVD100)

player20112015PPEVD100$GoalonIce20142015<-(
  player20112015PPEVD100$NonAssistGoal+player20112015PPEVD100$X1stAssist+player20112015PPEVD100$X2ndAssisted1stAssist+
    player20112015PPEVD100$X2ndAssist+player20112015PPEVD100$NonAssistPlayers+player20112015PPEVD100$X1stAssistPlayers+
    player20112015PPEVD100$X2ndAssistPlayers+
    player20112015PPEVD100$X1stAssistGoal+
    player20112015PPEVD100$X2ndAssistGoal)

player20112015PPEVD100$NonAssistGoal.xPer60<-player20112015PPEVD100$NonAssistGoal.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$X1stAssist.xPer60<-player20112015PPEVD100$X1stAssist.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$X2ndAssisted1stAssist.xPer60<-player20112015PPEVD100$X2ndAssisted1stAssist.x*60/player20112015PPEVD100$TOI.x
#player20112015PPEVD100$X2ndAssisted2ndAssist.xPer60<-player20112015PPEVD100$X2ndAssisted2ndAssist.x/player20112015PPEVD100$TOI..x
player20112015PPEVD100$NonAssistPlayers.xPer60<-player20112015PPEVD100$NonAssistPlayers.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$X1stAssistPlayers.xPer60<-player20112015PPEVD100$X1stAssistPlayers.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$X2ndAssistPlayers.xPer60<-player20112015PPEVD100$X2ndAssistPlayers.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$X1stAssistGoal.xPer60<-player20112015PPEVD100$X1stAssistGoal.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$X2ndAssistGoal.xPer60<-player20112015PPEVD100$X2ndAssistGoal.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$X2ndAssist.xPer60<-player20112015PPEVD100$X2ndAssist.x*60/player20112015PPEVD100$TOI.x
player20112015PPEVD100$GoalonIce20142015Per60<-player20112015PPEVD100$GoalonIce20142015*60/player20112015PPEVD100$newTOI


player20112015PPEVD100$NonAssistGoal.yPer60<-player20112015PPEVD100$NonAssistGoal.y*60/player20112015PPEVD100$TOI.y
player20112015PPEVD100$X1stAssist.yPer60<-player20112015PPEVD100$X1stAssist.y*60/player20112015PPEVD100$TOI.y
player20112015PPEVD100$X2ndAssisted1stAssist.yPer60<-player20112015PPEVD100$X2ndAssisted1stAssist.y*60/player20112015PPEVD100$TOI.y
#player20112015PPEVD100$X2ndAssisted2ndAssist.yPer60<-player20112015PPEVD100$X2ndAssisted2ndAssist.y/player20112015PPEVD100$TOI..y
player20112015PPEVD100$NonAssistPlayers.yPer60<-player20112015PPEVD100$NonAssistPlayers.y*60/player20112015PPEVD100$TOI.y
player20112015PPEVD100$X1stAssistPlayers.yPer60<-player20112015PPEVD100$X1stAssistPlayers.y*60/player20112015PPEVD100$TOI.y
player20112015PPEVD100$X2ndAssistPlayers.yPer60<-player20112015PPEVD100$X2ndAssistPlayers.y*60/player20112015PPEVD100$TOI.y
player20112015PPEVD100$X1stAssistGoal.yPer60<-player20112015PPEVD100$X1stAssistGoal.y*60/player20112015PPEVD100$TOI.y
player20112015PPEVD100$X2ndAssistGoal.yPer60<-player20112015PPEVD100$X2ndAssistGoal.y*60/player20112015PPEVD100$TOI.y
player20112015PPEVD100$X2ndAssist.yPer60<-player20112015PPEVD100$X2ndAssist.y*60/player20112015PPEVD100$TOI.y
hist(player20112015PPEVD100$GoalonIce20142015Per60)
par(mar = rep(2, 4))
model5PPEV=lm(player20112015PPEVD100$GoalonIce20142015Per60~
                sqrt(player20112015PPEVD100$NonAssistGoal.xPer60)+sqrt(player20112015PPEVD100$X1stAssist.xPer60)+sqrt(player20112015PPEVD100$X2ndAssisted1stAssist.xPer60)+
                sqrt(player20112015PPEVD100$X2ndAssist.xPer60)+sqrt(player20112015PPEVD100$NonAssistPlayers.xPer60)+sqrt(player20112015PPEVD100$X1stAssistPlayers.xPer60)+
                sqrt(player20112015PPEVD100$X2ndAssistPlayers.xPer60)+
                sqrt(player20112015PPEVD100$X1stAssistGoal.xPer60)+
                sqrt(player20112015PPEVD100$X2ndAssistGoal.xPer60)+
                sqrt(player20112015PPEVD100$NonAssistGoal.yPer60)+sqrt(player20112015PPEVD100$X1stAssist.yPer60)+sqrt(player20112015PPEVD100$X2ndAssisted1stAssist.yPer60)+
                sqrt(player20112015PPEVD100$X2ndAssist.yPer60)+sqrt(player20112015PPEVD100$NonAssistPlayers.yPer60)+sqrt(player20112015PPEVD100$X1stAssistPlayers.yPer60)+
                sqrt(player20112015PPEVD100$X2ndAssistPlayers.yPer60)+
                sqrt(player20112015PPEVD100$X1stAssistGoal.yPer60)+
                sqrt(player20112015PPEVD100$X2ndAssistGoal.yPer60),
              weight=(player20112015PPEVD100$TOI.y)+(player20112015PPEVD100$TOI.x))
summary(model5PPEV)
nrow(player20112015PPEVD100)




player20112015PPEVD100$GoalPredictWithout<-  predict(model77, player20112015PPEVD100) -1.4473 *sqrt(player20112015PPEVD100$X1stAssistGoal.xPer60) 
sortplayer20112015PPEVD100<-player20112015PPEVD100
sortplayer20112015PPEVD100<-sortplayer20112015PPEVD100[order(sortplayer20112015PPEVD100$GoalPredictWithout),] 
nrow(sortplayer20112015PPEVD100)
sortplayer20112015PPEVD100Top30 =sortplayer20112015PPEVD100[93:123,] 
plot(sortplayer20112015PPEVD100Top30$GoalonIce20142015Per60~sortplayer20112015PPEVD100Top30$X2ndAssistGoal.xPer60,cex = .3,xlab="1stAssistPlayers (PP)", ylab="Next Year's Goals",col="red")
#points(x, cex = .1, col = "dark red")
text(x=sortplayer20112015PPEVD100Top30$X2ndAssistGoal.xPer60,y=sortplayer20112015PPEVD100Top30$GoalonIce20142015Per60,
     sortplayer20112015PPEVD100Top30$Name,cex=0.8)
sortplayer20112015PPEVD100Top30










plot(model5PP)
summary(modelStepBothPPEV7)
summary(modelStepBothPPEV5)


modelStepBothPPEV5<-step(modelDropPPPEV5,scope=list(upper=~.^2,lower=~1),data=data,direction="both",,trace=0)
modelStepBothPPEV7<-step(modelDropPPPEV7,scope=list(upper=~.^2,lower=~1),data=data,direction="both",,trace=0)
modelDropPPPEV7=lm(player20112015PPEVD100$GoalonIce20142015Per60~
                    #sqrt(player20112015PPEVD100$NonAssistGoal.xPer60)+
                    # sqrt(player20112015PPEVD100$X1stAssist.xPer60)+
                    # sqrt(player20112015PPEVD100$X2ndAssisted1stAssist.xPer60)+
                    #  sqrt(player20112015PPEVD100$X2ndAssist.xPer60)+
                    #  sqrt(player20112015PPEVD100$NonAssistPlayers.xPer60)+
                    # sqrt(player20112015PPEVD100$X1stAssistPlayers.xPer60)+
                    #  sqrt(player20112015PPEVD100$X2ndAssistPlayers.xPer60)+
                    #     sqrt(player20112015PPEVD100$X1stAssistGoal.xPer60)+
                    sqrt(player20112015PPEVD100$X2ndAssistGoal.xPer60)+
                    # sqrt(player20112015PPEVD100$NonAssistGoal.yPer60)+
                    #  sqrt(player20112015PPEVD100$X1stAssist.yPer60)+
                    #   sqrt(player20112015PPEVD100$X2ndAssisted1stAssist.yPer60)+
                    #         sqrt(player20112015PPEVD100$X2ndAssist.yPer60)+
                    sqrt(player20112015PPEVD100$NonAssistPlayers.yPer60)+
                    sqrt(player20112015PPEVD100$X1stAssistPlayers.yPer60)
                  #sqrt(player20112015PPEVD100$X2ndAssistPlayers.yPer60)+
                  # sqrt(player20112015PPEVD100$X1stAssistGoal.yPer60)
                  #  sqrt(player20112015PPEVD100$X2ndAssistGoal.yPer60)
                  ,
                  weight=(player20112015PPEVD100$TOI.y)+(player20112015PPEVD100$TOI.x))

modelDropPPPEV5=lm(player20112015PPEVD100$GoalonIce20142015Per60~
                #sqrt(player20112015PPEVD100$NonAssistGoal.xPer60)+
                 # sqrt(player20112015PPEVD100$X1stAssist.xPer60)+
                 # sqrt(player20112015PPEVD100$X2ndAssisted1stAssist.xPer60)+
              #  sqrt(player20112015PPEVD100$X2ndAssist.xPer60)+
                #  sqrt(player20112015PPEVD100$NonAssistPlayers.xPer60)+
                 # sqrt(player20112015PPEVD100$X1stAssistPlayers.xPer60)+
              #  sqrt(player20112015PPEVD100$X2ndAssistPlayers.xPer60)+
           #     sqrt(player20112015PPEVD100$X1stAssistGoal.xPer60)+
               sqrt(player20112015PPEVD100$X2ndAssistGoal.xPer60)+
               # sqrt(player20112015PPEVD100$NonAssistGoal.yPer60)+
                #  sqrt(player20112015PPEVD100$X1stAssist.yPer60)+
               #   sqrt(player20112015PPEVD100$X2ndAssisted1stAssist.yPer60)+
       #         sqrt(player20112015PPEVD100$X2ndAssist.yPer60)+
                  sqrt(player20112015PPEVD100$NonAssistPlayers.yPer60)
                  #sqrt(player20112015PPEVD100$X1stAssistPlayers.yPer60)
                #sqrt(player20112015PPEVD100$X2ndAssistPlayers.yPer60)+
               # sqrt(player20112015PPEVD100$X1stAssistGoal.yPer60)
              #  sqrt(player20112015PPEVD100$X2ndAssistGoal.yPer60)
              ,
              weight=(player20112015PPEVD100$TOI.y)+(player20112015PPEVD100$TOI.x))
summary(modelDropPPPEV)
modelDropPPPEV=lm(player20112015PPEVF100$GoalonIce20142015Per60~
              sqrt(player20112015PPEVF100$X2ndAssist.xPer60)+sqrt(player20112015PPEVF100$NonAssistPlayers.xPer60)+sqrt(player20112015PPEVF100$X1stAssistPlayers.xPer60)+
              sqrt(player20112015PPEVF100$X2ndAssistPlayers.xPer60)+
              sqrt(player20112015PPEVF100$X1stAssistGoal.xPer60)+
              sqrt(player20112015PPEVF100$X2ndAssistGoal.xPer60)+
              sqrt(player20112015PPEVF100$X1stAssist.yPer60)+
                sqrt(player20112015PPEVF100$NonAssistPlayers.yPer60)+
                sqrt(player20112015PPEVF100$X1stAssistPlayers.yPer60)+
              sqrt(player20112015PPEVF100$X2ndAssistGoal.yPer60),
            weight=(player20112015PPEVF100$TOI.y)+(player20112015PPEVF100$TOI.x))


summary(modelDropPPPEV)

nrow(player20112015PPEVD100)


data=player20112015PPEVF100
set.seed(420); nsims=1000; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
#fitSQRTPer60F20112013<-lm(player20112013F500$GoalonIce20142015Per60~
#                           sqrt(player20112013F500$NonAssistGoal.xPer60)+sqrt(player20112013F500$X1stAssist.xPer60)+sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60)+
#                            sqrt(player20112013F500$X2ndAssist.xPer60)+sqrt(player20112013F500$NonAssistPlayers.xPer60)+sqrt(player20112013F500$X1stAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistPlayers.xPer60)+
#                            sqrt(player20112013F500$X1stAssistGoal.xPer60)+
#                            sqrt(player20112013F500$X2ndAssistGoal.xPer60),
#                          weight=player20112013F500$TOI.y)
modelStepBothPPEV<-step(modelDropPPPEV,scope=list(upper=~.^2,lower=~1),data=data,direction="both",,trace=0)
summary(modelStepBothPPEV)

modelPPEVTest<-lm(formula = GoalonIce20142015Per60 ~ sqrt(X2ndAssist.xPer60) + 
     sqrt(NonAssistPlayers.xPer60) + sqrt(X1stAssistPlayers.xPer60) + 
     sqrt(X2ndAssistPlayers.xPer60) + sqrt(X1stAssistGoal.xPer60) + 
     sqrt(X2ndAssistGoal.xPer60) + sqrt(X1stAssist.yPer60) + sqrt(X1stAssistPlayers.yPer60) + 
     sqrt(X2ndAssistGoal.yPer60) + sqrt(X1stAssistGoal.xPer60):sqrt(X2ndAssistGoal.xPer60) + 
     sqrt(X2ndAssistPlayers.xPer60):sqrt(X1stAssistPlayers.yPer60) + 
    # sqrt(X1stAssistGoal.xPer60):sqrt(X1stAssist.yPer60) + 
       sqrt(NonAssistPlayers.xPer60):sqrt(X2ndAssistPlayers.xPer60) + 
   
     sqrt(X1stAssistPlayers.xPer60):sqrt(X1stAssist.yPer60) + 
   
     
     sqrt(X2ndAssist.xPer60):sqrt(X1stAssist.yPer60) , 
   data =  data, weights = (TOI.y + TOI.x))
summary(modelPPEVTest)



player20112015PPEVF100$GoalPredict<-  predict(modelStepBothPPEV7, player20112015PPEVF100)
#player20112015PPEVF100$GoalPredictWithout<-  predict(fitSQRTPer60F20132015, player20112015PPEVF100) -0.8914 *sqrt(player20112015PPEVF100$X2ndAssisted1stAssist.xPer60)
predictplayer20112015PPEVF100<-player20112015PPEVF100[order(player20112015PPEVF100$GoalPredict),] 
nrow(predictplayer20112015PPEVF100)
View(predictplayer20112015PPEVF100)
predictsortPreGoalplayer20142015F500<-predictplayer20112015PPEVF100[predictplayer20112015PPEVF100$Year==20142015,]
nrow(predictsortPreGoalplayer20142015F500)

predictsortPreGoalplayer20142015F500Top30<-predictsortPreGoalplayer20142015F500[163:183,]

plot(predictsortPreGoalplayer20142015F500Top30$GoalPredict,cex = .4,main='Predicted Top30 Forwards (EH)',xlab="Ranking", ylab="Predicted Team Goals",col="red")
text(x=predictsortPreGoalplayer20142015F500Top30$ID,y=predictsortPreGoalplayer20142015F500Top30$GoalPredict,
     predictsortPreGoalplayer20142015F500Top30$Name,cex=0.7)
predictsortPreGoalplayer20142015F500Top30$ID<-seq.int(nrow(predictsortPreGoalplayer20142015F500Top30))
predictsortPreGoalplayer20142015F500Top30$ID

myvars <- c("Name", "GoalPredict")
newpredictsortPreGoalplayer20142015F500Top30 <- predictsortPreGoalplayer20142015F500Top30[myvars]
newpredictsortPreGoalplayer20142015F500Top30<-newpredictsortPreGoalplayer20142015F500Top30[newpredictsortPreGoalplayer20142015F500Top30$Name!='Andrej.Meszaros',]
newpredictsortPreGoalplayer20142015F500Top30<-newpredictsortPreGoalplayer20142015F500Top30[newpredictsortPreGoalplayer20142015F500Top30$Name!='Nick.Holden',]
View(newpredictsortPreGoalplayer20142015F500Top30)




modelDropPPPEV7=lm(GoalonIce20142015Per60~
                #sqrt(NonAssistGoal.xPer60)+
                  #sqrt(X1stAssist.xPer60)+
                  #sqrt(X2ndAssisted1stAssist.xPer60)+
                sqrt(X2ndAssist.xPer60)+
                  #sqrt(NonAssistPlayers.xPer60)+
                  #sqrt(X1stAssistPlayers.xPer60)+
                sqrt(X2ndAssistPlayers.xPer60)+
                #sqrt(X1stAssistGoal.xPer60)+
                sqrt(X2ndAssistGoal.xPer60)+
               # sqrt(NonAssistGoal.yPer60)+
                  #sqrt(X1stAssist.yPer60)+
                 # sqrt(X2ndAssisted1stAssist.yPer60)+
               # sqrt(X2ndAssist.yPer60)+
                  #sqrt(NonAssistPlayers.yPer60)+
                 # sqrt(X1stAssistPlayers.yPer60)+
                #sqrt(X2ndAssistPlayers.yPer60)+
                #sqrt(X1stAssistGoal.yPer60)+
                sqrt(X2ndAssistGoal.yPer60),
              weight=(TOI.y+TOI.x),data=data)
summary(modelDropPPPEV7)

modelDropPPPEV5=lm(GoalonIce20142015Per60~
                     #sqrt(NonAssistGoal.xPer60)+
                     #sqrt(X1stAssist.xPer60)+
                     #sqrt(X2ndAssisted1stAssist.xPer60)+
                     sqrt(X2ndAssist.xPer60)+
                     #sqrt(NonAssistPlayers.xPer60)+
                     #sqrt(X1stAssistPlayers.xPer60)+
                     sqrt(X2ndAssistPlayers.xPer60)+
                     #sqrt(X1stAssistGoal.xPer60)+
                     #sqrt(X2ndAssistGoal.xPer60)+
                     # sqrt(NonAssistGoal.yPer60)+
                     #sqrt(X1stAssist.yPer60)+
                     # sqrt(X2ndAssisted1stAssist.yPer60)+
                     # sqrt(X2ndAssist.yPer60)+
                     #sqrt(NonAssistPlayers.yPer60)+
                     # sqrt(X1stAssistPlayers.yPer60)+
                     #sqrt(X2ndAssistPlayers.yPer60)+
                     #sqrt(X1stAssistGoal.yPer60)+
                     sqrt(X2ndAssistGoal.yPer60),
                   weight=(TOI.y+TOI.x),data=data)
summary(modelDropPPPEV5)


data=player20112015PPEVD100

nrow(data)
set.seed(420); nsims=500; n=nrow(data); sse1=sse2=sse3=sse4=sse5=sse6=rep(NA,nsims)
model77<-lm(formula = GoalonIce20142015Per60 ~ 
     sqrt(X2ndAssistGoal.xPer60) + sqrt(NonAssistPlayers.yPer60), 
   data = data, weights = (TOI.y + TOI.x))
summary(model77)
modelStepBothPPEV5<-step(modelDropPPPEV5,scope=list(upper=~.^2,lower=~1),data=data,direction="both",,trace=0)
modelStepBothPPEV7<-step(modelDropPPPEV7,scope=list(upper=~.^2,lower=~1),data=data,direction="both",,trace=0)

summary(modelStepBothPPEV7)
summary(modelStepBothPPEV5)
for(i in 1:nsims){  
  reorder=sample(n) 
  train=data[reorder[1:160],]  
  test=data[reorder[30:n],] 

model5PPEV=lm(GoalonIce20142015Per60~
                sqrt(NonAssistGoal.xPer60)+sqrt(X1stAssist.xPer60)+sqrt(X2ndAssisted1stAssist.xPer60)+
                sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
                sqrt(X2ndAssistPlayers.xPer60)+
                sqrt(X1stAssistGoal.xPer60)+
                sqrt(X2ndAssistGoal.xPer60)+
                sqrt(NonAssistGoal.yPer60)+sqrt(X1stAssist.yPer60)+sqrt(X2ndAssisted1stAssist.yPer60)+
                sqrt(X2ndAssist.yPer60)+sqrt(NonAssistPlayers.yPer60)+sqrt(X1stAssistPlayers.yPer60)+
                sqrt(X2ndAssistPlayers.yPer60)+
                sqrt(X1stAssistGoal.yPer60)+
                sqrt(X2ndAssistGoal.yPer60),
              weight=(TOI.y+TOI.x),data=train)

#modelDropPPPEV=lm(GoalonIce20142015Per60~
 #                   sqrt(X2ndAssist.xPer60)+sqrt(NonAssistPlayers.xPer60)+sqrt(X1stAssistPlayers.xPer60)+
  #                  sqrt(X2ndAssistPlayers.xPer60)+
   #                 sqrt(X1stAssistGoal.xPer60)+
    #                sqrt(X2ndAssistGoal.xPer60)+
     #               sqrt(X1stAssist.yPer60)+
      #              sqrt(NonAssistPlayers.yPer60)+
       #             sqrt(X1stAssistPlayers.yPer60)+
        #            sqrt(X2ndAssistGoal.yPer60),
         #         weight=(TOI.y+TOI.x),data=train)
modelDropPPPEV5=lm(GoalonIce20142015Per60~
                     sqrt(X1stAssistPlayers.yPer60)+
                     sqrt(X2ndAssistGoal.xPer60)+
                     sqrt(NonAssistPlayers.yPer60),
                   weight=(TOI.y+TOI.x),data=train)
modelDropPPPEV7=lm(GoalonIce20142015Per60~
                     #sqrt(X1stAssistPlayers.yPer60)+
                     sqrt(X2ndAssistGoal.xPer60)+
                     sqrt(NonAssistPlayers.yPer60),
                   weight=(TOI.y+TOI.x),data=train)

sse1[i]=sum((test$GoalonIce20142015Per60-predict(model5PPEV,new=test))^2)
sse2[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBothPPEV5,new=test))^2)
sse3[i]=sum((test$GoalonIce20142015Per60-predict(modelStepBothPPEV7,new=test))^2)
sse4[i]=sum((test$GoalonIce20142015Per60-predict(modelDropPPPEV7,new=test))^2) 
sse5[i]=sum((test$GoalonIce20142015Per60-predict(modelDropPPPEV5,new=test))^2) 

}
#summary(modelStepBothPPEV7)
c(mean(sse1),mean(sse2),mean(sse3),mean(sse4),mean(sse5))/(n+300)

modelDropPPPEV5=lm(GoalonIce20142015Per60~
                     sqrt(X1stAssistPlayers.yPer60)+
                     sqrt(X2ndAssistGoal.xPer60)+
                     sqrt(NonAssistPlayers.yPer60),
                   weight=(TOI.y+TOI.x),data=data)
summary(modelDropPPPEV5)

summary(modelDropPPPEV)

hist(pTOIEH20142015D$TOI , main="Equal Handed Time (Defence)", xlab='Time On Ice (Defence)')
hist(pTOIEH20142015D$TOI , main="Equal Handed Time (Defence)", xlab='Time On Ice (Defence)')

hist(pTOIEH20142015F$TOI , main="Equal Handed Time (Defence)", xlab='Time On Ice (Defence)')

hist(sortPreGoalplayer20112015F500$NonAssistGoal.xPer60,main="Histogram for NonAssist Per60 (2011-2015)", xlab='NonAssist')
hist(sortPreGoalplayer20112015F500$X1stAssistGoal.xPer60,main="Histogram for 1stAssist Per60 (2011-2015)", xlab='1stAssist')
hist(sortPreGoalplayer20112015F500$X2ndAssistGoal.xPer60,main="Histogram for 2ndAssist Per60 (2011-2015)", xlab='2ndAssist')
hist(sortPreGoalplayer20112015F500$X1stAssist.xPer60,main="Histogram for 1stAssist Per60 (2011-2015)", xlab='1stAssist')
hist(sortPreGoalplayer20112015F500$X2ndAssisted1stAssist.xPer60,main="Histogram for 2ndAssisted1stAssist Per60 (2011-2015)", xlab='2ndAssisted1stAssist')
hist(sortPreGoalplayer20112015F500$X2ndAssist.xPer60,main="Histogram for 2ndAssist Per60 (2011-2015)", xlab='2ndAssist')

hist(sortPreGoalplayer20112015F500$NonAssistPlayers.xPer60,main="Histogram for NonAssistPlayers Per60 (2011-2015)", xlab='NonAssistPlayers')
hist(sortPreGoalplayer20112015F500$X1stAssistPlayers.xPer60,main="Histogram for 1stAssistPlayers Per60 (2011-2015)", xlab='1stAssistPlayers')
hist(sortPreGoalplayer20112015F500$X2ndAssistPlayers.xPer60,main="Histogram for 2ndAssistPlayers Per60 (2011-2015)", xlab='2ndAssistPlayers')

hist(sortPreGoalplayer20112015F500$GoalonIce20142015Per60,main="Histogram for Team Goals Per60 (2011-2015)", xlab='Team Goals')



summary(fitSQRTPer60F20112015)

player20112015PPEVD100$GoalPredict<-  predict(model77, player20112015PPEVD100)
#player20112015PPEVD100$GoalPredictWithout<-  predict(fitSQRTPer60D20132015, player20112015PPEVD100) -0.8914 *sqrt(player20112015PPEVD100$X2ndAssisted1stAssist.xPer60)
predictplayer20112015PPEVD100<-player20112015PPEVD100[order(player20112015PPEVD100$GoalPredict),] 
nrow(predictplayer20112015PPEVD100)
View(predictplayer20112015PPEVD100)
predictsortPreGoalplayer20142015D500<-predictplayer20112015PPEVD100[predictplayer20112015PPEVD100$Year==20142015,]
nrow(predictsortPreGoalplayer20142015D500)

predictsortPreGoalplayer20142015D500Top30<-predictsortPreGoalplayer20142015D500[48:63,]

plot(predictsortPreGoalplayer20142015D500Top30$GoalPredict,cex = .4,main='Predicted Top30 Dorwards (EH)',xlab="Ranking", ylab="Predicted Team Goals",col="red")
text(x=predictsortPreGoalplayer20142015D500Top30$ID,y=predictsortPreGoalplayer20142015D500Top30$GoalPredict,
     predictsortPreGoalplayer20142015D500Top30$Name,cex=0.7)
predictsortPreGoalplayer20142015D500Top30$ID<-seq.int(nrow(predictsortPreGoalplayer20142015D500Top30))
predictsortPreGoalplayer20142015D500Top30$ID

myvars <- c("Name", "GoalPredict")
newpredictsortPreGoalplayer20142015D500Top30 <- predictsortPreGoalplayer20142015D500Top30[myvars]
newpredictsortPreGoalplayer20142015D500Top30<-newpredictsortPreGoalplayer20142015D500Top30[newpredictsortPreGoalplayer20142015D500Top30$Name!='Andrej.Meszaros',]
newpredictsortPreGoalplayer20142015D500Top30<-newpredictsortPreGoalplayer20142015D500Top30[newpredictsortPreGoalplayer20142015D500Top30$Name!='Nick.Holden',]
View(newpredictsortPreGoalplayer20142015D500Top30)
