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
player20112012PPEV <- merge(player20112012PP,player20112012,by="Name")

player20122013PP <- merge(data20122013PP,pTOIPP20122013,by="Name")
newPlayer20122013PP<-player20122013PP

colnames(newPlayer20122013PP)[34] 
colnames(newPlayer20122013PP)[35] 
colnames(newPlayer20122013PP)[36] 


colnames(newPlayer20122013PP)[34]  <- "newTOI"
colnames(newPlayer20122013PP)[35]  <- "newTOIoff"
colnames(newPlayer20122013PP)[36]  <- "newTOI."




player20112013PPEV <- merge(player20112012PPEV,newPlayer20122013PP,by="Name")
player20112013PPEVD=player20112013PPEV[player20112013PPEV$pos.x=='D',]
player20112013PPEVF=player20112013PPEV[player20112013PPEV$pos.x!='D',]


100*48/82
player20112013PPEVF100<-player20112013PPEVF[player20112013PPEVF$newTOI>50,]
player20112013PPEVF100<-player20112013PPEVF100[player20112013PPEVF100$TOI.x>50,]
player20112013PPEVF100<-player20112013PPEVF100[player20112013PPEVF100$TOI.y>500,]

player20112013PPEVF100$GoalonIce20142015<-(
  player20112013PPEVF100$NonAssistGoal+player20112013PPEVF100$X1stAssist+player20112013PPEVF100$X2ndAssisted1stAssist+
    player20112013PPEVF100$X2ndAssist+player20112013PPEVF100$NonAssistPlayers+player20112013PPEVF100$X1stAssistPlayers+
    player20112013PPEVF100$X2ndAssistPlayers+
    player20112013PPEVF100$X1stAssistGoal+
    player20112013PPEVF100$X2ndAssistGoal)



player20112013PPEVF100$NonAssistGoal.xPer60<-player20112013PPEVF100$NonAssistGoal.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$X1stAssist.xPer60<-player20112013PPEVF100$X1stAssist.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$X2ndAssisted1stAssist.xPer60<-player20112013PPEVF100$X2ndAssisted1stAssist.x*60/player20112013PPEVF100$TOI.x
#player20112013PPEVF100$X2ndAssisted2ndAssist.xPer60<-player20112013PPEVF100$X2ndAssisted2ndAssist.x/player20112013PPEVF100$TOI..x
player20112013PPEVF100$NonAssistPlayers.xPer60<-player20112013PPEVF100$NonAssistPlayers.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$X1stAssistPlayers.xPer60<-player20112013PPEVF100$X1stAssistPlayers.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$X2ndAssistPlayers.xPer60<-player20112013PPEVF100$X2ndAssistPlayers.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$X1stAssistGoal.xPer60<-player20112013PPEVF100$X1stAssistGoal.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$X2ndAssistGoal.xPer60<-player20112013PPEVF100$X2ndAssistGoal.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$X2ndAssist.xPer60<-player20112013PPEVF100$X2ndAssist.x*60/player20112013PPEVF100$TOI.x
player20112013PPEVF100$GoalonIce20142015Per60<-player20112013PPEVF100$GoalonIce20142015*60/player20112013PPEVF100$newTOI


player20112013PPEVF100$NonAssistGoal.yPer60<-player20112013PPEVF100$NonAssistGoal.y*60/player20112013PPEVF100$TOI.y
player20112013PPEVF100$X1stAssist.yPer60<-player20112013PPEVF100$X1stAssist.y*60/player20112013PPEVF100$TOI.y
player20112013PPEVF100$X2ndAssisted1stAssist.yPer60<-player20112013PPEVF100$X2ndAssisted1stAssist.y*60/player20112013PPEVF100$TOI.y
#player20112013PPEVF100$X2ndAssisted2ndAssist.yPer60<-player20112013PPEVF100$X2ndAssisted2ndAssist.y/player20112013PPEVF100$TOI..y
player20112013PPEVF100$NonAssistPlayers.yPer60<-player20112013PPEVF100$NonAssistPlayers.y*60/player20112013PPEVF100$TOI.y
player20112013PPEVF100$X1stAssistPlayers.yPer60<-player20112013PPEVF100$X1stAssistPlayers.y*60/player20112013PPEVF100$TOI.y
player20112013PPEVF100$X2ndAssistPlayers.yPer60<-player20112013PPEVF100$X2ndAssistPlayers.y*60/player20112013PPEVF100$TOI.y
player20112013PPEVF100$X1stAssistGoal.yPer60<-player20112013PPEVF100$X1stAssistGoal.y*60/player20112013PPEVF100$TOI.y
player20112013PPEVF100$X2ndAssistGoal.yPer60<-player20112013PPEVF100$X2ndAssistGoal.y*60/player20112013PPEVF100$TOI.y
player20112013PPEVF100$X2ndAssist.yPer60<-player20112013PPEVF100$X2ndAssist.y*60/player20112013PPEVF100$TOI.y




fitSQRTPer60F20112013PPEV<-lm(player20112013PPEVF100$GoalonIce20142015Per60~
                                sqrt(player20112013PPEVF100$NonAssistGoal.xPer60)+sqrt(player20112013PPEVF100$X1stAssist.xPer60)+sqrt(player20112013PPEVF100$X2ndAssisted1stAssist.xPer60)+
                                sqrt(player20112013PPEVF100$X2ndAssist.xPer60)+sqrt(player20112013PPEVF100$NonAssistPlayers.xPer60)+sqrt(player20112013PPEVF100$X1stAssistPlayers.xPer60)+
                                sqrt(player20112013PPEVF100$X2ndAssistPlayers.xPer60)+
                                sqrt(player20112013PPEVF100$X1stAssistGoal.xPer60)+
                                sqrt(player20112013PPEVF100$X2ndAssistGoal.xPer60)+
                                sqrt(player20112013PPEVF100$NonAssistGoal.yPer60)+sqrt(player20112013PPEVF100$X1stAssist.yPer60)+sqrt(player20112013PPEVF100$X2ndAssisted1stAssist.yPer60)+
                                sqrt(player20112013PPEVF100$X2ndAssist.yPer60)+sqrt(player20112013PPEVF100$NonAssistPlayers.yPer60)+sqrt(player20112013PPEVF100$X1stAssistPlayers.yPer60)+
                                sqrt(player20112013PPEVF100$X2ndAssistPlayers.yPer60)+
                                sqrt(player20112013PPEVF100$X1stAssistGoal.yPer60)+
                                sqrt(player20112013PPEVF100$X2ndAssistGoal.yPer60),
                              weight=(player20112013PPEVF100$TOI.y)+(player20112013PPEVF100$TOI.x))

summary(fitSQRTPer60F20112013PPEV)

