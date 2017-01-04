fname=file.choose()
data20132014=read.csv(fname,header=T)

fname=file.choose()
data20142015=read.csv(fname,header=T)

total <- merge(data20142015,data20132014,by="Name")
total

x=merge20132015[merge20132015["Name"]!="0"]

correctedmerge20132015=merge20132015[merge20132015$Name!=0, ]
correctedmerge20132015A=correctedmerge20132015[correctedmerge20132015$Year.x==20132014, ]
correctedmerge20132015B=correctedmerge20132015A[correctedmerge20132015A$Number.y!=20142015, ]
correctedmerge20132015C=correctedmerge20132015B[correctedmerge20132015B$Year.x==20132014, ]
correctedmerge20132015D=correctedmerge20132015C[correctedmerge20132015C$Number.x!=20132014, ]


head(correctedmerge20132015B, n=10)
r=as.numeric(as.character(correctedmerge20132015D$NonAssistGoal.y))
p=as.numeric(as.character(correctedmerge20132015D$X2ndAssistGoal.y))
correctedmerge20132015D$GoalonIce<-(r+p+
                          correctedmerge20132015D$X1stAssistGoal.y+
                          correctedmerge20132015D$X1stAssist.y+correctedmerge20132015D$X2ndAssisted1stAssist.y+
                          correctedmerge20132015D$X2ndAssist.y+correctedmerge20132015D$NonAssistPlayers.y+
                        correctedmerge20132015D$X1stAssistPlayers.y+correctedmerge20132015D$X2ndAssistPlayers.y)

class(correctedmerge20132015B$NonAssistGoal.y)
class(correctedmerge20132015B$X2ndAssistGoal.y)
class(correctedmerge20132015B$X2ndAssistPlayers.y)
r=as.numeric(as.character(correctedmerge20132015B$NonAssistGoal.y))
r
p=as.numeric(as.character(correctedmerge20132015B$X2ndAssistGoal.y))
p
class(r)

class(correctedmerge20132015D$NonAssistGoal.x)
p2013NonAssistGoal=as.numeric(as.character(correctedmerge20132015D$NonAssistGoal.x))
class(correctedmerge20132015D$X2ndAssistGoal.x)
p20132ndAssistGoal=as.numeric(as.character(correctedmerge20132015D$X2ndAssistGoal.x))
class(correctedmerge20132015B$X1stAssistGoal.x)
class(correctedmerge20132015B$X2ndAssistPlayers.x)

p2013NonAssistGoal
p20132ndAssistGoal




fit<-lm(correctedmerge20132015D$GoalonIce~p2013NonAssistGoal+p20132ndAssistGoal+
          correctedmerge20132015D$X1stAssistGoal.x+
          correctedmerge20132015D$X1stAssist.x+correctedmerge20132015D$X2ndAssisted1stAssist.x+
          correctedmerge20132015D$X2ndAssist.x+correctedmerge20132015D$NonAssistPlayers.x+
          correctedmerge20132015D$X1stAssistPlayers.x+correctedmerge20132015D$X2ndAssistPlayers.x)
summary(fit)
fname=file.choose()
playerTOI20142015=read.csv(fname,header=T)
fname=file.choose()
playerTOI20132014=read.csv(fname,header=T)
playerTOI20142015
head(playerTOI20132014, n=10)
head(playerTOI20142015, n=10)
head(correctedmerge20132015D, n=10)
correctedmerge20132015D
player20142015 <- merge(correctedmerge20132015D,playerTOI20142015,by="Name")
head(playerTOI20132015, n=10)
playerTOI20132015=playerTOI20132015[playerTOI20132015$Year.x==20132014, ]
head(playerTOI20132015, n=10)

NROW(playerTOI20132015)
NROW(correctedmerge20132015D)
correctedmerge20132015E<- merge(correctedmerge20132015D,playerTOI20132015,by="Name")
NROW(correctedmerge20132015E)









player20132014 <- merge(data20132014,playerTOI20132014,by="Name")
head(player20132014, n=10)

player20132014=player20132014[player20132014$Year.x==20132014, ]
player20132014=player20132014[player20132014$Number.y!=20132014, ]
summedplayerTOI20132014 <-ddply(playerTOI20132014, "Name", numcolwise(sum))
summedplayerTOI20132014=summedplayerTOI20132014[summedplayerTOI20132014$Year==20132014, ]
summedplayerTOI20132014=summedplayerTOI20132014[summedplayerTOI20132014$Number!=20132014, ]

# summedplayerTOI20142015 <-ddply(playerTOI20142015, "Name", numcolwise(sum))
summedplayerTOI20142015=summedplayerTOI20142015[summedplayerTOI20142015$Year==20142015, ]
summedplayerTOI20142015=summedplayerTOI20142015[summedplayerTOI20142015$Number!=20142015, ]

summeddata20142015 <-ddply(data20142015, "Name", numcolwise(sum))
summeddata20142015=summeddata20142015[summeddata20142015$Year==20142015, ]
summeddata20142015=summeddata20142015[summeddata20142015$Number!=20142015, ]

correcteddata20132014=data20132014[data20132014$Year==20132014, ]
correcteddata20132014=correcteddata20132014[correcteddata20132014$Number!=20132014, ]
correcteddata20132014$CorrectedNonAssistGoal<-as.numeric(as.character(correcteddata20132014$NonAssistGoal))
correcteddata20132014$CorrectedX2ndAssistGoal<-as.numeric(as.character(correcteddata20132014$X2ndAssistGoal))
summeddata20132014 <-ddply(correcteddata20132014, "Name", numcolwise(sum))

correcteddata20142015=data20142015[data20142015$Year==20142015, ]
correcteddata20142015=correcteddata20142015[correcteddata20142015$Number!=20142015, ]
correcteddata20142015$CorrectedNonAssistGoal<-as.numeric(as.character(correcteddata20142015$NonAssistGoal))
correcteddata20142015$CorrectedX2ndAssistGoal<-as.numeric(as.character(correcteddata20142015$X2ndAssistGoal))
summeddata20142015 <-ddply(correcteddata20142015, "Name", numcolwise(sum))

totalsummeddata20132014<- merge(correcteddata20132014,playerTOI20132014,by="Name")
totalsummeddata20142015<- merge(correcteddata20142015,playerTOI20142015,by="Name")


totalsummeddata20132014<- merge(summeddata20132014,playerTOI20132014,by="Name")
totalsummeddata20142015<- merge(summeddata20142015,playerTOI20142015,by="Name")

totalsummeddata20132015<- merge(totalsummeddata20132014,totalsummeddata20142015,by="Name")
totalsummeddata20132015$GoalonIce20142015<-(
  totalsummeddata20132015$X1stAssistGoal.y+totalsummeddata20132015$X1stAssist.y+totalsummeddata20132015$X2ndAssisted1stAssist.y+
    totalsummeddata20132015$X2ndAssist.y+totalsummeddata20132015$NonAssistPlayers.y+totalsummeddata20132015$X1stAssistPlayers.y+
    totalsummeddata20132015$X2ndAssistPlayers.y+
    totalsummeddata20132015$CorrectedNonAssistGoal.y+
    totalsummeddata20132015$CorrectedX2ndAssistGoal.y)

fit<-lm(totalsummeddata20132015$GoalonIce~
          totalsummeddata20132015$X1stAssistGoal.x+totalsummeddata20132015$X1stAssist.x+totalsummeddata20132015$X2ndAssisted1stAssist.x+
          totalsummeddata20132015$X2ndAssist.x+totalsummeddata20132015$NonAssistPlayers.x+totalsummeddata20132015$X1stAssistPlayers.x+
          totalsummeddata20132015$X2ndAssistPlayers.x+
          totalsummeddata20132015$CorrectedNonAssistGoal.x+
          totalsummeddata20132015$CorrectedX2ndAssistGoal.x
          )
summary(fit)
#totalsummeddata20132015$GoalonIceNumericTOI.y<-strtoi(totalsummeddata20132015$GoalonIceNumericTOI.y)
totalsummeddata20132015$NumericTOIPG.y<-as.numeric(as.character(totalsummeddata20132015$TOIPG.y))
totalsummeddata20132015$NumericGames.y<-as.numeric(as.character(totalsummeddata20132015$Games.y))
totalsummeddata20132015$WeightY=totalsummeddata20132015$NumericGames.y*totalsummeddata20132015$NumericTOIPG.y



#totalsummeddata20132015$GoalonIceNumericTOI.x<-strtoi(totalsummeddata20132015$GoalonIceNumericTOI.x)
totalsummeddata20132015$NumericTOIPG.x<-as.numeric(as.character(totalsummeddata20132015$TOIPG.x))
totalsummeddata20132015$NumericGames.x<-as.numeric(as.character(totalsummeddata20132015$Games.x))
totalsummeddata20132015$WeightX=totalsummeddata20132015$NumericGames.x*totalsummeddata20132015$NumericTOIPG.x

fit<-lm(totalsummeddata20132015$GoalonIce~
          totalsummeddata20132015$X1stAssistGoal.x+totalsummeddata20132015$X1stAssist.x+totalsummeddata20132015$X2ndAssisted1stAssist.x+
          totalsummeddata20132015$X2ndAssist.x+totalsummeddata20132015$NonAssistPlayers.x+totalsummeddata20132015$X1stAssistPlayers.x+
          totalsummeddata20132015$X2ndAssistPlayers.x+
          totalsummeddata20132015$CorrectedNonAssistGoal.x+
          totalsummeddata20132015$CorrectedX2ndAssistGoal.x,
        weight=totalsummeddata20132015$WeightY
)
summary(fit)


fit2<-lm(totalsummeddata20132015$GoalonIce~
          totalsummeddata20132015$X1stAssistGoal.x+totalsummeddata20132015$X1stAssist.x+totalsummeddata20132015$X2ndAssisted1stAssist.x+
          totalsummeddata20132015$X2ndAssist.x+totalsummeddata20132015$NonAssistPlayers.x+totalsummeddata20132015$X1stAssistPlayers.x+
          totalsummeddata20132015$X2ndAssistPlayers.x+
          totalsummeddata20132015$CorrectedNonAssistGoal.x+
          totalsummeddata20132015$CorrectedX2ndAssistGoal.x,
        weight=(totalsummeddata20132015$WeightX+totalsummeddata20132015$WeightY)/2
)
summary(fit2)

totalsummeddata20132015$X1stAssistGoalPer60.x<-totalsummeddata20132015$X1stAssistGoal.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$X1stAssistPer60.x<-totalsummeddata20132015$X1stAssist.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$X2ndAssisted1stAssistPer60.x<-totalsummeddata20132015$X2ndAssisted1stAssist.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$X2ndAssistPer60.x<-totalsummeddata20132015$X2ndAssist.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$NonAssistPlayersPer60.x<-totalsummeddata20132015$NonAssistPlayers.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$X1stAssistPlayersPer60.x<-totalsummeddata20132015$X1stAssistPlayers.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$X2ndAssistPlayersPer60.x<-totalsummeddata20132015$X2ndAssistPlayers.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$CorrectedNonAssistGoalPer60.x<-totalsummeddata20132015$CorrectedNonAssistGoal.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$CorrectedX2ndAssistGoalPer60.x<-totalsummeddata20132015$CorrectedX2ndAssistGoal.x*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$GoalonIcePer60<-totalsummeddata20132015$GoalonIce*60/totalsummeddata20132015$WeightX
totalsummeddata20132015$sqrtGoalonIcePer60<-sqrt(totalsummeddata20132015$GoalonIcePer60)
fit2<-lm(totalsummeddata20132015$sqrtGoalonIcePer60~
           totalsummeddata20132015$X1stAssistGoalPer60.x+totalsummeddata20132015$X1stAssistPer60.x+totalsummeddata20132015$X2ndAssisted1stAssistPer60.x+
           totalsummeddata20132015$X2ndAssistPer60.x+totalsummeddata20132015$NonAssistPlayersPer60.x+totalsummeddata20132015$X1stAssistPlayersPer60.x+
           totalsummeddata20132015$X2ndAssistPlayersPer60.x+
           totalsummeddata20132015$CorrectedNonAssistGoalPer60.x+
           totalsummeddata20132015$CorrectedX2ndAssistGoalPer60.x,
         weight=(totalsummeddata20132015$WeightX+totalsummeddata20132015$WeightY)/2
)
summary(fit2)

adjustedTotalsummeddata20132015<-totalsummeddata20132015[totalsummeddata20132015$WeightX>150,]
adjustedTotalsummeddata20132015<-adjustedTotalsummeddata20132015[adjustedTotalsummeddata20132015$WeightY>150,]
fit3<-lm(adjustedTotalsummeddata20132015$sqrtGoalonIcePer60~
           adjustedTotalsummeddata20132015$X1stAssistGoalPer60.x+adjustedTotalsummeddata20132015$X1stAssistPer60.x+adjustedTotalsummeddata20132015$X2ndAssisted1stAssistPer60.x+
           adjustedTotalsummeddata20132015$X2ndAssistPer60.x+adjustedTotalsummeddata20132015$NonAssistPlayersPer60.x+adjustedTotalsummeddata20132015$X1stAssistPlayersPer60.x+
           adjustedTotalsummeddata20132015$X2ndAssistPlayersPer60.x+
           adjustedTotalsummeddata20132015$CorrectedNonAssistGoalPer60.x+
           adjustedTotalsummeddata20132015$CorrectedX2ndAssistGoalPer60.x,
         weight=(adjustedTotalsummeddata20132015$WeightX+adjustedTotalsummeddata20132015$WeightY)/2
)
summary(fit3)

#correctedmerge20132015D=correctedmerge20132015C[correctedmerge20132015C$Number.x!=20132014, ]


#fit<-lm(totalsummeddata20132015$GoalonIce~p2013NonAssistGoal+p20132ndAssistGoal+
 #         correctedmerge20132015D$X1stAssistGoal.x+
#          correctedmerge20132015D$X1stAssist.x+correctedmerge20132015D$X2ndAssisted1stAssist.x+
 #         correctedmerge20132015D$X2ndAssist.x+correctedmerge20132015D$NonAssistPlayers.x+
  #        correctedmerge20132015D$X1stAssistPlayers.x+correctedmerge20132015D$X2ndAssistPlayers.x)
#player20142015 <- merge(data20142015,playerTOI20142015,by="Name")
#head(player20142015, n=10)

#player20142015=player20142015[player20142015$Year.x==20142015, ]
#player20142015=player20142015[player20142015$Number.y!=20142015, ]
#player20132015<- merge(player20132014,player20142015,by="Name")
#head(player20132015, n=10)

#NROW(player20132015)
#NROW(correctedmerge20132015D)
#correctedmerge20132015E<- merge(correctedmerge20132015D,playerTOI20132015,by="Name")
#NROW(correctedmerge20132015E)
#totalsummeddata20132015

#r=as.numeric(as.character(totalsummeddata20132015$NonAssistGoal.y))
#p=as.numeric(as.character(totalsummeddata20132015$X2ndAssistGoal.y))
#correctedmerge20132015D$GoalonIce<-(r+p+
 #                                     correctedmerge20132015D$X1stAssistGoal.y+
  #                                    correctedmerge20132015D$X1stAssist.y+correctedmerge20132015D$X2ndAssisted1stAssist.y+
   #                                   correctedmerge20132015D$X2ndAssist.y+correctedmerge20132015D$NonAssistPlayers.y+
  ##                                    correctedmerge20132015D$X1stAssistPlayers.y+correctedmerge20132015D$X2ndAssistPlayers.y)

#fit<-lm(correctedmerge20132015D$GoalonIce~p2013NonAssistGoal+p20132ndAssistGoal+
 #         correctedmerge20132015D$X1stAssistGoal.x+
  #        correctedmerge20132015D$X1stAssist.x+correctedmerge20132015D$X2ndAssisted1stAssist.x+
  #        correctedmerge20132015D$X2ndAssist.x+correctedmerge20132015D$NonAssistPlayers.x+
  #        correctedmerge20132015D$X1stAssistPlayers.x+correctedmerge20132015D$X2ndAssistPlayers.x)
fname=file.choose()
data20132014=read.csv(fname,header=T)

fname=file.choose()
data20142015=read.csv(fname,header=T)

fname=file.choose()
pTOIEH20132014=read.csv(fname,header=T)

fname=file.choose()
pTOIEH20142015=read.csv(fname,header=T)

data20132014EH=data20132014
data20142015EH=data20142015
data20132014EH$Name<-str_replace_all(data20132014EH$Name," ",".")
data20142015EH$Name<-str_replace_all(data20142015EH$Name," ",".")

data20132014EH <-ddply(data20132014EH, "Name", numcolwise(sum))
data20142015EH <-ddply(data20142015EH, "Name", numcolwise(sum))
player20132014 <- merge(data20132014EH,pTOIEH20132014,by="Name")
head(pTOIEH20132014, n=10)
head(data20132014EH, n=10)
player20142015 <- merge(data20142015EH,pTOIEH20142015,by="Name")
head(pTOIEH20142015, n=10)
head(data20142015EH, n=10)

head(player20132014, n=10)
head(player20142015, n=10)

player20132015 <- merge(player20132014,player20142015,by="Name")
player20132015
#player20132014=player20132014[player20132014$Year.x==20132014, ]
player20132015D=player20132015[player20132015$pos.x=='D',]
player20132015F=player20132015[player20132015$pos.x!='D',]
#Number.y  NonAssistGoal.y	X1stAssistGoal.y	X2ndAssistGoal.y	X1stAssist.y	
#X2ndAssisted1stAssist.y	X2ndAssist.y	NonAssistPlayers.y	X1stAssistPlayers.y	X2ndAssistPlayers.y

#EntirePlayers
player20132015$GoalonIce20142015<-(
  player20132015$NonAssistGoal.y+player20132015$X1stAssist.y+player20132015$X2ndAssisted1stAssist.y+
    player20132015$X2ndAssist.y+player20132015$NonAssistPlayers.y+player20132015$X1stAssistPlayers.y+
    player20132015$X2ndAssistPlayers.y+
    player20132015$X1stAssistGoal.y+
    player20132015$X2ndAssistGoal.y)

fitMeanTime<-lm(player20132015$GoalonIce20142015~
           player20132015$NonAssistGoal.x+player20132015$X1stAssist.x+player20132015$X2ndAssisted1stAssist.x+
           player20132015$X2ndAssist.x+player20132015$NonAssistPlayers.x+player20132015$X1stAssistPlayers.x+
           player20132015$X2ndAssistPlayers.x+
           player20132015$X1stAssistGoal.x+
           player20132015$X2ndAssistGoal.x,
         weight=(player20132015$TOI..x+player20132015$TOI..y)/2
)
summary(fitMeanTime)

fitOnlyY<-lm(player20132015$GoalonIce20142015~
           player20132015$NonAssistGoal.x+player20132015$X1stAssist.x+player20132015$X2ndAssisted1stAssist.x+
           player20132015$X2ndAssist.x+player20132015$NonAssistPlayers.x+player20132015$X1stAssistPlayers.x+
           player20132015$X2ndAssistPlayers.x+
           player20132015$X1stAssistGoal.x+
           player20132015$X2ndAssistGoal.x,
         weight=player20132015$TOI..y
)
summary(fitOnlyY)

player20132015$NonAssistGoal.xPer60<-player20132015$NonAssistGoal.x*60/player20132015$TOI.x
player20132015$X1stAssist.xPer60<-player20132015$X1stAssist.x*60/player20132015$TOI.x
player20132015$X2ndAssisted1stAssist.xPer60<-player20132015$X2ndAssisted1stAssist.x*60/player20132015$TOI.x
#player20132015$X2ndAssisted2ndAssist.xPer60<-player20132015$X2ndAssisted2ndAssist.x/player20132015$TOI..x
player20132015$NonAssistPlayers.xPer60<-player20132015$NonAssistPlayers.x*60/player20132015$TOI.x
player20132015$X1stAssistPlayers.xPer60<-player20132015$X1stAssistPlayers.x*60/player20132015$TOI.x
player20132015$X2ndAssistPlayers.xPer60<-player20132015$X2ndAssistPlayers.x*60/player20132015$TOI.x
player20132015$X1stAssistGoal.xPer60<-player20132015$X1stAssistGoal.x*60/player20132015$TOI.x
player20132015$X2ndAssistGoal.xPer60<-player20132015$X2ndAssistGoal.x*60/player20132015$TOI.x
player20132015$X2ndAssist.xPer60<-player20132015$X2ndAssist.x*60/player20132015$TOI.x
player20132015$GoalonIce20142015Per60<-player20132015$GoalonIce20142015*60/player20132015$TOI.y
fitPer60<-lm(player20132015$GoalonIce20142015Per60~
               player20132015$NonAssistGoal.xPer60+player20132015$X1stAssist.xPer60+player20132015$X2ndAssisted1stAssist.xPer60+
               player20132015$X2ndAssist.xPer60+player20132015$NonAssistPlayers.xPer60+player20132015$X1stAssistPlayers.xPer60+
               player20132015$X2ndAssistPlayers.xPer60+
               player20132015$X1stAssistGoal.xPer60+
               player20132015$X2ndAssistGoal.xPer60,
             weight=player20132015$TOI.y,subset=(player20132015$TOI.y>200)
)
summary(fitPer60)

#Only Forwards
player20132015F$GoalonIce20142015<-(
  player20132015F$NonAssistGoal.y+player20132015F$X1stAssist.y+player20132015F$X2ndAssisted1stAssist.y+
    player20132015F$X2ndAssist.y+player20132015F$NonAssistPlayers.y+player20132015F$X1stAssistPlayers.y+
    player20132015F$X2ndAssistPlayers.y+
    player20132015F$X1stAssistGoal.y+
    player20132015F$X2ndAssistGoal.y)


fitMeanTimeF<-lm(player20132015F$GoalonIce20142015~
                  player20132015F$NonAssistGoal.x+player20132015F$X1stAssist.x+player20132015F$X2ndAssisted1stAssist.x+
                  player20132015F$X2ndAssist.x+player20132015F$NonAssistPlayers.x+player20132015F$X1stAssistPlayers.x+
                  player20132015F$X2ndAssistPlayers.x+
                  player20132015F$X1stAssistGoal.x+
                  player20132015F$X2ndAssistGoal.x,
                weight=(player20132015F$TOI..x+player20132015F$TOI..y)/2
)
summary(fitMeanTimeF)
summary(fitMeanTime)
fitOnlyYF<-lm(player20132015F$GoalonIce20142015~
               player20132015F$NonAssistGoal.x+player20132015F$X1stAssist.x+player20132015F$X2ndAssisted1stAssist.x+
               player20132015F$X2ndAssist.x+player20132015F$NonAssistPlayers.x+player20132015F$X1stAssistPlayers.x+
               player20132015F$X2ndAssistPlayers.x+
               player20132015F$X1stAssistGoal.x+
               player20132015F$X2ndAssistGoal.x,
             weight=player20132015F$TOI..y
)
summary(fitOnlyYF)

player20132015F$NonAssistGoal.xPer60<-player20132015F$NonAssistGoal.x*60/player20132015F$TOI.x
player20132015F$X1stAssist.xPer60<-player20132015F$X1stAssist.x*60/player20132015F$TOI.x
player20132015F$X2ndAssisted1stAssist.xPer60<-player20132015F$X2ndAssisted1stAssist.x*60/player20132015F$TOI.x
#player20132015F$X2ndAssisted2ndAssist.xPer60<-player20132015F$X2ndAssisted2ndAssist.x/player20132015F$TOI..x
player20132015F$NonAssistPlayers.xPer60<-player20132015F$NonAssistPlayers.x*60/player20132015F$TOI.x
player20132015F$X1stAssistPlayers.xPer60<-player20132015F$X1stAssistPlayers.x*60/player20132015F$TOI.x
player20132015F$X2ndAssistPlayers.xPer60<-player20132015F$X2ndAssistPlayers.x*60/player20132015F$TOI.x
player20132015F$X1stAssistGoal.xPer60<-player20132015F$X1stAssistGoal.x*60/player20132015F$TOI.x
player20132015F$X2ndAssistGoal.xPer60<-player20132015F$X2ndAssistGoal.x*60/player20132015F$TOI.x
player20132015F$X2ndAssist.xPer60<-player20132015F$X2ndAssist.x*60/player20132015F$TOI.x
player20132015F$GoalonIce20142015Per60<-player20132015F$GoalonIce20142015*60/player20132015F$TOI.y
player20132015F=player20132015F[player20132015F$Name!="Dustin.Penner",]
fitPer60F<-lm(player20132015F$GoalonIce20142015Per60~
               player20132015F$NonAssistGoal.xPer60+player20132015F$X1stAssist.xPer60+player20132015F$X2ndAssisted1stAssist.xPer60+
               player20132015F$X2ndAssist.xPer60+player20132015F$NonAssistPlayers.xPer60+player20132015F$X1stAssistPlayers.xPer60+
               player20132015F$X2ndAssistPlayers.xPer60+
               player20132015F$X1stAssistGoal.xPer60+
               player20132015F$X2ndAssistGoal.xPer60,
             weight=player20132015F$TOI.y,subset=(player20132015F$TOI.y>200 &player20132015F$Name!="Jesper.Fast")
)
summary(fitPer60F)
plot(fitPer60F)


summary(fitPer60)
fitPer60F$resid[fitPer60F$resid< (-2)]


player20132015F500<-player20132015F[player20132015F$TOI.y>500,]
fitSQRTPer60F20132015<-lm(player20132015F500$GoalonIce20142015Per60~
                            sqrt(player20132015F500$NonAssistGoal.xPer60)+sqrt(player20132015F500$X1stAssist.xPer60)+sqrt(player20132015F500$X2ndAssisted1stAssist.xPer60)+
                            sqrt(player20132015F500$X2ndAssist.xPer60)+sqrt(player20132015F500$NonAssistPlayers.xPer60)+sqrt(player20132015F500$X1stAssistPlayers.xPer60)+
                            sqrt(player20132015F500$X2ndAssistPlayers.xPer60)+
                            sqrt(player20132015F500$X1stAssistGoal.xPer60)+
                            sqrt(player20132015F500$X2ndAssistGoal.xPer60),
                          weight=player20132015F500$TOI.y)




summary(fitSQRTPer60F20132015)
#only defenders
player20132015D$GoalonIce20142015<-(
  player20132015D$NonAssistGoal.y+player20132015D$X1stAssist.y+player20132015D$X2ndAssisted1stAssist.y+
    player20132015D$X2ndAssist.y+player20132015D$NonAssistPlayers.y+player20132015D$X1stAssistPlayers.y+
    player20132015D$X2ndAssistPlayers.y+
    player20132015D$X1stAssistGoal.y+
    player20132015D$X2ndAssistGoal.y)


fitMeanTimeD<-lm(player20132015D$GoalonIce20142015~
                  player20132015D$NonAssistGoal.x+player20132015D$X1stAssist.x+player20132015D$X2ndAssisted1stAssist.x+
                  player20132015D$X2ndAssist.x+player20132015D$NonAssistPlayers.x+player20132015D$X1stAssistPlayers.x+
                  player20132015D$X2ndAssistPlayers.x+
                  player20132015D$X1stAssistGoal.x+
                  player20132015D$X2ndAssistGoal.x,
                weight=(player20132015D$TOI..x+player20132015D$TOI..y)/2
)
summary(fitMeanTimeD)

fitOnlyYD<-lm(player20132015D$GoalonIce20142015~
               player20132015D$NonAssistGoal.x+player20132015D$X1stAssist.x+player20132015D$X2ndAssisted1stAssist.x+
               player20132015D$X2ndAssist.x+player20132015D$NonAssistPlayers.x+player20132015D$X1stAssistPlayers.x+
               player20132015D$X2ndAssistPlayers.x+
               player20132015D$X1stAssistGoal.x+
               player20132015D$X2ndAssistGoal.x,
             weight=player20132015D$TOI..y
)
summary(fitOnlyYD)

player20132015D$NonAssistGoal.xPer60<-player20132015D$NonAssistGoal.x*60/player20132015D$TOI.x
player20132015D$X1stAssist.xPer60<-player20132015D$X1stAssist.x*60/player20132015D$TOI.x
player20132015D$X2ndAssisted1stAssist.xPer60<-player20132015D$X2ndAssisted1stAssist.x*60/player20132015D$TOI.x
#player20132015D$X2ndAssisted2ndAssist.xPer60<-player20132015D$X2ndAssisted2ndAssist.x/player20132015D$TOI..x
player20132015D$NonAssistPlayers.xPer60<-player20132015D$NonAssistPlayers.x*60/player20132015D$TOI.x
player20132015D$X1stAssistPlayers.xPer60<-player20132015D$X1stAssistPlayers.x*60/player20132015D$TOI.x
player20132015D$X2ndAssistPlayers.xPer60<-player20132015D$X2ndAssistPlayers.x*60/player20132015D$TOI.x
player20132015D$X1stAssistGoal.xPer60<-player20132015D$X1stAssistGoal.x*60/player20132015D$TOI.x
player20132015D$X2ndAssistGoal.xPer60<-player20132015D$X2ndAssistGoal.x*60/player20132015D$TOI.x
player20132015D$X2ndAssist.xPer60<-player20132015D$X2ndAssist.x*60/player20132015D$TOI.x
player20132015D$GoalonIce20142015Per60<-player20132015D$GoalonIce20142015*60/player20132015D$TOI.y

fitPer60D<-lm(player20132015D$GoalonIce20142015Per60~
               player20132015D$NonAssistGoal.xPer60+player20132015D$X1stAssist.xPer60+player20132015D$X2ndAssisted1stAssist.xPer60+
               player20132015D$X2ndAssist.xPer60+player20132015D$NonAssistPlayers.xPer60+player20132015D$X1stAssistPlayers.xPer60+
               player20132015D$X2ndAssistPlayers.xPer60+
               player20132015D$X1stAssistGoal.xPer60+
               player20132015D$X2ndAssistGoal.xPer60,
             weight=player20132015D$TOI.y,subset=(player20132015D$TOI.y>200)
)
summary(fitPer60D)



#2012-2014

fname=file.choose()
data20122013=read.csv(fname,header=T)

fname=file.choose()
data20132014=read.csv(fname,header=T)

fname=file.choose()
pTOIEH20122013=read.csv(fname,header=T)

fname=file.choose()
pTOIEH20132014=read.csv(fname,header=T)


data20122013EH=data20122013
data20132014EH=data20132014
data20122013EH$Name<-str_replace_all(data20122013EH$Name," ",".")
data20132014EH$Name<-str_replace_all(data20132014EH$Name," ",".")

data20122013EH <-ddply(data20122013EH, "Name", numcolwise(sum))
data20132014EH <-ddply(data20132014EH, "Name", numcolwise(sum))
player20122013 <- merge(data20122013EH,pTOIEH20122013,by="Name")
head(pTOIEH20122013, n=10)
head(data20122013EH, n=10)
player20132014 <- merge(data20132014EH,pTOIEH20132014,by="Name")
head(pTOIEH20132014, n=10)
head(data20132014EH, n=10)

head(player20122013, n=10)
head(player20132014, n=10)

player20122014 <- merge(player20122013,player20132014,by="Name")
player20122014
#player20122013=player20122013[player20122013$Year.x==20122013, ]
player20122014D=player20122014[player20122014$pos.x=='D',]
player20122014F=player20122014[player20122014$pos.x!='D',]

#EntirePlayers
player20122014$GoalonIce20142015<-(
  player20122014$NonAssistGoal.y+player20122014$X1stAssist.y+player20122014$X2ndAssisted1stAssist.y+
    player20122014$X2ndAssist.y+player20122014$NonAssistPlayers.y+player20122014$X1stAssistPlayers.y+
    player20122014$X2ndAssistPlayers.y+
    player20122014$X1stAssistGoal.y+
    player20122014$X2ndAssistGoal.y)

fitMeanTime20122014<-lm(player20122014$GoalonIce20142015~
                          player20122014$NonAssistGoal.x+player20122014$X1stAssist.x+player20122014$X2ndAssisted1stAssist.x+
                          player20122014$X2ndAssist.x+player20122014$NonAssistPlayers.x+player20122014$X1stAssistPlayers.x+
                          player20122014$X2ndAssistPlayers.x+
                          player20122014$X1stAssistGoal.x+
                          player20122014$X2ndAssistGoal.x,
                        weight=(player20122014$TOI..x+player20122014$TOI..y)/2
)
summary(fitMeanTime20122014)

fitOnlyY20122014<-lm(player20122014$GoalonIce20142015~
                       player20122014$NonAssistGoal.x+player20122014$X1stAssist.x+player20122014$X2ndAssisted1stAssist.x+
                       player20122014$X2ndAssist.x+player20122014$NonAssistPlayers.x+player20122014$X1stAssistPlayers.x+
                       player20122014$X2ndAssistPlayers.x+
                       player20122014$X1stAssistGoal.x+
                       player20122014$X2ndAssistGoal.x,
                     weight=player20122014$TOI..y
)
summary(fitOnlyY20122014)

player20122014$NonAssistGoal.xPer60<-player20122014$NonAssistGoal.x*60/player20122014$TOI.x
player20122014$X1stAssist.xPer60<-player20122014$X1stAssist.x*60/player20122014$TOI.x
player20122014$X2ndAssisted1stAssist.xPer60<-player20122014$X2ndAssisted1stAssist.x*60/player20122014$TOI.x
#player20122014$X2ndAssisted2ndAssist.xPer60<-player20122014$X2ndAssisted2ndAssist.x/player20122014$TOI..x
player20122014$NonAssistPlayers.xPer60<-player20122014$NonAssistPlayers.x*60/player20122014$TOI.x
player20122014$X1stAssistPlayers.xPer60<-player20122014$X1stAssistPlayers.x*60/player20122014$TOI.x
player20122014$X2ndAssistPlayers.xPer60<-player20122014$X2ndAssistPlayers.x*60/player20122014$TOI.x
player20122014$X1stAssistGoal.xPer60<-player20122014$X1stAssistGoal.x*60/player20122014$TOI.x
player20122014$X2ndAssistGoal.xPer60<-player20122014$X2ndAssistGoal.x*60/player20122014$TOI.x
player20122014$X2ndAssist.xPer60<-player20122014$X2ndAssist.x*60/player20122014$TOI.x
player20122014$GoalonIce20142015Per60<-player20122014$GoalonIce20142015*60/player20122014$TOI.y
fitPer6020122014<-lm(player20122014$GoalonIce20142015Per60~
                       player20122014$NonAssistGoal.xPer60+player20122014$X1stAssist.xPer60+player20122014$X2ndAssisted1stAssist.xPer60+
                       player20122014$X2ndAssist.xPer60+player20122014$NonAssistPlayers.xPer60+player20122014$X1stAssistPlayers.xPer60+
                       player20122014$X2ndAssistPlayers.xPer60+
                       player20122014$X1stAssistGoal.xPer60+
                       player20122014$X2ndAssistGoal.xPer60,
                     weight=player20122014$TOI.y,subset=(player20122014$TOI.y>200)
)
summary(fitPer6020122014)

#Only Forwards

player20122014F$GoalonIce20142015<-(
  player20122014F$NonAssistGoal.y+player20122014F$X1stAssist.y+player20122014F$X2ndAssisted1stAssist.y+
    player20122014F$X2ndAssist.y+player20122014F$NonAssistPlayers.y+player20122014F$X1stAssistPlayers.y+
    player20122014F$X2ndAssistPlayers.y+
    player20122014F$X1stAssistGoal.y+
    player20122014F$X2ndAssistGoal.y)


fitMeanTimeF20122014<-lm(player20122014F$GoalonIce20142015~
                           player20122014F$NonAssistGoal.x+player20122014F$X1stAssist.x+player20122014F$X2ndAssisted1stAssist.x+
                           player20122014F$X2ndAssist.x+player20122014F$NonAssistPlayers.x+player20122014F$X1stAssistPlayers.x+
                           player20122014F$X2ndAssistPlayers.x+
                           player20122014F$X1stAssistGoal.x+
                           player20122014F$X2ndAssistGoal.x,
                         weight=(player20122014F$TOI..x+player20122014F$TOI..y)/2
)
summary(fitMeanTimeF20122014)

fitOnlyYF20122014<-lm(player20122014F$GoalonIce20142015~
                        player20122014F$NonAssistGoal.x+player20122014F$X1stAssist.x+player20122014F$X2ndAssisted1stAssist.x+
                        player20122014F$X2ndAssist.x+player20122014F$NonAssistPlayers.x+player20122014F$X1stAssistPlayers.x+
                        player20122014F$X2ndAssistPlayers.x+
                        player20122014F$X1stAssistGoal.x+
                        player20122014F$X2ndAssistGoal.x,
                      weight=player20122014F$TOI..y
)
summary(fitOnlyYF20122014)

player20122014F$NonAssistGoal.xPer60<-player20122014F$NonAssistGoal.x*60/player20122014F$TOI.x
player20122014F$X1stAssist.xPer60<-player20122014F$X1stAssist.x*60/player20122014F$TOI.x
player20122014F$X2ndAssisted1stAssist.xPer60<-player20122014F$X2ndAssisted1stAssist.x*60/player20122014F$TOI.x
#player20122014F$X2ndAssisted2ndAssist.xPer60<-player20122014F$X2ndAssisted2ndAssist.x/player20122014F$TOI..x
player20122014F$NonAssistPlayers.xPer60<-player20122014F$NonAssistPlayers.x*60/player20122014F$TOI.x
player20122014F$X1stAssistPlayers.xPer60<-player20122014F$X1stAssistPlayers.x*60/player20122014F$TOI.x
player20122014F$X2ndAssistPlayers.xPer60<-player20122014F$X2ndAssistPlayers.x*60/player20122014F$TOI.x
player20122014F$X1stAssistGoal.xPer60<-player20122014F$X1stAssistGoal.x*60/player20122014F$TOI.x
player20122014F$X2ndAssistGoal.xPer60<-player20122014F$X2ndAssistGoal.x*60/player20122014F$TOI.x
player20122014F$X2ndAssist.xPer60<-player20122014F$X2ndAssist.x*60/player20122014F$TOI.x
player20122014F$GoalonIce20142015Per60<-player20122014F$GoalonIce20142015*60/player20122014F$TOI.y
fitPer60F20122014<-lm(player20122014F$GoalonIce20142015Per60~
                        player20122014F$NonAssistGoal.xPer60+player20122014F$X1stAssist.xPer60+player20122014F$X2ndAssisted1stAssist.xPer60+
                        player20122014F$X2ndAssist.xPer60+player20122014F$NonAssistPlayers.xPer60+player20122014F$X1stAssistPlayers.xPer60+
                        player20122014F$X2ndAssistPlayers.xPer60+
                        player20122014F$X1stAssistGoal.xPer60+
                        player20122014F$X2ndAssistGoal.xPer60,
                      weight=player20122014F$TOI.y,subset=(player20122014F$TOI.y>500 &player20122014F$Name!= "Mark.Stone")
)
player20122014F500<-player20122014F[player20122014F$TOI.y>500,]
fitSQRTPer60F20122014<-lm(player20122014F500$GoalonIce20142015Per60~
                        sqrt(player20122014F500$NonAssistGoal.xPer60)+sqrt(player20122014F500$X1stAssist.xPer60)+sqrt(player20122014F500$X2ndAssisted1stAssist.xPer60)+
                          sqrt(player20122014F500$X2ndAssist.xPer60)+sqrt(player20122014F500$NonAssistPlayers.xPer60)+sqrt(player20122014F500$X1stAssistPlayers.xPer60)+
                          sqrt(player20122014F500$X2ndAssistPlayers.xPer60)+
                          sqrt(player20122014F500$X1stAssistGoal.xPer60)+
                          sqrt(player20122014F500$X2ndAssistGoal.xPer60),
                      weight=player20122014F500$TOI.y)
)
summary(fitSQRTPer60F20122014)

boxplot(sqrt(player20122014F500$NonAssistGoal.xPer60),sqrt(player20122014F500$X1stAssist.xPer60),sqrt(player20122014F500$X2ndAssisted1stAssist.xPer60),
  sqrt(player20122014F500$X2ndAssist.xPer60),sqrt(player20122014F500$NonAssistPlayers.xPer60),sqrt(player20122014F500$X1stAssistPlayers.xPer60),
  sqrt(player20122014F500$X2ndAssistPlayers.xPer60),
  sqrt(player20122014F500$X1stAssistGoal.xPer60),
  sqrt(player20122014F500$X2ndAssistGoal.xPer60))
hist(player20122014F$NonAssistGoal.xPer60)
summary(fitPer60F20122014)
plot(fitPer60F20122014)
#only defenders
player20122014D$GoalonIce20142015<-(
  player20122014D$NonAssistGoal.y+player20122014D$X1stAssist.y+player20122014D$X2ndAssisted1stAssist.y+
    player20122014D$X2ndAssist.y+player20122014D$NonAssistPlayers.y+player20122014D$X1stAssistPlayers.y+
    player20122014D$X2ndAssistPlayers.y+
    player20122014D$X1stAssistGoal.y+
    player20122014D$X2ndAssistGoal.y)


fitMeanTimeD20122014<-lm(player20122014D$GoalonIce20142015~
                           player20122014D$NonAssistGoal.x+player20122014D$X1stAssist.x+player20122014D$X2ndAssisted1stAssist.x+
                           player20122014D$X2ndAssist.x+player20122014D$NonAssistPlayers.x+player20122014D$X1stAssistPlayers.x+
                           player20122014D$X2ndAssistPlayers.x+
                           player20122014D$X1stAssistGoal.x+
                           player20122014D$X2ndAssistGoal.x,
                         weight=(player20122014D$TOI..x+player20122014D$TOI..y)/2
)
summary(fitMeanTimeD20122014)

fitOnlyYD20122014<-lm(player20122014D$GoalonIce20142015~
                        player20122014D$NonAssistGoal.x+player20122014D$X1stAssist.x+player20122014D$X2ndAssisted1stAssist.x+
                        player20122014D$X2ndAssist.x+player20122014D$NonAssistPlayers.x+player20122014D$X1stAssistPlayers.x+
                        player20122014D$X2ndAssistPlayers.x+
                        player20122014D$X1stAssistGoal.x+
                        player20122014D$X2ndAssistGoal.x,
                      weight=player20122014D$TOI..y
)
summary(fitOnlyYD20122014)

player20122014D$NonAssistGoal.xPer60<-player20122014D$NonAssistGoal.x*60/player20122014D$TOI.x
player20122014D$X1stAssist.xPer60<-player20122014D$X1stAssist.x*60/player20122014D$TOI.x
player20122014D$X2ndAssisted1stAssist.xPer60<-player20122014D$X2ndAssisted1stAssist.x*60/player20122014D$TOI.x
#player20122014D$X2ndAssisted2ndAssist.xPer60<-player20122014D$X2ndAssisted2ndAssist.x/player20122014D$TOI..x
player20122014D$NonAssistPlayers.xPer60<-player20122014D$NonAssistPlayers.x*60/player20122014D$TOI.x
player20122014D$X1stAssistPlayers.xPer60<-player20122014D$X1stAssistPlayers.x*60/player20122014D$TOI.x
player20122014D$X2ndAssistPlayers.xPer60<-player20122014D$X2ndAssistPlayers.x*60/player20122014D$TOI.x
player20122014D$X1stAssistGoal.xPer60<-player20122014D$X1stAssistGoal.x*60/player20122014D$TOI.x
player20122014D$X2ndAssistGoal.xPer60<-player20122014D$X2ndAssistGoal.x*60/player20122014D$TOI.x
player20122014D$X2ndAssist.xPer60<-player20122014D$X2ndAssist.x*60/player20122014D$TOI.x
player20122014D$GoalonIce20142015Per60<-player20122014D$GoalonIce20142015*60/player20122014D$TOI.y
fitPer60D20122014<-lm(player20122014D$GoalonIce20142015Per60~
                        player20122014D$NonAssistGoal.xPer60+player20122014D$X1stAssist.xPer60+player20122014D$X2ndAssisted1stAssist.xPer60+
                        player20122014D$X2ndAssist.xPer60+player20122014D$NonAssistPlayers.xPer60+player20122014D$X1stAssistPlayers.xPer60+
                        player20122014D$X2ndAssistPlayers.xPer60+
                        player20122014D$X1stAssistGoal.xPer60+
                        player20122014D$X2ndAssistGoal.xPer60,
                      weight=player20122014D$TOI.y,subset=(player20122014D$TOI.y>200)
)
summary(fitPer60D20122014)
summary(fitPer60F20122014)
summary(fitPer6020122014)
plot(fitPer60F20122014)

plot(player20122014F$GoalonIce20142015Per60~player20122014F$X2ndAssisted1stAssist.xPer60,subset=(player20122014F$TOI.y>20))
fit1<-lm(player20122014F$GoalonIce20142015Per60~player20122014F$X2ndAssisted1stAssist.xPer60,subset=(player20122014F$TOI.y>20))
summary(fit1)




#2011-2013

fname=file.choose()
data20112012=read.csv(fname,header=T)

fname=file.choose()
data20122013=read.csv(fname,header=T)

fname=file.choose()
pTOIEH20112012=read.csv(fname,header=T)

fname=file.choose()
pTOIEH20122013=read.csv(fname,header=T)


data20112012EH=data20112012
data20122013EH=data20122013
data20112012EH$Name<-str_replace_all(data20112012EH$Name," ",".")
data20122013EH$Name<-str_replace_all(data20122013EH$Name," ",".")

data20112012EH <-ddply(data20112012EH, "Name", numcolwise(sum))
data20122013EH <-ddply(data20122013EH, "Name", numcolwise(sum))
player20112012 <- merge(data20112012EH,pTOIEH20112012,by="Name")
head(pTOIEH20112012, n=10)
head(data20112012EH, n=10)
player20122013 <- merge(data20122013EH,pTOIEH20122013,by="Name")
head(pTOIEH20122013, n=10)
head(data20122013EH, n=10)

head(player20112012, n=10)
head(player20122013, n=10)

player20112013 <- merge(player20112012,player20122013,by="Name")
player20112013
#player20112012=player20112012[player20112012$Year.x==20112012, ]
player20112013D=player20112013[player20112013$pos.x=='D',]
player20112013F=player20112013[player20112013$pos.x!='D',]

#EntirePlayers
player20112013$GoalonIce20142015<-(
  player20112013$NonAssistGoal.y+player20112013$X1stAssist.y+player20112013$X2ndAssisted1stAssist.y+
    player20112013$X2ndAssist.y+player20112013$NonAssistPlayers.y+player20112013$X1stAssistPlayers.y+
    player20112013$X2ndAssistPlayers.y+
    player20112013$X1stAssistGoal.y+
    player20112013$X2ndAssistGoal.y)

fitMeanTime20112013<-lm(player20112013$GoalonIce20142015~
                          player20112013$NonAssistGoal.x+player20112013$X1stAssist.x+player20112013$X2ndAssisted1stAssist.x+
                          player20112013$X2ndAssist.x+player20112013$NonAssistPlayers.x+player20112013$X1stAssistPlayers.x+
                          player20112013$X2ndAssistPlayers.x+
                          player20112013$X1stAssistGoal.x+
                          player20112013$X2ndAssistGoal.x,
                        weight=(player20112013$TOI..x+player20112013$TOI..y)/2
)
summary(fitMeanTime20112013)

fitOnlyY20112013<-lm(player20112013$GoalonIce20142015~
                       player20112013$NonAssistGoal.x+player20112013$X1stAssist.x+player20112013$X2ndAssisted1stAssist.x+
                       player20112013$X2ndAssist.x+player20112013$NonAssistPlayers.x+player20112013$X1stAssistPlayers.x+
                       player20112013$X2ndAssistPlayers.x+
                       player20112013$X1stAssistGoal.x+
                       player20112013$X2ndAssistGoal.x,
                     weight=player20112013$TOI..y
)
summary(fitOnlyY20112013)

player20112013$NonAssistGoal.xPer60<-player20112013$NonAssistGoal.x*60/player20112013$TOI.x
player20112013$X1stAssist.xPer60<-player20112013$X1stAssist.x*60/player20112013$TOI.x
player20112013$X2ndAssisted1stAssist.xPer60<-player20112013$X2ndAssisted1stAssist.x*60/player20112013$TOI.x
#player20112013$X2ndAssisted2ndAssist.xPer60<-player20112013$X2ndAssisted2ndAssist.x/player20112013$TOI..x
player20112013$NonAssistPlayers.xPer60<-player20112013$NonAssistPlayers.x*60/player20112013$TOI.x
player20112013$X1stAssistPlayers.xPer60<-player20112013$X1stAssistPlayers.x*60/player20112013$TOI.x
player20112013$X2ndAssistPlayers.xPer60<-player20112013$X2ndAssistPlayers.x*60/player20112013$TOI.x
player20112013$X1stAssistGoal.xPer60<-player20112013$X1stAssistGoal.x*60/player20112013$TOI.x
player20112013$X2ndAssistGoal.xPer60<-player20112013$X2ndAssistGoal.x*60/player20112013$TOI.x
player20112013$X2ndAssist.xPer60<-player20112013$X2ndAssist.x*60/player20112013$TOI.x
player20112013$GoalonIce20142015Per60<-player20112013$GoalonIce20142015*60/player20112013$TOI.y
fitPer6020112013<-lm(player20112013$GoalonIce20142015Per60~
                       player20112013$NonAssistGoal.xPer60+player20112013$X1stAssist.xPer60+player20112013$X2ndAssisted1stAssist.xPer60+
                       player20112013$X2ndAssist.xPer60+player20112013$NonAssistPlayers.xPer60+player20112013$X1stAssistPlayers.xPer60+
                       player20112013$X2ndAssistPlayers.xPer60+
                       player20112013$X1stAssistGoal.xPer60+
                       player20112013$X2ndAssistGoal.xPer60,
                     weight=player20112013$TOI.y,subset=(player20112013$TOI.y>200)
)
summary(fitPer6020112013)

#Only Forwards

player20112013F$GoalonIce20142015<-(
  player20112013F$NonAssistGoal.y+player20112013F$X1stAssist.y+player20112013F$X2ndAssisted1stAssist.y+
    player20112013F$X2ndAssist.y+player20112013F$NonAssistPlayers.y+player20112013F$X1stAssistPlayers.y+
    player20112013F$X2ndAssistPlayers.y+
    player20112013F$X1stAssistGoal.y+
    player20112013F$X2ndAssistGoal.y)


fitMeanTimeF20112013<-lm(player20112013F$GoalonIce20112013~
                           player20112013F$NonAssistGoal.x+player20112013F$X1stAssist.x+player20112013F$X2ndAssisted1stAssist.x+
                           player20112013F$X2ndAssist.x+player20112013F$NonAssistPlayers.x+player20112013F$X1stAssistPlayers.x+
                           player20112013F$X2ndAssistPlayers.x+
                           player20112013F$X1stAssistGoal.x+
                           player20112013F$X2ndAssistGoal.x,
                         weight=(player20112013F$TOI..x+player20112013F$TOI..y)/2
)
summary(fitMeanTimeF20112013)

fitOnlyYF20112013<-lm(player20112013F$GoalonIce20112013~
                        player20112013F$NonAssistGoal.x+player20112013F$X1stAssist.x+player20112013F$X2ndAssisted1stAssist.x+
                        player20112013F$X2ndAssist.x+player20112013F$NonAssistPlayers.x+player20112013F$X1stAssistPlayers.x+
                        player20112013F$X2ndAssistPlayers.x+
                        player20112013F$X1stAssistGoal.x+
                        player20112013F$X2ndAssistGoal.x,
                      weight=player20112013F$TOI..y
)
summary(fitOnlyYF20112013)

player20112013F$NonAssistGoal.xPer60<-player20112013F$NonAssistGoal.x*60/player20112013F$TOI.x
player20112013F$X1stAssist.xPer60<-player20112013F$X1stAssist.x*60/player20112013F$TOI.x
player20112013F$X2ndAssisted1stAssist.xPer60<-player20112013F$X2ndAssisted1stAssist.x*60/player20112013F$TOI.x
#player20112013F$X2ndAssisted2ndAssist.xPer60<-player20112013F$X2ndAssisted2ndAssist.x/player20112013F$TOI..x
player20112013F$NonAssistPlayers.xPer60<-player20112013F$NonAssistPlayers.x*60/player20112013F$TOI.x
player20112013F$X1stAssistPlayers.xPer60<-player20112013F$X1stAssistPlayers.x*60/player20112013F$TOI.x
player20112013F$X2ndAssistPlayers.xPer60<-player20112013F$X2ndAssistPlayers.x*60/player20112013F$TOI.x
player20112013F$X1stAssistGoal.xPer60<-player20112013F$X1stAssistGoal.x*60/player20112013F$TOI.x
player20112013F$X2ndAssistGoal.xPer60<-player20112013F$X2ndAssistGoal.x*60/player20112013F$TOI.x
player20112013F$X2ndAssist.xPer60<-player20112013F$X2ndAssist.x*60/player20112013F$TOI.x
player20112013F$GoalonIce20142015Per60<-player20112013F$GoalonIce20142015*60/player20112013F$TOI.y

fitPer60F20112013<-lm(player20112013F$GoalonIce20142015Per60~
                        player20112013F$NonAssistGoal.xPer60+player20112013F$X1stAssist.xPer60+player20112013F$X2ndAssisted1stAssist.xPer60+
                        player20112013F$X2ndAssist.xPer60+player20112013F$NonAssistPlayers.xPer60+player20112013F$X1stAssistPlayers.xPer60+
                        player20112013F$X2ndAssistPlayers.xPer60+
                        player20112013F$X1stAssistGoal.xPer60+
                        player20112013F$X2ndAssistGoal.xPer60,
                      weight=player20112013F$TOI.y,subset=(player20112013F$TOI.y>500 &player20112013F$Name!= "Mark.Stone")
)
500*48/82
player20112013F500<-player20112013F[player20112013F$TOI.y>292,]
player20112013F500<-player20112013F500[player20112013F500$TOI.x>500,]
fitSQRTPer60F20112013<-lm(player20112013F500$GoalonIce20142015Per60~
                            sqrt(player20112013F500$NonAssistGoal.xPer60)+sqrt(player20112013F500$X1stAssist.xPer60)+sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60)+
                            sqrt(player20112013F500$X2ndAssist.xPer60)+sqrt(player20112013F500$NonAssistPlayers.xPer60)+sqrt(player20112013F500$X1stAssistPlayers.xPer60)+
                            sqrt(player20112013F500$X2ndAssistPlayers.xPer60)+
                            sqrt(player20112013F500$X1stAssistGoal.xPer60)+
                            sqrt(player20112013F500$X2ndAssistGoal.xPer60),
                          weight=player20112013F500$TOI.y)

summary(fitSQRTPer60F20112013)


player20112013F500$GoalPredictWithout<-  predict(fitSQRTPer60F20112013, player20112013F500) -0.78097 *sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60)
sortPreGoalplayer20112013F500=player20112013F500
sortPreGoalplayer20112013F500<-sortPreGoalplayer20112013F500[order(sortPreGoalplayer20112013F500$GoalPredictWithout),] 

sortPreGoalplayer20112013F500Top30 = sortPreGoalplayer20112013F500[1:30,] 
sortPreGoalplayer20112013F500Top30 <-sortPreGoalplayer20112013F500Top30[order(sortPreGoalplayer20112013F500Top30$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500Top60 =sortPreGoalplayer20112013F500[31:60,] 
sortPreGoalplayer20112013F500Top60 <-sortPreGoalplayer20112013F500Top60[order(sortPreGoalplayer20112013F500Top60$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500Top90 =sortPreGoalplayer20112013F500[61:90,] 
sortPreGoalplayer20112013F500Top90 <-sortPreGoalplayer20112013F500Top90[order(sortPreGoalplayer20112013F500Top90$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500Top120 =sortPreGoalplayer20112013F500[91:120,] 
sortPreGoalplayer20112013F500Top120 <-sortPreGoalplayer20112013F500Top120[order(sortPreGoalplayer20112013F500Top120$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500Top150 =sortPreGoalplayer20112013F500[121:150,] 
sortPreGoalplayer20112013F500Top150 <-sortPreGoalplayer20112013F500Top150[order(sortPreGoalplayer20112013F500Top150$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500Top180 =sortPreGoalplayer20112013F500[151:180,] 
sortPreGoalplayer20112013F500Top180 <-sortPreGoalplayer20112013F500Top180[order(sortPreGoalplayer20112013F500Top180$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500Top210 =sortPreGoalplayer20112013F500[181:210,] 
sortPreGoalplayer20112013F500Top210 <-sortPreGoalplayer20112013F500Top210[order(sortPreGoalplayer20112013F500Top210$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500Top240 =sortPreGoalplayer20112013F500[211:240,] 
sortPreGoalplayer20112013F500Top240 <-sortPreGoalplayer20112013F500Top240[order(sortPreGoalplayer20112013F500Top240$X2ndAssisted1stAssist.x),]

sortPreGoalplayer20112013F500[1:1,] 
sortPreGoalplayer20112013F500[30:30,] 
sortPreGoalplayer20112013F500[31:31,] 
sortPreGoalplayer20112013F500[60:60,] 
sortPreGoalplayer20112013F500[61:61,] 
sortPreGoalplayer20112013F500[90:90,] 
sortPreGoalplayer20112013F500[91:91,] 
sortPreGoalplayer20112013F500[120:120,] 
sortPreGoalplayer20112013F500[121:121,] 
sortPreGoalplayer20112013F500[150:150,] 
sortPreGoalplayer20112013F500[151:151,] 
sortPreGoalplayer20112013F500[180:180,] 
sortPreGoalplayer20112013F500[181:181,] 
sortPreGoalplayer20112013F500[210:210,] 
sortPreGoalplayer20112013F500[211:211,] 
sortPreGoalplayer20112013F500[240:240,] 
sortPreGoalplayer20112013F500[241:241,] 
sortPreGoalplayer20112013F500[270:270,] 
sortPreGoalplayer20112013F500[271:271,] 
sortPreGoalplayer20112013F500[285:285,] 

sortPreGoalplayer20112013F500[1:1,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[1:1,]$GoalPredictWithout
sortPreGoalplayer20112013F500[30:30,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[30:30,]$GoalPredictWithout

sortPreGoalplayer20112013F500[31:31,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[31:31,]$GoalPredictWithout
sortPreGoalplayer20112013F500[60:60,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[60:60,]$GoalPredictWithout


sortPreGoalplayer20112013F500[61:61,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[61:61,]$GoalPredictWithout
sortPreGoalplayer20112013F500[90:90,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[90:90,]$GoalPredictWithout

sortPreGoalplayer20112013F500[91:91,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[91:91,]$GoalPredictWithout
sortPreGoalplayer20112013F500[120:120,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[120:120,]$GoalPredictWithout

sortPreGoalplayer20112013F500[121:121,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[121:121,]$GoalPredictWithout
sortPreGoalplayer20112013F500[150:150,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[150:150,]$GoalPredictWithout

sortPreGoalplayer20112013F500[151:151,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[151:151,]$GoalPredictWithout
sortPreGoalplayer20112013F500[180:180,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[180:180,]$GoalPredictWithout

sortPreGoalplayer20112013F500[181:181,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[181:181,]$GoalPredictWithout
sortPreGoalplayer20112013F500[210:210,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[210:210,]$GoalPredictWithout

sortPreGoalplayer20112013F500[211:211,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[211:211,]$GoalPredictWithout
sortPreGoalplayer20112013F500[240:240,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[240:240,]$GoalPredictWithout

sortPreGoalplayer20112013F500[241:241,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[241:241,]$GoalPredictWithout
sortPreGoalplayer20112013F500[270:270,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[270:270,]$GoalPredictWithout

sortPreGoalplayer20112013F500[271:271,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[271:271,]$GoalPredictWithout
sortPreGoalplayer20112013F500[285:285,]$GoalonIce20142015Per60-sortPreGoalplayer20112013F500[285:285,]$GoalPredictWithout

boxplot(sqrt(player20112013F500$NonAssistGoal.xPer60),sqrt(player20112013F500$X1stAssist.xPer60),sqrt(player20112013F500$X2ndAssisted1stAssist.xPer60),
        sqrt(player20112013F500$X2ndAssist.xPer60),sqrt(player20112013F500$NonAssistPlayers.xPer60),sqrt(player20112013F500$X1stAssistPlayers.xPer60),
        sqrt(player20112013F500$X2ndAssistPlayers.xPer60),
        sqrt(player20112013F500$X1stAssistGoal.xPer60),
        sqrt(player20112013F500$X2ndAssistGoal.xPer60))
hist(player20112013F$NonAssistGoal.xPer60)
summary(fitPer60F20112015)
plot(fitPer60F20112013)

#only defenders
player20112013D$GoalonIce20142015<-(
  player20112013D$NonAssistGoal.y+player20112013D$X1stAssist.y+player20112013D$X2ndAssisted1stAssist.y+
    player20112013D$X2ndAssist.y+player20112013D$NonAssistPlayers.y+player20112013D$X1stAssistPlayers.y+
    player20112013D$X2ndAssistPlayers.y+
    player20112013D$X1stAssistGoal.y+
    player20112013D$X2ndAssistGoal.y)


fitMeanTimeD20112013<-lm(player20112013D$GoalonIce20142015~
                           player20112013D$NonAssistGoal.x+player20112013D$X1stAssist.x+player20112013D$X2ndAssisted1stAssist.x+
                           player20112013D$X2ndAssist.x+player20112013D$NonAssistPlayers.x+player20112013D$X1stAssistPlayers.x+
                           player20112013D$X2ndAssistPlayers.x+
                           player20112013D$X1stAssistGoal.x+
                           player20112013D$X2ndAssistGoal.x,
                         weight=(player20112013D$TOI..x+player20112013D$TOI..y)/2
)
summary(fitMeanTimeD20112013)

fitOnlyYD20112013<-lm(player20112013D$GoalonIce20142015~
                        player20112013D$NonAssistGoal.x+player20112013D$X1stAssist.x+player20112013D$X2ndAssisted1stAssist.x+
                        player20112013D$X2ndAssist.x+player20112013D$NonAssistPlayers.x+player20112013D$X1stAssistPlayers.x+
                        player20112013D$X2ndAssistPlayers.x+
                        player20112013D$X1stAssistGoal.x+
                        player20112013D$X2ndAssistGoal.x,
                      weight=player20112013D$TOI..y
)
summary(fitOnlyYD20112013)

player20112013D$NonAssistGoal.xPer60<-player20112013D$NonAssistGoal.x*60/player20112013D$TOI.x
player20112013D$X1stAssist.xPer60<-player20112013D$X1stAssist.x*60/player20112013D$TOI.x
player20112013D$X2ndAssisted1stAssist.xPer60<-player20112013D$X2ndAssisted1stAssist.x*60/player20112013D$TOI.x
#player20112013D$X2ndAssisted2ndAssist.xPer60<-player20112013D$X2ndAssisted2ndAssist.x/player20112013D$TOI..x
player20112013D$NonAssistPlayers.xPer60<-player20112013D$NonAssistPlayers.x*60/player20112013D$TOI.x
player20112013D$X1stAssistPlayers.xPer60<-player20112013D$X1stAssistPlayers.x*60/player20112013D$TOI.x
player20112013D$X2ndAssistPlayers.xPer60<-player20112013D$X2ndAssistPlayers.x*60/player20112013D$TOI.x
player20112013D$X1stAssistGoal.xPer60<-player20112013D$X1stAssistGoal.x*60/player20112013D$TOI.x
player20112013D$X2ndAssistGoal.xPer60<-player20112013D$X2ndAssistGoal.x*60/player20112013D$TOI.x
player20112013D$X2ndAssist.xPer60<-player20112013D$X2ndAssist.x*60/player20112013D$TOI.x
player20112013D$GoalonIce20142015Per60<-player20112013D$GoalonIce20142015*60/player20112013D$TOI.y
fitPer60D20112013<-lm(player20112013D$GoalonIce20142015Per60~
                        player20112013D$NonAssistGoal.xPer60+player20112013D$X1stAssist.xPer60+player20112013D$X2ndAssisted1stAssist.xPer60+
                        player20112013D$X2ndAssist.xPer60+player20112013D$NonAssistPlayers.xPer60+player20112013D$X1stAssistPlayers.xPer60+
                        player20112013D$X2ndAssistPlayers.xPer60+
                        player20112013D$X1stAssistGoal.xPer60+
                        player20112013D$X2ndAssistGoal.xPer60,
                      weight=player20112013D$TOI.y,subset=(player20112013D$TOI.y>200)
)
summary(fitPer60D20112013)
summary(fitPer60F20112013)
summary(fitPer6020112013)
plot(fitPer60F20112013)

plot(player20112013F$GoalonIce20142015Per60~player20112013F$X2ndAssisted1stAssist.xPer60,subset=(player20112013F$TOI.y>20))
fit1<-lm(player20112013F$GoalonIce20142015Per60~player20112013F$X2ndAssisted1stAssist.xPer60,subset=(player20112013F$TOI.y>20))
summary(fit1)





fitPer60F20112015<-lm(player20112015F500$GoalonIce20142015Per60~
                        player20112015F500$NonAssistGoal.xPer60+player20112015F500$X1stAssist.xPer60+player20112015F500$X2ndAssisted1stAssist.xPer60+
                        player20112015F500$X2ndAssist.xPer60+player20112015F500$NonAssistPlayers.xPer60+player20112015F500$X1stAssistPlayers.xPer60+
                        player20112015F500$X2ndAssistPlayers.xPer60+
                        player20112015F500$X1stAssistGoal.xPer60+
                        player20112015F500$X2ndAssistGoal.xPer60,
                      weight=player20112015F500$TOI.y
)
summary(fitPer60F20112015)
