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
sortPreGoalplayer20112013F500Top30 <-sortPreGoalplayer20112013F500Top30[order(sortPreGoalplayer20112013F500Top30$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112013F500Top60 =sortPreGoalplayer20112013F500[31:60,] 
sortPreGoalplayer20112013F500Top60 <-sortPreGoalplayer20112013F500Top60[order(sortPreGoalplayer20112013F500Top60$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112013F500Top90 =sortPreGoalplayer20112013F500[61:90,] 
sortPreGoalplayer20112013F500Top90 <-sortPreGoalplayer20112013F500Top90[order(sortPreGoalplayer20112013F500Top90$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112013F500Top120 =sortPreGoalplayer20112013F500[91:120,] 
sortPreGoalplayer20112013F500Top120 <-sortPreGoalplayer20112013F500Top120[order(sortPreGoalplayer20112013F500Top120$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112013F500Top150 =sortPreGoalplayer20112013F500[121:150,] 
sortPreGoalplayer20112013F500Top150 <-sortPreGoalplayer20112013F500Top150[order(sortPreGoalplayer20112013F500Top150$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112013F500Top180 =sortPreGoalplayer20112013F500[151:180,] 
sortPreGoalplayer20112013F500Top180 <-sortPreGoalplayer20112013F500Top180[order(sortPreGoalplayer20112013F500Top180$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112013F500Top210 =sortPreGoalplayer20112013F500[181:210,] 
sortPreGoalplayer20112013F500Top210 <-sortPreGoalplayer20112013F500Top210[order(sortPreGoalplayer20112013F500Top210$X2ndAssisted1stAssist.xPer60),]

sortPreGoalplayer20112013F500Top240 =sortPreGoalplayer20112013F500[211:240,] 
sortPreGoalplayer20112013F500Top240 <-sortPreGoalplayer20112013F500Top240[order(sortPreGoalplayer20112013F500Top240$X2ndAssisted1stAssist.xPer60),]


sortPreGoalplayer20112013F500Top30[30,83]-sortPreGoalplayer20112013F500Top30[1,83]+
  sortPreGoalplayer20112013F500Top60[30,83]-sortPreGoalplayer20112013F500Top60[1,83]+
  sortPreGoalplayer20112013F500Top90[30,83]-sortPreGoalplayer20112013F500Top90[1,83]+
  sortPreGoalplayer20112013F500Top120[30,83]-sortPreGoalplayer20112013F500Top120[1,83]+
  sortPreGoalplayer20112013F500Top150[30,83]-sortPreGoalplayer20112013F500Top150[1,83]+
  sortPreGoalplayer20112013F500Top180[30,83]-sortPreGoalplayer20112013F500Top180[1,83]+
  sortPreGoalplayer20112013F500Top210[30,83]-sortPreGoalplayer20112013F500Top210[1,83]+
  sortPreGoalplayer20112013F500Top240[30,83]-sortPreGoalplayer20112013F500Top240[1,83]+
  sortPreGoalplayer20112013F500Top270[30,83]-sortPreGoalplayer20112013F500Top270[1,83]

3.938403

