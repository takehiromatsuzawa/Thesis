fname=file.choose()

#Read df_new_user.csv
df_result_strd_1996=read.csv(fname,header=T)

fname=file.choose()

#Read df_new_user.csv
df_result_pos_1996=read.csv(fname,header=T)
df_result=merge(df_result_strd_1996, df_result_pos_1996, all.x = TRUE, by = c("name","season","TOT"))
write.csv(df_result, file = "df_result_1996.csv")

