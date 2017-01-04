fname=file.choose()
library(ROCR)
#Read df_new_user.csv
df_new_asana=read.csv(fname,header=T,stringsAsFactors=FALSE)

#Remove unnecessary columns
data <- subset(df_new_asana,select=c(6,8,9,12))
str(data)

#Check Non NA values
#sapply(data,function(x) sum(is.na(x)))
#levels(df$species) <- c(levels(df$species), "unknown") 
data$is_adopted[data$is_adopted == 'True'] <- 1
data$is_adopted[data$is_adopted == "False"] <- 0
data$is_adopted <- as.numeric(data$is_adopted)
data$creation_source <- as.character(data$creation_source)
data$creation_source <- as.numeric(data$creation_source)
#as.numeric(directions.factor)
train <- data[1:(nrow(data)*0.8),]
test <- data[(nrow(data)*0.8+1):(nrow(data)),]
table(train$is_adopted)
#library(rpart)
#install.packages("ROSE")

#library(ROSE)

treeimb <- rpart(is_adopted ~ ., data = train)
pred.treeimb <- predict(treeimb, newdata = test)
roc.curve(test$is_adopted, pred.treeimb[,2], plotit = F)

data_balanced_over <- ovun.sample(is_adopted ~ ., data = train, method = "over",N = 16580)$data
table(data_balanced_over$is_adopted)

data_balanced_under <- ovun.sample(is_adopted ~ ., data = train, method = "under",N = 2620)$data
table(data_balanced_under$is_adopted)

#data.rose <- ROSE(is_adopted ~ ., data = train, seed = 1)$data
data.rose <- ROSE(is_adopted ~ ., data = train, seed = 1)$data

model <- glm(is_adopted ~.,family=binomial(link='logit'),data=train)
model_over <- glm(is_adopted ~.,family=binomial(link='logit'),data=data_balanced_over)
model_under <- glm(is_adopted ~.,family=binomial(link='logit'),data=data_balanced_under)

prob_under <- predict(model_under, newdata=test, type="response")
pred_under <- prediction(prob_under, test$is_adopted)
auc_under <- performance(pred_under, measure = "auc")
auc_under <- auc_under@y.values[[1]]
auc_under

prob_over <- predict(model_over, newdata=test, type="response")
pred_over <- prediction(prob_over, test$is_adopted)
auc_over <- performance(pred_over, measure = "auc")
auc_over <- auc_over@y.values[[1]]
auc_over

prob <- predict(model, newdata=test, type="response")
pred <- prediction(prob, test$is_adopted)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc



#Plot Roc Curve
plot(perf,col='red', lwd = 3,main ='ROC Curve')
abline(0, 1, untf = FALSE,col = "blue", lwd = 2)

#Calculate the area under the Curve
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

