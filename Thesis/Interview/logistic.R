fname=file.choose()
training.data.raw=read.csv(fname,header=T)


sapply(training.data.raw,function(x) sum(is.na(x)))
sapply(training.data.raw, function(x) length(unique(x)))

library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

data <- subset(training.data.raw,select=c(1,2,3,5,6,7,8,12))

data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

is.factor(data$Sex)

is.factor(data$Embarked)

data <- data[!is.na(data$Embarked),]

train <- data[1:800,]
test <- data[801:889,]

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

