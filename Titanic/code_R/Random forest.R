
# load library ------------------------------------------------------------

library(randomForest)

# before treat ------------------------------------------------------------

train2 <- train 
test2 <- test

# Age:NA complemant
train2$Age <- ifelse(is.na(train2$Age),mean(na.omit(train$Age)),train$Age)
test2$Age <- ifelse(is.na(test2$Age),mean(na.omit(test$Age)),test$Age)

# 
train2$Survived <- as.factor(train2$Survived)
train2$Sex <- as.factor(train2$Sex)
test$Sex <- as.factor(test$Sex)

# Learning
train.rf <- randomForest(Survived~Sex+Age+Pclass,data=train2)
pred <- predict(train.rf,test2)

ans_4 <- test2 %>% 
  select(PassengerId) %>% 
  mutate(Survived=pred) %>% 
  select(PassengerId, Survived)


# output ------------------------------------------------------------------

write.table(ans_4,"ans_4.csv", quote = F, row.names = F,sep = ",")  







