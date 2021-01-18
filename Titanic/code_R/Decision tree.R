
# load package ------------------------------------------------------------
library(tidyverse)
library(rpart)
library(rpart.plot)

# decision tree plot-----------------------------------------------------------
train.rpart <- rpart(Survived~Sex+Age+Pclass,data=train)
rpart.plot(train.rpart,type = 2, extra = 101)


# prediction --------------------------------------------------------------

ans_2 <- predict(train.rpart, test, method="class")
hist(ans_2)

ans_2b <- test %>% 
  select(PassengerId) %>% 
  mutate(Survived=ifelse(ans_2>0.5,1,0)) %>% 
  select(PassengerId,Survived)
  
write.table(ans_2b,"ans_2.csv", quote = F, row.names = F,sep = ",")  



