library(tidyverse)


# load data ---------------------------------------------------------------

train <- read.csv("train.csv",stringsAsFactors = F)
test <- read.csv("test.csv",stringsAsFactors = F)


# rule base "gender" ------------------------------------------------------
ans_1 <- test %>% 
  select(PassengerId,Sex) %>% 
  mutate(Survived=ifelse(Sex=="female",1,0)) %>% 
  select(PassengerId, Survived)

write.table(ans_1,"ans_1.csv", quote = F, row.names = F,sep = ",")  
  




