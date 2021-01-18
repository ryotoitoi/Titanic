
# load library ------------------------------------------------------------

library(tidyverse)
library(dummies)
library(ranger)
library(e1071)

# Survivedと変数xのクロス集計の右側に
# 変数x内のカテゴリー比率を加えたものを表示する関数
S_table <- function(x, survived){
  tbl <- table(x, survived)
  row_sum <- apply(tbl, 1, sum)
  s_ratio <- tbl[,2]/row_sum
  
  return(cbind(tbl, s_ratio))
}

# クロス割合を表示する関数
# (クロス集計表の要素が割合)
rate_table <- function(x, y){
  tbl <- table(x, y)
  row_sum <- apply(tbl, 1, sum)
  s_ratio <- tbl/row_sum
  
  return(s_ratio)
}

# データ読み込み
d <- read.csv("train.csv")
d_t <- read.csv("test.csv")

# Cabinの処理
train <- d %>% 
  separate("Cabin", into=c("Cabin1", "Cabin2", "Cabin3", "Cabin4"), sep=" ") %>% 
  mutate(Cabin1 = substr(Cabin1, 1, 1),
         Cabin2 = substr(Cabin2, 1, 1),
         Cabin3 = substr(Cabin3, 1, 1),
         Cabin4 = substr(Cabin4, 1, 1)) %>% 
  mutate(Cabin1 = if_else(Cabin1=="", "U", Cabin1),
         Cabin2 = if_else(is.na(Cabin2), "U", Cabin2),
         Cabin3 = if_else(is.na(Cabin3), "U", Cabin3),
         Cabin4 = if_else(is.na(Cabin4), "U", Cabin4))

# Nameの処理
train <- train %>% 
  separate("Name", into=c("Last_Name", "Title"), sep=",") %>% 
  mutate(Title = gsub("\\..+$", "", Title)) %>% 
  mutate(Title = gsub(" ", "", Title))

###################################
# 基礎集計や分布の確認
###################################
# 全体の生存率
# 0.3838384
sum(train$Survived)/nrow(train)

library(ggplot2)


# 2変数の関係の確認
# 対 Survived
# Pclass
S_table(train$Pclass, train$Survived)

# Sex
S_table(train$Sex, train$Survived)

# Title
S_table(train$Title, train$Survived)

# Embarked
S_table(train$Embarked, train$Survived)

# SibSp
S_table(train$SibSp, train$Survived)

# Parch
S_table(train$Parch, train$Survived)

# Cabin1
S_table(train$Cabin1, train$Survived)
S_table(train$Family_size, train$Survived)

# 気になった変数
table(train$Cabin1, train$Pclass)
rate_table(train$Embarked, train$Pclass)
rate_table(train$Embarked, train$Cabin1)

#Age (散布図)
plot(train$Age, train$Survived+runif(nrow(train), -0.3, 0.3))

# Age (密度トレース)
dat <- train %>% 
  select(Survived, Age) %>% 
  mutate(Survived = as.factor(Survived))
g <- ggplot(dat, aes(Age, colour=Survived, fill=Survived, alpha=0.5)) +
  geom_density()
plot(g)


# Fare (散布図)
plot(train$Fare, train$Survived+runif(nrow(train), -0.3, 0.3))

# Fare (密度トレース)
dat <- train %>% 
  select(Survived, Fare) %>% 
  mutate(Survived = as.factor(Survived))
g <- ggplot(dat, aes(Fare, colour=Survived, fill=Survived, alpha=0.5)) +
  geom_density()
plot(g)


# PclassとFareの密度トレース
dat <- train %>% 
  select(Pclass, Fare) %>% 
  mutate(Pclass = as.factor(Pclass))
g <- ggplot(dat, aes(Fare, colour=Pclass, fill=Pclass, alpha=0.5)) +
  geom_density()
plot(g)




# 3変数の関係の確認
# SurvivedとFareとPclass
plot(train$Fare, train$Pclass+runif(nrow(train), -0.3, 0.3), 
     col=ifelse(train$Survived == 1, "red", "blue"))

# SurvivedとFareとSex
plot(train$Fare, train$Sex+runif(nrow(train), -0.3, 0.3), 
     col=ifelse(train$Survived == 1, "red", "blue"))

# SurvivedとFareとPclass
dat <- train %>% 
  select(Survived, Pclass, Fare) %>% 
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass))
g <- ggplot(dat, aes(Fare, colour=Pclass, fill=Pclass, alpha=0.5)) +
  geom_density() +
  facet_wrap(~Survived, nrow=2) 
plot(g)

# Age (密度トレース)
dat <- train %>% 
  select(Survived, Age) %>% 
  mutate(Survived = as.factor(Survived))
g <- ggplot(dat, aes(Age, colour=Survived, fill=Survived, alpha=0.5)) +
  geom_density()
plot(g)


convDummies <- function(data, is.drop = FALSE){
  library(dummies)
  
  N <- ncol(data)
  row_names <- names(data)
  
  names_list <- c()
  new_data <- rep(NA, nrow(data))
  for(n in 1:N){
    unique_value <- sort(unique(data[,n]))
    dummied_data <- dummy(data[,n])
    
    if(is.drop == TRUE){
      new_data <- cbind(new_data, dummied_data[,-ncol(dummied_data)])
      names_list <- c(names_list, 
                      paste(row_names[n], unique_value, sep = ".")[-ncol(dummied_data)])
    } else {
      new_data <- cbind(new_data, dummied_data)
      names_list <- c(names_list, paste(row_names[n], unique_value, sep = "."))
    }
  }
  
  new_data <- as.data.frame(new_data)
  names(new_data) <- c("temp", names_list)
  
  return(new_data[,-1])
}

# データ整形関数
createData <- function(){
  library(dplyr)
  library(tidyr)
  library(ranger)
  
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  
  train$Embarked[train$Embarked == ""] <- "S"
  
  Survived <- train$Survived
  
  X <- train %>% 
    select(-Survived) %>% 
    bind_rows(test) %>% 
    select(-PassengerId, -Ticket)
  
  # Cabinの処理
  X <- X %>% 
    separate("Cabin", into=c("Cabin1", "Cabin2", "Cabin3", "Cabin4"), sep=" ") %>% 
    mutate(Cabin1 = substr(Cabin1, 1, 1),
           Cabin2 = substr(Cabin2, 1, 1),
           Cabin3 = substr(Cabin3, 1, 1),
           Cabin4 = substr(Cabin4, 1, 1)) %>% 
    mutate(Cabin1 = if_else(Cabin1=="", "U", Cabin1),
           Cabin2 = if_else(is.na(Cabin2), "U", Cabin2),
           Cabin3 = if_else(is.na(Cabin3), "U", Cabin3),
           Cabin4 = if_else(is.na(Cabin4), "U", Cabin4)) %>% 
    select(-c(Cabin2, Cabin3, Cabin4))
  
  X <- X %>% 
    mutate(is_Cabin = if_else(Cabin1 == "U", "No", "Yes"))
  
  # Nameの処理
  Name_list <- c("Master", "Miss", "Mr", "Mrs", "Rev")
  X <- X %>% 
    separate("Name", into=c("Last_Name", "Title"), sep=",") %>% 
    mutate(Title = gsub("\\..+$", "", Title)) %>% 
    mutate(Title = gsub(" ", "", Title)) %>% 
    mutate(Title = if_else(Title %in% Name_list, Title, "Otherwise"))
  
  # Family_sizeの追加
  X <- X %>% 
    mutate(Family_size = SibSp + Parch +1)
  
  # Familly_sizeで家族をカテゴリー分け
  X <- X %>% 
    mutate(Family_type = if_else(Family_size == 1, "singleton",
                                 if_else(2 <= Family_size & Family_size <= 4, "middle", "large")))
  
  # Ageの欠損値補完
  age_for_na <- 
    na.omit(X) %>% 
    group_by(Pclass, Title) %>% 
    summarise(age_ave = mean(Age))
  
  X <- X %>% 
    left_join(age_for_na) %>% 
    mutate(Age = if_else(is.na(Age), age_ave, Age)) %>% 
    select(-age_ave)
  
  X$Age[is.na(X$Age)] <- mean(X$Age[!is.na(X$Age) & X$SibSp == 0 & X$Parch == 0])
  
  X <- X %>%
    mutate(Age_desc = if_else(0 <= Age & Age <= 6, "0_6",
                              if_else(7 <= Age & Age <=10, "7_10",
                                      if_else(11 <= Age & Age <= 15, "11_15",
                                              if_else(16 <= Age & 20 <= Age, "16_20",
                                                      if_else(21 <= Age & Age <= 30, "21_30", "30_"))))))
  
  # Fareの欠損値補完と正規化
  X$Fare[is.na(X$Fare)] <- 0.0
  X$Fare <- (X$Fare - mean(X$Fare))/sd(X$Fare)
  
  # カテゴリー変数のダミー化
  df_category <- X %>% 
    select(Sex, Title, Pclass, Cabin1, is_Cabin, Family_type, Age_desc, Embarked)
  X_continuous <- X %>% 
    select(-c(Sex, Title, Pclass, Cabin1, is_Cabin, Family_type, Age_desc, Embarked))
  
  X_dummy <- convDummies(df_category, is.drop = TRUE)
  
  X <- cbind(X_continuous, X_dummy)
  
  
  # trainデータとtestデータに分割
  train_new <- X[1:891,]
  train_new <- data.frame(Survived = Survived) %>% 
    cbind(train_new)
  test_new <- X[892:1309,]
  
  # 自分以外の家族の生存割合を追加
  fam_suv <- train_new %>% 
    group_by(Last_Name) %>% 
    summarise(f_n = n(), s_n = sum(Survived))
  
  train_new <- train_new %>% 
    left_join(fam_suv) %>% 
    mutate(fam_suv_rate = if_else(f_n==1, 0, (s_n-Survived)/(f_n-1))) %>% 
    select(-f_n, -s_n, -Last_Name)
  
  test_new <- test_new %>% 
    left_join(fam_suv) %>% 
    mutate(fam_suv_rate = if_else(Family_size==1, 0, s_n/f_n)) %>% 
    select(-f_n, -s_n, -Last_Name)
  
  fam_suv_df <- train_new %>% 
    select(-Survived)
  fam_suv_df_na <- test_new[is.na(test_new$fam_suv_rate),] %>% 
    select(-fam_suv_rate)
  fam_suv.fit <- ranger(fam_suv_rate~., fam_suv_df[,29:31])
  pred_fam_suv <- predict(fam_suv.fit, fam_suv_df_na)
  test_new$fam_suv_rate[is.na(test_new$fam_suv_rate)] <- pred_fam_suv$predictions
  
  
  
  
  train_test <- list()
  train_test[[1]] <- train_new
  train_test[[2]] <- test_new
  
  return(train_test)
}

convDummies <- function(data, is.drop = FALSE){
  library(dummies)
  
  N <- ncol(data)
  row_names <- names(data)
  
  names_list <- c()
  new_data <- rep(NA, nrow(data))
  for(n in 1:N){
    unique_value <- sort(unique(data[,n]))
    dummied_data <- dummy(data[,n])
    
    if(is.drop == TRUE){
      new_data <- cbind(new_data, dummied_data[,-ncol(dummied_data)])
      names_list <- c(names_list, 
                      paste(row_names[n], unique_value, sep = ".")[-ncol(dummied_data)])
    } else {
      new_data <- cbind(new_data, dummied_data)
      names_list <- c(names_list, paste(row_names[n], unique_value, sep = "."))
    }
  }
  
  new_data <- as.data.frame(new_data)
  names(new_data) <- c("temp", names_list)
  
  return(new_data[,-1])
}

# データ整形関数
createData <- function(){
  library(dplyr)
  library(tidyr)
  library(ranger)
  
  train <- read.csv("train.csv")
  test <- read.csv("test.csv")
  
  train$Embarked[train$Embarked == ""] <- "S"
  
  Survived <- train$Survived
  
  X <- train %>% 
    select(-Survived) %>% 
    bind_rows(test) %>% 
    select(-PassengerId, -Ticket)
  
  # Cabinの処理
  X <- X %>% 
    separate("Cabin", into=c("Cabin1", "Cabin2", "Cabin3", "Cabin4"), sep=" ") %>% 
    mutate(Cabin1 = substr(Cabin1, 1, 1),
           Cabin2 = substr(Cabin2, 1, 1),
           Cabin3 = substr(Cabin3, 1, 1),
           Cabin4 = substr(Cabin4, 1, 1)) %>% 
    mutate(Cabin1 = if_else(Cabin1=="", "U", Cabin1),
           Cabin2 = if_else(is.na(Cabin2), "U", Cabin2),
           Cabin3 = if_else(is.na(Cabin3), "U", Cabin3),
           Cabin4 = if_else(is.na(Cabin4), "U", Cabin4)) %>% 
    select(-c(Cabin2, Cabin3, Cabin4))
  
  X <- X %>% 
    mutate(is_Cabin = if_else(Cabin1 == "U", "No", "Yes"))
  
  # Nameの処理
  Name_list <- c("Master", "Miss", "Mr", "Mrs", "Rev")
  X <- X %>% 
    separate("Name", into=c("Last_Name", "Title"), sep=",") %>% 
    mutate(Title = gsub("\\..+$", "", Title)) %>% 
    mutate(Title = gsub(" ", "", Title)) %>% 
    mutate(Title = if_else(Title %in% Name_list, Title, "Otherwise"))
  
  # Family_sizeの追加
  X <- X %>% 
    mutate(Family_size = SibSp + Parch +1)
  
  # Familly_sizeで家族をカテゴリー分け
  X <- X %>% 
    mutate(Family_type = if_else(Family_size == 1, "singleton",
                                 if_else(2 <= Family_size & Family_size <= 4, "middle", "large")))
  
  # Ageの欠損値補完
  age_for_na <- 
    na.omit(X) %>% 
    group_by(Pclass, Title) %>% 
    summarise(age_ave = mean(Age))
  
  X <- X %>% 
    left_join(age_for_na) %>% 
    mutate(Age = if_else(is.na(Age), age_ave, Age)) %>% 
    select(-age_ave)
  
  X$Age[is.na(X$Age)] <- mean(X$Age[!is.na(X$Age) & X$SibSp == 0 & X$Parch == 0])
  
  X <- X %>%
    mutate(Age_desc = if_else(0 <= Age & Age <= 6, "0_6",
                              if_else(7 <= Age & Age <=10, "7_10",
                                      if_else(11 <= Age & Age <= 15, "11_15",
                                              if_else(16 <= Age & 20 <= Age, "16_20",
                                                      if_else(21 <= Age & Age <= 30, "21_30", "30_"))))))
  
  # Fareの欠損値補完と正規化
  X$Fare[is.na(X$Fare)] <- 0.0
  X$Fare <- (X$Fare - mean(X$Fare))/sd(X$Fare)
  
  # カテゴリー変数のダミー化
  df_category <- X %>% 
    select(Sex, Title, Pclass, Cabin1, is_Cabin, Family_type, Age_desc, Embarked)
  X_continuous <- X %>% 
    select(-c(Sex, Title, Pclass, Cabin1, is_Cabin, Family_type, Age_desc, Embarked))
  
  X_dummy <- convDummies(df_category, is.drop = TRUE)
  
  X <- cbind(X_continuous, X_dummy)
  
  
  # trainデータとtestデータに分割
  train_new <- X[1:891,]
  train_new <- data.frame(Survived = Survived) %>% 
    cbind(train_new)
  test_new <- X[892:1309,]
  
  # 自分以外の家族の生存割合を追加
  fam_suv <- train_new %>% 
    group_by(Last_Name) %>% 
    summarise(f_n = n(), s_n = sum(Survived))
  
  train_new <- train_new %>% 
    left_join(fam_suv) %>% 
    mutate(fam_suv_rate = if_else(f_n==1, 0, (s_n-Survived)/(f_n-1))) %>% 
    select(-f_n, -s_n, -Last_Name)
  
  test_new <- test_new %>% 
    left_join(fam_suv) %>% 
    mutate(fam_suv_rate = if_else(Family_size==1, 0, s_n/f_n)) %>% 
    select(-f_n, -s_n, -Last_Name)
  
  fam_suv_df <- train_new %>% 
    select(-Survived)
  fam_suv_df_na <- test_new[is.na(test_new$fam_suv_rate),] %>% 
    select(-fam_suv_rate)
  fam_suv.fit <- ranger(fam_suv_rate~., fam_suv_df[,29:31])
  pred_fam_suv <- predict(fam_suv.fit, fam_suv_df_na)
  test_new$fam_suv_rate[is.na(test_new$fam_suv_rate)] <- pred_fam_suv$predictions
  
  
  
  
  train_test <- list()
  train_test[[1]] <- train_new
  train_test[[2]] <- test_new
  
  return(train_test)
}

# パラメータチューニング
# Cross Validation
svm.df <- train[(c("Survived", as.character(pd$Variable[17:31])))]
svm.df_t <- test[as.character(pd$Variable[17:31])]


tune_res <- tune.svm(as.factor(Survived)~., data=svm.df)
tune_res$best.model

svm.fit <- svm(as.factor(Survived)~., data=svm.df, cost=1, gamma=0.06666667)

# 予測
pred <- predict(svm.fit, svm.df_t)
prediction <- data.frame(PassengerId = 892:1309,
                         Survived = pred)

write.csv(prediction, "prediction.csv", row.names = FALSE)

















