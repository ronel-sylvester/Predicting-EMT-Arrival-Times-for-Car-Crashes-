library(readr)
library(dplyr)
library(ggplot2)
library(caret)
df_2015 <- read_csv("accident_2015.csv")
df_2016 <- read_csv("accident_2016.csv")
View(colnames(df_2015))
colnames(df_2016)

str(df_2015)
summary(df_2015)
unique(df_2015$first_harmful_event_name)
length(is.na(df_2015))

df_all <- rbind(df_2015, df_2016)

df <- df_all%>% 
  dplyr::select(2, 5:6, 8:9, 13:19, 21, 23, 27, 38:39, 41:43, 45, 47, 53, 56:61, 68:69)

df <- data.frame(unclass(df))
View(colnames(df_all))

hist(df$hour_of_crash)
table(df$day_of_crash)
str(df)
summary(df)
as.data.frame(sort(table(df$state_name)))
barplot(table(df$state_name))

df$number_of_persons <- df$number_of_persons_not_in_motor_vehicles_in_transport_mvit +
  df$number_of_persons_in_motor_vehicles_in_transport_mvit
  
df <- df %>% 
  dplyr::select(-number_of_persons_in_motor_vehicles_in_transport_mvit, -number_of_persons_not_in_motor_vehicles_in_transport_mvit)
df <- df%>% 
  dplyr::filter(hour_of_crash != 99 & hour_of_notification < 24 & hour_of_arrival_at_scene < 24 &
                  minute_of_crash < 60 & minute_of_notification < 60 & 
                  minute_of_arrival_at_scene < 60 & national_highway_system != 9 &
                  land_use_name != "Not Reported" & land_use_name !="Unknown" & 
                  land_use_name != "Trafficway Not in State Inventory")

df$time_of_notification <- paste(df$year_of_crash,"-", df$month_of_crash,"-", df$day_of_crash," ", 
                                              df$hour_of_notification,":", df$minute_of_notification, sep="")
df$time_of_arrival <- paste(df$year_of_crash,"-", df$month_of_crash,"-", df$day_of_crash," ", 
                            df$hour_of_arrival_at_scene, ":", df$minute_of_arrival_at_scene, sep="")
df$time_of_notification <- as.POSIXct(df$time_of_notification,format="%Y-%m-%d %H:%M")
df$time_of_arrival <- as.POSIXct(df$time_of_arrival,format="%Y-%m-%d %H:%M")
df$time_it_took <- as.numeric((df$time_of_arrival - df$time_of_notification)/60)
hist(df$time_it_took)

df <- df%>% 
  dplyr::filter(time_it_took > 0)

df$time_bin <- cut(df$time_it_took, c(0, 2, 5, 10, 15, 30, 60, 251))
barplot(table(df$time_bin))
boxplot(df$time_it_took)

levels(df$time_bin) <- c("class1", "class2", "class3", "class4", "class5", "class6", "class7")
View(colnames(df))
df2 <- df
df <- df%>% 
  dplyr::select("state_name", "month_of_crash", "day_of_week", "hour_of_crash",
                "land_use_name", "functional_system_name", "route_signing_name", 
                "relation_to_junction_specific_location_name", 
                "type_of_intersection", "relation_to_trafficway_name", 
                "light_condition_name", "atmospheric_conditions_name", "national_highway_system",
                "time_of_notification", "time_of_arrival", "time_it_took", "time_bin")
write.csv(df, file = "EMT_bins.csv")

#################################################################
library(splitstackshape)
index <- stratified(df, "time_bin", .75, bothSets = TRUE)
trainSet <- index$SAMP1
testSet <- index$SAMP2
write.csv(trainSet, file = "EMT_bins_train.csv")
write.csv(testSet, file = "EMT_bins_test.csv")


fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

View(colnames(df))
predictors <- c("state_name", "month_of_crash", "day_of_week", "hour_of_crash",
                "land_use_name", "functional_system_name", "route_signing_name", 
                "relation_to_junction_specific_location_name", 
                "type_of_intersection", "relation_to_trafficway_name", 
                "light_condition_name", "atmospheric_conditions_name", "national_highway_system")
outcomeName <- 'time_bin'

barplot(table(df2$bin_notify), xlab = "Time Bin", ylab = "Frequency", main = "Notification Recieving Time")
library(naivebayes)
library(e1071)

model_tree <- train(x= trainSet[,..predictors],trainSet$time_bin,method='ctree2',trControl=fitControl, tuneLength = 3)
testSet$pred_tree<-predict(object = model_tree,testSet[,..predictors])
confusionMatrix(testSet$time_bin,testSet$pred_tree)

model_rf <- train(x= trainSet[,..predictors],trainSet$time_bin,method='rf',trControl=fitControl, tuneLength = 3)
testSet$pred_rf<-predict(object = model_rf,testSet[,..predictors])
confusionMatrix(testSet$time_bin,testSet$rf)

model_nb <- naiveBayes(time_bin ~ state_name + month_of_crash + day_of_week + hour_of_crash +
                         land_use_name + functional_system_name + route_signing_name + 
                         relation_to_junction_specific_location_name + 
                         type_of_intersection + relation_to_trafficway_name + 
                         light_condition_name + atmospheric_conditions_name, data=trainSet, laplace = 59)

testSet$pred_nb <- predict(object = model_nb,testSet[,..predictors])
confusionMatrix(testSet$time_bin,testSet$pred_nb)

library(readxl)
levels(trainSet$atmospheric_conditions_name)

eavg <- read.csv("EMT_Average.csv")
eavg = tail(eavg, -2)
View(colnames(eavg))
eavg <- eavg[ -c(1:13, 71,72)]
eavg <- eavg[ -c(51:57)]
eavg$time_bin <- factor(eavg$time_bin)
levels(eavg$time_bin) <- c("class1", "class2", "class3", "class4", "class5", "class6", "class7")

eavg[,c(9:50)] <- apply(eavg[,c(9:50)], 2, function(x) as.numeric(as.character(x)))
Averages_EMT <- vector()

for (i in 1:nrow(eavg)) {
  for (j in 1:7){
    j_char <- as.character(j)
    class_j <- paste("class", j_char, sep = "")
    col_index <-grepl(class_j, colnames(eavg))
    sub_class <- eavg[i,colnames(eavg)[grepl(class_j, colnames(eavg))]]
    mean_j <- rowMeans(sub_class)
    Averages_EMT <- rbind(Averages_EMT, c(i, j, class_j,mean_j))
  }
}

Averages_EMT <- as.data.frame(Averages_EMT)
names(Averages_EMT) <- c("Row", "Group", "Class", "Average")
new_EMT <- Averages_EMT
new_EMT$Average <- as.numeric(as.character(new_EMT$Average))

new_EMT <- new_EMT%>%
  group_by(Row) %>%
  filter(Average == max(Average))

confusionMatrix(eavg$time_bin, new_EMT$Class)




summary(trainSet)
table(df$atmospheric_conditions_name)

#######################################################################################################
df_norm <- df
df_norm$log_time <- log(df_norm$time_it_took)
hist(df_norm$log_time)
dummy_df <- fastDummies::dummy_cols(df_norm, select_columns = predictors)
View(colnames(dummy_df))
dummy_df <- dummy_df%>%
  dplyr::select(-predictors, -time_of_notification, -time_of_arrival, -time_it_took, -time_bin)
sample <- sample(nrow(dummy_df), .8 * nrow(dummy_df))

df_train <- dummy_df[sample, ]
df_test <- dummy_df[-sample, ]
head(df_train)

library(gam)
library(MASS)
train_1 <- lm(log_time ~., data = df_train)
summary(train_1)

step_train_1 <- stepAIC(train_1, direction = "both")
summary(step_train_1)

predict_df <- predict(step_train_1, df_test)
RMSE(df_test$log_time, predict_df)
summary(t(as.matrix(coef(step_train_1))))

plot(10^predict_df, 10^df_test$log_time)
cbind(predict_df, df_test$log_time)

hist(df_test$number_of_persons)
summary(step_train1)
attach(stepwise_hour)
hour_2015 <- cbind(df, state_Texas, state_Hawaii, state_Oregon,
                   state_Florida, state_Vermont, state_Delaware, state_Minnesota,
                   state_Tennessee, state_Connecticut, state_Massachusetts,
                   `state_New Hampshire`) 
colnames(hour_2015)
detach(stepwise_hour)

hour_2015 <- hour_2015%>%
  dplyr::select(-state_name)

hour_final <- lm(hour_of_crash ~., data = hour_2015)
steps_hour_final <- stepAIC(hour_final, direction = "both")
summary(steps_hour_final)


hour_train <- hour_2015[sample, ]
hour_test <- hour_2015[-sample, ]

hour_final <- lm(hour_of_crash ~., data = hour_train)
plot(hour_final)
steps_hour_final <- stepAIC(hour_final, direction = "both")
summary(steps_hour_final)

###########################################################
df2$time_of_crash <- paste(df2$year_of_crash,"-", df2$month_of_crash,"-", df2$day_of_crash," ", 
                            df2$hour_of_crash, ":", df2$minute_of_crash, sep="")
df2$time_of_crash <- as.POSIXct(df2$time_of_crash,format="%Y-%m-%d %H:%M")
df2$notification_time <- as.numeric(df2$time_of_notification - df2$time_of_crash)/60
hist(df2$notification_time)

df2 <- df2%>% 
  dplyr::filter(notification_time >= 0)

table(df2$notification_time)
df2$bin_notify <- cut(df2$notification_time, c(0, 1, 2, 5, 15, 30, 60, 1419))
barplot(table(df2$bin_notify))

levels(df2$bin_notify) <- c("class1", "class2", "class3", "class4", "class5", "class6", "class7")
View(colnames(df))
df2 <- df2%>% 
  dplyr::select("state_name", "month_of_crash", "day_of_week", "hour_of_crash",
                "land_use_name", "functional_system_name", "route_signing_name", 
                "relation_to_junction_specific_location_name", 
                "type_of_intersection", "relation_to_trafficway_name", 
                "light_condition_name", "atmospheric_conditions_name", "national_highway_system",
                "time_of_notification", "time_of_crash", "notification_time", "bin_notify")
write.csv(df, file = "EMT_bins.csv")

index_notify <- stratified(df2, "bin_notify", .75, bothSets = TRUE)
trainSet_notify <- index_notify$SAMP1
testSet_notify <- index_notify$SAMP2
write.csv(trainSet_notify, file = "notify_bins_train.csv")
write.csv(testSet_notify, file = "notify_bins_test.csv")

model_tree2 <- train(trainSet_notify[,..predictors],trainSet_notify$bin_notify,method='ctree2',trControl=fitControl, tuneLength = 3)
testSet_notify$pred_tree <-predict(object = model_tree2,testSet_notify[,..predictors])
confusionMatrix(testSet_notify$time_bin,testSet_notify$pred_tree)


model_nb <- naiveBayes(time_bin ~ state_name + month_of_crash + day_of_week + hour_of_crash +
                         land_use_name + functional_system_name + route_signing_name + 
                         relation_to_junction_specific_location_name + 
                         type_of_intersection + relation_to_trafficway_name + 
                         light_condition_name + atmospheric_conditions_name, data=trainSet, laplace = 59)

testSet$pred_nb <- predict(object = model_nb,testSet[,..predictors])
confusionMatrix(testSet$time_bin,testSet$pred_nb)

navg <- read.csv("Notification_Average.csv")
navg = tail(navg, -2)
navg <- navg[ -c(1:13, 71,72)]
navg <- navg[ -c(51:57)]
navg$bin_notify <- factor(navg$bin_notify)
levels(navg$bin_notify) <- c("class1", "class2", "class3", "class4", "class5", "class6", "class7")

View(colnames(navg))
navg[,c(9:50)] <- apply(navg[,c(9:50)], 2, function(x) as.numeric(as.character(x)))
Averages <- vector()

for (i in 1:nrow(navg)) {
  for (j in 1:7){
    j_char <- as.character(j)
    class_j <- paste("class", j_char, sep = "")
    col_index <-grepl(class_j, colnames(navg))
    sub_class <- navg[i,colnames(navg)[grepl(class_j, colnames(navg))]]
    mean_j <- rowMeans(sub_class)
    Averages <- rbind(Averages, c(i, j, class_j,mean_j))
  }
}

Averages <- as.data.frame(Averages)
names(Averages) <- c("Row", "Group", "Class", "Average")
new <- Averages
new$Average <- as.numeric(as.character(new$Average))

new <- new%>%
  group_by(Row) %>%
  filter(Average == max(Average))

confusionMatrix(navg$bin_notify, new$Class)


ggplot(data = Shiny_EMT, aes(x = reorder(Var1, Team_Height, FUN = mean), y = Freq)) + 
  geom_bar(stat = "identity", aes(fill = Team_Height)) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  xlab("Teams") + ylab("Number of Wins") + ggtitle("Team Wins in Order of Mean Height")

sum(df_all$number_of_fatalities)
