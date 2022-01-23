rm(list=ls()) # Clear environment
cat("\014")   # Clear Console
dev.off()     # Clear plots
# packages
library(dplyr)    # For data preparation
library(ROCR)     # For evaluation metrics
library(caret)    # For confusion matrix
library(e1071)    # For svm

#read data
hotel_data <- read.csv("hotel_bookings.csv")
#colnames(hotel_data)
#dim(hotel_data)

#data preparation
# Move out the obvirous outier
hotel_data<-subset(hotel_data,hotel_data$adr<4000)
# classification data set as factor
hotel_data<-hotel_data%>%
  mutate(
    hotel=as.factor(hotel),      
    is_canceled=as.factor(is_canceled),
    meal=as.factor(meal),
    country=as.factor(country),
    market_segment=as.factor(market_segment),
    distribution_channel=as.factor(distribution_channel),
    is_repeated_guest=as.factor(is_repeated_guest),
    reserved_room_type=as.factor(reserved_room_type),
    assigned_room_type=as.factor(assigned_room_type),
    deposit_type=as.factor(deposit_type),
    customer_type=as.factor(customer_type),
    reservation_status=as.factor(reservation_status),
    agent=as.factor(agent),
    company=as.factor(company),
    arrival_date_day_of_month=as.factor(arrival_date_day_of_month),
    arrival_date_month=as.factor(arrival_date_month),
    arrival_date_year=as.factor(arrival_date_year)
   
)
hotel_data[is.na(hotel_data)]<-0

# training data and test data preparation
a = 1:nrow(hotel_data)
set.seed(1)
ind   = sample(a, floor(nrow(hotel_data)*0.9), replace=FALSE)  
train_choose = hotel_data[ind,]
test  = hotel_data[-ind,]
# SVM model encounter with problem with large scale of data
# choose actual train data again(reduce the scale)
b = 1:nrow(train_choose)
set.seed(1)
ind   = sample(a, floor(nrow(train_choose)*0.2), replace=FALSE)  
train = train_choose[ind,]

# show the changes
#table(hotel_data$hotel)
#table(hotel_data$is_canceled)
#table(hotel_data$lead_time)
#table(hotel_data$adults)
#table(hotel_data$children)
#table(hotel_data$babies)
#table(hotel_data$meal)
#table(hotel_data$market_segment)
#table(hotel_data$distribution_channel)
#table(hotel_data$is_repeated_guest)
#table(hotel_data$previous_cancellations)
#table(hotel_data$previous_bookings_not_canceled)
#table(hotel_data$reserved_room_type)
#table(hotel_data$deposit_type)
#table(hotel_data$days_in_waiting_list)
#table(hotel_data$customer_type)
#table(hotel_data$adr)
#table(hotel_data$required_car_parking_spaces)
#table(hotel_data$is_repeated_guest)


train_data <- train[c('hotel','is_canceled','lead_time','adults','children','babies','meal',
                         'market_segment','distribution_channel','is_repeated_guest',
                         'previous_cancellations','previous_bookings_not_canceled','reserved_room_type',
                         'deposit_type','days_in_waiting_list','customer_type','adr',
                         'required_car_parking_spaces')]
test_data <- test[c('hotel','is_canceled','lead_time','adults','children','babies','meal',
                      'market_segment','distribution_channel','is_repeated_guest',
                      'previous_cancellations','previous_bookings_not_canceled','reserved_room_type',
                      'deposit_type','days_in_waiting_list','customer_type','adr',
                      'required_car_parking_spaces')]

tune_out_prob=tune(svm,
              is_canceled~.,
              data=train_data,          
              ranges =list(cost=c(0.01,5,10,100)), probability=TRUE)
summary(tune_out_prob)

#apply to the test data
best_mod_prob = tune_out_prob$best.model   # best_mod is trained based on entire data
summary(best_mod_prob)  

best.pred = predict(best_mod_prob, newdata = test_data, probability=TRUE)
acc_best_prob = mean(as.numeric(best.pred==test_data$is_canceled))  
acc_best_prob 
test_data$Yhat_svm = attr(best.pred,'probabilities')[,1]   
head(test_data)
test_data$Yhat_svm
attach(test_data)

#try cut_off = 0.5
class_svm <- function(x){ifelse(Yhat_svm > x, 1, 0)}
predicted_svm1 = class_svm(0.5)
CF1 <- confusionMatrix(factor(predicted_svm1), is_canceled, positive=as.character(1))
CF1
CF1$table    
CF1$overall 
CF1$byClass
# create a prediction object
predict_svm <- prediction(Yhat_svm, is_canceled)

# accuracy curve
acc_svm = performance(predict_svm,'acc')
plot.new()
plot(acc_svm, col='cyan3', lwd=2)
title("Accuracy curves")
legend(0.7, 0.5 ,"SVM", 
       lty = c(1,1,1), 
       lwd = c(2,2,2),
       col =  "cyan3",
       ncol=1, cex=0.9, y.intersp=1.2)

#ROC Curve
ROC_svm <- performance(predict_svm, "tpr", "fpr")
plot.new()
plot(ROC_svm, add = TRUE, col= "cyan3")
abline(0,1, col = "black")
title("ROC curves")
legend(0.7, 0.5 ,"SVM", 
       lty = c(1,1,1), 
       lwd = c(2,2,2),
       col =  "cyan3",
       ncol=1, cex=0.9, y.intersp=1.2)

#AUC value
auc_svm  = performance(predict_svm,"auc")
auc_svm@y.values 

#CRC Curve
CRC_svm <- performance(predict_svm, "tpr", "rpp")
plot.new()
plot(CRC_svm, add = TRUE, col= "cyan3")
abline(0,1, col = "black")
title("CRC curves")
legend(0.7, 0.5 ,"SVM", 
       lty = c(1,1,1), 
       lwd = c(2,2,2),
       col =  "cyan3",
       ncol=1, cex=0.9, y.intersp=1.2)

#Lift Curves 
LIFT_svm <- performance(predict_svm, "lift", "rpp")
plot.new()
plot(LIFT_svm, add = TRUE, col= "cyan3")
abline(1,0, col = "black")
title("Lift curves")
legend(0.7, 0.5 ,"SVM", 
       lty = c(1,1,1), 
       lwd = c(2,2,2),
       col =  "cyan3",
       ncol=1, cex=0.9, y.intersp=1.2)



