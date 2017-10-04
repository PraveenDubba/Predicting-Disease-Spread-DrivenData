setwd('C:/Users/prave/Downloads/Praveen/UConn/Predictive modeling/My Learnings/Predicting Disease Spread/Predicting Disease Spread')
getwd()

# Importing Dataset
orig_dataset = read.csv('dengue_features_train.csv',header=T,sep = ',',quote = "")
labels = read.csv("dengue_labels_train.csv",header=T,sep = ',',quote = "")

# Merging labels with the train dataset
dataset = merge(x = orig_dataset,y=labels,by.x=c("city","year","weekofyear"),by.y =c("city","year","weekofyear"))
dataset = dataset[,c(25,4,1,2,3,5:24)]

# Exploring data for missing values and outliers
summary(dataset)
# Checking for outliers in the dependent/response variable
hist(dataset$total_cases)
boxplot(dataset$total_cases)  # confirms the presence of outliers


# Separating dataset based on city
dataset_iq = dataset[dataset$city=='iq',]
dataset_sj = dataset[dataset$city=='sj',]

#======================================== Analysis for San Juan city =========================
# Exploring missing values
summary(dataset_sj)
# Imputing missing values with its mean
for(i in 6: (ncol(dataset_sj))) 
{
  dataset_sj[[i]] = ifelse(is.na(dataset_sj[[i]]), mean(dataset_sj[[i]],na.rm = T), dataset_sj[[i]])  
}

# Computing z-scores
dataset_sj$z_scores =  (dataset_sj[[1]] - mean( dataset_sj[[1]]))/sd(dataset_sj[[1]])

#numer of outliers
length(dataset_sj$z_scores[abs(dataset_sj$z_scores) > 3])
# Since the number of outliers are very few (1.37% of the dataset), we can remove them

# Removing outliers
dataset_sj = dataset_sj[(abs(dataset_sj$z_scores) <= 3),]

# Removing the z-scores column
dataset_sj = dataset_sj[-26]

# Extracting month from Week_Start_Date
dataset_sj$month = as.factor(substring(dataset_sj$week_start_date,6,7))
# Converting week of year to factor
dataset_sj$weekofyear = as.factor(dataset_sj$weekofyear)

# library(dummies)
# creating dummy variables for month and week of year features
ds_month = dummy.data.frame(dataset_sj[c(5,26)])
dataset_sj = data.frame(dataset_sj[c(-5,-26)],ds_month)

#standardizing all the independent variables
for(i in 5: (ncol(dataset_sj)))
{
  dataset_sj[[i]] =  (dataset_sj[[i]] - mean( dataset_sj[[i]]))/sd(dataset_sj[[i]])
}


# ============================================= experimenting ===============================================
nrow(dataset_sj[abs(dataset_sj$ndvi_ne)>3 | abs(dataset_sj$ndvi_nw)>3 | abs(dataset_sj$ndvi_se)>3 | abs(dataset_sj$ndvi_sw)>3 |
                abs(dataset_sj$precipitation_amt_mm)>3 | abs(dataset_sj$reanalysis_dew_point_temp_k)>3 | abs(dataset_sj$reanalysis_min_air_temp_k)>3 |
                  abs(dataset_sj$reanalysis_precip_amt_kg_per_m2)>3 | abs(dataset_sj$reanalysis_relative_humidity_percent)>3 | 
                  abs(dataset_sj$reanalysis_sat_precip_amt_mm)>3 | abs(dataset_sj$station_precip_mm)>3 | abs(dataset_sj$station_min_temp_c)>3 |
                  abs(dataset_sj$station_diur_temp_rng_c)>3, ])

dataset_sj = (dataset_sj[abs(dataset_sj$ndvi_ne)<3 & abs(dataset_sj$ndvi_nw)<3 & abs(dataset_sj$ndvi_se)<3 & abs(dataset_sj$ndvi_sw)<3 &
                           abs(dataset_sj$precipitation_amt_mm)<3 & abs(dataset_sj$reanalysis_dew_point_temp_k)<3 & 
                           abs(dataset_sj$reanalysis_min_air_temp_k)<3 & abs(dataset_sj$station_diur_temp_rng_c)<3 &
                           abs(dataset_sj$reanalysis_precip_amt_kg_per_m2)<3 & abs(dataset_sj$reanalysis_relative_humidity_percent)<3 & 
                           abs(dataset_sj$reanalysis_sat_precip_amt_mm)<3 & abs(dataset_sj$station_precip_mm)<3 & abs(dataset_sj$station_min_temp_c)<3,])

summary(dataset_sj)
# ===========================================================================================================
# Checking for multi-collinearity
kappa(dataset_sj[-1:-4],exact = T)  
# This large value indicates that there is multi-collinearity issue in the dataset

#============================ Backward selection to remove multi-collinearity ===============================
linear_reg_tuning = lm(total_cases~.,data = dataset_sj[c(-2,-3,-4)])
linear_reg_tuning_summ = (summary(linear_reg_tuning))
coeff_df = data.frame(linear_reg_tuning_summ$coefficients)
dum = row.names.data.frame(coeff_df[!is.na(coeff_df$Pr...t..),])

for(i in 1:1000 ) {
  linear_reg_tuned = lm(total_cases~.,data = dataset_sj[c("total_cases",dum[-1])])
  linear_reg_tuned_summ = (summary(linear_reg_tuned))
  linear_reg_tuned_summ
  
  coeff_df = data.frame(linear_reg_tuned_summ$coefficients)
  if (max(coeff_df$Pr...t..) <= 0.1) {
    break
  }
  dum = row.names.data.frame(coeff_df[coeff_df$Pr...t.. < max(coeff_df$Pr...t..),])
}
dum
linear_reg_tuned_summ

# Check for multi-collinearity after backward elimination
kappa(dataset_sj[dum[-1]],exact = T)   # A lower value indicating less or no multi-collinearity
# ======================= Data splitting sj=========================
library(caTools)
set.seed(12345)
split = sample.split(dataset_sj$total_cases, SplitRatio = 0.7)
training_set = subset(dataset_sj[c("total_cases",dum[-1])], split == TRUE)
test_set = subset(dataset_sj[c("total_cases",dum[-1])], split == FALSE)

#==================================== Linear regression model for sj city ======== Rank #2=================================================
linear_reg_sj = lm(total_cases~.,data = training_set)
ln_reg_summ = (summary(linear_reg_sj))
ln_reg_summ

y_pred_linear_reg_sj = predict(linear_reg_sj, newdata = test_set[-1])

#test set rmse and r2
library(caret)
eval = setNames(data.frame(test_set$total_cases,y_pred_linear_reg_sj),c("obs","pred"))
defaultSummary(eval)
mean(abs(eval$obs - eval$pred))

# RMSE: 22.19   RSquared: 22  Mae: 16.16

#==================================== xgboost==================== Rank #3=================
library(xgboost)

xgb_train_ds= xgb.DMatrix(as.matrix(training_set[-1]), label=(training_set$total_cases))
xgb_test_ds= xgb.DMatrix(as.matrix(test_set[c(-1)]) ,label=test_set$total_cases)


xgboost_model = xgb.cv(data = xgb_train_ds,  
                       nrounds = 3000,  prediction = T, eta = 0.005, gamma = 10, lambda =3, lambda_bias=3,
                       base_score =mean(training_set$total_cases),colsample_bytree=0.9,subsample=0.95, max_depth=4,early_stopping_rounds = 30, objective = "reg:linear", nfold = 5) #, feval = R2, maximize = T )

xgboost_model = xgb.train(data = xgb_train_ds,  
                          nrounds = 578,  prediction = T, eta = 0.005, gamma = 10, lambda =3, lambda_bias=3,
                          base_score =mean(training_set$total_cases),colsample_bytree=0.9,subsample=0.95, max_depth=3,  objective = "reg:linear")

y_pred = predict(xgboost_model, newdata = xgb_test_ds)

#test set rmse and r2
# library(caret)
eval = setNames(data.frame(test_set$total_cases,y_pred),c("obs","pred"))
defaultSummary(eval)
mean(abs(eval$obs - eval$pred))
# RMSE: 22.2   RSqaured: 19  MAE: 16.87
#===================================Artificial Neural Network========== Rank #1===============
# install.packages("h2o")
library(h2o)

h2o.init(nthreads = -1)
ann_model_sj = h2o.deeplearning(y = 'total_cases',training_frame = as.h2o(training_set), nfolds = 5,keep_cross_validation_predictions = T,
                             activation = 'Rectifier',hidden = 1,epochs = 300, score_each_iteration = T,adaptive_rate = T,
                             train_samples_per_iteration = -2,stopping_metric = "MAE")
#eval defaultSummary

y_pred_ann_sj = h2o.predict(object = ann_model_sj, newdata = as.h2o(test_set[-1]))


#test set rmse and r2
# library(caret)
eval = setNames(data.frame(test_set$total_cases,as.data.frame(y_pred_ann_sj)),c("obs","pred"))
defaultSummary(eval)

mean(abs(eval$obs - eval$pred))

# RMSE 21.7    RSqaured 24  mae: 16.71

# x1 = as.data.frame(y_pred_ann_sj)
# y1 = test_set[1] - x1
# 
# #residual vs predicted plot
# ggplot() + geom_point(aes(x=x1 ,y=y1 ))
# 
# #actual vs predicted
# ggplot() + geom_point(aes(x=test_set[[1]] ,y= x1[[1]]))


# h2o.shutdown()
# y
#======================================== Analysis for iq city =========================
# Looking for missing values
summary(dataset_iq)
# Imputing missing values with its mean
for(i in 6: (ncol(dataset_iq))) 
{
  dataset_iq[[i]] = ifelse(is.na(dataset_iq[[i]]), mean(dataset_iq[[i]],na.rm = T), dataset_iq[[i]])  
}

# Computing z-scores
dataset_iq$z_scores =  (dataset_iq[[1]] - mean( dataset_iq[[1]]))/sd(dataset_iq[[1]])

#numer of outliers
length(dataset_iq$z_scores[abs(dataset_iq$z_scores) > 3])
# Since the number of outliers are very few (1.37% of the dataset), we can remove them

# Removing outliers
dataset_iq = dataset_iq[(abs(dataset_iq$z_scores) <= 3),]

# Removing z-scores column
dataset_iq = dataset_iq[-26]

# Extracting month
dataset_iq$month = as.factor(substring(dataset_iq$week_start_date,6,7))
# dataset_iq$year = as.factor(dataset_iq$year)
dataset_iq$weekofyear = as.factor(dataset_iq$weekofyear)

# library(dummies)
ds_month = dummy.data.frame(dataset_iq[c(5,26)])
dataset_iq = data.frame(dataset_iq[c(-5,-26)],ds_month)

#standardizing all the independent variables
for(i in 5: (ncol(dataset_iq)))
{
  dataset_iq[[i]] =  (dataset_iq[[i]] - mean( dataset_iq[[i]]))/sd(dataset_iq[[i]])
}


# ======================================== experimenting =============================================================
nrow(dataset_iq[abs(dataset_iq$ndvi_ne)>3 | abs(dataset_iq$ndvi_nw)>3 | abs(dataset_iq$ndvi_se)>3 | abs(dataset_iq$ndvi_sw)>3 |
                  abs(dataset_iq$precipitation_amt_mm)>3 | abs(dataset_iq$reanalysis_dew_point_temp_k)>3 | abs(dataset_iq$reanalysis_min_air_temp_k)>3 |
                  abs(dataset_iq$reanalysis_precip_amt_kg_per_m2)>3 | abs(dataset_iq$reanalysis_relative_humidity_percent)>3 | 
                  abs(dataset_iq$reanalysis_sat_precip_amt_mm)>3 | abs(dataset_iq$station_precip_mm)>3 | abs(dataset_iq$station_min_temp_c)>3 |
                  abs(dataset_iq$station_diur_temp_rng_c)>3 | abs(dataset_iq$station_avg_temp_c)>3 | abs(dataset_iq$reanalysis_air_temp_k)>3, ])


dataset_iq = (dataset_iq[abs(dataset_iq$ndvi_ne)<3 & abs(dataset_iq$ndvi_nw)<3 & abs(dataset_iq$ndvi_se)<3 & abs(dataset_iq$ndvi_sw)<3 &
                           abs(dataset_iq$precipitation_amt_mm)<3 & abs(dataset_iq$reanalysis_dew_point_temp_k)<3 & 
                           abs(dataset_iq$reanalysis_min_air_temp_k)<3 & abs(dataset_iq$station_diur_temp_rng_c)<3 &
                           abs(dataset_iq$reanalysis_precip_amt_kg_per_m2)<3 & abs(dataset_iq$reanalysis_relative_humidity_percent)<3 & 
                           abs(dataset_iq$reanalysis_sat_precip_amt_mm)<3 & abs(dataset_iq$station_precip_mm)<3 & abs(dataset_iq$station_min_temp_c)<3 &
                           abs(dataset_iq$station_avg_temp_c)<3 & abs(dataset_iq$reanalysis_air_temp_k)<3, ])

summary(dataset_iq)
# ====================================================================================================================


# Check for multi-collinearity
kappa(dataset_iq[-1:-4],exact = T)  # This large value indicates that there is multi-collinearity issue 

#============================ Backward selection to remove multi-collinearity in dataset_iq ===============================
linear_reg_tuning = lm(total_cases~.,data = dataset_iq[c(-2,-3,-4)])
linear_reg_tuning_summ = (summary(linear_reg_tuning))
coeff_df = data.frame(linear_reg_tuning_summ$coefficients)
dum = row.names.data.frame(coeff_df[!is.na(coeff_df$Pr...t..),])

for(i in 1:1000 ) {
  linear_reg_tuned = lm(total_cases~.,data = dataset_iq[c("total_cases",dum[-1])])
  linear_reg_tuned_summ = (summary(linear_reg_tuned))
  linear_reg_tuned_summ
  
  coeff_df = data.frame(linear_reg_tuned_summ$coefficients)
  if (max(coeff_df$Pr...t..) <= 0.1) {
    break
  }
  dum = row.names.data.frame(coeff_df[coeff_df$Pr...t.. < max(coeff_df$Pr...t..),])
}
dum
linear_reg_tuned_summ
# ======================= Data splitting iq=========================
# library(caTools)
set.seed(54321)
split = sample.split(dataset_iq$total_cases, SplitRatio = 0.7)
training_set = subset(dataset_iq[c("total_cases",dum[-1])], split == TRUE)
test_set = subset(dataset_iq[c("total_cases",dum[-1])], split == FALSE)

#==================================== Linear regression model iq========== Rank #1=========================================
linear_reg_iq = lm(total_cases~.,data = training_set)
ln_reg_summ = (summary(linear_reg_iq))
ln_reg_summ

y_pred_linear_reg_iq = predict(linear_reg_iq, newdata = test_set[-1])

#test set rmse and r2
# library(caret)
eval = setNames(data.frame(test_set$total_cases,y_pred_linear_reg_iq),c("obs","pred"))
defaultSummary(eval)
mean(abs(eval$obs - eval$pred))

# RMSE: 6.88   RSquared: 18.34  Mae: 4.77

# x1 = as.data.frame(y_pred_linear_reg_iq)
# y1 = test_set[1] - x1
# hist(test_set$total_cases - x1$y_pred_linear_reg_iq)
# #residual vs predicted plot
# ggplot() + geom_point(aes(x=x1 ,y=y1 ))
# 
# #actual vs predicted
# ggplot() + geom_point(aes(x=test_set[[1]] ,y= x1[[1]]))


#===================================Artificial Neural Network==========Rank #2===============
# install.packages("h2o")
library(h2o)

h2o.init(nthreads = -1)
ann_model_iq = h2o.deeplearning(y = 'total_cases',training_frame = as.h2o(training_set), nfolds = 5,keep_cross_validation_predictions = T,
                                activation = 'Rectifier',hidden = 1,epochs = 300, score_each_iteration = T,adaptive_rate = T,
                                train_samples_per_iteration = -2,stopping_metric = "MAE")
#eval defaultSummary

y_pred_ann_iq = h2o.predict(object = ann_model_iq, newdata = as.h2o(test_set[-1]))


#test set rmse and r2
# library(caret)
eval = setNames(data.frame(test_set$total_cases,as.data.frame(y_pred_ann_iq)),c("obs","pred"))
defaultSummary(eval)

mean(abs(eval$obs - eval$pred))

# RMSE 7.19    RSqaured 11.21  mae: 4.79

# x1 = as.data.frame(y_pred_ann_iq)
# y1 = test_set[1] - x1
# 
# #residual vs predicted plot
# ggplot() + geom_point(aes(x=x1 ,y=y1 ))
# 
# #actual vs predicted
# ggplot() + geom_point(aes(x=test_set[[1]] ,y= x1[[1]]))


# h2o.shutdown()
# y
#=========================== test set prep =================
test_data = read.csv('dengue_features_test.csv',header=T,sep = ',',quote = "")

test_data = test_data[,c(4,1,2,3,5:24)]

# creating dummies for month and Week features 
test_data$month = as.factor(substring(test_data$week_start_date,6,7))
test_data$weekofyear = as.factor(test_data$weekofyear)
# library(dummies)
ts_ds_month = dummy.data.frame(test_data[c(4,25)])
test_data = data.frame(test_data[c(-25)],ts_ds_month)

# Understanding the data 
summary(test_data)
boxplot(dataset$ndvi_ne)
hist(dataset$ndvi_ne)

# Separating dataset based on city
testset_iq = test_data[test_data$city=='iq',]
testset_sj = test_data[test_data$city=='sj',]

# Imputing missing values with its mean
for(i in 5: (ncol(testset_sj))) 
{
  testset_sj[[i]] = ifelse(is.na(testset_sj[[i]]), mean(testset_sj[[i]],na.rm = T), testset_sj[[i]])  
  testset_iq[[i]] = ifelse(is.na(testset_iq[[i]]), mean(testset_iq[[i]],na.rm = T), testset_iq[[i]])
}

#standardizing all the independent variables
for(i in 5: (ncol(testset_sj)))
{
  testset_sj[[i]] =  (testset_sj[[i]] - mean( testset_sj[[i]]))/sd(testset_sj[[i]])
  testset_iq[[i]] =  (testset_iq[[i]] - mean( testset_iq[[i]]))/sd(testset_iq[[i]])
}

summary(testset_sj)
test_final = rbind.data.frame(testset_sj,testset_iq)

#========================== predictions ========================
# ann predictions
y_pred_test_ann_sj = h2o.predict(object = ann_model_sj, newdata = as.h2o(testset_sj))
y_pred_test_ann_iq = h2o.predict(object = ann_model_iq, newdata = as.h2o(testset_iq))

y_pred_test_ann = rbind.data.frame(as.data.frame(y_pred_test_ann_sj),as.data.frame(y_pred_test_ann_iq))

submission_file = data.frame(test_final[c("city","year","weekofyear")], y_pred_test_ann)
submission_file$predict = round(submission_file$predict)
colnames(submission_file)[4] = 'total_cases'

# ann submission file
write.csv(submission_file,"submission_file_sep_city_X_outliers_ann.csv",row.names = F)

# Linear reg predictions
y_pred_test_lin_reg_sj = predict(linear_reg_sj, newdata = testset_sj)
y_pred_test_lin_reg_iq = predict(linear_reg_iq, newdata = testset_iq)

y_pred_test_lin = c(y_pred_test_lin_reg_sj,y_pred_test_lin_reg_iq)

submission_file = data.frame(test_final[c("city","year","weekofyear")], y_pred_test_lin)
submission_file$y_pred_test_lin = round(submission_file$y_pred_test_lin)
colnames(submission_file)[4] = 'total_cases'

write.csv(submission_file,"submission_file_sepcity_X_outliers.csv",row.names = F)
