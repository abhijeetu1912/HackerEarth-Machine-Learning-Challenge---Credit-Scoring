library(doParallel)
cl <- makeCluster(detectCores()-1)
stopCluster(cl)



####loading data sets
train_indessa <- read.csv("C:/Users/Abhijeet/Desktop/Kaggle/Hacker Rank Bank Defaulter/train_indessa.csv", header = TRUE, stringsAsFactors = TRUE)
test_indessa <- read.csv("C:/Users/Abhijeet/Desktop/Kaggle/Hacker Rank Bank Defaulter/test_indessa.csv", header = TRUE, stringsAsFactors = TRUE)

loan_status <- train_indessa$loan_status

str(train_indessa)
str(test_indessa)

summary(train_indessa$batch_enrolled)

#View(train_indessa)
#View(test_indessa)

####extracting numeric part of some variables
library(stringr)

train_indessa$term <- as.factor(str_extract(train_indessa$term, "[[:digit:]]+"))
test_indessa$term <- as.factor(str_extract(test_indessa$term, "[[:digit:]]+"))

train_indessa$batch_enrolled <- as.factor(str_extract(train_indessa$batch_enrolled, "[[:digit:]]+"))
test_indessa$batch_enrolled <- as.factor(str_extract(test_indessa$batch_enrolled, "[[:digit:]]+"))

train_indessa$last_week_pay <- as.integer(str_extract(train_indessa$last_week_pay, "[[:digit:]]+"))
test_indessa$last_week_pay <- as.integer(str_extract(test_indessa$last_week_pay, "[[:digit:]]+"))

####removing unwanted variables
train_indessa <- train_indessa[,-c(6,10,16,19,20,37,39)]
test_indessa <- test_indessa[,-c(6,10,16,19,20,37,39)]

####outliers treatment

colnames(train_indessa)[sapply(train_indessa, function(x) is.numeric(x))]
                        
outlier <- function(x) {
  x[x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)] <- (quantile(x,0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE))
  x[x > quantile(x,0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)] <- (quantile(x,0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
  x
}

#train data
boxplot(train_indessa$loan_amnt)

boxplot(train_indessa$funded_amnt)

boxplot(train_indessa$funded_amnt_inv)

boxplot(train_indessa$int_rate)
train_indessa$int_rate <- outlier(train_indessa$int_rate)

boxplot(train_indessa$annual_inc)
train_indessa$annual_inc <- outlier(train_indessa$annual_inc)

boxplot(train_indessa$dti)
train_indessa$dti <- outlier(train_indessa$dti)

boxplot(train_indessa$delinq_2yrs)
train_indessa$delinq_2yrs <- outlier(train_indessa$delinq_2yrs) ##its outlier treatment fills all values with zero

boxplot(train_indessa$inq_last_6mths)  
train_indessa$inq_last_6mths <- outlier(train_indessa$inq_last_6mths)

boxplot(train_indessa$mths_since_last_delinq)  
train_indessa$mths_since_last_delinq <- outlier(train_indessa$mths_since_last_delinq)

boxplot(train_indessa$mths_since_last_record)  

boxplot(train_indessa$open_acc)  
train_indessa$open_acc <- outlier(train_indessa$open_acc)

boxplot(train_indessa$pub_rec)  
train_indessa$pub_rec <- outlier(train_indessa$pub_rec) ##its outlier treatment fills all values with zero

boxplot(train_indessa$revol_bal)  
train_indessa$revol_bal <- outlier(train_indessa$revol_bal)

boxplot(train_indessa$revol_util)  
train_indessa$revol_util <- outlier(train_indessa$revol_util)

boxplot(train_indessa$total_acc)  
train_indessa$total_acc <- outlier(train_indessa$total_acc)

boxplot(train_indessa$total_rec_int)  
train_indessa$total_rec_int <- outlier(train_indessa$total_rec_int)

boxplot(train_indessa$total_rec_late_fee)  
train_indessa$total_rec_late_fee <- outlier(train_indessa$total_rec_late_fee) ##its outlier treatment fills all values with zero

boxplot(train_indessa$recoveries)  
train_indessa$recoveries <- outlier(train_indessa$recoveries) ##its outlier treatment fills all values with zero

boxplot(train_indessa$collection_recovery_fee)  
train_indessa$collection_recovery_fee <- outlier(train_indessa$collection_recovery_fee) ##its outlier treatment fills all values with zero

boxplot(train_indessa$collections_12_mths_ex_med)  
train_indessa$collections_12_mths_ex_med <- outlier(train_indessa$collections_12_mths_ex_med) ##its outlier treatment fills all values with zero

boxplot(train_indessa$last_week_pay)  
train_indessa$last_week_pay <- outlier(train_indessa$last_week_pay)

boxplot(train_indessa$acc_now_delinq)
train_indessa$acc_now_delinq <- outlier(train_indessa$acc_now_delinq) ##its outlier treatment fills all values with zero

boxplot(train_indessa$tot_coll_amt)
train_indessa$tot_coll_amt <- outlier(train_indessa$tot_coll_amt) ##its outlier treatment fills all values with zero

boxplot(train_indessa$tot_cur_bal)
train_indessa$tot_cur_bal <- outlier(train_indessa$tot_cur_bal)

boxplot(train_indessa$total_rev_hi_lim)
train_indessa$total_rev_hi_lim <- outlier(train_indessa$total_rev_hi_lim)

#test data
test_indessa$int_rate <- outlier(test_indessa$int_rate)

test_indessa$annual_inc <- outlier(test_indessa$annual_inc)

test_indessa$dti <- outlier(test_indessa$dti)

test_indessa$delinq_2yrs <- outlier(test_indessa$delinq_2yrs) ##its outlier treatment fills all values with zero

test_indessa$inq_last_6mths <- outlier(test_indessa$inq_last_6mths)

test_indessa$mths_since_last_delinq <- outlier(test_indessa$mths_since_last_delinq)

test_indessa$open_acc <- outlier(test_indessa$open_acc)

test_indessa$pub_rec <- outlier(test_indessa$pub_rec) ##its outlier treatment fills all values with zero

test_indessa$revol_bal <- outlier(test_indessa$revol_bal)

test_indessa$revol_util <- outlier(test_indessa$revol_util)

test_indessa$total_acc <- outlier(test_indessa$total_acc)

test_indessa$total_rec_int <- outlier(test_indessa$total_rec_int)

test_indessa$total_rec_late_fee <- outlier(test_indessa$total_rec_late_fee) ##its outlier treatment fills all values with zero

test_indessa$recoveries <- outlier(test_indessa$recoveries) ##its outlier treatment fills all values with zero

test_indessa$collection_recovery_fee <- outlier(test_indessa$collection_recovery_fee) ##its outlier treatment fills all values with zero

test_indessa$collections_12_mths_ex_med <- outlier(test_indessa$collections_12_mths_ex_med) ##its outlier treatment fills all values with zero

test_indessa$last_week_pay <- outlier(test_indessa$last_week_pay)

test_indessa$acc_now_delinq <- outlier(test_indessa$acc_now_delinq) ##its outlier treatment fills all values with zero

test_indessa$tot_coll_amt <- outlier(test_indessa$tot_coll_amt) ##its outlier treatment fills all values with zero

test_indessa$tot_cur_bal <- outlier(test_indessa$tot_cur_bal)

test_indessa$total_rev_hi_lim <- outlier(test_indessa$total_rev_hi_lim)



####missing value imputation 
#any(is.na(full_indessa))
#summary(full_indessa)

#library(mice)
#full_indessa <- mice(full_indessa,m=10,maxit=25,meth='pmm',seed=500)
#summary(full_indessa)
#any(is.na(full_indessa))

#full_indessa <- complete(full_indessa)


#library(missForest)

#impute missing values, using all parameters as default values
#full_indessa <- missForest(full_indessa)

#check imputed values
#full_indessa$ximp

#check imputation error
#full_indessa$OOBerror

##To find variables having missing values
colnames(train_indessa)[apply(is.na(train_indessa), 2, any)]
colnames(test_indessa)[apply(is.na(test_indessa), 2, any)]

#train data
train_indessa$annual_inc[is.na(train_indessa$annual_inc)] <- round(mean(train_indessa$annual_inc, na.rm = TRUE))

train_indessa$delinq_2yrs[is.na(train_indessa$delinq_2yrs)] <- round(mean(train_indessa$delinq_2yrs, na.rm = TRUE))

train_indessa$inq_last_6mths[is.na(train_indessa$inq_last_6mths)] <- round(mean(train_indessa$inq_last_6mths, na.rm = TRUE))

train_indessa$mths_since_last_delinq[is.na(train_indessa$mths_since_last_delinq)] <- round(mean(train_indessa$mths_since_last_delinq, na.rm = TRUE))

train_indessa$mths_since_last_record[is.na(train_indessa$mths_since_last_record)] <- round(mean(train_indessa$mths_since_last_record, na.rm = TRUE))

train_indessa$open_acc[is.na(train_indessa$open_acc)] <- round(mean(train_indessa$open_acc, na.rm = TRUE))

train_indessa$pub_rec[is.na(train_indessa$pub_rec)] <- round(mean(train_indessa$pub_rec, na.rm = TRUE))

train_indessa$revol_util[is.na(train_indessa$revol_util)] <- round(mean(train_indessa$revol_util, na.rm = TRUE))

train_indessa$total_acc[is.na(train_indessa$total_acc)] <- round(mean(train_indessa$total_acc, na.rm = TRUE))

train_indessa$collections_12_mths_ex_med[is.na(train_indessa$collections_12_mths_ex_med)] <- round(mean(train_indessa$collections_12_mths_ex_med, na.rm = TRUE))

summary(train_indessa$last_week_pay)
train_indessa$last_week_pay[is.na(train_indessa$last_week_pay)] <- round(mean(train_indessa$last_week_pay, na.rm = TRUE))

summary(train_indessa$acc_now_delinq)
train_indessa$acc_now_delinq[is.na(train_indessa$acc_now_delinq)] <- round(mean(train_indessa$acc_now_delinq, na.rm = TRUE))

summary(train_indessa$tot_coll_amt)
train_indessa$tot_coll_amt[is.na(train_indessa$tot_coll_amt)] <- round(mean(train_indessa$tot_coll_amt, na.rm = TRUE))

summary(train_indessa$tot_cur_bal)
train_indessa$tot_cur_bal[is.na(train_indessa$tot_cur_bal)] <- round(mean(train_indessa$tot_cur_bal, na.rm = TRUE))

summary(train_indessa$total_rev_hi_lim)
train_indessa$total_rev_hi_lim[is.na(train_indessa$total_rev_hi_lim)] <- round(mean(train_indessa$total_rev_hi_lim, na.rm = TRUE))



#test data
test_indessa$annual_inc[is.na(test_indessa$annual_inc)] <- round(mean(test_indessa$annual_inc, na.rm = TRUE))

test_indessa$delinq_2yrs[is.na(test_indessa$delinq_2yrs)] <- round(mean(test_indessa$delinq_2yrs, na.rm = TRUE))

test_indessa$inq_last_6mths[is.na(test_indessa$inq_last_6mths)] <- round(mean(test_indessa$inq_last_6mths, na.rm = TRUE))

test_indessa$mths_since_last_delinq[is.na(test_indessa$mths_since_last_delinq)] <- round(mean(test_indessa$mths_since_last_delinq, na.rm = TRUE))

test_indessa$mths_since_last_record[is.na(test_indessa$mths_since_last_record)] <- round(mean(test_indessa$mths_since_last_record, na.rm = TRUE))

test_indessa$open_acc[is.na(test_indessa$open_acc)] <- round(mean(test_indessa$open_acc, na.rm = TRUE))

test_indessa$pub_rec[is.na(test_indessa$pub_rec)] <- round(mean(test_indessa$pub_rec, na.rm = TRUE))

test_indessa$revol_util[is.na(test_indessa$revol_util)] <- round(mean(test_indessa$revol_util, na.rm = TRUE))

test_indessa$total_acc[is.na(test_indessa$total_acc)] <- round(mean(test_indessa$total_acc, na.rm = TRUE))

sum(is.na(test_indessa$collections_12_mths_ex_med))/nrow(test_indessa)
test_indessa$collections_12_mths_ex_med[is.na(test_indessa$collections_12_mths_ex_med)] <- round(mean(test_indessa$collections_12_mths_ex_med, na.rm = TRUE))

summary(test_indessa$last_week_pay)
test_indessa$last_week_pay[is.na(test_indessa$last_week_pay)] <- round(mean(test_indessa$last_week_pay, na.rm = TRUE))

summary(test_indessa$acc_now_delinq)
test_indessa$acc_now_delinq[is.na(test_indessa$acc_now_delinq)] <- round(mean(test_indessa$acc_now_delinq, na.rm = TRUE))

summary(test_indessa$tot_coll_amt)
test_indessa$tot_coll_amt[is.na(test_indessa$tot_coll_amt)] <- round(mean(test_indessa$tot_coll_amt, na.rm = TRUE))

summary(test_indessa$tot_cur_bal)
test_indessa$tot_cur_bal[is.na(test_indessa$tot_cur_bal)] <- round(mean(test_indessa$tot_cur_bal, na.rm = TRUE))

summary(test_indessa$total_rev_hi_lim)
test_indessa$total_rev_hi_lim[is.na(test_indessa$total_rev_hi_lim)] <- round(mean(test_indessa$total_rev_hi_lim, na.rm = TRUE))

any(is.na(train_indessa))
any(is.na(test_indessa))


####feature engineering and variable transformation
hist(train_indessa$loan_amnt)
hist(sqrt(train_indessa$loan_amnt+400))
train_indessa$loan_amnt <- sqrt(train_indessa$loan_amnt+400)

hist(train_indessa$funded_amnt)
hist(sqrt(train_indessa$funded_amnt+400))
train_indessa$funded_amnt <- sqrt(train_indessa$funded_amnt+400)

hist(train_indessa$funded_amnt_inv)
hist(sqrt(train_indessa$funded_amnt_inv+1200))
train_indessa$funded_amnt_inv <- sqrt(train_indessa$funded_amnt_inv+1200)

hist(train_indessa$int_rate) ##
hist(sqrt(train_indessa$int_rate-3))
train_indessa$int_rate <- sqrt(train_indessa$int_rate-3)

hist(train_indessa$annual_inc) ##
hist(sqrt(train_indessa$annual_inc))
train_indessa$annual_inc <- sqrt(train_indessa$annual_inc)

hist(train_indessa$dti)##

##WOE & IV
#library(woe)
#woe(Data = train_indessa, "delinq_2yrs", TRUE, "loan_status", 5, Bad = 1, Good = 0  )

####delinq_2yrs
# summary(train_indessa$delinq_2yrs)
#train_indessa$delinq_2yrss <- NULL
# train_indessa$delinq_2yrss[train_indessa$delinq_2yrs <= 2] <- "<=2"
# train_indessa$delinq_2yrss[train_indessa$delinq_2yrs > 2] <- ">2"
# 
# levels(as.factor(train_indessa$delinq_2yrss))
# 
# neTab<-table(train_indessa$delinq_2yrss,train_indessa$loan_status)
# sum(neTab[1,])/sum(neTab)*100
# sum(neTab[2,])/sum(neTab)*100
# View(neTab)
# 
# nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
# event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
# neTab<-cbind(neTab,nonevent_percent,event_percent)
# View(neTab)
# 
# woe<-log(neTab[,3]/neTab[,4])
# neTab<-cbind(neTab,woe)
# View(neTab)
# 
# iv<-(neTab[,3]-neTab[,4])*neTab[,5]
# neTab<-cbind(neTab,iv)
# View(neTab)
# 
# sum(neTab[,6])
# 
# train_indessa$delinq_2yrss[train_indessa$delinq_2yrs <= 2] <- "<=2"
# train_indessa$delinq_2yrss[train_indessa$delinq_2yrs > 2] <- ">2"



####inq_last_6mths
summary(as.factor(train_indessa$inq_last_6mths))

train_indessa$inq_last_6mthss <- NULL
train_indessa$inq_last_6mthss[train_indessa$inq_last_6mths <= 1] <- "<=2"
train_indessa$inq_last_6mthss[train_indessa$inq_last_6mths > 1] <- ">2"

levels(as.factor(train_indessa$inq_last_6mthss))

train_indessa$inq_last_6mths<- as.factor(train_indessa$inq_last_6mths)

neTab<-table(train_indessa$inq_last_6mths,train_indessa$loan_status)
sum(neTab[1,])/sum(neTab)*100
sum(neTab[2,])/sum(neTab)*100

View(neTab)

nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
neTab<-cbind(neTab,nonevent_percent,event_percent)
View(neTab)

woe<-log(neTab[,3]/neTab[,4])
neTab<-cbind(neTab,woe)
View(neTab)

iv<-(neTab[,3]-neTab[,4])*neTab[,5]
neTab<-cbind(neTab,iv)
View(neTab)

sum(neTab[,6])

train_indessa$inq_last_6mthss[train_indessa$inq_last_6mths <= 2] <- "<=2"
train_indessa$inq_last_6mthss[train_indessa$inq_last_6mths > 2] <- ">2"



##
hist(train_indessa$open_acc)


####pub_rec
summary(as.factor(train_indessa$pub_rec))

train_indessa$pub_recs <- NULL
train_indessa$pub_recs[train_indessa$pub_rec == 0] <- "0"
train_indessa$pub_recs[train_indessa$pub_rec > 0 ] <- ">0"

neTab<-table(train_indessa$pub_recs,train_indessa$loan_status)
sum(neTab[1,])/sum(neTab)*100
sum(neTab[2,])/sum(neTab)*100

View(neTab)

nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
neTab<-cbind(neTab,nonevent_percent,event_percent)
View(neTab)

woe<-log(neTab[,3]/neTab[,4])
neTab<-cbind(neTab,woe)
View(neTab)

iv<-(neTab[,3]-neTab[,4])*neTab[,5]
neTab<-cbind(neTab,iv)
View(neTab)

sum(neTab[,6])

train_indessa$pub_recs[train_indessa$pub_rec == 0] <- "0"
train_indessa$pub_recs[train_indessa$pub_rec > 0 ] <- ">0"



##
hist(train_indessa$revol_bal)
hist(sqrt(train_indessa$revol_bal))
train_indessa$revol_bal <- sqrt(train_indessa$revol_bal)

hist(train_indessa$revol_util)

hist(train_indessa$total_acc)

hist(train_indessa$total_rec_int)
hist(sqrt(train_indessa$total_rec_int))
train_indessa$total_rec_int <- sqrt(train_indessa$total_rec_int)


####recoveries
summary(as.factor(train_indessa$recoveries))

train_indessa$recoveriess <- NULL
train_indessa$recoveriess[train_indessa$recoveries == 0] <- "0"
train_indessa$recoveriess[train_indessa$recoveries > 0 ] <- ">0"

neTab<-table(train_indessa$recoveriess,train_indessa$loan_status)
sum(neTab[1,])/sum(neTab)*100
sum(neTab[2,])/sum(neTab)*100

View(neTab)

nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
neTab<-cbind(neTab,nonevent_percent,event_percent)
View(neTab)

woe<-log(neTab[,3]/neTab[,4])
neTab<-cbind(neTab,woe)
View(neTab)

iv<-(neTab[,3]-neTab[,4])*neTab[,5]
neTab<-cbind(neTab,iv)
View(neTab)

sum(neTab[,6])

train_indessa$recoveriess[train_indessa$recoveries == 0] <- "0"
train_indessa$recoveriess[train_indessa$recoveries > 0 ] <- ">0"



####collection_recovery_fee
summary(as.factor(train_indessa$collection_recovery_fee))

train_indessa$collection_recovery_fees <- NULL
train_indessa$collection_recovery_fees[train_indessa$collection_recovery_fee == 0] <- "0"
train_indessa$collection_recovery_fees[train_indessa$collection_recovery_fee > 0 ] <- ">0"

neTab<-table(train_indessa$collection_recovery_fees,train_indessa$loan_status)
sum(neTab[1,])/sum(neTab)*100
sum(neTab[2,])/sum(neTab)*100

View(neTab)

nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
neTab<-cbind(neTab,nonevent_percent,event_percent)
View(neTab)

woe<-log(neTab[,3]/neTab[,4])
neTab<-cbind(neTab,woe)
View(neTab)

iv<-(neTab[,3]-neTab[,4])*neTab[,5]
neTab<-cbind(neTab,iv)
View(neTab)

sum(neTab[,6])

train_indessa$collection_recovery_fees[train_indessa$collection_recovery_fee == 0] <- "0"
train_indessa$collection_recovery_fees[train_indessa$collection_recovery_fee > 0 ] <- ">0"



####collections_12_mths_ex_med
summary(as.factor(train_indessa$collections_12_mths_ex_med))

train_indessa$collections_12_mths_ex_meds <- NULL
train_indessa$collections_12_mths_ex_meds[train_indessa$collections_12_mths_ex_med == 0] <- "0"
train_indessa$collections_12_mths_ex_meds[train_indessa$collections_12_mths_ex_med > 0 ] <- ">0"

neTab<-table(train_indessa$collections_12_mths_ex_meds,train_indessa$loan_status)
sum(neTab[1,])/sum(neTab)*100
sum(neTab[2,])/sum(neTab)*100

View(neTab)

nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
neTab<-cbind(neTab,nonevent_percent,event_percent)
View(neTab)

woe<-log(neTab[,3]/neTab[,4])
neTab<-cbind(neTab,woe)
View(neTab)

iv<-(neTab[,3]-neTab[,4])*neTab[,5]
neTab<-cbind(neTab,iv)
View(neTab)

sum(neTab[,6])

train_indessa$collections_12_mths_ex_meds[train_indessa$collections_12_mths_ex_med == 0] <- "0"
train_indessa$collections_12_mths_ex_meds[train_indessa$collections_12_mths_ex_med > 0 ] <- ">0"



##
hist(train_indessa$last_week_pay)
hist(sqrt(train_indessa$last_week_pay))
train_indessa$last_week_pay <- sqrt(train_indessa$last_week_pay)


####tot_coll_amt
summary(as.factor(train_indessa$tot_coll_amt))

train_indessa$tot_coll_amts <- NULL
train_indessa$tot_coll_amts[train_indessa$tot_coll_amt == 0] <- "0"
train_indessa$tot_coll_amts[train_indessa$tot_coll_amt > 0 & train_indessa$tot_coll_amt <= 600] <- "<=600"
train_indessa$tot_coll_amts[train_indessa$tot_coll_amt > 600] <- ">600"

neTab<-table(train_indessa$tot_coll_amts,train_indessa$loan_status)
sum(neTab[1,])/sum(neTab)*100
sum(neTab[2,])/sum(neTab)*100
sum(neTab[3,])/sum(neTab)*100

View(neTab)

nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
neTab<-cbind(neTab,nonevent_percent,event_percent)
View(neTab)

woe<-log(neTab[,3]/neTab[,4])
neTab<-cbind(neTab,woe)
View(neTab)

iv<-(neTab[,3]-neTab[,4])*neTab[,5]
neTab<-cbind(neTab,iv)
View(neTab)

sum(neTab[,6])

train_indessa$tot_coll_amts[train_indessa$tot_coll_amt == 0] <- "0"
train_indessa$tot_coll_amts[train_indessa$tot_coll_amt > 0 & train_indessa$tot_coll_amt <= 600] <- "<=600"
train_indessa$tot_coll_amts[train_indessa$tot_coll_amt > 600] <- ">600"



####tot_cur_bal
summary(as.factor(train_indessa$tot_cur_bal))

train_indessa$tot_cur_bals <- NULL
train_indessa$tot_cur_bals[train_indessa$tot_cur_bal <= 10000] <- "<=10000"
train_indessa$tot_cur_bals[train_indessa$tot_cur_bal > 10000] <- ">10000"

neTab<-table(train_indessa$tot_cur_bals,train_indessa$loan_status)
sum(neTab[1,])/sum(neTab)*100
sum(neTab[2,])/sum(neTab)*100

View(neTab)

nonevent_percent<-neTab[,1]/sum(neTab[,1])*100
event_percent<-a<-neTab[,2]/sum(neTab[,2])*100
neTab<-cbind(neTab,nonevent_percent,event_percent)
View(neTab)

woe<-log(neTab[,3]/neTab[,4])
neTab<-cbind(neTab,woe)
View(neTab)

iv<-(neTab[,3]-neTab[,4])*neTab[,5]
neTab<-cbind(neTab,iv)
View(neTab)

sum(neTab[,6])

train_indessa$tot_cur_bals[train_indessa$tot_cur_bal <= 10000] <- "<=10000"
train_indessa$tot_cur_bals[train_indessa$tot_cur_bal > 10000] <- ">10000"



##
hist(train_indessa$total_rev_hi_lim)
hist(sqrt(train_indessa$total_rev_hi_lim))
train_indessa$total_rev_hi_lim <- sqrt(train_indessa$total_rev_hi_lim)

any(is.na(train_indessa))

View(train_indessa)

train_indessa <- train_indessa[,-c(15,17,18,22,28,29,30,31,34,35,36)]

a <- train_indessa[,c(1:12)]
b <- train_indessa[,c(12:20)]
c <- train_indessa[,-c(1:20)]


####dummy variable creation 
library("dummies")
a <- dummy.data.frame(a)
b <- dummy.data.frame(b)
c <- dummy.data.frame(c)


train_indessa <- bind_cols(a,b,c)


###DIVIDING DATA INTO TRAINING & TEST SET
## set the seed to make your partition reproductible

nrow(train_indessa)

indessa_train <- full_indessa[c(1:532428),]
indessa_test <- full_indessa[-c(1:532428),]

indessa_train <- bind_cols(indessa_train, as.data.frame(loan_status))
View(indessa_train)
indessa_train$loan_status



####LOGISTIC REGRESSION



indessa_glm <- glm(loan_status ~ ., data = indessa_train[,-1], family = "binomial")
summary(indessa_glm)

library(MASS)
indessa_glm_step <- stepAIC(glm(loan_status ~ ., data = indessa_train[,-1], family = "binomial"), direction = "backward")

indessa_glm_final <- glm(loan_status ~ loan_amnt + funded_amnt_inv + term36 + int_rate + 
                         gradeA + gradeB + gradeC + gradeD + gradeE + 
                         sub_gradeA1 + sub_gradeA2 + sub_gradeA3 + sub_gradeA4 + sub_gradeB1 +
                         sub_gradeB2 + sub_gradeB3 + sub_gradeB4 + sub_gradeC1 + sub_gradeC2 + 
                         sub_gradeC3 + sub_gradeC4 + sub_gradeD1 + sub_gradeD2 + sub_gradeD3 +
                         sub_gradeD4 + sub_gradeE1 + sub_gradeE2 + sub_gradeE3 + sub_gradeE4 +
                         sub_gradeF1 + sub_gradeF2 + sub_gradeF3 + sub_gradeF4 + 
                         `emp_length< 1 year` + `emp_length1 year` + `emp_length10+ years` + 
                         `emp_length2 years` + `emp_length3 years` + `emp_length4 years` + 
                         `emp_length5 years` + `emp_length6 years` + `emp_length7 years` + 
                         `emp_length8 years` + `emp_length9 years` + home_ownershipMORTGAGE + 
                         home_ownershipNONE + home_ownershipOTHER + home_ownershipOWN + 
                         annual_inc + `verification_statusSource Verified` + purposecar + 
                         purposecredit_card + purposedebt_consolidation + purposeeducational + 
                         purposehome_improvement + purposehouse + purposemajor_purchase + 
                         purposemedical + purposemoving + purposeother + purposerenewable_energy + 
                         purposesmall_business + purposevacation + dti + mths_since_last_delinq + 
                         mths_since_last_record + open_acc + revol_bal + revol_util + total_acc + 
                         initial_list_statusf + total_rec_int + application_typeINDIVIDUAL + 
                         last_week_pay +total_rev_hi_lim + `delinq_2yrss<=2` + `inq_last_6mthss<=2` + 
                         `pub_recs>0` + `collections_12_mths_ex_meds>0` + `tot_coll_amts<=600` +
                         `tot_coll_amts>600` + `tot_cur_bals<=10000` , 
                         data = indessa_train[,-1], family = "binomial") 
summary(indessa_glm_final)


####funtion for HL Test for goodness of fit
library(ResourceSelection)
hoslem.test(indessa_train$loan_status, fitted(indessa_glm), g = 10)

####ROC plot
library(Deducer)
rocplot(indessa_glm)

####function fr concordance
bruteforce<-function(model){
  # Get all actual observations and their fitted values into a frame
  fitted<-data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted)<-c('respvar','score')
  # Subset only ones
  ones<-fitted[fitted[,1]==1,]
  # Subset only zeros
  zeros<-fitted[fitted[,1]==0,]
  
  # Initialise all the values
  pairs_tested<-0
  conc<-0
  disc<-0
  ties<-0
  
  # Get the values in a for-loop
  for(i in 1:nrow(ones))
  {
    for(j in 1:nrow(zeros))
    {
      pairs_tested<-pairs_tested+1
      if(ones[i,2]>zeros[j,2]) {conc<-conc+1}
      else if(ones[i,2]==zeros[j,2]){ties<-ties+1}
      else {disc<-disc+1}
    }
  }
  # Calculate concordance, discordance and ties
  concordance<-conc/pairs_tested
  discordance<-disc/pairs_tested
  ties_perc<-ties/pairs_tested
  return(list("Concordance"=concordance,
              "Discordance"=discordance,
              "Tied"=ties_perc,
              "Pairs"=pairs_tested))
}

bruteforce(indessa_glm)





indessa_test$loan_status_prob <- predict(indessa_glm,indessa_test[,-1] , type = "response")

write.csv(indessa_test[,c(1,ncol(indessa_test))], "C:/Users/Abhijeet/Desktop/Kaggle/Hacker Rank Bank Defaulter/result.csv")



prob <- predict(indessa_glm,indessa_train[,-1] , type = "response")

default <- ifelse(prob >= 0.5, 1, 0)

accuracy <- mean(loan_status==default)
accuracy



##HL Test
library(ResourceSelection)
hoslem.test(loan_status, fitted(indessa_glm), g = 10)


##Lorenz Curve and Gini index
library(ineq)
# Gini Index
ineq(prob,type="Gini")
## Lorenz Curve
plot(Lc(prob),col="darkred",lwd=2)


###ks test 
library(stats)
ks.test(loan_status, prob)

library(ROCR)
prob <- predict(indessa_glm,indessa_train[,-1] , type = "response")
pred <- prediction(prob,loan_status)
perf <- performance(pred,"tpr","fpr")
plot(perf)

ks <- max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
ks

plot(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

##lift chart
lift.obj <- performance(pred, measure = "lift", x.measure = "rpp")
plot(lift.obj,
     main="Lift Chart",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")


##GAINS TABLE 
library(gains)

gains(loan_status, prob, groups = 10,
ties.method = c("max","min","first","average","random"),
conf = c("none","normal","t","boot"), boot.reps = 1000, conf.level = 0.95, 
optimal = TRUE, percents = TRUE)


###prediction with ks value
default <- ifelse(prob >= ks, 1, 0)

accuracy <- mean(loan_status==default)
accuracy

##ks test 
library(rgr)
gx.ks.test(loan_status, prob)

##ks test
library(Matching)
ks.boot(loan_status, prob, nboots=1000, alternative = c("two.sided", "less", "greater"), print.level=0)