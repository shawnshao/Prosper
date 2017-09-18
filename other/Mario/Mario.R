install.packages('ggplot2')
install.packages('dplyr')
install.packages('lubridate')
install.packages('chron')
install.packages('RcppBDT')
library(RcppBDT)
library(chron)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

sessions <- read_csv("~/other/Mario/sessions.csv")
users <- read_cs("~/other/Mario/users.csv")
iaps<- read_csv("~/other/Mario/iaps.csv")
# Check the data
head(sessions)
attach(sessions)
sessions$last_session_termination_type<-NULL

## Create Time vs. time count variables
sessions$time = ymd_hms(sessions$ts)
sessions$weekday<-ifelse(weekdays(sessions$date)=='Sunday' |weekdays(sessions$date)=='Saturday',
                         'weekend','weekday')
sessions$hour = hour(sessions$time)
sessions$day_period<-cut(sessions$hour, breaks = c(3,11,19,23), 
                         labels = c('morning','afternoon','night'))
sessions$day_period[is.na(sessions$day_period)]<-'night'

df1<-sessions %>% group_by(udid,weekday) %>% summarise(week_count=n())
week_count<-spread(df1,weekday,week_count)

df2<- sessions%>%group_by(udid,day_period)%>%summarise(day_period_count=n())
day_period_count<-spread(df2,day_period,day_period_count)

duration<-sessions %>% group_by(udid) %>% summarise(max_date=max(time),
                                                    min_date=min(time)) %>% 
  mutate(duration=round(difftime(max_date,min_date,units='days'),0))
duration$min_date<-NULL
duration$max_date<-NULL

remove(df1)
remove(df2)

## Create variables in two weeks 
install_date<-users %>% distinct(udid,install_date)
install_date$install_first_week<-(install_date$install_date)+6
install_date$install_second_week<-(install_date$install_date)+13
two_weeks<-sessions %>% select(udid,date,session_num)

## Create variables in two weeks and two weeks count/avg count
two_weeks_count <- left_join(two_weeks,install_date,by='udid')
first_week_count<-two_weeks_count %>% filter(date>=install_date & date<=install_first_week) %>% group_by(udid) %>% count()
names(first_week_count)[2]<-'first_week_count'
first_week_count$first_week_avg<-round(first_week_count$first_week_count/7)

second_week_count<-two_weeks_count %>% filter(date>install_first_week & date<=install_second_week) %>% group_by(udid) %>% count()
names(second_week_count)[2]<-'second_week_count'
second_week_count$second_week_avg<- round(second_week_count$second_week_count/7)

## Create datediff
two_weeks_count$datediff<-difftime(two_weeks_count$date,two_weeks_count$install_date,units = 'days')
datediff_count<- two_weeks_count %>% 
  filter(date<=install_second_week) %>% 
  group_by(udid,datediff)%>%
  summarise(datediff_count=n())

datediff_count<-spread(datediff_count,datediff,datediff_count)


## Create how may days the first week per userid
day_played_firstweek <- two_weeks_count%>% filter(date>=install_date & date<=install_first_week) %>% 
  group_by(udid,date) %>% tally()
names(day_played_firstweek)[3]<-'day_played'

days_played_firstweek <- day_played_firstweek%>%group_by(udid)%>%tally()  
names(days_played_firstweek)[2]<-'days_played_firstweek'

## Create how may days the first week per userid
day_played_secondweek <- two_weeks_count%>% filter(date<=install_second_week & date>install_first_week) %>% 
  group_by(udid,date) %>% tally()
names(day_played_secondweek)[3]<-'day_played'

days_played_secondweek <- day_played_secondweek%>%group_by(udid)%>%tally()  
names(days_played_secondweek)[2]<-'days_played_secondweek'


## Join insights tables
sessions_insights<-inner_join(day_period_count,week_count,by='udid')
sessions_insights<-inner_join(sessions_insights,duration,by='udid')

sessions_insights<-left_join(sessions_insights,datediff_count,by='udid')

sessions_insights<-left_join(sessions_insights,first_week_count,by='udid')
sessions_insights<-left_join(sessions_insights,days_played_firstweek,by='udid')

sessions_insights<-left_join(sessions_insights,second_week_count,by='udid')
sessions_insights<-left_join(sessions_insights,days_played_secondweek,by='udid')

## Create reponse variable
purchased<-iaps %>% distinct(udid) %>% select(udid)
purchased_user = purchased$udid
users$purchased = ifelse(users$udid %in% purchased_user, "Y", "N")

## Join users and insights tables
users_insights<-left_join(users,sessions_insights,by='udid')
users_insights$install_date<-NULL

write.table(users_insights, file = "users_insights.csv", sep = ",",col.names = NA)


## Create new sessions table
new_sessions<-left_join(df2,df3,by='udid')
new_sessions<-left_join(new_sessions,df4,by='udid')
new_sessions<-left_join(new_sessions,total_count,by='udid')
new_sessions<-left_join(new_sessions,df5,by='udid')

# Select purchased customers from iaps
purchased<-iaps %>% distinct(udid) %>% select(udid)
purchased_user = purchased$udid
users$purchased = ifelse(users$udid %in% purchased_user, "Y", "N")
#new_users %>%filter(puchased=='Y') %>%  distinct(udid) %>% count()
#iaps %>% distinct(udid) %>% select(udid) %>% count()

## Create tot_purchased_times and total_rev
purchased_times<-iaps %>% 
  group_by(udid) %>% 
  summarise(purchased_times = n(),
            total_rev=sum(rev))

## join user with sessions
user_sessions<-left_join(users,new_sessions,by='udid')
remove(user_sessions)
write.table(user_sessions, file = "user_sessions1.csv", sep = ",",col.names = NA)
sum(is.na(user_sessions$weekday))
as.data.frame(sessions)
which(is.na(user_sessions$weekday))

## join spend with purchased
purchased_spend<-spendevents %>% filter(spendtype=='IAP') 
iaps$ts<-as.POSIXct(iaps$ts)
purchased_spend$ts<-as.POSIXct(purchased_spend$ts)

purchased_spend_customer<-left_join(iaps,purchased_spend,by=c('udid','ts'='ts'))
purchased_spend_customer$date.x<-NULL
purchased_spend_customer$date.y<-NULL
purchased_spend_customer$amount[is.na(purchased_spend_customer$amount)]=0
write.table(purchased_spend_customer, file = "purchased_spend_customer.csv", sep = ",",col.names = NA)

## creat count variables
sessions$month<-month(sessions$time)
sessions$day<-day(sessions$time)
df3<-sessions %>% group_by(udid,day) %>% summarise(day_count=n())
day_count<-spread(df3,day,day_count)

df1<-sessions %>% group_by(udid,month) %>% summarise(month_count=n())
month_count<-spread(df1,month,month_count)
names(month_count)[2]<-'times_played_march'
names(month_count)[3]<-'times_played_April'
names(month_count)[4]<-'times_played_May'


## Fit model for classification
library(caret)
library(rpart)
library(randomForest)
library(readr)
library(dummies)
library(ROCR)

users_insights<-read_csv("users_insights_copy.csv")
users_insights$udid<-NULL
users_insights$purchased<-ifelse(users_insights$purchased=='Y',1,0)

# change character vectors to factor
users_insights$lang<-factor(users_insights$lang)
users_insights$country<-factor(users_insights$country)
users_insights$hw_ver<-factor(users_insights$hw_ver)
users_insights$os_ver<-factor(users_insights$os_ver)
users_insights$purchased<-factor(users_insights$purchased)

## dummy variavbles
users_insights<- cbind(users_insights, dummy(users_insights$lang, sep = "_"))
users_insights <- cbind(users_insights, dummy(users_insights$country, sep = "_"))

index <- createDataPartition(users_insights$purchased, p = 0.7, list = FALSE)
traindata <-  users_insights[index, ]
testdata  <- users_insights[-index, ]


### Random Freast 
rf <- randomForest(x=traindata[,-c(1,2,5)],
                   y=traindata[[5]], 
                   ntree=150, importance=TRUE,
                   classwt = c(1,10))
importance(rf)
varImpPlot(rf)

#### Confusion matrix
pred<-predict(rf,newdata=testdata,type='class') 
table<-table(testdata$purchased,pred) 
table
test.err <- 1-sum(diag(table))/sum(table)
test.err

fpr=table['0','1']/(table['0','1']+table['0','0'])
fpr

## Roc
pred_prob = predict(rf, newdata=testdata, type="prob")
pred2 = prediction(pred_prob[,2], testdata$purchased)
roc = performance(pred2, "tpr", "fpr")
plot(roc,colorize=T)
abline(0,1)

## Auroc 
auc = performance(pred2,'auc')
auc = slot(auc, 'y.values')[[1]]
legend(0.4,0.3,round(auc,4), title="AUC", cex=1)


