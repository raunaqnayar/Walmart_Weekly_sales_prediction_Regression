library(ggplot2)
library(plotly)
library(data.table)
library(stats)
library(usdm)
library(caTools)
walmart<-read.csv('D:\\Data_Analysis_Simplilearn_materials\\Data_Science_with_R\\Walmart_Store_sales.csv')
#View(walmart)

# Analysis Task

# Basic Statistics tasks

# 1. Which store has maximum sales 

store_sales<-summarise(group_by(walmart,Store),sum(Weekly_Sales),mean(Weekly_Sales),sd(Weekly_Sales))
names(store_sales)<-c('Store','Total_sales','avg_weekly_sales','sales_sd')

ggplot(store_sales,aes(Store,Total_sales))+geom_col(fill='red',alpha=0.4)+
  labs(x='Store Number',y='Total Sales',title='Number of sales in each store')+theme_classic()
ggplotly()
cat("Store number ",head(arrange(store_sales,-Total_sales),1)$Store,"has maximum sales =",
    head(arrange(store_sales,-Total_sales),1)$Total_sales)

# 2. Which store has maximum standard deviation i.e., the sales vary a lot. 
# Also, find out the coefficient of mean to standard deviation

ggplot(store_sales,aes(Store,sales_sd))+geom_col(fill='red',alpha=0.4)+
  labs(x='Store Number',y='Standard Deviation of Sales',title='Standard Deviation of sales in each store')+
  theme_classic()
ggplotly()
cat("Store number ",head(arrange(store_sales,-sales_sd),1)$Store,"has maximum standard deviation of sales =",
    head(arrange(store_sales,-sales_sd),1)$sales_sd)

store_sales<-mutate(store_sales,sales_mean_to_sd=avg_weekly_sales/sales_sd)
ggplot(store_sales,aes(Store,sales_mean_to_sd))+geom_col(fill='red',alpha=0.4)+
  labs(x='Store Number',y='Mean/Std Dev',title='Mean/Standard Deviation of sales in each store')+
  theme_classic()
ggplotly()
cat("Store number ",head(arrange(store_sales,-sales_mean_to_sd),1)$Store,
    "has maximum mean to standard deviation ratio of sales =",
    head(arrange(store_sales,-sales_mean_to_sd),1)$sales_mean_to_sd)


# 3. Which store/s has good quarterly growth rate in Q3’2012

walmart$Date<-as.Date(walmart$Date,format='%d-%m-%Y')
storewise_sales_Q3_2012<-walmart%>%filter(Date>='2012-07-01',Date<='2012-09-30')%>%
  group_by(Store)%>%summarise(sum(Weekly_Sales))
names(storewise_sales_Q3_2012)<-c('Store','sales_q3_2012')
storewise_sales_Q2_2012<-walmart%>%filter(Date>='2012-04-01',Date<='2012-06-30')%>%
  group_by(Store)%>%summarise(sum(Weekly_Sales))
names(storewise_sales_Q2_2012)<-c('Store','sales_q2_2012')
storewise_sales_Q2_Q3_2012<-merge(storewise_sales_Q2_2012,storewise_sales_Q3_2012)
storewise_sales_Q2_Q3_2012$q3_growth_rate<-
  ((storewise_sales_Q2_Q3_2012$sales_q3_2012/storewise_sales_Q2_Q3_2012$sales_q2_2012)-1)*100;

ggplot(storewise_sales_Q2_Q3_2012,aes(Store,q3_growth_rate))+
  geom_col(fill='red',alpha=0.4)+labs(x='Store Number',y='Q3-2012 growth rate (percentage)',
                                      title='Percentage Q3-2012 sales growth in each store')+theme_classic()
ggplotly()
print('Top 4 stores with good Q3 growth rate')
print(head(arrange(storewise_sales_Q2_Q3_2012,-q3_growth_rate),4))

# 4. Some holidays have a negative impact on sales. Find out holidays which have higher 
# sales than the mean sales in non-holiday season for all stores together
HolidayType<-function(x){
  if(x=='February'){return('Super Bowl')}
  else if(x=='September'){return('Labour Day')}
  else if(x=='November'){return('Thanksgiving')}
  else if(x=='December'){return('Christmas')}
  else{return('Not Holiday Month')}};


daywise_sales<-summarise(group_by(walmart,Date,Holiday_Flag),sum(Weekly_Sales));
names(daywise_sales)<-c("Date","Holiday_Flag",'Total_Weekly_Sales');
daywise_sales<-as.data.frame(daywise_sales)
working_week_sales<-summarise(filter(daywise_sales,Holiday_Flag==0),mean(Total_Weekly_Sales))[[1]]
print("Holiday weeks when sales are more than mean sales")
print(mutate(filter(daywise_sales,Holiday_Flag==1,Total_Weekly_Sales>working_week_sales),
             festival_season=sapply(months(Date),HolidayType)))

holiday_sales<-summarise(group_by(filter(daywise_sales,Holiday_Flag==1),months(Date)),
                         mean(Total_Weekly_Sales))
names(holiday_sales)<-c('HolidayMonth','MeanWeeklySales')
holiday_sales<-as.data.frame(holiday_sales)
holiday_sales<-mutate(holiday_sales,Holiday_Festival=sapply(HolidayMonth,HolidayType))
print("Festive Seasons when mean sales is higher than mean sales in non-holiday season")
print(filter(holiday_sales,MeanWeeklySales>working_week_sales))



# 5. Provide a monthly and semester view of sales in units and give insights
monthly_sales<-summarise(group_by(walmart,month(as.IDate(Date)),year(as.IDate(Date))),sum(Weekly_Sales))
names(monthly_sales)<-c("Month",'Year','TotalSales')
arrange(monthly_sales,Month,Year)

View(monthly_sales)

ggplot(monthly_sales,aes(x=factor(Month),y=TotalSales))+
  geom_col(aes(fill=factor(Year)),alpha=0.7,position = position_dodge(preserve = "single"))+
  labs(y='Total Sales',title='Monthly Sales (Walmart)')+theme_classic()+
  scale_x_discrete(name='Month',labels=month_name)
ggplotly()

monthly_sales=as.data.frame(monthly_sales)
monthly_sales$Semester=ifelse(monthly_sales$Month<=6,1,2)
View(summarise(group_by(monthly_sales,Semester,Year),TotalSales=sum(TotalSales)))
ggplot(summarise(group_by(monthly_sales,Semester,Year),TotalSales=sum(TotalSales)),
       aes(x=factor(Semester),y=TotalSales))+
  geom_col(aes(fill=factor(Year)),alpha=0.7,position = position_dodge(preserve = "single"))+
  labs(x='Semester',y='Total Sales',title='Semester-wise Sales (Walmart)')+theme_classic()
ggplotly()

# Statistical Model

# For Store 1 – Build  prediction models to forecast demand

# 1. Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 
# (starting from the earliest date in order). Hypothesize if CPI, unemployment, 
# and fuel price have any impact on sales.

# 2. Change dates into days by creating new variable.

View(walmart)
walmart_store1<-filter(walmart,Store==1);

View(walmart_store1)




walmart_store1$Day_number<-as.numeric(walmart_store1$Date-as.Date('2010-02-04'))
walmart_store1$month=month(walmart_store1$Date);
walmart_store1$year=year(walmart_store1$Date);
walmart_store1$Super_Bowl=as.numeric(walmart_store1$Holiday_Flag & walmart_store1$month==2)
walmart_store1$Labour_Day=as.numeric(walmart_store1$Holiday_Flag & walmart_store1$month==9)
walmart_store1$Thanksgiving=as.numeric(walmart_store1$Holiday_Flag & walmart_store1$month==11)
walmart_store1$Christmas=as.numeric(walmart_store1$Holiday_Flag & walmart_store1$month==12)
View(walmart_store1)

set.seed(101);
train_sample<-sample.split(walmart_store1$Weekly_Sales,SplitRatio = 0.8)
train_data<-subset(walmart_store1,train_sample==T);
test_data<-subset(walmart_store1,train_sample==F);

# Impact of CPI on sales
ggplot(train_data,aes(CPI,Weekly_Sales))+geom_point()
cor.test(train_data$CPI,train_data$Weekly_Sales,method='spearman')

# Impact of Unemployment on sales
ggplot(train_data,aes(Unemployment,Weekly_Sales))+geom_point()
cor.test(train_data$Unemployment,train_data$Weekly_Sales,method='spearman')

# Impact of Fuel Price on sales
ggplot(train_data,aes(Fuel_Price,Weekly_Sales))+geom_point()
cor.test(train_data$Fuel_Price,train_data$Weekly_Sales,method='spearman')

# Impact of Temperature on sales
ggplot(train_data,aes(Temperature,Weekly_Sales))+geom_point()
cor.test(train_data$Temperature,train_data$Weekly_Sales,method='spearman')

# Impact of year on sales
ggplot(train_data,aes(factor(year),Weekly_Sales))+geom_boxplot()
cor.test(train_data$year,train_data$Weekly_Sales,method='spearman')

# Impact of day number on sales
ggplot(train_data,aes(Day_number,Weekly_Sales))+geom_point()
cor.test(train_data$Day_number,train_data$Weekly_Sales,method='spearman')

# Impact of Thanksgiving season on sales
ggplot(train_data,aes(factor(Thanksgiving),Weekly_Sales))+geom_boxplot()
cor.test(train_data$Thanksgiving,train_data$Weekly_Sales,method='spearman')

# Impact of Super Bowl season on sales
ggplot(train_data,aes(factor(Super_Bowl),Weekly_Sales))+geom_boxplot()
cor.test(train_data$Super_Bowl,train_data$Weekly_Sales,method='spearman')

# Sales of Store 1 depend on factors like CPI,Fuel Price, year, Temperature,
#Thanksgiving Season, Super Bowl season,Day Number

vif(train_data[c('CPI','Fuel_Price','year','Temperature','Thanksgiving','Super_Bowl','Day_number')])
vif(train_data[c('CPI','Fuel_Price','year','Temperature','Thanksgiving','Super_Bowl')])
vif(train_data[c('CPI','Fuel_Price','Temperature','Thanksgiving','Super_Bowl')])

Sales_estimate<-lm(Weekly_Sales~CPI+Fuel_Price+Temperature+Thanksgiving+Super_Bowl,data=train_data)
Sales_estimate
summary(Sales_estimate)

Sales_estimate<-lm(Weekly_Sales~CPI+Temperature+Thanksgiving+Super_Bowl,data=train_data)
Sales_estimate
summary(Sales_estimate)

Sales_estimate<-lm(Weekly_Sales~CPI+Temperature+Thanksgiving,data=train_data)
Sales_estimate
summary(Sales_estimate)

sales_prediction<-predict(Sales_estimate,test_data[c('CPI','Temperature','Thanksgiving')])
testing_prediction<-cbind(sales_prediction,test_data$Weekly_Sales)
df.test.predict<-data.frame(testing_prediction)
colnames(df.test.predict)<-c('Prediction','True_Value')
MSE_value<-mean((df.test.predict$Prediction-df.test.predict$True_Value)^2)
cat("Mean Square Error is", MSE_value)
RMSE_value<-sqrt(mean((df.test.predict$Prediction-df.test.predict$True_Value)^2))
cat("Root Mean Square Error is", RMSE_value)
SSR_value<-sum((df.test.predict$Prediction-mean(df.test.predict$True_Value))^2);
SST_value<-sum((df.test.predict$True_Value-mean(df.test.predict$True_Value))^2);
test_R2_value<-SSR_value/SST_value;
cat("Coefficient of determination (R^2) on Test Data is equal to",test_R2_value )
