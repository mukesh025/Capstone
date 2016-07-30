library(dplyr)
library(plyr)
library(data.table)
library(sqldf)
library("forecast")
library("zoo")
# Read working directory path
setwd("/Project/R/Capstoneproject")


# Read Monthly TSAT Data from file
MonthlyTSATData <- read.csv('TSAT_Monthly_With_BE.csv')
colnames(MonthlyTSATData)[2]<-"Product_Family"
colnames(MonthlyTSATData)[1]<-"Fiscal_Period_ID"
colnames(MonthlyTSATData)[3]<-"Business_Entity"

#Read EOL_PRODUCTS
EOL_PRODUCTS <- read.csv('EOL_PRODUCTS.csv')
colnames(EOL_PRODUCTS)[1]<-"Product_Family"
INTEREST_PFs <- read.csv('rJoinedN.csv')


#Remove EOl Products from TSAT Data

#MonthlyTSATData <- sqldf("SELECT *
#                                      FROM MonthlyTSATData where Product_Family not in 
#                                      (select Product_Family from EOL_PRODUCTS)")


#Get TSAT data Aggrgated at PF Level
TSAT_agg_data<-ddply(MonthlyTSATData,c("Product_Family","Fiscal_Period_ID"),summarize,
                     mean = round(mean(TSAT),2))

colnames(TSAT_agg_data)[3]<-"TSAT"
head(TSAT_agg_data)
summary(TSAT_agg_data)
pairs(TSAT_agg_data[-1])
hist(TSAT_agg_data$TSAT)


# Read Bookings data from file
Bookingbymonth <- read.csv('SVC_Bookings_Monthly_PF.csv')
colnames(Bookingbymonth)[2]<-"Business_Entity"
colnames(Bookingbymonth)[1]<-"Fiscal_Period_ID"
colnames(Bookingbymonth)[4]<-"Product_Family"
colnames(Bookingbymonth)[5]<-"Service_Bookings"


#Remove EOl Products from Bookings Data

#Bookingbymonth <- sqldf("SELECT *
#                                      FROM Bookingbymonth where Product_Family not in 
#                                      (select Product_Family from EOL_PRODUCTS)")


#Get Bookings data Aggrgated at PF Level
Bookings_agg_data<-ddply(Bookingbymonth,c("Product_Family","Fiscal_Period_ID"),summarize,
                         Service_Bookings = round(sum(Service_Bookings),2))


head(Bookings_agg_data)
summary(Bookingbymonth)
hist(Bookingbymonth$Service_Bookings)

BookingsbyTSATbyFiscalPeriod <- sqldf("SELECT Product_Family,Fiscal_Period_ID,TSAT,Service_Bookings
                                      FROM TSAT_agg_data
                                      JOIN Bookings_agg_data USING(Product_Family,Fiscal_Period_ID)")



Resultset <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(Resultset)<- c("Product_Family","intercept","slope","n","slope.SE","p.value","r.squared")



PF_List1 <- sqldf("SELECT distinct Product_Family FROM BookingsbyTSATbyFiscalPeriod where Product_Family in (SELECT distinct Product_Family FROM INTEREST_PFs)")
PF_List <- sqldf("SELECT distinct Product_Family FROM PF_List1 where Product_Family not in 
                 ('AIR1700','WDM','WSA','X2','XFP','TELPRES','SWMAN2','MEDIAS','SBPHONE','PZ58','RFC38','PHON3PC','PHONCOL','OP35','PDCM','ME2600X','ME1200','FPMID','FSIGHT','IPCCE','AIRANT','FPHIGH','EWMGMT','AIRAP','ENTATA','AIRMGMA','AIRMOD','C2940','C3550','C5000','DSBUOTH','CAAPL','CFP100G','CTLSYS','ENCXXM')")
for(i in 1:length(PF_List[[1]])) {

  PF_name<-PF_List[[1]][i]
  print(PF_name)
  
  BookingsbyTSATbyFiscalPeriodEnt <- filter(BookingsbyTSATbyFiscalPeriod, Product_Family == PF_name)
  #Third column is TSAT so get TSAT time series by passing that column
  TSATbyFiscalPeriodts <- ts(BookingsbyTSATbyFiscalPeriodEnt[,3], start=c(2012, 1), end=c(2015, 12), frequency=12)
  #Fourth column is Bookings so get Bookings time series by passing that column
  BookingsFiscalPeriodts<-ts(BookingsbyTSATbyFiscalPeriodEnt[,4], start=c(2012, 1), end=c(2015, 12), frequency=12)

  #Taking rolling mean
  TSATbyFiscalPeriodts_mean <- rollmean(TSATbyFiscalPeriodts, k = 12, align = "right")
  
  BookingsFiscalPeriodts_mean <- rollmean(BookingsFiscalPeriodts, k = 12, align = "right")
  
  
  alldata=ts.intersect(BookingsFiscalPeriodts_mean, TSATbyFiscalPeriodtslag12 = lag(TSATbyFiscalPeriodts_mean,12))
  
  tryit = lm(log(BookingsFiscalPeriodts_mean)~log(TSATbyFiscalPeriodtslag12), data = alldata)
  acf2(residuals(tryit))
  
  Resultset[i,1]<-PF_name
  Resultset[i,2]<-tryit$coefficients[1]
  Resultset[i,3]<-tryit$coefficients[2]
  Resultset[i,4]<-length(tryit$model$y)
  Resultset[i,5]<-summary(tryit)$coefficients[2,2]
  Resultset[i,6]<- pf(summary(tryit)$fstatistic[1], summary(tryit)$fstatistic[2],
                      summary(tryit)$fstatistic[3], lower.tail = FALSE)
  
  
  Resultset[i,7]<-summary(tryit)$r.squared
 print(Resultset)
 
}

write.csv(Resultset, file="Resultset.csv")