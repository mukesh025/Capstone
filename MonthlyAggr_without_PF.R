# Install required packages
install.packages("data.table")
install.packages("dplyr")
library(plyr)
library(dplyr)
library(data.table)

# Read working directory path
setwd("/Project/R/Capstoneproject")

# Read data from file
FY12_Data <- read.csv('FY12.csv')

#We want to group average of 'overall satisfaction score by fiscal month and product family
#In order to maintain consistency over diferent data files we will name those first

colnames(FY12_Data)[69]<-"Q8OverallSatisfaction"
FY12_Data <- FY12_Data[!is.na(FY12_Data$Q8OverallSatisfaction),]

#Rename current fiscal_month to FISCAL_MONTH_CHAR
colnames(FY12_Data)[58]<-"FISCAL_MONTH_CHAR"

#Add numeric fiscal month column
FY12_Data$FISCAL_MONTH <- NA
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 01-AUG'] <- 201201
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 02-SEP'] <- 201202
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 03-OCT'] <- 201203
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 04-NOV'] <- 201204
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 05-DEC'] <- 201205
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 06-JAN'] <- 201206
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 07-FEB'] <- 201207
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 08-MAR'] <- 201208
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 09-APR'] <- 201209
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 10-MAY'] <- 201210
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 11-JUN'] <- 201211
FY12_Data$FISCAL_MONTH[FY12_Data$FISCAL_MONTH_CHAR=='FY12 12-JUL'] <- 201212

#Aggregate by Fiscal Month........Note that it is fiscal month 201201 means Aug 2012

FY12_agg_data<-ddply(FY12_Data,c("FISCAL_MONTH"),summarize,
                     mean = round(mean(Q8OverallSatisfaction),2))

#FY 13 Data

FY13_Data <- read.csv('FY13.csv')
colnames(FY13_Data)[42]<-"Q8OverallSatisfaction"
FY13_Data <- FY13_Data[!is.na(FY13_Data$Q8OverallSatisfaction),]
#Rename current fiscal_month to FISCAL_MONTH_CHAR
colnames(FY13_Data)[32]<-"FISCAL_MONTH_CHAR"
#Add numeric fiscal month column
FY13_Data$FISCAL_MONTH <- NA
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='AUG FY2013'] <- 201301
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='SEP FY2013'] <- 201302
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='OCT FY2013'] <- 201303
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='NOV FY2013'] <- 201304
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='DEC FY2013'] <- 201305
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='JAN FY2013'] <- 201306
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='FEB FY2013'] <- 201307
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='MAR FY2013'] <- 201308
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='APR FY2013'] <- 201309
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='MAY FY2013'] <- 201310
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='JUN FY2013'] <- 201311
FY13_Data$FISCAL_MONTH[FY13_Data$FISCAL_MONTH_CHAR=='JUL FY2013'] <- 201312

FY13_agg_data<-ddply(FY13_Data,c("FISCAL_MONTH"),summarize,
                     mean = round(mean(Q8OverallSatisfaction),2))
AggData<-rbind(FY12_agg_data,FY13_agg_data)

#FY 14 Data
FY14_Data <- read.csv('FY14.csv')
colnames(FY14_Data)[42]<-"Q8OverallSatisfaction"
FY14_Data <- FY14_Data[!is.na(FY14_Data$Q8OverallSatisfaction),]
#Rename current fiscal_month to FISCAL_MONTH_CHAR
colnames(FY14_Data)[32]<-"FISCAL_MONTH_CHAR"
#Add numeric fiscal month column
FY14_Data$FISCAL_MONTH <- NA
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='AUG FY2014'] <- 201401
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='SEP FY2014'] <- 201402
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='OCT FY2014'] <- 201403
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='NOV FY2014'] <- 201404
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='DEC FY2014'] <- 201405
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='JAN FY2014'] <- 201406
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='FEB FY2014'] <- 201407
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='MAR FY2014'] <- 201408
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='APR FY2014'] <- 201409
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='MAY FY2014'] <- 201410
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='JUN FY2014'] <- 201411
FY14_Data$FISCAL_MONTH[FY14_Data$FISCAL_MONTH_CHAR=='JUL FY2014'] <- 201412

FY14_agg_data<-ddply(FY14_Data,c("FISCAL_MONTH"),summarize,
                     mean = round(mean(Q8OverallSatisfaction),2))

AggData<-rbind(AggData,FY14_agg_data)

#FY 15 Data
FY15_Data <- read.csv('FY15.csv')
colnames(FY15_Data)[42]<-"Q8OverallSatisfaction"
FY15_Data <- FY15_Data[!is.na(FY15_Data$Q8OverallSatisfaction),]
#Rename current fiscal_month to FISCAL_MONTH_CHAR
colnames(FY15_Data)[32]<-"FISCAL_MONTH_CHAR"
#Add numeric fiscal month column

FY15_Data$FISCAL_MONTH <- NA
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='AUG FY2015'] <- 201501
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='SEP FY2015'] <- 201502
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='OCT FY2015'] <- 201503
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='NOV FY2015'] <- 201504
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='DEC FY2015'] <- 201505
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='JAN FY2015'] <- 201506
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='FEB FY2015'] <- 201507
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='MAR FY2015'] <- 201508
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='APR FY2015'] <- 201509
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='MAY FY2015'] <- 201510
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='JUN FY2015'] <- 201511
FY15_Data$FISCAL_MONTH[FY15_Data$FISCAL_MONTH_CHAR=='JUL FY2015'] <- 201512

FY15_agg_data<-ddply(FY15_Data,c("FISCAL_MONTH"),summarize,
                     mean = round(mean(Q8OverallSatisfaction),2))

AggData<-rbind(AggData,FY15_agg_data)


#FY 16 Q1Q2 Data
#Since columns names are same for Q1 and Q2 data we are able to merge those and process as one go
#Q3 data has some diferent names used so we will aggregate that separately
FY16Q1_Data <- read.csv('FY16Q1.csv')
FY16Q2_Data <- read.csv('FY16Q2.csv')
FY16_Data<-rbind(FY16Q1_Data,FY16Q2_Data)

#Since FY16 survey changed so we will consider NPS score as overall score.
#Also scale changed from 5 to 10 so we will take half of the mean to be consistent with earlier scale
#Column 33 is NPS and Column 29 is Fiscal Month
colnames(FY16_Data)[33]<-"Q8OverallSatisfaction"
FY16_Data <- FY16_Data[!is.na(FY16_Data$Q8OverallSatisfaction),]
#Rename current fiscal_month to FISCAL_MONTH_CHAR
colnames(FY16_Data)[29]<-"FISCAL_MONTH_CHAR"

#Add numeric fiscal month column
FY16_Data$FISCAL_MONTH <- NA
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='AUG FY2016'] <- 201601
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='SEP FY2016'] <- 201602
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='OCT FY2016'] <- 201603
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='NOV FY2016'] <- 201604
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='DEC FY2016'] <- 201605
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='JAN FY2016'] <- 201606
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='FEB FY2016'] <- 201607
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='MAR FY2016'] <- 201608
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='APR FY2016'] <- 201609
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='MAY FY2016'] <- 201610
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='JUN FY2016'] <- 201611
FY16_Data$FISCAL_MONTH[FY16_Data$FISCAL_MONTH_CHAR=='JUL FY2016'] <- 201612

FY16_agg_data<-ddply(FY16_Data,c("FISCAL_MONTH"),summarize,
                     mean = round(mean(Q8OverallSatisfaction)/2,2))

AggData<-rbind(AggData,FY16_agg_data)


FY16Q3_Data <- read.csv('FY16Q3.csv')
colnames(FY16Q3_Data)[33]<-"Q8OverallSatisfaction"
FY16Q3_Data <- FY16Q3_Data[!is.na(FY16Q3_Data$Q8OverallSatisfaction),]
#Rename current fiscal_month to FISCAL_MONTH_CHAR
colnames(FY16Q3_Data)[29]<-"FISCAL_MONTH_CHAR"
#Add numeric fiscal month column

FY16Q3_Data$FISCAL_MONTH <- NA
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='AUG FY2016'] <- 201601
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='SEP FY2016'] <- 201602
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='OCT FY2016'] <- 201603
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='NOV FY2016'] <- 201604
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='DEC FY2016'] <- 201605
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='JAN FY2016'] <- 201606
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='FEB FY2016'] <- 201607
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='MAR FY2016'] <- 201608
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='APR FY2016'] <- 201609
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='MAY FY2016'] <- 201610
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='JUN FY2016'] <- 201611
FY16Q3_Data$FISCAL_MONTH[FY16Q3_Data$FISCAL_MONTH_CHAR=='JUL FY2016'] <- 201612

FY16Q3_agg_data<-ddply(FY16Q3_Data,c("FISCAL_MONTH"),summarize,
                     mean = round(mean(Q8OverallSatisfaction)/2,2))

AggData<-rbind(AggData,FY16Q3_agg_data)

#order dataframe based on product and fiscal month

df1 <- tbl_df(AggData)
AggData<-arrange_(df1, c('FISCAL_MONTH'))

#change column name for 'mean' column as it stores average of overall satisfaction score
colnames(AggData)[2]<-"AVG_OVERALL_SATISFACTION"

#Save Aggregate data in csv file

write.csv(AggData, file="MONTHYLY_TSAT_TOTAL.csv")

