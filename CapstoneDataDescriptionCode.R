# install required packages
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("sqldf")

library(dplyr)
library(data.table)
library(sqldf)
# Read working directory path
setwd("/Project/R/Capstoneproject")

# Read Monthly TSAT Data from file
MonthlyTSATData <- read.csv('MONTHYLY_TSAT.csv')
summary(MonthlyTSATData)
head(MonthlyTSATData)
filter(MonthlyTSATData, Product_family == 1800)
pairs(MonthlyTSATData[-1])


# Read Bookings data from file
Bookingbymonth <- read.csv('Bookings_by_Month.csv')
summary(Bookingbymonth)
head(Bookingbymonth)

filter(Bookingbymonth, Product_Family == 1800)

BookingsbyTSATbyFiscalPeriod <- sqldf("SELECT Product_Family, Fiscal_Period_ID, AVG_OVERALL_SATISFACTION,Product_Bookings_Net
              FROM MonthlyTSATData
              JOIN Bookingbymonth USING(Product_Family,Fiscal_Period_ID)")

BookingsbyTSATbyFiscalPeriod

names(BookingsbyTSATbyFiscalPeriod)

BookingsbyTSATbyFiscalPeriod1800 <- filter(BookingsbyTSATbyFiscalPeriod, Product_family == 1800)
#Missing Data for 2016 Jan

BookingsbyTSATbyFiscalPeriod1800

TSATbyFiscalPeriod1800 <- sqldf("SELECT AVG_OVERALL_SATISFACTION
              FROM BookingsbyTSATbyFiscalPeriod1800")
                                
TSATbyFiscalPeriod1800


TSATbyFiscalPeriod1800ts <- ts(TSATbyFiscalPeriod1800, start=c(2012, 1), end=c(2015, 12), frequency=12) 

TSATbyFiscalPeriod1800ts

BookingsTbyFiscalPeriod1800 <- sqldf("SELECT Product_Bookings_Net
              FROM BookingsbyTSATbyFiscalPeriod1800")

BookingsTbyFiscalPeriod1800

BookingsTbyFiscalPeriod1800ts <- ts(BookingsTbyFiscalPeriod1800, start=c(2012, 1), end=c(2015, 12), frequency=12) 
BookingsTbyFiscalPeriod1800ts

plot(TSATbyFiscalPeriod1800ts,col="red")
par(new=TRUE)
plot(BookingsTbyFiscalPeriod1800ts, col = "green")


# Doing same analysis for C3560
BookingsbyTSATbyFiscalPeriodC3560 <- filter(BookingsbyTSATbyFiscalPeriod, Product_family == "C3560")

BookingsbyTSATbyFiscalPeriodC3560

TSATbyFiscalPeriodC3560 <- sqldf("SELECT AVG_OVERALL_SATISFACTION
              FROM BookingsbyTSATbyFiscalPeriodC3560")

TSATbyFiscalPeriodC3560


TSATbyFiscalPeriodC3560ts <- ts(TSATbyFiscalPeriodC3560, start=c(2012, 1), end=c(2015, 12), frequency=12) 

TSATbyFiscalPeriodC3560ts

BookingsTbyFiscalPeriodC3560 <- sqldf("SELECT Product_Bookings_Net
                                     FROM BookingsbyTSATbyFiscalPeriodC3560")

BookingsTbyFiscalPeriodC3560

BookingsTbyFiscalPeriodC3560ts <- ts(BookingsTbyFiscalPeriodC3560, start=c(2012, 1), end=c(2015, 12), frequency=12) 
BookingsTbyFiscalPeriodC3560ts

plot(TSATbyFiscalPeriodC3560ts,col="red")
par(new=TRUE)
plot(BookingsTbyFiscalPeriodC3560ts, col = "blue")
# using log to plot the graph
par(new=TRUE)
plot(log(BookingsTbyFiscalPeriodC3560ts), col = "green")
###-----------------------------------------------------------------
#Standardizing the data and then running the analysis
BookingsTbyFiscalPeriodC3560Std <- BookingsTbyFiscalPeriodC3560
  
BookingsTbyFiscalPeriodC3560Std[,1] <-  (BookingsTbyFiscalPeriodC3560Std[,1] - mean(BookingsTbyFiscalPeriodC3560Std[,1]))/sd(BookingsTbyFiscalPeriodC3560Std[,1])

BookingsTbyFiscalPeriodC3560tsStd <- ts(BookingsTbyFiscalPeriodC3560Std, start=c(2012, 1), end=c(2015, 12), frequency=12) 

plot(TSATbyFiscalPeriodC3560ts,col="red")
par(new=TRUE)
plot(BookingsTbyFiscalPeriodC3560tsStd, col = "blue")
###----------------------------------------------------------------
#Clustering analysis on the whole data
BookingsbyTSATbyFiscalPeriod <- sqldf("SELECT Product_Family, Fiscal_Period_ID, AVG_OVERALL_SATISFACTION,Product_Bookings_Net
              FROM MonthlyTSATData
                                        JOIN Bookingbymonth USING(Product_Family,Fiscal_Period_ID)")
BookingsbyTSATbyFiscalPeriodCluster <- BookingsbyTSATbyFiscalPeriod [,3:4]

BookingsbyTSATbyFiscalPeriodCluster

#Euclidean Distance and plotting the cluster
distance <- dist(BookingsbyTSATbyFiscalPeriodCluster,method="euclidean")
hiercluster <- hclust(distance,method="ward.D")
plot(hiercluster,labels=F)
rect.hclust(hiercluster, k=5, border="red")


# Finding the number of clusters by the elbow method....
# Utility unctions to help compute sums of squares to choose number of clusters
sqr_edist <- function(x, y) {             	 
  sum((x-y)^2)
}

wss.cluster <- function(clustermat) {     	
  c0 <- apply(clustermat, 2, FUN=mean)    	 
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))     	
}

wss.total <- function(dmatrix, labels) {                               	
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))         	
  wsstot
}

totss <- function(dmatrix) {                 	
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}


num_samples <- nrow(BookingsbyTSATbyFiscalPeriodCluster)
max_clusters <- 20

totss.BookingsbyTSATbyFiscalPeriodCluster <- totss(BookingsbyTSATbyFiscalPeriodCluster) 
wss.BookingsbyTSATbyFiscalPeriodCluster <- numeric(max_clusters)
wss.BookingsbyTSATbyFiscalPeriodCluster[1] <- (num_samples-1)*sum(apply(BookingsbyTSATbyFiscalPeriodCluster, 2, var))

for(k in 2:max_clusters){
  
  distance.BookingsbyTSATbyFiscalPeriodCluster <- dist(BookingsbyTSATbyFiscalPeriodCluster, method="euclidean")
  hiercluster.BookingsbyTSATbyFiscalPeriodCluster <- hclust(distance.BookingsbyTSATbyFiscalPeriodCluster, method="ward.D")
  labels.BookingsbyTSATbyFiscalPeriodCluster <- cutree(hiercluster.BookingsbyTSATbyFiscalPeriodCluster, k=k)
  wss.BookingsbyTSATbyFiscalPeriodCluster[k] <- wss.total(BookingsbyTSATbyFiscalPeriodCluster, labels.BookingsbyTSATbyFiscalPeriodCluster)
}

plot(wss.BookingsbyTSATbyFiscalPeriodCluster, type='b', xlab="Clusters", ylab="Total Within Sum of Squares")
#By the elbow method we see there are 3 clusters.....Hence we conclude there are 3 clusters.
#Re plotting using 3 clusters....
plot(hiercluster,labels=F)
rect.hclust(hiercluster, k=3, border="red")
###---------------------------------------------------------------------------------------
### Use only 4 years data 2012,2013,2014,2015.... Let's not use 2016....