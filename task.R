library(mice)
library(tidyverse)
#preparing the data
# unify all missing data to be NA
myData <- read.csv("G3_sydney_hobart_times.csv",T,na.strings='')
#removing " day" from Time column
myData$Time<- gsub("day",'',myData$Time)
myData$Time<- gsub(' ','',myData$Time)
myData$Time<-as.double(myData$Time)
#missing data
#predict the missing data
predicts <- mice(myData,m = 1,meth = c("", "pmm", "", "", ""),maxit = 30)
cData <- complete(predicts, 1)
#fill the last column of the dataset
cData$Code.Time.less.than.3 =ifelse(cData$Time<3,T,F)
#converting column 5 to boolean
cData$`Time.less.than.3`<-as.logical(cData$`Time.less.than.3`)
#renaming the columns
names(cData)<-c("Year","Time","start","finish", "Time.less.than.3")

#DATA IS READY
#show the structure
str(cData)
#show the statiscs
summary(cData)
#Visualization
#Histogram for times frequancy
histogram <- ggplot(cData,aes(cData$Time))
histogram+geom_histogram(color="brown")
#the years that was fleet take time less than 3
sPlot <- ggplot(cData,aes(x=Time.less.than.3,y=Year))+geom_point() + labs(x="Time less than 3", y="Year")
#adding a row 
newRecord <- c(2023,1.56319444444444,88,83,F)
cData = rbind(cData,newRecord)

#adding a column
cData$nintens= ifelse(cData$Year<2000 & cData$Year>=1990,T,F)

#deleting column "Code.Time.less.than.3"
#cData$Code.Time.less.than.3 =NULL

#factors
cData$`Time.less.than.3`<-factor(cData$Time.less.than.3)
cData$nintens <-factor(cData$nintens)
#mean and median
timeMean<-mean(cData$Time)
yeersMedian<-median(cData$Year)
#filter get all years of the fleet time  less than 3
filtered <- cData[cData$`Time.less.than.3`,"Year"]

#deleting the duplicate 
u <- unique(filtered)

