#Load library
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
#Our dataset involves various time-frames. 
#In order to understand our data in separate time categories,
#we will make use of the lubridate package
install.packages("lubridate")
library(lubridate)

#This package is the lingua franca of data manipulation in R
library(dplyr)

#This package will help you to tidy your data. The basic principle of tidyr is to tidy the columns where each variable is present in a column,
#each observation is represented by a row and each value depicts a cell.
library(tidyr)

#With the help of this package,
#we will be able to interface with the JavaScript Library called – Datatables.
library(DT)
#With the help of graphical scales, we can automatically 
#map the data to the correct scales with well-placed axes and legends.
library(scales)

#Creating Color Vector

colors<-c("#CC1011", "#665555", "#05a399", "#cfcaca",  "#f5e840", 
          "#0683c9", "#e075b0")
           
#Load the Data set

apr_data<-read.csv("E:/study Material/data science/Uber DAta/uber-raw-data-apr14.csv")
may_data<-read.csv("E:/study Material/data science/Uber DAta/uber-raw-data-may14.csv")
june_data<-read.csv("E:/study Material/data science/Uber DAta/uber-raw-data-jun14.csv")
july_data<-read.csv("E:/study Material/data science/Uber DAta/uber-raw-data-jul14.csv")
aug_data<-read.csv("E:/study Material/data science/Uber DAta/uber-raw-data-aug14.csv")
sep_data<-read.csv("E:/study Material/data science/Uber DAta/uber-raw-data-sep14.csv")


#Bind the all data into a single data

data_2014<-rbind(apr_data,may_data,june_data,july_data,aug_data,sep_data)
View(data_2014)


data_2014$Date.Time<-as.POSIXct(data_2014$Date.Time,format="%m/%d/%Y %H:%M:%S")

data_2014$Time<-format(as.POSIXct(data_2014$Date.Time,format="%m/%d/%Y %H:%M:%S"),format = "%H:%M:%S")

data_2014$Date.Time<-ymd_hms(data_2014$Date.Time)

#change in the DAy
data_2014$Day<-factor(day(data_2014$Date.Time))

#change in the Month
data_2014$Month<-factor(month(data_2014$Date.Time,label = TRUE))

#Change in the year
data_2014$Year<-factor(year(data_2014$Date.Time))

#Change in the week days
data_2014$dayofweek<-factor(wday(data_2014$Date.Time,label = TRUE))

#chaneg in the time in hour,minute and second

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

View(data_2014)

#Aggregate the hour in number of taxi 

hour_data<-data_2014 %>%
  group_by(hour)%>%
  dplyr:: summarise(Total=n())
datatable(hour_data)

ggplot(hour_data,aes(hour,Total))+
  geom_bar(stat = "identity",fill="steelblue",color="red")+
  ggtitle("Trips Every Hours")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

#Month wise number of taxi

month_data<-data_2014%>%
  group_by(Month,hour)%>%
  dplyr::summarise(Total=n())
datatable(month_data)

ggplot(month_data,aes(hour,Total,fill=Month))+
  geom_bar(stat = "identity")+
  ggtitle("Trips by Hours and month")+
  scale_y_continuous(labels = comma)

#Plotting data by trips during every day of the month

#Aggregate the data by day
day_data<-data_2014%>%
  group_by(Day)%>%
  dplyr::summarise(Total=n())
datatable(day_data)

ggplot(day_data,aes(Day,Total))+
  geom_bar(stat = "identity",fill="Steelblue")+
  ggtitle("Trip every DAy")+
  scale_y_continuous(labels = comma)

#Agrregate by day in month
month_day_data<-data_2014%>%
  group_by(Month,Day)%>%
  dplyr::summarise(Total=n())
datatable(month_day_data)


ggplot(month_day_data,aes(Day,Total,fill=Month))+
  geom_bar(stat = "identity")+
  ggtitle("Trip in month day wise")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

# Aggregare by the month

#Number of Trips taking place during months in a year

month_data<-data_2014%>%
  group_by(Month)%>%
  dplyr::summarise(Total=n())
datatable(month_data)

ggplot(month_data,aes(Month,Total,fill=Month))+
  geom_bar(stat = "identity")+
  ggtitle("Trip in every month")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values=colors)

#Aggregate teh date of week
month_dayofweek_data<-data_2014%>%
  group_by(Month,dayofweek)%>%
  dplyr::summarise(Total=n())
datatable(month_dayofweek_data)

ggplot(month_dayofweek_data,aes(Month,Total,fill=dayofweek))+
  geom_bar(stat = "identity",position = "dodge")+
  ggtitle("trip by day and month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)


#Finding out the number of Trips by bases
ggplot(data_2014,aes(Base))+
  geom_bar(fill="darkred")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trip by base")

#Finding the trip by base based on the month

ggplot(data_2014,aes(Base,fill=Month))+
  geom_bar(position = "dodge")+
  ggtitle("Trip by base in month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

#Finding the trip by based on the day of week

ggplot(data_2014,aes(Base,fill=dayofweek))+
  geom_bar(position = "dodge")+
  ggtitle("Trip bu base in the day of the week")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)

#Creating a Heatmap visualization of day, hour and month

#We will plot heatmap bu hour and day

#Aggregate the DAy and hour wise.


Day_hour_data<-data_2014%>%
  group_by(Day,hour)%>%
  dplyr::summarise(Total=n())
datatable(Day_hour_data)

ggplot(Day_hour_data,aes(Day,hour,fill=Total))+
  geom_tile(color="White")+
  ggtitle("Heat map by hour and day")

# we will plot Heatmap by Month and Day