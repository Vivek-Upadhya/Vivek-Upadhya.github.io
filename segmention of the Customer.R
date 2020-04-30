#Load library

library(ggplot2)

#Checking and setting the directory
getwd()
setwd("E:/study Material/data science")
getwd()

#Load the Data
Mall_data<-read.csv("Mall_Customers.csv")
View(Mall_data)

#EDA Of the data

head(Mall_data,10)

tail(Mall_data,10)

str(Mall_data)

names(Mall_data)
summary(Mall_data)

#Checking the missing value in the data
sum(is.na(Mall_data))

#Outlier analysis

boxplot(Mall_data$Age)
boxplot(Mall_data$Annual.Income..k..)

#Removing the Outlier
numeric_index<-sapply(Mall_data, is.numeric)
numeric_value<-Mall_data[,numeric_index]
cname<-colnames(numeric_value)


boxplot(Mall_data$Annual.Income..k..)
boxplot(Mall_data$Spending.Score..1.100.)

# Visulatization of the Data
# bar ploting the graph based on the gender in total mall distribution
a<-table(Mall_data$Genre)
barplot(a,main = "Comparison with the Gender", xlab = "Gender",ylab = "Number of gender",col = rainbow(2),legend=rownames(a))


# Visualization in percentage of the gender comparison

pct<-round(100*a/sum(a))
lbs<-paste(c("Female","Male"),pct,"%",sep = "")
library(plotrix)
pie3D(a,labels = lbs,main="Percentage of distribution of gender")


# Visulization of the age
summary(Mall_data$Age)
hist(Mall_data$Age,col = "blue",labels = TRUE,xlab = "Age class",ylab = "Frequency",main = "Histogram show the count of the ages")

# Most of the age goes to mall visualization
boxplot(Mall_data$Age,col = "#ff0066",main="Boxplot for Descriptive Analysis of Age")

#Visualization of the Annual income of the customer

summary(Mall_data$Annual.Income..k..)

hist(Mall_data$Annual.Income..k..,main = "Histogram of Annual income",xlab = "Annual income class",ylab = "Frequency",labels = TRUE,col = "#ff0066")

#Density plot for clear understanding of income

plot(density(Mall_data$Annual.Income..k..),col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(Mall_data$Annual.Income..k..),col = "#ccff66")

#Visualization of the Spending scores
summary(Mall_data$Spending.Score..1.100.)

hist(Mall_data$Spending.Score..1.100.,main = "Histogram of spending score",xlab = "Spending score class",ylab = "Frequency",labels = TRUE,col = "#ccff66")

#Elbow method to select the K value 

library(purrr)
set.seed(123)

# function to calculate total intra-cluster sum of square
iss<-function(k){
  kmeans(Mall_data[,3:5],k,iter.max = 100,nstart = 100,algorithm = "Lloyd")$tot.withinss
}

k.values <- 1:10

iss_vlaues<-map_dbl(k.values,iss)
plot(k.values,iss_vlaues,type = "b",pch=19,frame=FALSE,xlab = "Number of cluster",
     ylab = "Total intra-clusters sum of squares")

#we use average silhouette width  method to select the best K value value which gives better clustring

library(cluster)
library(gridExtra)
library(grid)

#silhouette method on k value 2

k2<-kmeans(Mall_data[,3:5],2,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s2<-plot(silhouette(k2$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 3
k3<-kmeans(Mall_data[,3:5],3,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s3<-plot(silhouette(k3$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 4
k4<-kmeans(Mall_data[,3:5],4,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s4<-plot(silhouette(k4$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 5
k5<-kmeans(Mall_data[,3:5],5,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s5<-plot(silhouette(k5$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 6
k6<-kmeans(Mall_data[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s6<-plot(silhouette(k6$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 7
k7<-kmeans(Mall_data[,3:5],7,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s7<-plot(silhouette(k7$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 8
k8<-kmeans(Mall_data[,3:5],8,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s8<-plot(silhouette(k8$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 9
k9<-kmeans(Mall_data[,3:5],9,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s9<-plot(silhouette(k9$cluster,dist(Mall_data[,3:5],"euclidean")))

#silhouette method on k value 10
k10<-kmeans(Mall_data[,3:5],10,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s10<-plot(silhouette(k10$cluster,dist(Mall_data[,3:5],"euclidean")))


#Lets plot the optimal k values

library(NbClust)
library(factoextra)

fviz_nbclust(Mall_data[,3:5], kmeans, method = "silhouette")


#Gap method  visulaization

set.seed(123)
stat_gap<-clusGap(Mall_data[,3:5],FUN=kmeans,nstart=25,K.max = 10,B=50)
fviz_gap_stat(stat_gap)

#Now select the k value 6 build the model

k6<-kmeans(Mall_data[,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
k6

#Visualizing the Clustering Results using the First Two Principle Components

##principal component analysis

pccclust<-prcomp(Mall_data[,3:5],scale =FALSE )
summary(pccclust)

pccclust$rotation[,1:2]

#Plot the final cluster result

set.seed(123)
ggplot(Mall_data,aes(x=Annual.Income..k..,y=Spending.Score..1.100.))+
  geom_point(stat = "identity", aes(color=as.factor(k6$cluster)))+
  scale_color_discrete(name=" ",breaks=c("1","2","3","4","5","6"),labels=c("Cluster 1","Custer 2","Cluster 3","Cluster 4","Cluster 5","cluster 6"))+
  ggtitle("Segment of Mall customers",subtitle = "Using Kmeans cluster")

#Summary
 #Cluster 6= These cluster represent the custer having high income and high spending
 #Cluster 2=These cluster represent the custer having loqwincome and high spending
 #Cluster 3=these cluster represent the low income and low spending
 #cluster 4&1=these cluster represent the medium inclome and midium speding
 #cluster 5=These cluster represnt high  inclome low spending.