#load data files and rename 
#bikeshops to customers and bikes to products
customers <- bikeshops
products <- bikes

names(orders)
names(customers)
names(products)

#Merge the customers, products and orders df using the dplyr package.
library(dplyr)
orders.ext <- merge(orders,customers,by.x="customer.id",by.y="bikeshop.id")
orders.ext <- merge(orders.ext,products,by.x="product.id",by.y="bike.id")
dim(orders.ext)  #15644x17
order.ext <- orders.ext %>% mutate(price.ext=price*quantity) %>% 
       select(order.date,order.id,order.line,bikeshop.name,model,quantity,
       price,price.ext,category1,category2,frame) %>% 
       arrange(order.id,order.line)
knitr::kable(head(orders.ext)) #preview the data

#developing a hypothesis
#bike shops purchase Cannondale bike models based on features 
#such as Mountain or Road Bikes and price (high/premium or low/affordable)

#need unit to measure the cluser
#quantity purchased is better than total value of purchases.
#premium bike can be sold 10x more which can mask the quantity buying habits

#MANIPULATING THE DATA FRAME
#get the df into a format conductive to clustering bike models to customer id's
#change price into a categorical variables  
#need to scale the quantities so that k-algo weights purchases of each customer evenly

#Group by model & model features, summarize by quantity purchases
library(tidyr) #for spread function
customerTrends <- orders.ext %>% group_by(bikeshop.name,model,category1,
      category2, frame, price) %>% summarise(total.qty=sum(quantity))%>% 
      spread(bikeshop.name,total.qty)
customerTrends[is.na(customerTrends)] <- 0 #Remove NA's

#convert unit price to categorical high/low variables.
#we can do using cut2() from Hmisc package
#g=2 splits in two using median as centre point
library(Hmisc)
customerTrends$price <- cut2(customerTrends$price,g=2)

#next scale using scale() or prop.table() matrix function.
#Convert customer purchase quantity to percentage of total quantity..
customerTrends.mat <- as.matrix(customerTrends[,-(1:5)]) #Drop first five columns
customerTrends.mat <- prop.table(customerTrends.mat,margin=2) #column-wise pct
customerTrends <- bind_cols(customerTrends[,1:5],as.data.frame(customerTrends.mat))

#View data post manipulation.....
knitr::kable(head(customerTrends))
fix(customerTrends)

#data ready for clustering#think customer base as hypothesis
#most likely to be at least 4 customer groups because of mountain bike vs road 
#bike and premium vs affordable.More category is possible as many not care about 
#price but may still prefer a specific bike category. #We assume max 8 clusters, as
#more is likely to overfit the segments.

#Running k-means
library(cluster)
kmeansDat <- customerTrends[,-(1:5)] #Extract only customer columns
kmeansDat.t <- t(kmeansDat) #get customers in rows and products in columns

#Setup for k-means loop
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()

minClust <- 4 #Hypothesized minimum number of segments
maxClust <- 8 #Hypothesized maximum number of segments

#Compute k-means clustering over various clusters, k, from minClust to maxClust
for(centr in minClust:maxClust) {
i <- centr-(minClust-1) #reveals start as 1. and increases with centr
set.seed(11) #For reproducibility
km.out[i] <- list(kmeans(kmeansDat.t,centers=centr,nstart=50))
sil.out[i] <- list(silhouette(km.out[[i]][[1]],dist(kmeansDat.t)))
#Used fro plotting silhouette average widths
x[i]=centr #value of k
y[i]=summary(sil.out[[i]])[[4]] #Silhouette average width
}

#Plot silhouette results to find best number of clusters; closer to 1 is better
library(ggplot2)
ggplot(data=data.frame(x,y),aes(x,y))+geom_point(size=3)+
  geom_line()+
  xlab("Number of Cluster Centres")+
  ylab("Silhoutte Average Width")+
  ggtitle("Silhouette Average Width as Cluster Centre Varies")

#Get customers names that are in each segment ---
#Get attributes of optimal k-means output
maxSilRow <- which.max(y) #Row number of max silhouette values
optimalClusters <- x[maxSilRow] #Number of clusters
km.out.best <- km.out[[maxSilRow]] #k-means output of best cluster

#Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for(clustr in 1:optimalClusters){
  clusterNames[clustr] <- paste0("X",clustr)
    clusterList[clustr] <- list(
      names(
        km.out.best$cluster[km.out.best$cluster==clustr]
      )
    )
}
names(clusterList) <- clusterNames
print(clusterList)

#Determining the preferences of the customer segments

#Combine cluster centroids with biek for feature inspection---
custSegmentCntrs <- t(km.out.best$centers) #Get centroids for groups
colnames(custSegmentCntrs) <- make.names(colnames(custSegmentCntrs))
customerTrends.clustered <- bind_cols(customerTrends[,1:5],as.data.frame(custSegmentCntrs))
#cluster1
#Arrange top 10 bike models by cluster in descending order---
attach(customerTrends.clustered) #Allows ordering by column name
knitr::kable(head(customerTrends.clustered[order(-X1),c(1:5,6)],10))
#cluster2
knitr::kable(head(customerTrends.clustered[order(-X2),c(1:5,7)],10))
#cluster3
knitr::kable(head(customerTrends.clustered[order(-X3),c(1:5,8)],10))
#cluster4
knitr::kable(head(customerTrends.clustered[order(-X4),c(1:5,9)],10))
#cluster5
knitr::kable(head(customerTrends.clustered[order(-X3),c(1:5,10)],10))
#cluster 2 and 4 looks similar and can be merged.

#Notes- As done here, the easiest way to determine the customer
#preferences to the model(eg price point, category of bike,etc).
#Advances algorithms can be used if there are many factors but 
#not needed usually.


































  












