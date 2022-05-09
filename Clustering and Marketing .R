### MIHIR NEVPURKAR ###
### https://www.linkedin.com/in/mihirnevpurkar/ ###




######## Clustering Analysis ######## 
# Clear the workspace
rm(list=ls())
cat("\014")

########  ########  
# load in the data file into data.frame
df <- read.csv(file='AirlineLoyalty.csv', stringsAsFactors = FALSE)

row.names(df) <- df[,1]
# remove the ID column
df <- df[,-1]

###### normalize input variables ######
# scale the data
df.norm <- data.frame(sapply(df, scale))
# add row names: 
row.names(df.norm) <- row.names(df) 

##-------------------------------------------------------####

## ###
###### hierarchical clustering using all variables ######
# calculate the distance matrix
dist <- dist(df.norm, method = "euclidean")



hc2 <- hclust(dist, method = "ward.D")
plot(hc2, hang = -1, ann = FALSE)

###### cut the tree into k clusters ######
# specify # of clusters k
member1 <- cutree(hc2, k = 2)  # Cut the dendrogram such that k clusters are produced
# check the number of records in each cluster
table(member1)
member1
## Attaching class label to the original DF #
df$cluster_hc_ward <- cutree(hc2, k = 2) 


###---------------------------------------------------------####

###  ###


### Without PCA ##
# for replication
set.seed(123)
clus.out_no_pca <- kmeans(df.norm[, 1:11], centers = 2, nstart = 10)
clus.out_no_pca$size


## with PCA ##
######## PCA + Clustering Analysis ########  
pca.out = prcomp(df.norm[, 1:11]) # perform PCA on columns 1~11
# summary report
summary(pca.out)

# get factor loadings/weights
rot = as.data.frame(pca.out$rotation)  # rotation matrix: rows are variable, columns are components

# now let us look at the pc scores for the observations
scores = as.data.frame(pca.out$x)

# now we perform cluster analysis
# instead of using Xs in df.nona, we want to using these PCs in scores
# lets use the first 6 components and make 2 clusters
clus.out <- kmeans(scores[,1:6],  centers = 2, nstart = 10)
# ratio of between-cluster variation to within-cluster variation => higher than using all the variables in the raw data
clus.out$betweenss/clus.out$tot.withinss
clus.out$size

###### Choosing K Using "elbow plot" or "Silhouette" ###### 

#install.packages('cluster')  
library(cluster)   # for use of silhouette()

# (1) "elbow plot" 
# -> calculate the total within-cluster sum of square measures
# -> the location of elbow (i.e., bend) indicate the appropriate number of clusters.
# (2) "Silhouette"
# use silhouette() in the cluster package to compute the average silhouette score
# -> A high average silhouette indicates a good clustering. 
# -> The optimal number of clusters k is the one that maximizes the average silhouette.

choosek <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(choosek) <- c("numClusters", "totWithinSS", "avg_silhouette")
for (k in 1:10) {
  set.seed(123)
  tempkm <- kmeans(scores[,1:6],  centers = k, nstart = 10)
  
  # Note that silhouette statistics are only defined if 2 <= k <= n-1.
  if (k==1) {
    ss <- 0
  } else {
    ss <- silhouette(tempkm$cluster, dist(scores[,1:6]))[, 3]
  }
  
  # append statistics
  tempdf <- data.frame(numClusters = k, totWithinSS = tempkm$tot.withinss, avg_silhouette = mean(ss))
  choosek <- rbind(choosek, tempdf)
}

require(ggplot2)

# elbow plot
g <- ggplot(choosek, aes(numClusters, totWithinSS))
g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Total Within-Cluster Squared Distance") +geom_text(aes(label=round(totWithinSS, 2)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))

# Silhouette #### Q3
g <- ggplot(choosek, aes(numClusters, avg_silhouette))
g + geom_line() + geom_point() + labs(x="Number of Clusters (k)", y="Average Silhouette") +geom_text(aes(label=round(avg_silhouette, 3)), vjust=-0.3) + scale_x_continuous(breaks = seq(1:10))


######--------------------------------------------------####







####Interpreting Results of Clustering ###

### Clustering using HCLUST ###
table(member1) ## Cluster 1: 1573 (39 %) Cluster 2 : 2426( 60% )
survey2 = cbind(df[,c(1,2,3,4,11)], cluster = member1)
aggdf <- aggregate(cbind(Balance,Qual_miles,cc1_miles,cc2_miles,Award.) ~ cluster, data=survey2, mean )
aggdf


### Clustering using PCA and Kmeans ##
clus.out ###Cluster 1: 2894 (72 % ) Cluster 2 ( 28 %) : 1105
clus.out$size
survey = cbind(df[,c(1,2,3,4,11)], cluster = clus.out$cluster)


aggdf_k_means <- aggregate(cbind(Balance,Qual_miles,cc1_miles,cc2_miles,Award.) ~ cluster, data=survey, mean )
aggdf_k_means



### If we compare results from HCLUST and KMEANS clustering we can notice that 
### HCLUST has given more balanced clustering results as compared to KMEANS #

#### cluster  Balance Qual_miles cc1_miles cc2_miles    Award.
#1       1 39163.85   1.367451  2932.295  2500.000 0.0000000
#2       2 95930.33 236.670651 18726.298  2650.453 0.6104699


### Characteristics of customers :
## Cluster 2 : When we look a the number we can easily identify that they are the frequent flyer customers and most of them are awarded before as well.
## Cluster 1 : As compared to cluster 2 they are less frequent flyers. We should target this cluster to get them more engaged with company offers.

## if we focus on clusters obtained by HCLUST the we have 2 clusters.

##----- Offers for customers in CLuster 2 ( Frequent flyers) ------- ###
###----******* Offer 1 *******-----####
## In cluster 2 ; the mean Balance (Number of miles eligible for award travel) is 95930.33.
## It clearly states cluster 2 is the loyal customer base and we should aim to retain these customers
## we can run campaign like : " Book one more Ticket with us to make your Balance to 1 Lacs and get 30 % off on your next eligible travel plus extra baggage allowance and stand a chance to win Award Travel 

###----******* Offer 2 *******-----####
## In cluster 2 ; the mean Qual_miles (Number of miles counted as qualifying for top flight status) is  236.670651.
## It clearly states cluster 2 is the loyal customer base and we should aim to retain these customers
## we can run campaign like : " Book one Flight Ticket of Business class on weekdays with us and get second business class ticket with 50 % Discount and get 30 % off on your next to next eligible travel plus extra baggage allowance  


##----- Offers for customers in CLuster 2 ( Non Frequent flyers) ------- ###
###----******* Offer 1 *******-----####
## In cluster 1 ; the mean cc1_miles (Number of miles earned with Frequent Flyer credit card in the past 12 months) is 2932.295 which is very less than cluster 2's cc1_miles.
## It clearly states cluster 1 is the inconsistent customer base and we should aim to attract these customers with stunning offers and we must encourage them to use frequent flyer credit card instead of normal cc
## we can run campaign like : " Book your ticket with frequent flyer credit card and get 2x miles as rewards and priority seat selection plus additional 10% off on your next flight and 25% off each on your next 2 flights ( To increase their engagement with company we must provoke them to use frequent flyer credit card) 

###----******* Offer 2 *******-----####
## In cluster 1 ; the mean Balance (Number of miles eligible for award trave) is 39163.85 and mean of Award is 0.
## It clearly states cluster 1 has less flying frequency and the company has never offered them chance of award travel. SO we should make them aware of Award Travel so that they could get more engaged with the company
## we can run campaign like : " Buy 3 flight tickets in the next two months and earn additional 20000 miles plus 50 % off on your 4th Travel Ticket  "  

###----******* Offer 3 *******-----####

## We can introduce one Travel Pass for less frequent flyers. In which we can offer them fixed number of tickets in certain duration with flexibility of dates so that they won't suffer from price fluctuations of flight tickets.

