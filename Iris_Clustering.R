data("iris")
iris <- data.frame(iris)
View(iris)

#basic summary
summary(iris)
dim(iris)
#150 rows, 5 coulmns

str(iris)
colnames(iris)
#Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species

#random sampling - 90% train, 10% test
set.seed(10669218)
index <- sample(x =nrow(iris), size = nrow(iris)*0.9)
iris_train <- iris[index,] #train
iris_test <- iris[-index,] #test

#fpc package
install.packages("fpc")
library(fpc)

iris_train_set <- iris_train[,1:4]
scaled_train <- scale(iris_train_set)

#K-means cluster
#k=2 clusters
fit1 <- kmeans(scaled_train,2)
table(fit1$cluster)
plotcluster(scaled_train,fit1$cluster)

#k=3 clusters
fit2 <- kmeans(scaled_train,3)
table(fit2$cluster)
plotcluster(scaled_train,fit2$cluster)

#k=4 clusters
fit3 <- kmeans(scaled_train,4)
table(fit3$cluster)
plotcluster(scaled_train,fit3$cluster)

#k=5 clusters
fit4 <- kmeans(scaled_train,5)
table(fit4$cluster)
plotcluster(scaled_train,fit4$cluster)

#elements of 1st cluster in fit1 (k=2)
scaled_train[fit1$cluster == 1,]
aggregate(scaled_train, by = list(fit1$cluster), FUN = mean)
#gives summary stats group wise 
fit1$centers #same working as above

#determine the number of clusters
#within group sse
wss <- (nrow(scaled_train)-1)*sum(apply(scaled_train, 2, var))
#initialises wss
for (i in 2:12) wss[i] <- sum(kmeans(scaled_train,centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#prediction strength
prediction.strength(scaled_train, Gmin=2, Gmax=15, M=10,cutoff=0.8)
#arbitary cutoff- 0.8

#silhoutte width
d = dist(scaled_train, method = "euclidean")
result = matrix(nrow = 14, ncol = 3)
for (i in 2:15){
  cluster_result = kmeans(scaled_train, i)
  clusterstat=cluster.stats(d, cluster_result$cluster)
  result[i-1,1]=i
  result[i-1,2]=clusterstat$avg.silwidth
  result[i-1,3]=clusterstat$dunn
}

#silhouette width
plot(result[,c(1,2)], type="l", ylab = 'silhouette width', xlab = 'number of clusters')

#dunn index
plot(result[,c(1,3)], type="l", ylab = 'dunn index', xlab = 'number of clusters')


#Hierarchial Clustering
iris_dist <- dist(scaled_train)
iris_hclust <- hclust(iris_dist, method = "ward.D")
plot(iris_hclust)

#cut at 2 cluster level
iris_2clust <- cutree(iris_hclust,k =2)
rect.hclust(tree = iris_hclust,k=2 ) #shows the graph

#cut at 3 cluster level
iris_3clust <- cutree(iris_hclust,k =3)
rect.hclust(tree = iris_hclust,k=3 ) #shows the graph

#cut at 4 cluster level
iris_4clust <- cutree(iris_hclust,k =4)
rect.hclust(tree = iris_hclust,k=4 ) #shows the graph

#cut at 5 cluster level
iris_5clust <- cutree(iris_hclust,k =5)
rect.hclust(tree = iris_hclust,k=5 ) #shows the graph

#items in the 3rd group
scaled_train[iris_3clust==3,]

#plot
plotcluster(scaled_train,iris_3clust)

table(iris_2clust)
table(iris_3clust)
table(iris_4clust)
table(iris_5clust)
