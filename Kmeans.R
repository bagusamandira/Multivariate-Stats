library(ggplot2)
library(factoextra)
library(readxl)

datakmeans <- read_excel("Downloads/Table11_9.xlsx")
View(datakmeans) 
datakmeans[1] = NULL
datakmeans[1] = NULL
data = datakmeans
data
dist = get_dist(data)

#elbow method
elbow = fviz_nbclust(data, kmeans, method='wss') 
plot(elbow)

#silhouette method
sil = fviz_nbclust(data, kmeans, method='silhouette') 
plot(sil)

#clustering
clustering = kmeans(data, 6, nstart=25)
clustering

#see clustering
data_cluster = data.frame(clustering$cluster)
data_cluster

#see centroid
clustering$centers

#plot dendogram
single_link = hclust(dist, method='single')
par(mar=c(1,1,1,1))
plot(single_link, hang=-1)

#plot clustering
fviz_cluster(clustering, data=data)

#Count in each cluster
cluster_count = data.frame(Count=table(data_cluster))
cluster_count
