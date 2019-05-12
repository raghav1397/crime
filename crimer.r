#hive connection from r studio

library(rJava)
library(RJDBC)

options(java.parameters = '-Xmx8g')
hadoop_jar_dirs <- c('/usr/lib/hadoop/lib',
                     '/usr/lib/hadoop',
                     '/usr/lib/hive/lib')
clpath <- c()
for (d in hadoop_jar_dirs) {
  clpath <- c(clpath, list.files(d, pattern = 'jar', full.names = TRUE))
}
.jinit(classpath = clpath)
.jaddClassPath(clpath)

hive_jdbc_jar <- '/usr/lib/hive/lib/hive-jdbc-2.1.1.jar'
hive_driver <- 'org.apache.hive.jdbc.HiveDriver'
hive_url <- 'jdbc:hive2://localhost:10000/default'
drv <- JDBC(hive_driver, hive_jdbc_jar)
conn <- dbConnect(drv, hive_url)
a <- dbGetQuery(conn, "select * from d2011 where sno is not null")

#dbscan and kmeans

require(dbscan)
require(factoextra)

# load and prepare the data
customers <- read.csv("2011.csv")
customers<-a
customers <- customers[, c("Robbery","Burglary","Theft")]
customers <- scale(customers)
customers <- as.data.frame(customers)

# plot the distribution of distances to the fifth nearest neighbors dbscan
kNNdistplot(customers, k = 5)
abline(h = 1.4, col = "red")


# find clusters dbscan
db_clusters_customers <- dbscan(customers, eps=1.4, minPts=5)
print(db_clusters_customers)

wordcloud2
mydata <- read.csv("2017.csv")
wordcloud2(mydata, minRotation = -pi/6, maxRotation = -pi/6, minSize = 1,
           rotateRatio = 1)
# plot clusters dbscan
fviz_cluster(db_clusters_customers, customers, ellipse = FALSE, geom = "point")

#kmeans
require(factoextra)

# remove values beyond 2.5 standard deviations 
customers_core <- customers[customers[['Burglary']] > -2.5 &
                              customers[['Burglary']] < 2.5, ]
customers_core <- customers_core[customers_core[['Theft']] > -2.5 &
                                   customers_core[['Theft']] < 2.5, ]
customers_core <- customers_core[customers_core[['Robbery']] > -2.5 &
                                   customers_core[['Robbery']] < 2.5, ]

# find clusters and plot them kmeans
km_clusters_customers <- kmeans(customers_core, centers = 4, nstart = 10)
fviz_cluster(km_clusters_customers,
             customers_core,
             ellipse = FALSE,
             geom = "point")


#dbsacn silhouette
require(dbscan)
require(cluster)
require(factoextra)

## DBSCAN results

# retrieve a vector of cluster assignments
db_clusters_vector <- db_clusters_customers[['cluster']]

# calculate distances between data points
db_distances <- dist(customers)

# get a silhouette information object
db_silhouette <- silhouette(db_clusters_vector, db_distances)

# plot the silhouette
fviz_silhouette(db_silhouette)




## k-means results silhouette

# retrieve a vector of cluster assignments
km_clusters_vector <- km_clusters_customers[['cluster']]

# calculate distances between data points
km_distances <- dist(customers_core)

# get a silhouette information object
km_silhouette <- silhouette(km_clusters_vector, km_distances)

# plot the silhouette
fviz_silhouette(km_silhouette)
