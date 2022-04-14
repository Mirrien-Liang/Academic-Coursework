# Question 1

# Helper function to calculate L1 distances

manhattan <- function(p1,p2) {
  distance <- matrix(NA, nrow=dim(p1)[1], ncol=dim(p2)[1])
  for(i in 1:nrow(p2)) {
    distance[,i] = rowSums(abs(t(t(p1)-p2[i,])))
  }
  return(distance)
}


# Another function for calculating L1 distances

manhattan2<-function(x,K){
  distance = matrix(NA, nrow= nrow(x), ncol = nrow(K))
  for(j in 1:nrow(K)) {
    for(i in 1:nrow(x)) {
      distance[i,j]<-dist(rbind(x[i,],K[j,]), method = "manhattan")
    }
  }
  return(distance)
}


kmedian <- function(x,K,iters) {
  # convert df to matrix
  x = as.matrix(x)
  
  # randomly sample some centers, set a seed 100
  set.seed(100)
  K <- x[sample(nrow(x), K),]
  
  # empty lists to store outputs
  assignments <- vector(iters, mode = "list")
  locations <- vector(iters, mode = "list")
  
  for(i in 1:iters) {
    # call manhattan distance helper function
    dists = manhattan(x,K)
    # find minimum distance
    clusters <- apply(dists,1,which.min)
    # tapply median()
    centers <- apply(x,2,tapply,clusters,median)
    
    # store outputs
    assignments[[i]] <- clusters
    locations[[i]] <- centers
  }
  
  # return outputs in list
  return(list(locations=locations[[1]], assignments = assignments[[1]]))
}

# Test:

# df = read.csv("parkinsons.data",row.names = 1)
# kmedian(df,3,10)



#################################################
# Question 2

kmedian2 <- function(x,K,iters) {
  # convert df to matrix
  x = as.matrix(x)
  
  # randomly sample some centers, set a seed 100
  set.seed(100)
  K <- x[sample(nrow(x), K),]
  
  # empty lists to store outputs
  assignments <- vector(iters, mode = "list")
  locations <- vector(iters, mode = "list")
  
  # initialize cluster assignments using ((i-1)%K)+1
  
  for (i in 1:nrow(x)) {
    assignments[i] = ((i-1)%%K)+1
  }
  
  for(i in 1:iters) {
    # call manhattan distance helper function
    dists = manhattan(x,K)
    # find minimum distance
    clusters <- apply(dists,1,which.min)
    # tapply median()
    centers <- apply(x,2,tapply,clusters,median)
    
    # store outputs
    assignments[[i]] <- clusters
    locations[[i]] <- centers
  }
  
  # return outputs in list
  return(list(locations=locations[[1]], assignments = assignments[[1]]))
}

df = read.csv("parkinsons.data",row.names = 1)
result = kmedian2(df,3,1000)
print(result$locations)

#################################################
# Question 3

# Helper function to calculate Euclidean distance

euclid <- function(x,K){
  distance = matrix(NA, nrow= nrow(x), ncol = nrow(K))
  for(j in 1:nrow(K)) {
    for(i in 1:nrow(x)) {
      distance[i,j]<-dist(rbind(x[i,],K[j,]), method = "euclidean")
    }
  }
  return(distance)
}

mykmeans <- function(x,K,iters) {
  # convert df to matrix
  x = as.matrix(x)
  
  # randomly sample some centers, set a seed 100
  set.seed(100)
  K <- x[sample(nrow(x), K),]
  
  # empty lists to store outputs
  assignments <- vector(iters, mode = "list")
  locations <- vector(iters, mode = "list")
  
  for(i in 1:iters) {
    # call euclidean distance helper function
    dists = euclid(x,K)
    # find minimum distance
    clusters <- apply(dists,1,which.min)
    # tapply mean()
    centers <- apply(x,2,tapply,clusters,mean)
    
    # store outputs
    assignments[[i]] <- clusters
    locations[[i]] <- centers
  }
  
  # return outputs in list
  return(list(locations=locations[[1]], assignments = assignments[[1]]))
}

# df = read.csv("parkinsons.data",row.names = 1)
result2 = mykmeans(df,3,10)
set.seed(123)
result3 = kmeans(df,3,10)


# Compare cluster assignments
v1 = result2$assignments
v2 = c()
for (i in seq_along(result3$cluster)) {
  v2[i] = result3$cluster[[i]]
}

compare1 = data.frame(mykmeans = v1, kmeans = v2)
compare2 = data.frame(cluster = c(1,2,3),
                      count_mykmeans = c(sum(v1 == 1), sum(v1==2),sum(v1==3)),
                      count_kmeans = c(sum(v2==1),sum(v2==2),sum(v2==3))
)
# compare1
compare2

# The comparisons show that cluster assignments may vary but the distribution of
# the sums of data points in each of the three clusters
# (i.e., the counts of cluster assignments: 109, 24, 62 versus 121, 63, 11)
# remains similar. It might be due to the different initial centroids of
# the two methods that were randomly generated. This is manifested in the 
# differences between result2$locations and result3$centers.

compare3 = list(mykmeans = result2$locations,
                kmeans = result3$centers)
compare3

# The other reason might be due to the different names created
# for the 3 clusters. Because the names (i.e., 1, 2, and 3) are just an
# indicator of three different groups rather than something meaningful. There
# is no real measurements differetiating the three groups. If we plot out the 
# distribution of the clustering, we would find the distributions of two methods
# are similar.

#################################################
# Question 4

# Three benefits:
# Because the method doesn't require the data to be labeled,
# it is frequently utilized in a variety of real-world problem statements.
# K-means clustering is easy to implement.
# The approach can handle massive amounts of data.


# Three drawbacks:
# The value of K must be manually selected.
# Outliers would have an adverse impact on the clustering.
# Clusters cannot overlap: one point can only belong to one cluster at a time,
# leading to certain points placed in incorrect clusters.

#################################################
# Question 5


suppressWarnings(suppressMessages(library(ggpubr)))
suppressWarnings(suppressMessages(library(factoextra)))

# Create a test data frame
set.seed(100)
sample_df = data.frame(V1 = rnorm(50,0,10), V2 = rnorm(50,0,10))
# head(sample_df)

# Use mykmeans()

result01 = mykmeans(scale(sample_df),3,1000)
result01$assignments

# Plot

plot1 = fviz_cluster(list(data = sample_df, cluster = result01$assignments),
             data = sample_df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


# Use kmeans()

set.seed(222)
result02 = kmeans(scale(sample_df),3,1000)
result02$cluster

# Plot

plot2 = fviz_cluster(result02, data = sample_df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

# Combine two plots to visualize comparison
figure <- ggarrange(plot1, plot2, labels = c("mykmeans", "kmeans"),
                    label.x = 0.35, label.y = 1,
                    ncol=2,nrow=1)

# Output the combined plot
figure
