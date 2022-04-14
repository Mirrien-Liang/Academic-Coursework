#################################################
# Question 2

manhattan <- function(p1,p2) {
  distance <- matrix(NA, nrow=dim(p1)[1], ncol=dim(p2)[1])
  for(i in 1:nrow(p2)) {
    distance[,i] = rowSums(abs(t(t(p1)-p2[i,])))
  }
  return(distance)
}

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
    assignments[[i]] = ((i-1)%%K)+1
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
