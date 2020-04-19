###
# This file is for clustering analysis (to be added to data exploration file/section of write up)
###

library(datasets)
library(ggplot2)
library(cluster)
library(factoextra) ## factoextra provides PCA capability on clustering results to visualize them
library(Hmisc)
library(gplots)


### Elbow Plots
### Determine the optimal number of clusters, where marginal decrease of SSE between points and centroids levels off 
plot_elbow <- function() {
  ### Step 1 - Data prep
  ### This includes filtering and scaling/normalization
  
  #read in data
  cbb_full <- read.csv('cbb_full.csv')
  
  #year to numeric
  cbb_full$YEAR <- as.numeric(cbb_full$YEAR)
  
  #filter out non-tournament teams
  teams <- cbb_full[-(which(is.na(cbb_full$SEED))),]
  
  #create id column ("YY_TEAM_SEED") for merging with cluster results 
  teams$id <- paste(substr(as.character(teams$YEAR),3,4), teams$TEAM, teams$SEED, sep="_")
  
  
  ### note: to avoid the "curse of dimensionality" we should be selective in what features we use for clustering
  ### distance-based algorithms suffer when number of features increases, so we can:
  ###   a) use domain knowledge to pick out relevant and non-redundant features; allows for better interpretability bc original features are preserved
  ###   b) correlation-based selection, i.e. eliminate variables from pairs of correlated ones; more systematic, and still preserves features
  ###   c) do PCA first before clustering; more efficient for the task at hand, but takes away from interpretability
  
  ### since we prefer interpretability of our model, we'll choose a combination of options a and b. First we'll use domain knowledge to pick out
  ### features we think are relevant and not redundant; then we can use correlation matrix on the reduced set to see if any others need to be removed
  
  
  # teams_reduced takes a reduced subset of gameplay metrics, including off/def efficiency, orebs, 3pt, turnovers, free throws, and experience
  teams_reduced <- teams[,c(1,24,23,5,6,10,12,14,18,20,26)]
  
  rcorr(as.matrix(teams_reduced[,-c(1:3)]),type="pearson")
  
  #scale the data
  teams_reduced_scaled <- as.data.frame(cbind(teams_reduced[,1:3],scale(teams_reduced[,c(4:11)],center=TRUE,scale=TRUE)))
  
  
  #declare the row names as "year_team", so that they show up as labels on the graph
  rownames(teams_reduced_scaled) <- paste(substr(as.character(teams_reduced_scaled$YEAR),3,4),
                                          teams_reduced_scaled$TEAM,
                                          teams_reduced_scaled$SEED,
                                          sep="_")
  
  
  kdata <- teams_reduced_scaled[,-c(1:3)]
  
  k_num = 7
  var_per <- sapply(1:k_num, function(k){kmeans(kdata, k, nstart=15)$tot.withinss})
  plot(1:k_num, var_per,type = 'b',xlab = "Number of Clusters", ylab = 'SSE between points and cluster centroids')
}

plot_fviz <- function() {
  ### Step 1 - Data prep
  ### This includes filtering and scaling/normalization
  
  #read in data
  cbb_full <- read.csv('cbb_full.csv')
  
  #year to numeric
  cbb_full$YEAR <- as.numeric(cbb_full$YEAR)
  
  #filter out non-tournament teams
  teams <- cbb_full[-(which(is.na(cbb_full$SEED))),]
  
  #create id column ("YY_TEAM_SEED") for merging with cluster results 
  teams$id <- paste(substr(as.character(teams$YEAR),3,4), teams$TEAM, teams$SEED, sep="_")
  
  
  ### note: to avoid the "curse of dimensionality" we should be selective in what features we use for clustering
  ### distance-based algorithms suffer when number of features increases, so we can:
  ###   a) use domain knowledge to pick out relevant and non-redundant features; allows for better interpretability bc original features are preserved
  ###   b) correlation-based selection, i.e. eliminate variables from pairs of correlated ones; more systematic, and still preserves features
  ###   c) do PCA first before clustering; more efficient for the task at hand, but takes away from interpretability
  
  ### since we prefer interpretability of our model, we'll choose a combination of options a and b. First we'll use domain knowledge to pick out
  ### features we think are relevant and not redundant; then we can use correlation matrix on the reduced set to see if any others need to be removed
  
  
  # teams_reduced takes a reduced subset of gameplay metrics, including off/def efficiency, orebs, 3pt, turnovers, free throws, and experience
  teams_reduced <- teams[,c(1,24,23,5,6,10,12,14,18,20,26)]
  
  rcorr(as.matrix(teams_reduced[,-c(1:3)]),type="pearson")
  
  #scale the data
  teams_reduced_scaled <- as.data.frame(cbind(teams_reduced[,1:3],scale(teams_reduced[,c(4:11)],center=TRUE,scale=TRUE)))
  
  
  #declare the row names as "year_team", so that they show up as labels on the graph
  rownames(teams_reduced_scaled) <- paste(substr(as.character(teams_reduced_scaled$YEAR),3,4),
                                          teams_reduced_scaled$TEAM,
                                          teams_reduced_scaled$SEED,
                                          sep="_")
  
  
  kdata <- teams_reduced_scaled[,-c(1:3)]


  km3 <- kmeans(kdata, 3, nstart = 15)
  km4 <- kmeans(kdata, 4, nstart = 15)
  
  
  #visualize the clusters
  c1 <- fviz_cluster(km3,data=kdata,geom="text",ggtheme=theme_classic())
  c2 <- fviz_cluster(km4,data=kdata,geom="text",ggtheme=theme_classic())
  
  return(list(c1,c2))

}

### Step 4 - Extract Info and Interpret Results


# km3_teams <- as.data.frame(km3$cluster)
# km3_teams$id <- rownames(km3_teams)
# 
# km4_teams <- as.data.frame(km4$cluster)
# km4_teams$id <- rownames(km4_teams)
# 
# teams_merge <- merge(teams,km3_teams,by="id")
# teams_merge <- merge(teams_merge,km4_teams,by="id")
# colnames(teams_merge)[29:30] <- c("c_3","c_4")
# 
# c3_means <- aggregate(teams_merge[, 23:24], list(teams_merge$c_3), mean)
# c4_means <- aggregate(teams_merge[, 23:24], list(teams_merge$c_4), mean)
# 
# 
# #% variance explained
# print(sprintf('The % of variance explained by 3 clusters is: %s', km3$betweenss/km3$totss))
# print(sprintf('The % of variance explained by 4 clusters is: %s', km4$betweenss/km4$totss))
# 
# 
# 
# #centers for group interpretations 
# #add average seed and games won to the $centers output
# cbind(km3$centers,c3_means[,2:3])
# cbind(km4$centers,c4_means[,2:3])








