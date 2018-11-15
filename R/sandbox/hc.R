# =================================================================
# Hierarchical Clustering using Single Linkage using CDC data
# Author: Zhihang Dong
# Dates: 07/18/2018 (ver. 0.1.0.)
# Version: 0.2.0 (Updated 08/04/2018)
# Notes:
# 1) Rows are observations (individuals) and columns are variables
# 2) Any missing value in the data must be removed or estimated.
# 3) The data must be standardized (i.e., scaled) to make variables comparable. 
# Recall that, standardization consists of transforming the variables such that 
# they have mean zero and standard deviation one.
# =================================================================

library(tidyverse)
library(factoextra)
library(cluster)
library(dendextend)
# load("person.Rdata")
df<-person6082
df<-df[1:150,]
df<-df[c(5,6)]
d <- dist(df, method = "euclidean")
hc1 <- hclust(d, method = "single" )
plot(hc1, cex = 0.6, hang = -1)
# Compute with agnes
hc2 <- agnes(df, method = "single")

# Agglomerative coefficient
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
# average    single  complete      ward 
# 0.9737577 0.9627772 0.9800452 0.9880115 

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

# Number of members in each cluster
table(sub_grp)
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

# Visualize the clusters
fviz_cluster(list(data = df, cluster = sub_grp))

# Compare two dedrogram using tanglegram
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc5)
tanglegram(dend1, dend2)

# Visualization Number 2
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

# Calculate how many clusters are optimized using average Silhouette Method 
fviz_nbclust(df, FUN = hcut, method = "silhouette")

