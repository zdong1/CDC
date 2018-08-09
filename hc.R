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
