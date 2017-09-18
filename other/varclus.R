library(Hmisc)
library(dplyr)
setwd('/home/sshao/other/movies')
movies <- read_csv('/home/sshao/other/movie_metadata.csv')

num <- select_if(movies,is.numeric)
num[is.na(num)] <- 0
clus <- varclus(as.matrix(num))
plot(clus)
cluster_assignment = cutree(clus$hclust, k=12)
cluster_assignment
names(which(cluster_assignment==3))
