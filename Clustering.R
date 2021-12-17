camp= read.csv("camp2.csv")
names(camp)

camp3 <- camp[,c(5:7,9:20)]
pairs(camp3)

library(tidyverse)
library(cluster)
library(factoextra)

camp_scaled <- scale(camp3)
index<- sample(nrow(camp_scaled), 0.6*nrow(camp_scaled))
camp_samp<- camp3[index,]
camp_scaled_samp<-camp_scaled[index,]

dim(camp_scaled)
str(camp_scaled)

#Elbow Method
set.seed(123)
fviz_nbclust(camp_scaled_samp, kmeans, method="wss",iter.max = 30,k.max = 10)

#Average Silhouette Method
fviz_nbclust(camp_scaled_samp, kmeans, method = "silhouette",iter.max = 30,k.max = 10)

#Gap Statistic Method
set.seed(123)
gap_stat <- clusGap(camp_scaled_samp, FUN =kmeans, nstart = 25, iter.max = 30,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
#plot the result
fviz_gap_stat(gap_stat)


k2 <- kmeans(camp_scaled_samp, centers = 2, nstart = 25, iter.max=30)
str(k2)
k2$totss

k3 <- kmeans(camp_scaled_samp, centers = 3, nstart = 25,iter.max=30)
str(k3)
length(k3$cluster)

clusters3<- (k3$cluster)
my_vec_new1 <- clusters3                     # Duplicate vector
my_vec_new1[my_vec_new1 == 2] <- 999
my_vec_new1[my_vec_new1 == 3] <- 2# Replace values
my_vec_new1[my_vec_new1 == 999] <- 3
my_vec_new1 <- clusters3                              # Print new vector


dim(camp_samp)
camp_new = cbind(camp_samp, clusters3)
head(camp_new)
dim(camp_new)
write.csv(camp_new,"camp_3clust.csv", row.names = FALSE)

k4 <- kmeans(camp_scaled_samp, centers = 4, nstart = 25,iter.max=30)
str(k4)

k5 <- kmeans(camp_scaled_samp, centers = 5, nstart = 25,iter.max=30)
str(k5)

k6 <- kmeans(camp_scaled_samp, centers = 6, nstart = 25,iter.max=30)
str(k6)

fviz_cluster(k2, data=camp_scaled_samp)
fviz_cluster(k3, data=camp_scaled_samp)
fviz_cluster(k5, data=camp_scaled_samp)
fviz_cluster(k6, data=camp_scaled_samp)


clusters6<- (k6$cluster)


dim(camp_samp)
camp_new = cbind(camp_samp, clusters6)
head(camp_new)
dim(camp_new)
write.csv(camp_new,"camp_6clust.csv", row.names = FALSE)


