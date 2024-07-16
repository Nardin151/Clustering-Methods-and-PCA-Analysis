install.packages("factoextra",dependencies = TRUE)
library(factoextra)
library(ggplot2)
# Loading data
data(iris)

# Structure
str(iris)
# Installing Packages
install.packages("fpc")

# Loading package
library(fpc)

# Remove label form dataset
iris_1 <- iris[-5]

# Fitting DBScan clustering Model
# to training dataset
set.seed(220) # Setting seed
DB <- dbscan(iris_1, eps = 0.5, MinPts = 4)
DB

# Checking cluster
DB$cluster

# Table to know each cluster contain how many numbers of data
table(DB$cluster, iris$Species)

# Plotting Cluster to all data
plot(DB, iris_1, main = "DBScan")
# Plotting Cluster to sepal Width vs Sepal Length
ne=iris_1[1:2]
plot(DB, ne, main = "sepal Width vs Sepal Length")

# or
fviz_cluster(DB,iris[,1:4],geom = "point")
#cluster 1 represents setosa
# cluster 2 represents versicolor
# cluster 3 represents virginica
