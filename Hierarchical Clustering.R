install.packages("factoextra",dependencies = TRUE)
library(factoextra)
library(ggplot2)

#data
iris
iris_data=iris[1:4]
iris_data
iris_data_scale=scale(iris_data)
iris_data_scale

#to get distance we use dist
d=dist(iris_data_scale)
d
#hierarichal clustrng it doesn't matter how many k because depend on distance
hc=hclust(d,method = "complete")
hc
#draw it
plot(hc)