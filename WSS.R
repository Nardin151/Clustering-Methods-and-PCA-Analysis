Data = iris[1:4]
total_k = 15
wss_per_trail = matrix(c(0),nrow = 1, ncol = total_k)
k = 1
while (k < total_k){
k_means = kmeans(Data , k ,nstart = 25)
names(k_means)
k_means["cluster"]
centers = k_means["centers"] # rows represent the number of k and col represent the dimensions of the point
centers = data.frame(centers)
cluster = k_means["cluster"]
cluster = data.frame(cluster)
counter = 1
counter2 = 1
value = 0
wss_per_cluster = matrix(c(0),nrow = 1, ncol = k)
while(counter <= nrow(Data))
{
  while(counter2 <= ncol(Data))
  {
    value = (Data[counter,counter2] - centers[cluster[counter,1],counter2])**2 + value
    print(value)
    counter2 = counter2 + 1
  }
    wss_per_cluster[1,cluster[counter,1]] = wss_per_cluster[1,cluster[counter,1]] + value
    value = 0;
    print(cluster[counter,1])
    counter = counter + 1
    counter2 = 1
}
counter3 = 1
total_wss = 0
while (counter3 <= k)
  {
    total_wss = total_wss + wss_per_cluster[1,counter3]
    counter3 =  counter3 + 1
  }
  
total_wss
wss_per_trail[1,k] = total_wss
k = k + 1

}
e=c(wss_per_trail)
plot(e,type = "b")
  