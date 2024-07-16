Data = iris[1:4]
a = matrix(c(0),nrow=nrow(Data));b = matrix(c(0),nrow=nrow(Data))
a  = data.frame(a);b = data.frame(b);S =matrix(c(0), nrow=nrow(Data));S = data.frame(S)
counter = 0
Man = 0
sum = 0
Silhoutte_average_per_k = 0;
silhouette_scores = matrix(c(0),nrow = 1,ncol = 7)
k_trials = 7
k = 2
while (k <= k_trials){
k_means = kmeans(Data,k,nstart = 25)
cluster = data.frame(k_means["cluster"])
new_data = cbind(Data,cluster,a,b,S)
  for (point in 1:nrow(new_data)){
    for (elements in 1:nrow(new_data)){
      if(point == elements)
      {next}
      if (new_data[point,"cluster"] == new_data[elements,"cluster"])
      {
        for (column in 1:(ncol(new_data)-4)){
          Man = abs(new_data[point , column] - new_data[elements, column]) + Man
        }
        sum = sum +1 
      }
    }
    new_data[point,"a"] = Man / sum
    Man = 0
    sum = 0
  }
  
  for (point in 1:nrow(new_data)){
    for (elements in 1:nrow(new_data)){
      if (new_data[point,"cluster"] != new_data[elements,"cluster"])
      {
        for (column in 1:(ncol(new_data)-4)){
          Man = abs(new_data[point , column] - new_data[elements, column]) + Man
        }
        sum =sum +1 
      }
    }
    new_data[point,"b"] = Man / sum
    Man = 0
    sum = 0
  }
  
  for (point in 1:nrow(new_data)){
    new_data[point,"S"] = (new_data[point,"b"] - new_data[point,"a"])/max(new_data[point,"b"] , new_data[point,"a"])
  }
  for (point in 1:max(new_data["cluster"])){
    sum = 0
    SC = 0
    for (elements in 1:nrow(new_data)){
      if (new_data[elements,"cluster"] == new_data[point,"cluster"])
      {
        sum = sum + 1
        SC = SC + new_data[point,"S"]
      }
    }
    SC = SC / sum
    Silhoutte_average_per_k = SC +  Silhoutte_average_per_k
    print(SC)
    SC = 0
    sum = 0
  }
  silhouette_scores[k] = Silhoutte_average_per_k
  Silhoutte_average_per_k  = 0;
  k = k + 1
}
silhouette_scores_vector = c(silhouette_scores)
plot(silhouette_scores_vector,type = "b")


