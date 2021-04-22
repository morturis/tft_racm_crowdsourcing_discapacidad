analisis_post_cluster <- function (new_dataset, kmeans, cmeans, knn, modelo_som, som){

  tabla_cli <-data.frame("Algoritmo"=character(),
                         "Dunn"=double(),
                         "Davies-Mouldin"=double(),
                         "Silueta"=double(),
                         "Xie-Beni"=double())

  distancia = dist(new_dataset)
  tabla_cli[1,] <- list("Kmeans",
                        dunn(distancia, kmeans),
                        index.DB(new_dataset, kmeans)$DB,
                        index.S(distancia, kmeans),
                        Inf)


  tabla_cli[2,] <- list("Cmeans",
                        dunn(distancia, cmeans$cluster),
                        index.DB(new_dataset, cmeans$cluster)$DB,
                        index.S(distancia, cmeans$cluster),
                        as.double(fclustIndex(y=cmeans, x=new_dataset, index="xie.beni")))


  tabla_cli[3,] <- list("Knn",
                        dunn(distancia, knn$cluster),
                        index.DB(new_dataset, knn$cluster)$DB,
                        index.S(distancia, knn$cluster),
                        Inf)



  distancia_som <- dist(modelo_som$codes[[1]])
  tabla_cli[4,] <- list("SOM",
                        dunn(distancia_som, som),
                        index.DB(modelo_som$codes[[1]], som)$DB,
                        index.S(distancia_som, som),
                        Inf)
  print(tabla_cli)

}
