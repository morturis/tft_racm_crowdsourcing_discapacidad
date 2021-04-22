calcular_k <- function(dataset){
  test <- 1:23
  for(num_clusters in 2:23){
    set.seed(12121)
    test[num_clusters] <- kmeans(new_dataset, num_clusters)$tot.withinss
  }
  print("Clusters a crear de acuerdo al SSE es el codo de la gráfica")
  plot( test, type="b", xlab="Number of Clusters",
        ylab="SSE")
  return;
}

calcular_modas <- function(dataset){
  set.seed(12121)

  best_k <- 1
  best_k_error_squared <- Inf
  tabla_errores <- data.frame("num_cluster"=integer(), "error^2"=integer())

  #pruebo varios valores de K
  for(k in 2:23){
    test<-kmodes(dataset,
                 modes = k,
                 iter.max = 100,
                 weighted = FALSE)
    current_error_squared <- sum(test$withindiff^2)
    if(current_error_squared < best_k_error_squared){
      best_k_error_squared<-current_error_squared
      best_k <- k
    }
    tabla_errores[nrow(tabla_errores)+1,] = list(k,current_error_squared)
  }
  plot(tabla_errores, type = "b", xlab="K", ylab = "SSE")
}

calcular_centroides <- function(dataset){
  #set.seed(12121)

  best_k <- 1
  best_k_xb <- Inf
  tabla_errores <- data.frame("num_cluster"=integer(), "XieBeni"=integer())

  #pruebo varios valores de K
  for(k in 2:18){
    test<-test_cmeans(dataset,k)

    current_xb <-fclustIndex(y=test, x=dataset, index="xie.beni")
    #current_xb <-sum(test$withinerror^2)

    if(current_xb < best_k_xb){
      best_k_xb<-current_xb
      best_k <- k
    }
    tabla_errores[nrow(tabla_errores)+1,] = list(k,current_xb)
  }
  # plot(tabla_errores, type = "b", xlab="K", ylab = "XieBeni", ylim=c(1, 100000))
  print(paste0("Mejor número de centroides ", best_k," con XieBeni = ", best_k_xb))
  print(tabla_errores)
}

