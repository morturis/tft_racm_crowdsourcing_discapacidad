test_kmeans <-function(dataset, num_clusters){
  set.seed(12121)   #seteo la seed para poder reproducir los resultados

  result <- kmeans(dataset,
                   num_clusters,
                   iter.max = 1000)

  obj <- fitted(result,
                method = "classes")   #este metodo se usa para obtener el output del modelo
  return(obj)
}

test_cmeans <-function(dataset, k){
  set.seed(12121)   #seteo la seed para poder reproducir los resultados

  #parametros para cmeans
  fuzzification = 2

  result <- cmeans(dataset,
                   centers = k,
                   iter.max = 1000,
                   dist="manhattan",
                   m = fuzzification)

  return(result)
}
test_knn <- function(dataset){
  set.seed(12121)
  #tamaño del vecindario, numero de vecinos (de cada punto) que se guardan
  tamano_vecindario <- floor(sqrt(nrow(dataset)))

  #si dos puntos comparten este numero de vecinos, ambos puntos son similares
  # a veces mencionado en literatura como K_min
  num_vecinos_para_similares <- floor(tamano_vecindario/5)

  #cuantos puntos tienen que ser similares entre si ppara considerarlo un cluster
  min_pts_per_cluster <- floor(tamano_vecindario*.95)


  result <- sNNclust(dataset,
                     k = tamano_vecindario,
                     eps = num_vecinos_para_similares,
                     minPts = min_pts_per_cluster,
                     search="dist")  #necesario para poder comparar con el resto de clusterizaciones
  print(paste0("SNN encuentra ", length(unique(result$cluster))-1, " clusteres"))


  return(result)
}
test_kmodes <- function(dataset, k){
  set.seed(12121)   #seteo la seed para poder reproducir los resultados


  return(kmodes(dataset,
                modes = k,
                iter.max = 100,
                weighted = FALSE))
}
test_som <-function(dataset){
  set.seed(12121)

  #según vesanto, coger un grid de tamaño 5*sqrt(dataset.size)
  size_of_grid <- floor(5*sqrt(nrow(dataset)))
  side_length <- ceiling(sqrt(size_of_grid))  #coge el entero más cercano por arriba

  result <- som(as.matrix(dataset),
                grid = somgrid(xdim=side_length, ydim=side_length, topo="hexagonal"),
                radius=floor(side_length/2))

  return(result)
}
