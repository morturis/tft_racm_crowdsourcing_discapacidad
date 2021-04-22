
#Asume que la clusterizacion tiene los colores
print_membresia_kmeans <- function (clusterizacion, dataset){
  print(table(clusterizacion))
  #sustituyo valores por clusteres
  colores <- c("red", "blue", "green", "magenta", "darkgoldenrod2", "darkgreen", "cyan", "yellow",
               "brown", "coral")
  for(cluster in 1:length(unique(clusterizacion))){
    clusterizacion <- replace(clusterizacion, clusterizacion==cluster, colores[cluster])
  }
  #Plots de las distintas dimensiones,
  # Intento usar estos plots para que se ilustre graficamente los clusteres
  # Para cambiar qué variables ocupan los ejes X o Y,
  #   modificar el vector que aparece junto a dataset
  for (i in 1:ncol(dataset)) {
    for(j in 2:ncol(dataset)){
      if(j<=i) next #skip graphs that were already drawn
      plot(dataset[c(i,j)],
           col = clusterizacion)

      #Añade lineas en 0 a las gráficas
      #Lineas de eje 0
      abline(h=0, lty = 2)
      abline(v=0, lty = 2)

    }
  }
}

print_membresia_cmeans <- function(clusterizacion, dataset){
  #sustituyo los valores por colores
  colores <- c("red", "blue", "green", "magenta", "darkgoldenrod2", "darkgreen", "cyan", "yellow",
               "brown", "coral")

  for(c in 1:length(unique(clusterizacion$cluster))){
    clusterizacion$cluster <- replace(clusterizacion$cluster,
                                      clusterizacion$cluster==c,
                                      colores[c])
  }
  print(table(clusterizacion$cluster))

  #Plots de las distintas dimensiones,
  # Intento usar estos plots para que se ilustre graficamente los clusteres
  # Para cambiar qué variables ocupan los ejes X o Y,
  #   modificar el vector que aparece junto a dataset
  for (i in 1:ncol(dataset)) {
    for(j in 2:ncol(dataset)){
      if(j<=i) next #skip graphs that were already drawn

      plot(dataset[c(i,j)],
           col = clusterizacion$cluster)

      # #esconder los puntos
      # plot(dataset[c(i,j)],
      #       col = "white")

      #Añade lineas en 0 a las gráficas
      #Lineas de eje 0
      abline(h=0, lty = 2)
      abline(v=0, lty = 2)

      # #Circulos para representar los clusteres
      # for(k in 1:nrow(clusterizacion$centers)){
      #   draw.circle(x=clusterizacion$centers[k,i],
      #               y=clusterizacion$centers[k,j],
      #               radius = 0.1,
      #               border = colores[k])
      # }
    }
  }
}
print_membresia_kmodes <- function(clusterizacion, dataset){
  #sustituyo los valores por colores
  colores <- c("red", "blue", "green", "magenta", "darkgoldenrod2", "darkgreen", "cyan", "yellow")
  for(c in 1:length(unique(clusterizacion$cluster))){
    clusterizacion$cluster <- replace(clusterizacion$cluster,
                                      clusterizacion$cluster==c,
                                      colores[c])
  }
  print(table(clusterizacion$cluster))

}

print_membresia_knn <- function(clusterizacion, dataset){
  print(table(clusterizacion$cluster))
  #sustituyo los valores por colores
  colores <- c("red", "blue", "green", "magenta", "darkgoldenrod2", "darkgreen", "cyan", "yellow")
  for(c in 1:length(unique(clusterizacion$cluster))){
    clusterizacion$cluster <- replace(clusterizacion$cluster,
                                      clusterizacion$cluster==c,
                                      colores[c])
  }
  #dibujo outliers que no pertenezcan a cluster en negro
  clusterizacion$cluster <- replace(clusterizacion$cluster,
                                    clusterizacion$cluster==0,"darkgrey")

  print((table(clusterizacion$cluster)))
  #Plots de las distintas dimensiones,
  # Intento usar estos plots para que se ilustre graficamente los clusteres
  # Para cambiar qué variables ocupan los ejes X o Y,
  #   modificar el vector que aparece junto a new_dataset
  for (i in 1:ncol(dataset)) {
    for(j in 2:ncol(dataset)){
      if(j<=i) next #skip graphs that were already drawn
      plot(dataset[c(i,j)],
           col = clusterizacion$cluster)

      #Añade lineas en 0 a las gráficas
      #Lineas de eje 0
      abline(h=0, lty = 2)
      abline(v=0, lty = 2)
    }
  }
}

print_membresia_som <- function (clusterizacion, modelo){

  print(table(clusterizacion))

  plot(modelo,
       type="mapping",
       #bgcol = pretty_palette[clusterizacion],
       main = "Clusters")

  add.cluster.boundaries(modelo, clusterizacion)


  plot(modelo,
       type="codes",
       #bgcol = pretty_palette[clusterizacion],
       main = "Clusters")

  add.cluster.boundaries(modelo, clusterizacion)


  plot(modelo,
       type="count",
       #bgcol = pretty_palette[clusterizacion],
       main = "Clusters")

  add.cluster.boundaries(modelo, clusterizacion)


  #   print(result)
  #   plot(result, type="count")
  #   plot(result, type="dist.neighbours")
  #   plot(result, type="codes")
  #   plot(result, type="property")
  #   plot(result, type="quality")
  #   plot(result, type="mapping")
  #   plot(result, type = "property",
  #        property = result$codes[[1]],
  #        main=names(result$data)[1])

  }
