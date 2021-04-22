#sólo valores numericos
principal_component_analysis <- function(dataset){

  #Para poder reproducir los resultados
  set.seed(12121)

  num_max_componentes <- 5

  new_dataset <- PCA(dataset,
                        ncp = num_max_componentes)

  return(new_dataset)
}

#no acepta valores numericos
multiple_correspondence_analysis <- function(dataset){

  #Para poder reproducir los resultados
  set.seed(12121)

    num_max_componentes <- 12

    result<- MCA(dataset,
                 ncp = num_max_componentes,
                 graph = FALSE,
                 quali.sup = 22)


  return(result)
}




