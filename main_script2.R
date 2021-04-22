source("reduccion_dimensionalidad.R")
source("export_excel.R")
source("print_membresia.R")
source("calcular_k.R")
source("algoritmos.R")
source("analisis_post_cluster.R")


install.packages("clValid")
install.packages("klaR")
install.packages("dplyr")
install.packages("XLConnect")
install.packages("e1071")
install.packages("dbscan")
install.packages("clusterSim")
install.packages("kohonen")
install.packages("FactoMineR")
install.packages("XLConnect")
install.packages("plotrix")


library(clValid) #para índice de Dunn
library(klaR) #para Kmodes
library(dplyr)  #para usar las funciones de manejo de dataframes
                #una de las dependencias de klaR sobreescribe select de dplyr
library(XLConnect)
library(e1071)  #para Cmeans
library(dbscan) #para sNN
library(clusterSim) #para los índices del análisis post-cluster
library(kohonen)  #para el SOM
library(FactoMineR) #dimensionality reduction, MCA
library(XLConnect)  #para exportar en Excel
library(plotrix)  #para algunas utilidades adjuntas a plot()



dataset <- as.data.frame(dataset2_tft)

pca <- principal_component_analysis(dataset)
# # # Exporto a excel para estudiar las dimensiones creadas
# exportar_eigenvalues_dimensiones(pca)
# # # Para la descripción de las dimensiones
# # # recomiendo hacer export a excel, que permite interpretar mejor los resultados
# # # "proba" es el coeficiente de significacion
# descripcion_dimensiones <- dimdesc(pca, axes=1:5, proba = 0.01)
# exportar_caracterizacion_dimensiones2(descripcion_dimensiones, 5)

#Saco el nuevo dataset con los nuevos nombres de variables para tratarlo más fácil
new_dataset <- as.data.frame(pca$ind$coord)
#cambio el nombre de las variables
nuevos_nombres <- c("1.Dinero-ismo",
                    "2.Tamaño-campaña-y-equipo-ismo",
                    "3.Todo-menos-dinero-y-duración-ismo",
                    "4.Muchos-años-pocas-personas-ismo",
                    "5.Inversiores-tardan-equipo-pequeño-ismo")
colnames(new_dataset) <- nuevos_nombres


# Llamadas a cada una de las clusterizaciones, descomentar la que se quiera
calcular_k(new_dataset)
num_clusters <- 6
kmeans <- test_kmeans(new_dataset, num_clusters)

calcular_centroides(new_dataset)
cmeans <- test_cmeans(new_dataset, 2)

knn <- test_knn(new_dataset)

#Kmodes necesita categoricas, lo cual implica que
# no podemos usar el dataset2

# el som requiere dos pasos
# esto es por tener un nivel de abstracción mayor
modelo_som <- test_som(new_dataset)
calcular_k(modelo_som$codes[[1]])
num_clusters <-  6
som <- test_kmeans(modelo_som$codes[[1]], num_clusters)


# print_membresia_kmeans(kmeans, dataset)
# print_membresia_cmeans(cmeans, dataset)
print_membresia_knn(knn, dataset)
# print_membresia_som(som, modelo_som)

#Script para el analisis post cluster
# analisis_post_cluster(new_dataset, kmeans, cmeans, knn, modelo_som, som)

