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


dataset <- as.data.frame(dataset1_tft)

#MCA para reducir la dimensionalidad
mca <- multiple_correspondence_analysis(dataset) #el número del dataset, 1 o 2


# # Exporto a excel para estudiar las dimensiones creadas
# exportar_eigenvalues_dimensiones(mca)
# # Para la descripción de las dimensiones
# # recomiendo hacer export a excel, que permite interpretar mejor los resultados
# # "proba" es el coeficiente de significacion
descripcion_dimensiones <- dimdesc(mca, axes=1:12, proba = 0.01)
exportar_caracterizacion_dimensiones(descripcion_dimensiones, 12)


#Saco el nuevo dataset con los nuevos nombres de variables para tratarlo más fácil
new_dataset <- as.data.frame(mca$ind$coord)
#cambio el nombre de las variables
nuevos_nombres <- c("1.Nivel de problemas sociales",
                    "2.JoverParado-ismo",
                    "3.AmaDeCasa-ismo",
                    "4.TyrionLannister-ismo",
                    "5.DivorciadoNini-ismo",
                    "6.ViejaGloria-ismo",
                    "7.NiTrabajoNiAhorro-ismo",
                    "8.FPsoltero-ismo",
                    "9.DivorciadaQuiereTrabajar-ismo",
                    "10.MayorSinEducación-ismo",
                    "11.SensorialSinPensión-ismo",
                    "12.PadreQueCamina-ismo" )
colnames(new_dataset) <- nuevos_nombres


# Llamadas a cada una de las clusterizaciones, descomentar la que se quiera
# Para saber el número de clusteres de ambos casos, mirar la consola
calcular_k(new_dataset)
num_clusters <- 8
kmeans <- test_kmeans(new_dataset, num_clusters)


calcular_centroides(new_dataset)
cmeans <- test_cmeans(new_dataset, 10)

knn <- test_knn(new_dataset)

#Kmodes necesita categoricas, lo cual implica que
# no podemos usar el dataset de dimensionalidad reducida
calcular_modas(dataset)
kmodes <- test_kmodes(dataset, 8) #aquí va el número de modas

#el som requiere dos pasos, el som y el kmeans
modelo_som <- test_som(new_dataset)
calcular_k(modelo_som$codes[[1]])
num_clusters <-  8
som <- test_kmeans(modelo_som$codes[[1]], num_clusters)

# Funciones para imprimer gráficas
print_membresia_kmeans(kmeans, new_dataset)
print_membresia_cmeans(cmeans, new_dataset)
print_membresia_kmodes(kmodes, dataset)
print_membresia_knn(knn, new_dataset)
print_membresia_som(som, modelo_som)

# #Script para el analisis post cluster
analisis_post_cluster(new_dataset, kmeans, cmeans, knn, modelo_som, som)
