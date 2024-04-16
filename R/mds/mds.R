# Cargar los paquetes necesarios
library(stats)   # Para la función cmdscale (MDS)
library(ggplot2) # Para graficar
library(plotly)  # Para graficar en 3D

# Leer los datos
data <- read.csv("../datos/breast_cancer_clean.csv")

# Seleccionar solo las características numéricas para el MDS
data_mds <- data[,3:ncol(data)]

# Calcular la matriz de distancias entre las muestras
dist_matrix <- dist(data_mds, method = "euclidean")

# Realizar MDS para 2 componentes
mds_2d <- cmdscale(dist_matrix, k = 2, eig = TRUE)

# Preparar los datos para graficar en 2D
data_2d <- as.data.frame(mds_2d$points)
data_2d$diagnosis <- data$diagnosis

# Graficar los resultados en 2D
ggplot(data_2d, aes(x = V1, y = V2, color = diagnosis)) +
  geom_point() +
  ggtitle("MDS - 2 Componentes") +
  xlab("Componente 1") +
  ylab("Componente 2")

# Realizar MDS para 3 componentes
mds_3d <- cmdscale(dist_matrix, k = 3, eig = TRUE)

# Preparar los datos para graficar en 3D
data_3d <- as.data.frame(mds_3d$points)
data_3d$diagnosis <- data$diagnosis

# Usar plotly para un gráfico interactivo en 3D
fig <- plot_ly(data_3d, x = ~V1, y = ~V2, z = ~V3, color = ~diagnosis, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Componente 1'),
                      yaxis = list(title = 'Componente 2'),
                      zaxis = list(title = 'Componente 3')),
         title = 'MDS - 3 Componentes')

# Mostrar el gráfico en 3D  
fig

# Calculo de la varianza explicada empleando el estrés
# Fórmula del estrés: sqrt(sum((dist(originales) - dist(proyectadas))^2) / sum(dist(originales)^2))
stress_2d <- sqrt(sum((dist_matrix - dist(as.matrix(mds_2d$points)))^2) / sum(dist_matrix^2))
stress_3d <- sqrt(sum((dist_matrix - dist(as.matrix(mds_3d$points)))^2) / sum(dist_matrix^2))

# Imprimir los valores de estrés
print(paste("Estrés para 2 componentes:", stress_2d))
print(paste("Estrés para 3 componentes:", stress_3d))

# Veamos con 4 componentes principales
mds_4d <- cmdscale(dist_matrix, k =4, eig = TRUE)
stress_4d <- sqrt(sum((dist_matrix - dist(as.matrix(mds_4d$points)))^2) / sum(dist_matrix^2))
print(paste("Estrés para 4 componentes:", stress_4d))
