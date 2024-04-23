# Cargar los paquetes necesarios
library(vegan)  # Para isomap
library(ggplot2) # Para graficar en 2D
library(plotly) # Para graficar en 3D

# Leer los datos
data <- read.csv("../datos/breast_cancer_clean.csv")

# Seleccionar solo las características numéricas para Isomap
data_iso <- data[,3:ncol(data)]

# Calcular la matriz de distancias entre las muestras
dist_matrix <- dist(data_iso, method = "euclidean")

# Realizar Isomap para 2 y 3 componentes
iso_2d <- isomap(dist_matrix, k = 5, ndim = 2)  # Usamos 5 vecinos más cercanos
iso_3d <- isomap(dist_matrix, k = 5, ndim = 3)

# Calcular estrés basado en la discrepancia de las distancias
# Estrés se calcula como: sqrt(sum((distancias originales - distancias en baja dimensión)^2) / sum(distancias originales^2))
# Valores Típicos y Guías Generales para el Estrés
# 0% a 0.05% (0 a 0.05): Estrés extremadamente bajo, excelente preservación de distancias.
# 0.05% a 0.1% (0.05 a 0.1): Estrés muy bajo, muy buena representación de las distancias.
# 0.1% a 0.2% (0.1 a 0.2): Estrés bajo, buena representación.
# 0.2% a 0.4% (0.2 a 0.4): Estrés moderado, representación aceptable.
# Mayor que 0.4% (>0.4): Estrés alto, la preservación de distancias puede ser inadecuada para algunas aplicaciones.
stress_2d <- sqrt(sum((dist_matrix - dist(as.matrix(iso_2d$points)))^2) / sum(dist_matrix^2))
stress_3d <- sqrt(sum((dist_matrix - dist(as.matrix(iso_3d$points)))^2) / sum(dist_matrix^2))
print(paste("Estrés para 2 componentes:", stress_2d))
print(paste("Estrés para 3 componentes:", stress_3d))

# Preparar datos para graficar
data_2d <- as.data.frame(iso_2d$points)
data_2d$diagnosis <- data$diagnosis

data_3d <- as.data.frame(iso_3d$points)
data_3d$diagnosis <- data$diagnosis

# Graficar Isomap en 2D
ggplot(data_2d, aes(x = Dim1, y = Dim2, color = diagnosis)) +
  geom_point() +
  ggtitle("Isomap - 2 Dimensiones") +
  xlab("Componente 1") +
  ylab("Componente 2")

# Usar plotly para un gráfico interactivo en 3D
fig <- plot_ly(data_3d, x = ~Dim1, y = ~Dim2, z = ~Dim3, color = ~diagnosis, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Componente 1'),
                      yaxis = list(title = 'Componente 2'),
                      zaxis = list(title = 'Componente 3')),
         title = 'Isomap - 3 Dimensiones')
fig

