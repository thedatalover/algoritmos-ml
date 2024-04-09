# Cargar el paquete necesario para PCA y gráficos
library(ggplot2) # Para gráficos 2D
library(plotly)  # Para gráficos 3D
library(readr)   # Para leer archivos CSV
library(stats)   # Para realizar PCA

# Leer los datos
data <- read.csv("../datos/breast_cancer_clean.csv")

# Seleccionar solo las características numéricas para el PCA
data_pca <- data[,3:ncol(data)]

# Realizar PCA
pca_result <- prcomp(data_pca, scale. = TRUE)

# Ver la varianza explicada por cada componente principal
print(summary(pca_result))

# PCA para 2 componentes principales
# Preparar los datos para graficar
data_2d <- data.frame(pca_result$x[,1:2])
data_2d$diagnosis <- data$diagnosis

# Graficar los resultados en 2D
ggplot(data_2d, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point() +
  ggtitle("PCA - 2 Componentes Principales") +
  xlab("Primer Componente Principal") +
  ylab("Segundo Componente Principal")

# PCA para 3 componentes principales
# Preparar los datos para graficar
data_3d <- data.frame(pca_result$x[,1:3])
data_3d$diagnosis <- data$diagnosis

# Usar plotly para un gráfico interactivo en 3D
fig <- plot_ly(data_3d, x = ~PC1, y = ~PC2, z = ~PC3, color = ~diagnosis, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = 'PCA - 3 Componentes Principales')

# Mostrar el gráfico
fig
