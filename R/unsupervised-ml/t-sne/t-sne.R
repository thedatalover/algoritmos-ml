# Cargar los paquetes necesarios
library(Rtsne) # Para t-SNE
library(FNN) # Para caldular la taza de conservación
library(ggplot2) # Para graficar en 2D
library(plotly) # Para graficar en 3D

# Leer los datos
data <- read.csv("../datos/breast_cancer_clean.csv")

# Seleccionar solo las características numéricas para t-SNE
data_tsne <- data[,3:ncol(data)]

# Ejecutar t-SNE para 2 componentes
set.seed(42)  # Para reproducibilidad
tsne_2d <- Rtsne(data_tsne, dims = 2, perplexity = 30, verbose = TRUE)

# Ejecutar t-SNE para 3 componentes
tsne_3d <- Rtsne(data_tsne, dims = 3, perplexity = 30, verbose = TRUE)

# Función para calcular la tasa de conservación de los k-vecinos más cercanos
conservation_rate <- function(original_data, reduced_data, k) {
  original_nn <- get.knnx(data = original_data, query = original_data, k = k)
  reduced_nn <- get.knnx(data = reduced_data, query = reduced_data, k = k)
  overlap_count <- sapply(1:nrow(original_data), function(i) {
    length(intersect(original_nn$nn.index[i, ], reduced_nn$nn.index[i, ]))
  })
  mean(overlap_count) / k
}

# Calcular la conservación de los 10 vecinos más cercanos para 2D y 3D
# Interpretación: Un valor cercano a 1 significa que la mayoría de los k-vecinos
# más cercanos en el espacio original se mantienen como vecinos más cercanos
# en el espacio reducido. Esto indica una excelente preservación de la
# estructura local del conjunto de datos.
k_neighbors <- 10
rate_2d <- conservation_rate(data_tsne, tsne_2d$Y, k_neighbors)
rate_3d <- conservation_rate(data_tsne, tsne_3d$Y, k_neighbors)

print(paste("Tasa de conservación de 10-vecinos más cercanos en 2D:", rate_2d))
print(paste("Tasa de conservación de 10-vecinos más cercanos en 3D:", rate_3d))

# Preparar los datos para graficar en 2D
data_2d <- as.data.frame(tsne_2d$Y)
data_2d$diagnosis <- data$diagnosis

# Graficar los resultados en 2D
ggplot(data_2d, aes(x = V1, y = V2, color = diagnosis)) +
  geom_point(alpha = 0.7) +
  ggtitle("t-SNE - 2 Componentes") +
  xlab("Componente 1") +
  ylab("Componente 2")

# Preparar los datos para graficar en 3D
data_3d <- as.data.frame(tsne_3d$Y)
data_3d$diagnosis <- data$diagnosis

# Usar plotly para un gráfico interactivo en 3D
fig <- plot_ly(data_3d, x = ~V1, y = ~V2, z = ~V3, color = ~diagnosis, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Componente 1'),
                      yaxis = list(title = 'Componente 2'),
                      zaxis = list(title = 'Componente 3')),
         title = 't-SNE - 3 Dimensiones')
fig
