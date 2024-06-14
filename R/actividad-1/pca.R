# Cargar la librería necesaria para leer archivos CSV
library(readr)

# Leer el archivo CSV normalizado
data <- read_csv("datos/gene_data_normalized.csv")

# Excluimos las columnas con datos NA
data_for_pca <- select(data, -MIER3, -ZCCHC12, -RPL22L1)

# Excluímos las últimas dos columnas ('sample', 'class'),
data_for_pca <- select(data_for_pca, -sample, -class)

# Aplicar PCA al conjunto de datos
# Utilizamos scale. = FALSE porque los datos ya están normalizados
pca_result <- prcomp(data_for_pca, scale. = FALSE, center = TRUE)

# Ver los resultados de PCA
# Imprimir la importancia de los componentes principales
print(summary(pca_result))

# Ver las cargas de los primeros dos componentes principales
loadings <- pca_result$rotation[, 1:2]

# Visualización de los resultados de PCA
# Instalar y cargar ggplot2 para la visualización si aún no está instalada
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Crear un dataframe con los resultados de PCA para los primeros dos componentes principales
pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2], Sample = data$sample)

# Gráfico de dispersión de los primeros dos componentes principales
ggplot(pca_data, aes(x = PC1, y = PC2, color = Sample)) +
  geom_point(alpha = 0.5) +
  ggtitle("PCA - Primeros dos componentes principales") +
  xlab("Primer Componente Principal (PC1)") +
  ylab("Segundo Componente Principal (PC2)")

