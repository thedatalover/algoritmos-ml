#################
# Carga de Datos#
#################

# Cargar librerías necesarias
library(readr) # Para leer ficheros CSV y TXT
library(dplyr) # Librería para manipulación de datos

# Leer los nombres de columnas
column_names <- read_lines("datos/column_names.txt")

# Leer datos desde el fichero gene_expression.csv
gene_data <- read_csv2("datos/gene_expression.csv", col_names = FALSE)

# Asignar los nombres de columnas
colnames(gene_data) <- column_names

# Verificar datos
head(gene_data)

# Anadir columnas con el número de muestra (sample) y la clase (class)

# Leer el fichero con las columnas "sample" y "class"
sample_class_data <- read_csv2("datos/classes.csv", col_names = FALSE)

# Asignar nombres de columna
colnames(sample_class_data) <- c("sample", "class")

# Combinar los dataframes gene_data y sample_class_data
gene_data$sample <- sample_class_data$sample
gene_data$class <- sample_class_data$class

# Comprobar datos
head(gene_data)

# Guardar datos unificados en un nuevo fichero CSV
write.csv(gene_data, "datos/gene_data.csv", row.names = FALSE)

################################
# Preparar Datos para Limpieza #
################################

# Crear una dataframe para limpieza excluyendo las últimas dos columnas
data_to_clean = select(gene_data, -sample, -class)

# Verificar las dimensiones del conjunto de datos
print(paste("El conjunto de datos tiene", nrow(data_to_clean), "filas y", ncol(data_to_clean), "columnas."))

##########################################
# Identificación de Valores no Numéricos #
##########################################

# Identificar qué columnas no son numéricas
# Usamos sapply para aplicar una función a cada columna y luego identificamos las que no son numéricas
non_numeric_columns <- sapply(data_to_clean, function(x) !is.numeric(x))

# Imprimir los nombres de las columnas no numéricas
non_numeric_names <- names(data_to_clean)[non_numeric_columns]
print(non_numeric_names)

# Verificar los datos almacenados como texto
# data_to_clean$CFB

# Visualizar el tipo de cada una de las columnas no numéricas
non_numeric_types <- sapply(data_to_clean[non_numeric_columns], class)
print(non_numeric_types)

# Transformar números almacenados como texto a datos numéricos
# asumiendo que se necesita revisar todas las columnas y convertir donde sea aplicable
data_to_clean <- data_to_clean %>%
  mutate(across(where(is.character), ~parse_number(.))) # Convierte texto a numéricos

# Verificar los cambios
non_numeric_columns <- sapply(data_to_clean, function(x) !is.numeric(x))
non_numeric_names <- names(data_to_clean)[non_numeric_columns]
print(non_numeric_names)

#################################
# Identificación de Datos Nulos #
#################################

# Identificar valores nulos en el conjunto de datos
# Calcular el número de valores NA por columna
na_count_per_column <- sapply(data_to_clean, function(x) sum(is.na(x)))

# Para obtener una visión más general, podemos calcular el total de NAs en todo el conjunto de datos
total_nas <- sum(na_count_per_column)
print(paste("Total de valores NA en el conjunto de datos:", total_nas))

# Para ver las filas que contienen al menos un NA, puedes usar:
rows_with_na <- which(rowSums(is.na(data_to_clean)) > 0)
print(paste("Hay", length(rows_with_na), "filas con al menos un valor NA."))

#####################################
# Identificación de Datos Infinitos #
#####################################

# Identificar valores nulos en el conjunto de datos
# Calcular el número de valores NA por columna
inf_count_per_column <- sapply(data_to_clean, function(x) sum(is.infinite(x)))

# Para obtener una visión más general, podemos calcular el total de NAs en todo el conjunto de datos
total_inf <- sum(inf_count_per_column)
print(paste("Total de valores infinitos en el conjunto de datos:", total_inf))

##########################
# Normalización de Datos #
##########################

# Normalizar los datos
# Escalar cada columna para que tenga media ~0 y desviación estándar ~1
normalized_data <- as.data.frame(lapply(data_to_clean, scale))

# Añadir de nuevo las columnas "sample" y "class"
normalized_data$sample <- gene_data$sample
normalized_data$class <- gene_data$class

# Guardar los datos normalizados en un fichero CSV
write.csv(normalized_data, "datos/gene_data_normalized.csv", row.names = FALSE)
