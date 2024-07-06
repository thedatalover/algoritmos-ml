#################
# Carga de Datos#
#################

# Instalar los paquetes necesarios (si es necesario)
if (!require(readr)) install.packages("readr")
if (!require(dplyr)) install.packages("dplyr")

# Cargar librerías necesarias
library(readr) # Para leer ficheros CSV y TXT
library(dplyr) # Librería para manipulación de datos

# Leer los nombres de columnas
column_names <- read_lines("datos/column_names.txt")

# Leer datos desde el fichero gene_expression.csv
gene_data <- read_delim(
  "datos/gene_expression.csv",
  delim = ";",
  col_names = FALSE
  )

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
data_to_clean <- gene_data

########################
# Funciones Auxiliares #
########################

# Función para contrar cuantas columnas hay por tipo de dato en un dataframe
count_columns_by_type <- function(df) {
  # Obtener los tipos de datos de cada columna
  types <- sapply(df, class)
  
  # Contar el número de columnas por cada tipo de dato
  type_counts <- table(types)
  
  # Convertir a dataframe para una presentación más clara
  type_counts_df <- as.data.frame(type_counts)
  colnames(type_counts_df) <- c("DataType", "ColumnCount")
  
  return(type_counts_df)
}

# Función personalizada para escalar
custom_scale <- function(x) {
  if (sd(x, na.rm = TRUE) != 0) {
    return(scale(x))
  } else {
    return(x)
  }
}

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

# Eliminar los valores nulos e infinitos (si los hubiera)
data_clean <- data_to_clean %>%
  filter_all(all_vars(!is.na(.))) %>%
  filter_all(all_vars(!is.infinite(.)))

###################################
# Remover Columnas sample y class #
###################################

class_data <- data_clean$class
data_clean <- select(data_clean, -c(sample, class))

###################################
# Identificación de Tipos de Dato #
###################################

tipos_dato <- count_columns_by_type(data_clean)
print(tipos_dato)

##########################################
# Identificación de Valores no Numéricos #
##########################################

# Identificar qué columnas no son numéricas
# Usamos sapply para aplicar una función a cada columna y luego identificamos las que no son numéricas
non_numeric_columns <- sapply(data_clean, function(x) !is.numeric(x))

# Transformar números almacenados como texto a datos numéricos
# asumiendo que se necesita revisar todas las columnas y convertir donde sea aplicable
data_clean <- data_clean %>%
  mutate(across(where(is.character), ~parse_number(.))) # Convierte texto a numéricos

# Verificar los cambios
tipos_dato <- count_columns_by_type(data_clean)
print(tipos_dato)

##########################
# Normalización de Datos #
##########################

# Normalizar los datos
# Escalar cada columna para que tenga media ~0 y desviación estándar ~1
normalized_data <- as.data.frame(lapply(data_clean, custom_scale))

# Añadir de nuevo las columnas "sample" y "class"
normalized_data$class <- class_data

# Guardar los datos normalizados en un fichero CSV
write.csv(normalized_data, "datos/gene_data_normalized.csv", row.names = FALSE)

