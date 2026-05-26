# 1. Instalación de paquetes necesarios
# install.packages("readxl")
# install.packages("dplyr")

# 2. Carga de librerías
library(readxl)
library(dplyr)

# 3. Definición de las rutas y hojas de los archivos
ruta_base_datos                  <- "Datos.xlsx"
hoja_escenarios                  <- "Escenarios"
hoja_variables_sociodemograficas <- "Variables sociodemográficas"

# 4. Carga de los conjuntos de datos
cat("Intentando cargar la hoja:", hoja_escenarios, "del archivo:", ruta_base_datos, "\n")
df_escenarios <- try(read_excel(ruta_base_datos, sheet = hoja_escenarios))

cat("Intentando cargar la hoja:", hoja_variables_sociodemograficas, "del archivo:", ruta_base_datos, "\n")
df_sociodemografia <- try(read_excel(ruta_base_datos, sheet = hoja_variables_sociodemograficas))

# Verificación de carga exitosa
if (exists("df_escenarios") && is.data.frame(df_escenarios)) {
  cat("¡Éxito! Archivos cargados.\n")
  cat("--- Nombres de columnas detectados en Escenarios ---\n")
  print(names(df_escenarios))
  cat("---------------------------------------\n")
}

# 5. Merge de los dataframe
# Hacemos la unión solo si ambas bases de datos son data.frames válidos
if (is.data.frame(df_escenarios) && is.data.frame(df_sociodemografia)) {
  
  cat("Uniendo bases de datos...\n")
  df_completo <- df_escenarios %>%
    left_join(df_sociodemografia, by = "identificacion")
  
  # 6. Verificación preliminar
  cat("Dimensiones del dataset final (Filas, Columnas): ", dim(df_completo), "\n")
  
  # Damos un vistazo a las primeras filas y la estructura
  glimpse(df_completo) 
  
} else {
  cat("Error: No se puede hacer el Merge. Verifica que el archivo Datos.xlsx esté en tu directorio de trabajo.\n")
}