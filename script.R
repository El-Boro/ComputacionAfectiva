# 1. Instalación de paquetes necesarios
# install.packages("readxl")
# install.packages("dplyr")

# 2. Carga de librerías
library(readxl)
library(dplyr)
library(ggplot2)

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
}

# 6. Gráfico Primer Actividad
# Creamos el gráfico de escenarios con múltiple dimensiones teniendo como eje X el tiempo
cat("Generando gráficos individuales y guardando en el disco...\n")

# Seed fija para reproducir resultados
set.seed(5) 
individuos_azar <- sample(unique(df_completo$identificacion), 3)

for (individuo in individuos_azar) {
  df_indiv <- df_completo %>%
    filter(identificacion == individuo) %>%
    arrange(`Hora actividad redondeada`) # Orden cronológico
  
  # 2. Creamos el gráfico individual
  grafico <- ggplot(df_indiv, aes(x = as.character(`Hora actividad redondeada`), 
                                  y = `Etiqueta Actividad`, 
                                  group = 1)) +
    geom_point(color = "red", size = 3) +
    geom_line(color = "red", alpha = 0.5, linetype = "dashed") +
    labs(title = paste("Secuencia de Actividades Diarias"),
         subtitle = paste("Individuo:", individuo), # El nombre cambia dinámicamente
         x = "Hora de la Actividad",
         y = "Tipo de Actividad") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  # 3. Definimos el nombre del archivo (Ej: "Grafico_Majo51309rodrigo.png")
  nombre_archivo <- paste0("Grafico_", individuo, ".png")
  
  # 4. Guardamos el gráfico en el disco duro
  # width y height están en pulgadas. dpi = 300 asegura calidad de impresión/publicación
  ggsave(filename = nombre_archivo, 
         plot = grafico, 
         width = 8, 
         height = 5, 
         dpi = 300,
         bg = "white") # Fondo blanco para evitar transparencias extrañas en el PNG
  
  # Mensaje en consola para avisarte que se guardó exitosamente
  cat("¡Gráfico guardado exitosamente como:", nombre_archivo, "!\n")
}

cat("\nProceso terminado. Revisa tu carpeta de trabajo para ver las imágenes.\n")