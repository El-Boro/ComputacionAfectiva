# 1. Instalación de paquetes necesarios
# install.packages("readxl")
# install.packages("dplyr")

# 2. Carga de librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

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
cat("Generando gráficos...\n")

set.seed(5) 
individuos_azar <- sample(unique(df_completo$identificacion), 3)

horas_completas <- c(sprintf("%02d:00", 1:23), "00:00")

for (individuo in individuos_azar) {
  
  # 1. Filtramos y preparamos la base
  df_indiv <- df_completo %>%
    filter(identificacion == individuo) %>%
    arrange(`Hora actividad redondeada`) %>%
    mutate(
      Hora_Limpia = format(as.POSIXct(`Hora actividad redondeada`), "%H:%M"),
      Hora_Num = match(Hora_Limpia, horas_completas) 
    )
  
  # 2. Obtenemos las categorías únicas para las 3 variables
  niveles_actividad <- levels(factor(df_indiv$`Etiqueta Actividad`))
  niveles_interaccion_raw <- levels(factor(df_indiv$`Etiqueta Interacción`))
  niveles_lugar <- levels(factor(df_indiv$`Lugar de actividad`))
  
  if ("Estaba solo" %in% niveles_interaccion_raw) {
    niveles_interaccion <- c("Estaba solo", setdiff(niveles_interaccion_raw, "Estaba solo"))
  } else {
    niveles_interaccion <- niveles_interaccion_raw
  }
  
  n_act <- length(niveles_actividad)
  n_int <- length(niveles_interaccion)
  n_lug <- length(niveles_lugar)
  
  # Ajuste de longitud de leyenda
  etiquetas_actividad_cortas   <- str_wrap(niveles_actividad, width = 20)
  etiquetas_interaccion_cortas <- str_wrap(niveles_interaccion, width = 20)
  etiquetas_lugar_cortas       <- str_wrap(niveles_lugar, width = 20)
  
  # 3. Algoritmo de distribución equitativa
  escala_interaccion <- function(x) {
    if (n_int > 1 && n_act > 1) { 1 + (x - 1) * (n_act - 1) / (n_int - 1) } 
    else { rep(max(n_act, 1) / 2, length(x)) }
  }
  
  # Algoritmo para estirar el Lugar de Actividad
  escala_lugar <- function(x) {
    if (n_lug > 1 && n_act > 1) { 1 + (x - 1) * (n_act - 1) / (n_lug - 1) } 
    else { rep(max(n_act, 1) / 2, length(x)) }
  }
  
  breaks_interaccion <- escala_interaccion(1:n_int)
  breaks_lugar       <- escala_lugar(1:n_lug)
  
  df_indiv <- df_indiv %>%
    mutate(
      Act_Num  = as.numeric(factor(`Etiqueta Actividad`, levels = niveles_actividad)),
      Int_Rank = as.numeric(factor(`Etiqueta Interacción`, levels = niveles_interaccion)),
      Int_Num  = escala_interaccion(Int_Rank),
      Lug_Rank = as.numeric(factor(`Lugar de actividad`, levels = niveles_lugar)),
      Lug_Num  = escala_lugar(Lug_Rank)
    )
  
  # 4. Creamos el gráfico
  grafico <- ggplot(df_indiv, aes(x = Hora_Num)) + 
    
    # Tipo de actividad
    geom_point(aes(y = Act_Num), color = "red", size = 3) +
    geom_line(aes(y = Act_Num, group = 1), color = "red", alpha = 1, linetype = "solid") +
    
    # Grupo social
    geom_point(aes(y = Int_Num), color = "blue", size = 3, shape = 17) +
    geom_line(aes(y = Int_Num, group = 1), color = "blue", alpha = 1, linetype = "solid") +
    
    # Lugar de actividad
    geom_point(aes(y = Lug_Num), color = "black", size = 3, alpha = 0.7, shape = 15) +
    geom_line(aes(y = Lug_Num, group = 1), color = "black", alpha = 1, linetype = "solid") +
    
  # Creación del eje negro extra
  
  # 1. Línea vertical del eje
  annotate("segment", x = -4, xend = -4, y = -Inf, yend = +Inf, color = "black", linewidth = 1) +
    # 2. Marca horizontal
    annotate("segment", x = -4, xend = 0.5, y = -Inf, yend = -Inf, color = "black", linewidth = 1) +
    # 3. Textos de las etiquetas
    annotate("text", x = -4.5, y = breaks_lugar, label = etiquetas_lugar_cortas, hjust = 1, color = "black", size = 3) +
    # 4. Título principal de este eje
    annotate("text", x = -7, y = (1 + max(n_act, 1)) / 2, label = "Lugar de Actividad", angle = 90, fontface = "bold", color = "black", size = 3.5) +
    
    # Permitimos a R dibujar por fuera del área cuadriculada para que aparezca nuestro eje
    coord_cartesian(clip = "off", xlim = c(1, 24)) +
    
    # Eje X Numérico (1 al 24) pero con etiquetas de texto (01:00 a 00:00)
    scale_x_continuous(
      breaks = 1:24,
      labels = horas_completas,
      expand = expansion(add = c(0.5, 0.5))
    ) +
    
    # Ejes Y Nativos (Rojo y Azul)
    scale_y_continuous(
      name = "Tipo de Actividad",
      breaks = 1:n_act,
      labels = etiquetas_actividad_cortas, 
      limits = c(1, max(n_act, 1)), 
      
      sec.axis = sec_axis(
        trans = ~., 
        name = "Grupo Social (Interacción)", 
        breaks = breaks_interaccion,
        labels = etiquetas_interaccion_cortas 
      )
    ) +
    
    labs(title = "Trayectoria Diaria: Actividad, Contexto Social y Lugar",
         subtitle = paste("Individuo:", individuo),
         x = "Hora del Día (Formato 24h)") +
    
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line.x = element_line(color = "black", linewidth = 1),
      panel.grid.minor = element_blank(),
      
      axis.line.y.left = element_line(color = "red", linewidth = 1),
      axis.text.y.left = element_text(color = "red", size = 9),
      axis.title.y.left = element_text(color = "red", face = "bold", margin = margin(r = 10)),
      
      axis.line.y.right = element_line(color = "blue", linewidth = 1),
      axis.text.y.right = element_text(color = "blue", size = 9),
      axis.title.y.right = element_text(color = "blue", face = "bold", margin = margin(l = 10)),
      
      # Expandimos el margen para agregar a mano el eje extra
      plot.margin = margin(t = 10, r = 10, b = 10, l = 200)
    )
  
  # 5. Guardamos el gráfico con proporción exacta 16:9
  nombre_archivo <- paste0("Grafico_", individuo, ".png")
  ggsave(filename = nombre_archivo, 
         plot = grafico, 
         width = 16,
         height = 9,
         dpi = 300,
         bg = "white")
  
  cat("Gráfico guardado como:", nombre_archivo, "\n")
}

cat("\nScript terminado.\n")