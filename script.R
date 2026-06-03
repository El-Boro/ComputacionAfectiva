# 1. Instalación de paquetes necesarios
# install.packages("readxl")
# install.packages("dplyr")

# 2. Carga de librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

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
cat("Generando gráficos de 4 Dimensiones...\n")

set.seed(8) 
individuos_azar <- sample(unique(df_completo$identificacion), 3)

horas_completas <- c(sprintf("%02d:00", 1:23), "00:00")
# Vector con el orden de las emociones
emociones_cols <- c("Preocupación", "Prisa", "Irritación", "Depresión", "Tensión", "Calma", "Disfrute")

for (individuo in individuos_azar) {
  
  # 1. Filtramos y preparamos la base base
  df_indiv <- df_completo %>%
    filter(identificacion == individuo) %>%
    arrange(`Hora actividad redondeada`) %>%
    mutate(
      Hora_Limpia = format(as.POSIXct(`Hora actividad redondeada`), "%H:%M"),
      Hora_Num = match(Hora_Limpia, horas_completas) 
    )
  
  # 2. Obtenemos las categorías únicas
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
  n_emo <- length(emociones_cols) # Serán 7 siempre
  
  # Ajuste de longitud de leyenda
  etiquetas_actividad_cortas   <- str_wrap(niveles_actividad, width = 20)
  etiquetas_interaccion_cortas <- str_wrap(niveles_interaccion, width = 20)
  etiquetas_lugar_cortas       <- str_wrap(niveles_lugar, width = 20)
  
  # 3. Algoritmos de distribución equitativa
  escala_interaccion <- function(x) {
    if (n_int > 1 && n_act > 1) { 1 + (x - 1) * (n_act - 1) / (n_int - 1) } else { rep(max(n_act, 1) / 2, length(x)) }
  }
  escala_lugar <- function(x) {
    if (n_lug > 1 && n_act > 1) { 1 + (x - 1) * (n_act - 1) / (n_lug - 1) } else { rep(max(n_act, 1) / 2, length(x)) }
  }
  escala_emocion <- function(x) {
    if (n_emo > 1 && n_act > 1) { 1 + (x - 1) * (n_act - 1) / (n_emo - 1) } else { rep(max(n_act, 1) / 2, length(x)) }
  }
  
  breaks_interaccion <- escala_interaccion(1:n_int)
  breaks_lugar       <- escala_lugar(1:n_lug)
  breaks_emocion     <- escala_emocion(1:n_emo)
  
  # 4. Asignación numérica a la base principal
  df_indiv <- df_indiv %>%
    mutate(
      Act_Num  = as.numeric(factor(`Etiqueta Actividad`, levels = niveles_actividad)),
      Int_Rank = as.numeric(factor(`Etiqueta Interacción`, levels = niveles_interaccion)),
      Int_Num  = escala_interaccion(Int_Rank),
      Lug_Rank = as.numeric(factor(`Lugar de actividad`, levels = niveles_lugar)),
      Lug_Num  = escala_lugar(Lug_Rank)
    )
  
  # Calcula la máxima emoción y preserva los empates generando múltiples filas
  df_emo <- df_indiv %>%
    select(identificacion, Hora_Num, all_of(emociones_cols)) %>%
    pivot_longer(cols = all_of(emociones_cols), names_to = "Emocion", values_to = "Valor") %>%
    filter(!is.na(Valor)) %>%
    group_by(Hora_Num) %>%
    filter(Valor == max(Valor, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(
      Emo_Rank = match(Emocion, emociones_cols),
      Emo_Num  = escala_emocion(Emo_Rank)
    )
  
  # 5. Creamos el gráfico central
  grafico <- ggplot() + 
    
    # Ejes Y
    geom_point(data = df_indiv, aes(x = Hora_Num, y = Act_Num), color = "red", size = 3) +
    geom_line(data = df_indiv, aes(x = Hora_Num, y = Act_Num, group = 1), color = "red", alpha = 1, linetype = "solid") +
    
    geom_point(data = df_indiv, aes(x = Hora_Num, y = Int_Num), color = "blue", size = 3, shape = 17) +
    geom_line(data = df_indiv, aes(x = Hora_Num, y = Int_Num, group = 1), color = "blue", alpha = 1, linetype = "solid") +
    
    geom_point(data = df_indiv, aes(x = Hora_Num, y = Lug_Num), color = "black", size = 3, alpha = 0.7, shape = 15) +
    geom_line(data = df_indiv, aes(x = Hora_Num, y = Lug_Num, group = 1), color = "black", alpha = 1, linetype = "solid") +
    
    geom_point(data = df_emo, aes(x = Hora_Num, y = Emo_Num), color = "green4", size = 3, shape = 18) +
    geom_line(data = df_emo, aes(x = Hora_Num, y = Emo_Num, group = 1), color = "green4", alpha = 1, linetype = "solid") +
    
    # Eje negro manual usando anotaciones
    annotate("segment", x = -2.5, xend = -2.5, y = -Inf, yend = +Inf, color = "black", linewidth = 1) +
    annotate("segment", x = -2.5, xend = 0.5, y = -Inf, yend = -Inf, color = "black", linewidth = 1) +
    annotate("text", x = -2.7, y = breaks_lugar, label = etiquetas_lugar_cortas, hjust = 1, color = "black", size = 3) +
    annotate("text", x = -4.5, y = (1 + max(n_act, 1)) / 2, label = "Lugar de Actividad", angle = 90, fontface = "bold", color = "black", size = 3.5) +
    
    # Eje verde manual usando anotacions
    annotate("segment", x = 27.5, xend = 27.5, y = -Inf, yend = +Inf, color = "green4", linewidth = 1) +
    annotate("segment", x = 24.5, xend = 27.5, y = -Inf, yend = -Inf, color = "black", linewidth = 1) +
    annotate("text", x = 27.7, y = breaks_emocion, label = emociones_cols, hjust = 0, color = "green4", size = 3) +
    annotate("text", x = 29, y = (1 + max(n_act, 1)) / 2, label = "Emoción Predominante", angle = 270, fontface = "bold", color = "green4", size = 3.5) +
    
    # Configuración del lienzo expandido
    coord_cartesian(clip = "off", xlim = c(1, 24)) +
    
    scale_x_continuous(
      breaks = 1:24,
      labels = horas_completas,
      expand = expansion(add = c(0.5, 0.5))
    ) +
    
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
    
    labs(title = "Trayectoria Diaria 4D: Actividad, Grupo Social, Lugar y Emociones",
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
      
      # 200px a la izquierda para el Negro y 250px a la derecha para el texto Verde
      plot.margin = margin(t = 10, r = 250, b = 10, l = 200)
    )
  
  nombre_archivo <- paste0("Grafico_", individuo, ".png")
  ggsave(filename = nombre_archivo, 
         plot = grafico, 
         width = 24,
         height = 7,
         dpi = 300,
         bg = "white")
  
  cat("Gráfico guardado como:", nombre_archivo, "\n")
}
#---------------------------------------------------------------------------------------------------
  
#SEGUNDO ITEM
# =========================================================================
# Filtrado de subconjunto de Actividades Específicas
# =========================================================================

cat("Filtrando dataframe por actividades específicas...\n")

# Definimos las actividades que nos interesan en un vector
actividades_objetivo <- c("Trabajó", "Leyó o estudió", "Leyó", "Trámites o Salud")

# Creamos el nuevo dataframe filtrado
df_actividades_cognitivas <- df_completo %>%
  filter(`Etiqueta Actividad Resumida` %in% actividades_objetivo)

# Verificación de los resultados
cat("Dimensiones del dataframe original: ", dim(df_completo), "\n")
cat("Dimensiones del nuevo dataframe: ", dim(df_actividades_cognitivas), "\n")

# =========================================================================
# Cálculo del tiempo total en actividades de deber por individuo
# =========================================================================

cat("Calculando el tiempo total de deberes por individuo...\n")

df_actividades_cognitivas <- df_actividades_cognitivas %>%
  # 1. Agrupamos los datos por cada individuo
  group_by(identificacion) %>%
  # 2. Creamos la nueva columna sumando los minutos. 
  # na.rm = TRUE ignora los valores vacíos para que no dé error en la suma
  mutate(Total_Minutos_Deber = sum(`Total en minutos de la actividad`, na.rm = TRUE)) %>%
  # 3. Desagrupamos (¡Muy importante como buena práctica para no afectar futuros cálculos!)
  ungroup()


# =========================================================================
# Generación de métricas afectivas ponderadas por individuo (df_salida)
# =========================================================================

cat("Calculando promedios ponderados de emociones por individuo...\n")

df_salida <- df_actividades_cognitivas %>%
  # 1. Agrupamos por individuo (cada persona será una fila al final)
  group_by(identificacion) %>%
  
  # 2. Resumimos la información aplicando tu fórmula matemática
  summarise(
    # Dividimos la suma de los "Minutos * Emoción" sobre el Total de minutos de deber
    # Usamos first() en Total_Minutos_Deber porque ese valor ya es el mismo en todas sus filas
    Preocupacion_Pond = sum(`Minutos*Preoc`, na.rm = TRUE) / first(Total_Minutos_Deber),
    Prisa_Pond        = sum(`Minutos*Prisa`, na.rm = TRUE) / first(Total_Minutos_Deber),
    Irritacion_Pond   = sum(`Minutos*Irritación`, na.rm = TRUE) / first(Total_Minutos_Deber),
    Depresion_Pond    = sum(`Minutos*Depresión`, na.rm = TRUE) / first(Total_Minutos_Deber),
    Tension_Pond      = sum(`Minutos*Tensión`, na.rm = TRUE) / first(Total_Minutos_Deber),
    
    # 3. Almacenamos la columna del tiempo total de deber como solicitaste
    Total_Minutos_Deber = first(Total_Minutos_Deber)
  ) %>%
  # 4. Desagrupamos por seguridad
  ungroup()

# =========================================================================
# Filtrado por tipo de Interacción Social (Entorno público/laboral)
# =========================================================================

cat("Filtrando actividades de deber por interacciones sociales específicas...\n")

# Definimos las interacciones que nos interesan en un vector
interacciones_objetivo <- c(
  "Con compañeros de trabajo / escuela / club", 
  "Destinatario de nuestra actividad laboral (Cliente / paciente / alumno)", 
  "Personal de servicios, responsables de nuestra actividad, extraños"
)

# Creamos el nuevo dataframe aplicando el filtro sobre el que ya teníamos
df_interacciones_cognitivas <- df_completo %>%
  filter(`Etiqueta Interacción` %in% interacciones_objetivo)

# Verificación de los resultados
cat("Dimensiones del dataframe de actividades (previo): ", dim(df_completo), "\n")
cat("Dimensiones del nuevo dataframe (filtrado por interacción): ", dim(df_interacciones_cognitivas), "\n")

# Comprobamos rápidamente que el filtro funcionó mostrando las interacciones únicas
cat("\nInteracciones presentes en el nuevo dataframe:\n")
print(unique(df_interacciones_cognitivas$`Etiqueta Interacción`))


# =========================================================================
# Cálculo de emociones ponderadas en Interacciones y Unión con df_salida
# =========================================================================

cat("Calculando emociones ponderadas para interacciones sociales y uniendo...\n")

# 1. Creamos un resumen temporal a partir del dataframe de interacciones
df_resumen_interacciones <- df_interacciones_cognitivas %>%
  group_by(identificacion) %>%
  summarise(
    # Primero calculamos el tiempo total real dedicado a estas interacciones
    Total_Minutos_Interaccion = sum(`Total en minutos de la actividad`, na.rm = TRUE),
    
    # Calculamos las métricas ponderadas dividiendo por el Total_Minutos_Interaccion
    # Le agregamos el sufijo "_Inter" para distinguirlas de las generales
    Preocupacion_Inter_Pond = sum(`Minutos*Preoc`, na.rm = TRUE) / Total_Minutos_Interaccion,
    Prisa_Inter_Pond        = sum(`Minutos*Prisa`, na.rm = TRUE) / Total_Minutos_Interaccion,
    Irritacion_Inter_Pond   = sum(`Minutos*Irritación`, na.rm = TRUE) / Total_Minutos_Interaccion,
    Depresion_Inter_Pond    = sum(`Minutos*Depresión`, na.rm = TRUE) / Total_Minutos_Interaccion,
    Tension_Inter_Pond      = sum(`Minutos*Tensión`, na.rm = TRUE) / Total_Minutos_Interaccion
  ) %>%
  ungroup()

# 2. Añadimos estas nuevas columnas a nuestro df_salida
df_salida <- df_salida %>%
  left_join(df_resumen_interacciones, by = "identificacion")

#-----------------------------------------------------------------------------------------------------
#Segundo item de la segunda parte

# =========================================================================
# Filtrado por Actividades de Recuperación, Ocio y Cuidado Personal
# =========================================================================

cat("Filtrando dataframe por actividades de dedicación personal y entretenimiento...\n")

# 1. Definimos las actividades de ocio/recuperación en un vector
actividades_ocio <- c(
  "Dedicación personal (comió, bañó, descansó)", 
  "Entretenimiento (radio, televisión, computadora, juego en casa)"
)

# 2. Creamos el nuevo dataframe filtrando desde el original (df_completo)
df_actividades_ocio <- df_completo %>%
  filter(`Etiqueta Actividad Resumida` %in% actividades_ocio)


# =========================================================================
# Cálculo del tiempo total en actividades de ocio por individuo
# =========================================================================

cat("Calculando el tiempo total de ocio por individuo...\n")

df_actividades_ocio <- df_actividades_ocio %>%
  # 1. Agrupamos los datos por cada individuo
  group_by(identificacion) %>%
  
  # 2. Creamos la nueva columna sumando los minutos de estas actividades
  # Usamos na.rm = TRUE para ignorar celdas vacías y evitar errores
  mutate(Total_Minutos_Ocio = sum(`Total en minutos de la actividad`, na.rm = TRUE)) %>%
  ungroup()

# =========================================================================
# Cálculo de emociones positivas ponderadas en Ocio y Unión con df_salida
# =========================================================================

cat("Calculando emociones positivas ponderadas para actividades de ocio y uniendo...\n")

# 1. Creamos un resumen temporal a partir del dataframe de ocio
df_resumen_ocio <- df_actividades_ocio %>%
  group_by(identificacion) %>%
  summarise(
    # Extraemos el tiempo total de ocio que ya habíamos calculado
    Total_Minutos_Ocio = first(Total_Minutos_Ocio),
    
    # Calculamos las métricas ponderadas de bienestar
    # Agregamos el sufijo "_Ocio_Pond" para identificarlas claramente
    Calma_Ocio_Pond    = sum(`Minutos*Calma`, na.rm = TRUE) / first(Total_Minutos_Ocio),
    Disfrute_Ocio_Pond = sum(`Minutos*Disfrute`, na.rm = TRUE) / first(Total_Minutos_Ocio)
  ) %>%
  ungroup()

# 2. Añadimos estas nuevas columnas a nuestro df_salida
df_salida <- df_salida %>%
  left_join(df_resumen_ocio, by = "identificacion")

# =========================================================================
# Filtrado por tipo de Interacción Social (Entorno íntimo/familiar)
# =========================================================================

cat("Filtrando dataframe por interacciones sociales íntimas y familiares...\n")

# 1. Definimos las interacciones del círculo íntimo en un vector
interacciones_intimas <- c(
  "Con sus hijos jóvenes o nietos", 
  "Con amigos", 
  "Con pareja e hijos", 
  "Con su pareja", 
  "Con otros familiares", 
  "Con sus hijos adultos"
)

# 2. Creamos el nuevo dataframe filtrando desde el original (df_completo)
df_interacciones_intimas <- df_completo %>%
  filter(`Etiqueta Interacción` %in% interacciones_intimas)

# =========================================================================
# Cálculo del tiempo total en interacciones íntimas por individuo
# =========================================================================

cat("Calculando el tiempo total de interacciones íntimas por individuo...\n")

df_interacciones_intimas <- df_interacciones_intimas %>%
  # 1. Agrupamos los datos por cada individuo
  group_by(identificacion) %>%
  
  # 2. Creamos la nueva columna sumando los minutos.
  # na.rm = TRUE previene errores si hay celdas vacías
  mutate(Total_Minutos_Intimos = sum(`Total en minutos de la actividad`, na.rm = TRUE)) %>%
  ungroup()

# =========================================================================
# Cálculo de emociones positivas ponderadas en Interacciones Íntimas y Unión con df_salida
# =========================================================================

cat("Calculando emociones positivas ponderadas para interacciones íntimas y uniendo...\n")

# 1. Creamos un resumen temporal a partir del dataframe de interacciones íntimas
df_resumen_intimas <- df_interacciones_intimas %>%
  group_by(identificacion) %>%
  summarise(
    # Extraemos el tiempo total que ya habíamos calculado
    Total_Minutos_Intimos = first(Total_Minutos_Intimos),
    
    # Calculamos las métricas ponderadas de bienestar y satisfacción
    # Agregamos el sufijo "_Intimo_Pond" para identificarlas claramente
    Calma_Intimo_Pond      = sum(`Minutos*Calma`, na.rm = TRUE) / first(Total_Minutos_Intimos),
    Disfrute_Intimo_Pond   = sum(`Minutos*Disfrute`, na.rm = TRUE) / first(Total_Minutos_Intimos),
    A_gusto_Intimo_Pond    = sum(`Minutos*AgustInter`, na.rm = TRUE) / first(Total_Minutos_Intimos)
  ) %>%
  ungroup()

# 2. Añadimos estas nuevas columnas a nuestro df_salida
df_salida <- df_salida %>%
  left_join(df_resumen_intimas, by = "identificacion")

# =========================================================================
# Filtrado por Actividades Físicas (Ejercicio o Paseo)
# =========================================================================

cat("Filtrando dataframe por actividades de ejercicio o paseo...\n")

# Creamos el nuevo dataframe filtrando desde el original (df_completo)
df_actividades_fisicas <- df_completo %>%
  filter(`Etiqueta Actividad Resumida` == "Hizo ejercicio o dio paseo")

# Verificación de los resultados
cat("Dimensiones del dataframe original completo: ", dim(df_completo), "\n")
cat("Dimensiones del nuevo dataframe de actividades físicas: ", dim(df_actividades_fisicas), "\n")

# Comprobamos rápidamente que el filtro funcionó mostrando las categorías únicas
cat("\nActividades presentes en el nuevo dataframe de actividades físicas:\n")
print(unique(df_actividades_fisicas$`Etiqueta Actividad Resumida`))

# =========================================================================
# Cálculo del tiempo total en actividades físicas por individuo
# =========================================================================

cat("Calculando el tiempo total de actividades físicas por individuo...\n")

df_actividades_fisicas <- df_actividades_fisicas %>%
  # 1. Agrupamos los datos por cada individuo
  group_by(identificacion) %>%
  
  # 2. Creamos la nueva columna sumando los minutos de estas actividades
  # Usamos na.rm = TRUE para ignorar celdas vacías y evitar errores en la suma
  mutate(Total_Minutos_Fisico = sum(`Total en minutos de la actividad`, na.rm = TRUE)) %>%
  ungroup()

# =========================================================================
# Cálculo de emociones positivas ponderadas en Actividad Física y Unión
# =========================================================================

cat("Calculando emociones positivas ponderadas para actividades físicas y uniendo...\n")

# 1. Creamos un resumen temporal a partir del dataframe de actividades físicas
df_resumen_fisicas <- df_actividades_fisicas %>%
  group_by(identificacion) %>%
  summarise(
    # Calculamos las métricas ponderadas de bienestar
    # Agregamos el sufijo "_Fisico_Pond" para identificarlas claramente
    Calma_Fisico_Pond    = sum(`Minutos*Calma`, na.rm = TRUE) / first(Total_Minutos_Fisico),
    Disfrute_Fisico_Pond = sum(`Minutos*Disfrute`, na.rm = TRUE) / first(Total_Minutos_Fisico),
    # Extraemos el tiempo total de actividad física que ya habíamos calculado
    Total_Minutos_Fisico = first(Total_Minutos_Fisico)
  ) %>%
  ungroup()

# 2. Añadimos estas nuevas columnas a nuestro df_salida original
df_salida <- df_salida %>%
  left_join(df_resumen_fisicas, by = "identificacion")

#----------------------------------------------------------------------------------------------------
# =========================================================================
# Cruce de datos afectivos con variables Sociodemográficas
# =========================================================================

cat("Extrayendo perfiles sociodemográficos y cruzando con df_salida...\n")

# 1. Extraemos 1 sola fila por individuo con sus datos demográficos desde df_completo
df_sociodemo <- df_completo %>%
  # Seleccionamos el ID y las columnas demográficas que nos interesan
  select(identificacion, `Ingreso Familiar Mensual del Hogar`, `Máximo nivel de estudios alcanzado`, `Generación`) %>%
  distinct(identificacion, .keep_all = TRUE)

# 2. Unimos estos datos sociodemográficos a nuestro df_salida
df_cruce <- df_salida %>%
  left_join(df_sociodemo, by = "identificacion")

# =========================================================================
# Análisis Específico: Preocupación en el trabajo
# =========================================================================

cat("Calculando el promedio de preocupación según ingresos y educación...\n")

resumen_preocupacion <- df_cruce %>%
  
  # 1. Agrupamos por los dos factores que quieres cruzar
  group_by(`Ingreso Familiar Mensual del Hogar`, `Máximo nivel de estudios alcanzado`) %>%
  
  # 2. Calculamos el promedio de la preocupación ponderada y contamos cuántos son
  summarise(
    # mean() calcula el promedio. na.rm = TRUE ignora los casos vacíos
    Promedio_Preocupacion = mean(Preocupacion_Pond, na.rm = TRUE),
    
    # n() nos dice cuántas personas cayeron en esta categoría exacta
    Cantidad = n(),
    
    .groups = "drop" # Desagrupamos al terminar
  ) %>%
  
  # 4. Ordenamos de mayor a menor preocupación para ver rápido quiénes están peor
  arrange(desc(Promedio_Preocupacion))

# =========================================================================
# Gráfico: Preocupación en el Trabajo en Personas (por Ingresos y Educación)
# =========================================================================

# 1. Orden lógico creciente de Educación
orden_educacion <- c(
  "Primario Incompleto",
  "Primario Completo", 
  "Secundario Incompleto", 
  "Secundario Completo", 
  "Terciario Incompleto", 
  "Terciarios Completo",
  "Universitario Incompleto",
  "Universitario Completo y más."
)

# 2. Orden lógico creciente de Ingresos
orden_ingresos <- c(
  "Ns-Ns",
  "Hasta $4185",
  "De $4186 a $8800",
  "De $8801 a $15600",
  "De $15601 a $42500",
  "Más de $42500"
)


# =========================================================================
# ANÁLISIS 1: Preocupación según NIVEL DE INGRESOS
# =========================================================================

cat("Calculando el promedio de preocupación según nivel de ingresos...\n")

resumen_ingresos <- df_cruce %>%
  filter(!is.na(`Ingreso Familiar Mensual del Hogar`)) %>%
  mutate(`Ingreso Familiar Mensual del Hogar` = factor(`Ingreso Familiar Mensual del Hogar`, levels = orden_ingresos)) %>%
  group_by(`Ingreso Familiar Mensual del Hogar`) %>%
  summarise(
    Promedio_Preocupacion = mean(Preocupacion_Pond, na.rm = TRUE),
    Cantidad = n(),
    .groups = "drop"
  )

cat("Generando gráfico de Ingresos...\n")

grafico_ingresos <- ggplot(resumen_ingresos, aes(x = `Ingreso Familiar Mensual del Hogar`, y = Promedio_Preocupacion)) +
  geom_col(fill = "darkorange", color = "black", width = 0.6) +
  geom_text(aes(label = round(Promedio_Preocupacion, 2)), vjust = -0.5, fontface = "bold", size = 4) +
  labs(
    title = "Nivel de Preocupación en el Trabajo",
    subtitle = "Promedio ponderado según Nivel de Ingresos",
    x = "Nivel de Ingresos Familiares",
    y = "Preocupación Promedio Ponderada"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank()
  )

ggsave("Grafico_Preoc_Ingresos.png", plot = grafico_ingresos, width = 10, height = 6, dpi = 300, bg="white")
cat("Gráfico guardado: 'Grafico_Preoc_Ingresos.png'.\n")

# =========================================================================
# ANÁLISIS 2: Depresión en el Trabajo según GENERACIÓN
# =========================================================================

cat("Calculando el promedio de depresión según generación...\n")

# 1. Definir el orden lógico cronológico de las generaciones
orden_generacion <- c(
  "Tradicionalistas", 
  "Baby Boomers", 
  "Generación X", 
  "Generación Y", 
  "Generación Z"
)

# 2. Resumen de datos filtrando los valores nulos en Generación
resumen_generacion <- df_cruce %>%
  filter(!is.na(Generación)) %>%
  mutate(Generación = factor(Generación, levels = orden_generacion)) %>%
  group_by(Generación) %>%
  summarise(
    Promedio_Depresion = mean(Depresion_Pond, na.rm = TRUE),
    Cantidad_Individuos = n(),
    .groups = "drop"
  )

cat("Generando gráfico de Generación...\n")

# 3. Creación del gráfico
grafico_generacion <- ggplot(resumen_generacion, aes(x = Generación, y = Promedio_Depresion)) +
  geom_col(fill = "purple4", color = "black", width = 0.6) +
  geom_text(aes(label = round(Promedio_Depresion, 2)), vjust = -0.5, fontface = "bold", size = 4) +
  labs(
    title = "Nivel de Depresión en el Trabajo por Generación",
    subtitle = "Promedio ponderado cruzado por grupo generacional",
    x = "Generación",
    y = "Depresión Promedio Ponderada"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# 4. Guardado del gráfico
ggsave("Grafico_Depresion_Generacion.png", plot = grafico_generacion, width = 10, height = 6, dpi = 300, bg="white")
cat("Gráfico guardado exitosamente como 'Grafico_Depresion_Generacion.png'.\n")

# =========================================================================
# ANÁLISIS 3: Bienestar en interacciones íntimas según INGRESOS
# =========================================================================

cat("Calculando el promedio de 'A gusto con la interacción' (íntima) según ingresos...\n")

# 1. Resumen de datos filtrando los valores nulos en Ingresos y en la métrica
resumen_agusto_ingresos <- df_cruce %>%
  filter(!is.na(`Ingreso Familiar Mensual del Hogar`), !is.na(A_gusto_Intimo_Pond)) %>%
  mutate(`Ingreso Familiar Mensual del Hogar` = factor(`Ingreso Familiar Mensual del Hogar`, levels = orden_ingresos)) %>%
  group_by(`Ingreso Familiar Mensual del Hogar`) %>%
  summarise(
    Promedio_Agusto = mean(A_gusto_Intimo_Pond, na.rm = TRUE),
    Cantidad_Individuos = n(),
    .groups = "drop"
  )

cat("Generando gráfico de Bienestar Íntimo por Ingresos...\n")

# 2. Creación del gráfico
grafico_agusto_ingresos <- ggplot(resumen_agusto_ingresos, aes(x = `Ingreso Familiar Mensual del Hogar`, y = Promedio_Agusto)) +
  geom_col(fill = "forestgreen", color = "black", width = 0.6) +
  geom_text(aes(label = round(Promedio_Agusto, 2)), vjust = -0.5, fontface = "bold", size = 4) +
  labs(
    title = "Bienestar en Interacciones Íntimas por Nivel de Ingresos",
    subtitle = "Promedio ponderado de 'A gusto con la interacción' (familia/amigos)",
    x = "Nivel de Ingresos Familiares",
    y = "Nivel Promedio 'A gusto' (Ponderado)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# 3. Guardado del gráfico
ggsave("Grafico_Agusto_Intimo_Ingresos.png", plot = grafico_agusto_ingresos, width = 10, height = 6, dpi = 300, bg="white")
cat("Gráfico guardado exitosamente como 'Grafico_Agusto_Intimo_Ingresos.png'.\n")

# =========================================================================
# ANÁLISIS 4: Bienestar en interacciones íntimas según EDUCACIÓN
# =========================================================================

cat("Calculando el promedio de 'A gusto con la interacción' (íntima) según nivel educativo...\n")

# 1. Resumen de datos filtrando los valores nulos en Educación y en la métrica
resumen_agusto_educacion <- df_cruce %>%
  filter(!is.na(`Máximo nivel de estudios alcanzado`), !is.na(A_gusto_Intimo_Pond)) %>%
  mutate(`Máximo nivel de estudios alcanzado` = factor(`Máximo nivel de estudios alcanzado`, levels = orden_educacion)) %>%
  group_by(`Máximo nivel de estudios alcanzado`) %>%
  summarise(
    Promedio_Agusto = mean(A_gusto_Intimo_Pond, na.rm = TRUE),
    Cantidad_Individuos = n(),
    .groups = "drop"
  )

cat("Generando gráfico de Bienestar Íntimo por Educación...\n")

# 2. Creación del gráfico
grafico_agusto_educacion <- ggplot(resumen_agusto_educacion, aes(x = `Máximo nivel de estudios alcanzado`, y = Promedio_Agusto)) +
  # Usamos un tono verde distinto para diferenciarlo del gráfico de ingresos
  geom_col(fill = "mediumseagreen", color = "black", width = 0.6) +
  geom_text(aes(label = round(Promedio_Agusto, 2)), vjust = -0.5, fontface = "bold", size = 4) +
  labs(
    title = "Bienestar en Interacciones Íntimas por Nivel Educativo",
    subtitle = "Promedio ponderado de 'A gusto con la interacción' (familia/amigos)",
    x = "Nivel Educativo",
    y = "Nivel Promedio 'A gusto' (Ponderado)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 20, hjust = 1), # Un ángulo un poco mayor por si los textos son largos
    panel.grid.major.x = element_blank()
  )

# 3. Guardado del gráfico
ggsave("Grafico_Agusto_Intimo_Educacion.png", plot = grafico_agusto_educacion, width = 10, height = 6, dpi = 300, bg="white")
cat("Gráfico guardado exitosamente como 'Grafico_Agusto_Intimo_Educacion.png'.\n")

# =========================================================================
# ANÁLISIS 5: Costo Emocional del Trabajo vs. Recuperación
# =========================================================================

cat("Calculando relación entre Tensión en Deberes vs Calma en Ocio...\n")

df_sociodemo <- df_completo %>%
  select(identificacion, `¿Cuál es su género?`, `Ingreso Familiar Mensual del Hogar`, `Máximo nivel de estudios alcanzado`, Generación) %>%
  distinct(identificacion, .keep_all = TRUE)

df_cruce <- df_salida %>%
  left_join(df_sociodemo, by = "identificacion")

# 1. Filtramos para quedarnos con los individuos que tengan datos en AMBAS variables
# (Es decir, que hayan registrado horas de deberes Y horas de ocio)
# También eliminamos NAs en género para tener grupos limpios
df_costo_emocional <- df_cruce %>%
  filter(!is.na(Tension_Pond), !is.na(Calma_Ocio_Pond), !is.na(`¿Cuál es su género?`))

cat("Generando gráfico de dispersión (Scatter Plot) con líneas de tendencia...\n")

# 2. Creación del Gráfico de Dispersión
grafico_costo_emocional <- ggplot(df_costo_emocional, aes(x = Tension_Pond, y = Calma_Ocio_Pond, color = `¿Cuál es su género?`)) +
  
  # Añadimos los puntos (cada punto es un individuo)
  # alpha = 0.7 hace los puntos un poco transparentes para ver si se superponen
  geom_point(size = 3, alpha = 0.7) +
  
  # Añadimos la línea de tendencia matemática (regresión lineal: method = "lm")
  # se = FALSE quita la sombra del intervalo de confianza para que el gráfico quede más limpio
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  
  # Etiquetas y títulos
  labs(
    title = "Costo Emocional: Tensión Laboral vs. Calma en el Ocio",
    subtitle = "Correlación entre el estrés en actividades de deber y la recuperación en tiempo libre",
    x = "Nivel de Tensión Ponderada (Deberes)",
    y = "Nivel de Calma Ponderada (Ocio)",
    color = "Género"
  ) +
  
  # Escala de colores personalizada para distinguir claramente los géneros
  # Asumiendo los clásicos "Femenino" y "Masculino" (puedes ajustar si hay más categorías)
  scale_color_manual(values = c("Masculino" = "steelblue", "Femenino" = "darkred", "Otro" = "goldenrod")) +
  
  # Tema visual
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5) # Un borde sutil alrededor
  )

# 3. Guardado del gráfico
ggsave("Grafico_Costo_Emocional.png", plot = grafico_costo_emocional, width = 10, height = 7, dpi = 300, bg="white")
cat("Gráfico guardado exitosamente como 'Grafico_Costo_Emocional.png'.\n")

# =========================================================================
# Cálculo de la correlación de Pearson exacta
# =========================================================================
correlacion_general <- cor(df_costo_emocional$Tension_Pond, df_costo_emocional$Calma_Ocio_Pond)
cat("\nLa correlación general (Pearson) entre Tensión laboral y Calma en el Ocio es:", round(correlacion_general, 3), "\n")

# =========================================================================
# ANÁLISIS 6: Análisis de "Déficit de Ocio" por Generación
# =========================================================================

cat("Calculando el Déficit de Ocio (Ratio Deberes/Ocio) por generación...\n")

# 1. Definir el orden lógico cronológico de las generaciones (si no lo hiciste en el paso anterior)
orden_generacion <- c(
  "Tradicionalistas", 
  "Baby Boomers", 
  "Generación X", 
  "Generación Y", 
  "Generación Z"
)

# 2. Resumen de datos: Calculamos el ratio y luego el promedio por generación
resumen_deficit_ocio <- df_cruce %>%
  # Filtramos NAs en las variables clave y en la Generación
  filter(!is.na(Total_Minutos_Deber), !is.na(Total_Minutos_Ocio), !is.na(Generación)) %>%
  
  # Evitamos dividir por cero. Si alguien tiene 0 minutos de ocio, el ratio tendería a infinito.
  # Le asignamos 1 minuto simbólico para que R pueda calcular algo, o los filtramos.
  # Lo más seguro metodológicamente es filtrar a los que tienen Ocio > 0.
  filter(Total_Minutos_Ocio > 0) %>%
  
  # Calculamos el ratio individual: Cuántos minutos de deber por cada minuto de ocio
  mutate(Ratio_Ocio = Total_Minutos_Deber / Total_Minutos_Ocio) %>%
  
  # Ordenamos la Generación
  mutate(Generación = factor(Generación, levels = orden_generacion)) %>%
  
  # Agrupamos y calculamos promedios
  group_by(Generación) %>%
  summarise(
    Promedio_Ratio_Ocio = mean(Ratio_Ocio, na.rm = TRUE),
    Cantidad_Individuos = n(),
    .groups = "drop"
  )

cat("Generando gráfico de Déficit de Ocio...\n")

# 3. Creación del gráfico
grafico_deficit_ocio <- ggplot(resumen_deficit_ocio, aes(x = Generación, y = Promedio_Ratio_Ocio)) +
  geom_col(fill = "firebrick", color = "black", width = 0.6) +
  geom_text(aes(label = round(Promedio_Ratio_Ocio, 2)), vjust = -0.5, fontface = "bold", size = 4) +
  
  # Añadimos una línea de referencia en el Y = 1 (donde Deberes = Ocio)
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 1) +
  annotate("text", x = 0.5, y = 1.1, label = "Equilibrio 1:1", color = "gray30", hjust = 0, size = 3.5, fontface = "italic") +
  
  labs(
    title = "Déficit de Ocio por Generación",
    subtitle = "Proporción promedio de minutos de deber por cada minuto de ocio",
    x = "Generación",
    y = "Ratio (Minutos Deber / Minutos Ocio)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# 4. Guardado del gráfico
ggsave("Grafico_Deficit_Ocio_Generacion.png", plot = grafico_deficit_ocio, width = 10, height = 6, dpi = 300, bg="white")
cat("Gráfico guardado exitosamente como 'Grafico_Deficit_Ocio_Generacion.png'.\n")

# =========================================================================
# ANÁLISIS 7: Tiempo de Ocio vs. Nivel Socioeconómico (Ingresos)
# =========================================================================

cat("Calculando relación entre Tiempo de Ocio e Ingresos Familiares...\n")

# 1. Preparación de los datos
# Asegurándonos de que el vector de orden de ingresos exista (por si acaso)
orden_ingresos <- c(
  "Hasta $4185",
  "De $4186 a $8800",
  "De $8801 a $15600",
  "De $15601 a $42500",
  "Más de $42500"
)

# Filtramos NA en las variables clave y aplicamos el factor ordenado a los ingresos
df_ocio_ingresos <- df_cruce %>%
  filter(!is.na(Total_Minutos_Ocio), !is.na(`Ingreso Familiar Mensual del Hogar`)) %>%
  mutate(`Ingreso Familiar Mensual del Hogar` = factor(`Ingreso Familiar Mensual del Hogar`, levels = orden_ingresos))

cat("Generando gráfico de cajas (Boxplot)...\n")

# 2. Creación del Gráfico de Cajas
grafico_ocio_ingresos <- ggplot(df_ocio_ingresos, aes(x = `Ingreso Familiar Mensual del Hogar`, y = Total_Minutos_Ocio, fill = `Ingreso Familiar Mensual del Hogar`)) +
  
  # geom_boxplot crea el gráfico de cajas. alpha le da un poco de transparencia.
  geom_boxplot(alpha = 0.8, color = "black", outlier.color = "red", outlier.size = 2) +
  
  # Añadimos un punto extra para marcar explícitamente la MEDIA (promedio)
  # ya que el boxplot por defecto marca la MEDIANA con una línea fuerte
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  
  # Etiquetas y títulos
  labs(
    title = "Distribución del Tiempo de Ocio según Nivel Socioeconómico",
    subtitle = "Minutos de ocio diarios vs. Ingreso Familiar (Los rombos blancos indican el promedio)",
    x = "Nivel de Ingreso Familiar Mensual",
    y = "Total de Minutos de Ocio",
    fill = "Nivel de Ingresos"
  ) +
  
  # Escala de colores (puedes elegir otra paleta si prefieres)
  scale_fill_brewer(palette = "YlGnBu") +
  
  # Tema visual
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "none", # Quitamos la leyenda lateral porque el eje X ya lo explica
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank() # Quitamos líneas secundarias para mayor limpieza
  )

# 3. Guardado del gráfico
ggsave("Grafico_Ocio_Socioeconomico.png", plot = grafico_ocio_ingresos, width = 10, height = 7, dpi = 300, bg="white")
cat("Gráfico guardado exitosamente como 'Grafico_Ocio_Socioeconomico.png'.\n")

cat("\nScript terminado.\n")
