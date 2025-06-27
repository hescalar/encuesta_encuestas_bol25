################################################################################
#Encuesta de encuestas:
#Este proyecto trabaja con datos de las principales encuestas oficiales de intención de voto
#para las elecciones generales de Bolivia 2025
# Autor: Esteban Calisaya
# Fecha: 2025-06-26
# Modificado: 2025-06-26
#GeoData-Huasi 
################################################################################


if (!require("pacman")) install.packages("pacman") ## instala pacman si se requiere

pacman::p_load(tidyverse,
               writexl, 
               haven,
               sjlabelled, 
               janitor,
               magrittr,
               viridis,
               ggthemes,
               ggpubr,
               GGally,
               DescTools, ## Paquete con pruebas estadísticas
               infer, ## inferencia compatible con formato tidy
               broom, ## paquete para volver _tidy_ resultados
               srvyr,
               readr,
               skimr,
               DataExplorer,
               gtsummary,
               flextable,
               huxtable,
               googlesheets4,
               stringr,
               scales,
               lubridate,
               forcats,
               plotly
)


####Datos----

gs4_deauth()

df<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Nr5mlVI1PAIXpaOtmRugJbwyriQYlAUt5Ldehq8VZ6E/edit?gid=0#gid=0")
dictionary <-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VJZkKd5dDWnHzbOO3G4o7yeW8jh1GZJIhykaApI05MQ/edit?gid=1761737635#gid=1761737635")

head(df)
head(dictionary)


etiquetar_df_base <- function(df, dictionary) {
  
  # Función para parsear etiquetas de valores tipo: 1 = "Texto"
  parse_labels <- function(value_string) {
    matches <- str_match_all(value_string, "(\\d+)\\s*=\\s*\\\"(.*?)\\\"")[[1]]
    if (nrow(matches) == 0) return(NULL)
    labels <- as.numeric(matches[, 2])
    names(labels) <- matches[, 3]
    return(labels)
  }
  
  for (var in names(df)) {
    if (var %in% dictionary$Variable) {
      row <- dictionary %>% filter(Variable == var)
      
      # Asigna la descripción como etiqueta de variable
      attr(df[[var]], "label") <- row$Descripción
      
      # Asigna las etiquetas de valores, si existen
      value_labels_str <- row$values
      if (!is.na(value_labels_str) && str_detect(value_labels_str, "=")) {
        value_labels <- parse_labels(value_labels_str)
        if (!is.null(value_labels)) {
          attr(df[[var]], "labels") <- value_labels
        }
      }
    }
  }
  
  return(df)
}

df<- etiquetar_df_base(df, dictionary)

sjlabelled::get_label(df_etiquetado$cons_emp)

sjlabelled::get_labels(df_etiquetado$cons_emp)  

str(df$fecha_pub)

df <- df |>
  mutate(across(matches("^fecha"), ~ as.Date(as.character(.), format = "%d/%m/%Y")))

str(df$fecha_pub)

head(df)

str(df)


candidat <- dictionary %>%
  filter(Variable %in% names(df)) %>%
  filter(Variable %in% c("ap", "ngp", "fp", "lyp_adn", "apb_sumate", "libre", "morena", "pdc", "mas_ipsp", "unidad", "blancos", "nulos", "indec")) %>%
  select(partido = Variable, candidat = descr_etiquetas)

# 2. Pivotar df e incorporar nombres de candidata/o
df_long <- df %>%
  pivot_longer(
    cols = ap:indec,
    names_to = "partido",
    values_to = "intencion"
  ) %>%
  left_join(candidat, by = "partido") %>%
  mutate(
    mes_ano = lubridate::floor_date(fecha_pub, unit = "month")
  )

#Crear nivel y variable de promedio
#El diccionario me dice que son 20 niveles (y 20 valores)


etiquetas_actuales <- attr(df_long$medio, "labels")

# Asegurarse de que los valores son numéricos
etiquetas_actuales <- setNames(as.numeric(etiquetas_actuales), names(etiquetas_actuales))

etiquetas_actuales
# Agregar el nuevo nivel "Promedio" con valor 21
nuevas_etiquetas <- c(etiquetas_actuales, "Promedio" = 21)

# Reasignar etiquetas a la variable 'medio'
df_long$medio <- set_labels(df_long$medio, labels = nuevas_etiquetas)

get_labels(df_long$medio)

#Calcular promedios mensuales
promedios <- df_long %>%
  group_by(mes_ano, candidat) %>%
  summarise(
    intencion = mean(intencion, na.rm = TRUE),
    margen_error = mean(margen_error, na.rm = TRUE),
    .groups = "drop"
  )|>
  mutate(medio = 21)# El valor 21 tiene la etiqueta "Promedio"
promedios

# Unir los promedios al df_long original
df_long <- bind_rows(df_long, promedios)
df_long

table(df_long$medio)
df_long$medio
#Ya aparece el promedio

df_long$medio <- set_labels(
  as.numeric(df_long$medio),  # Asegura que sea numérico
  labels = nuevas_etiquetas
)


#Roerdenar blancos y nulos y calcular ic
df_long <- df_long %>%
  mutate(
    candidat = fct_relevel(
      candidat,
      c(
        setdiff(unique(candidat), c("Blancos", "Nulos", "Indecisos")),
        "Blancos", "Nulos", "Indecisos"
      )
    )
  )|>
  mutate(
    lower_ci = intencion - margen_error,
    upper_ci = intencion + margen_error,
    lower_ci = pmax(lower_ci, 0),
    upper_ci = pmin(upper_ci, 1)
  )

colores_13 <- c(
  "#3dac34",  # ADR
  "#fbc51a",  # JD
  "#01a8ec",  # JF
  "#aa1622",  # AS
  "#541a67",  # MRV
  "#089836",  # TUTO
  "#a6761d",  # EVA
  "#ec452e",  # RPAZ
  "#002696",  # EDU
  "#ffc603",  # KENCHA
  "gray",  # BLANCO
  "black",  # NULO
  "red"   # INDE
)


##La mejor
g_2<-df_long %>%
  filter(lubridate::month(mes_ano) == 6) %>%
  ggplot(aes(
    x = candidat,
    y = intencion,
    color = as_label(medio),
    group = as_label(medio)
  )) +
  geom_point(position = position_dodge(width = 0.7), size = 4, alpha = 0.8) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 0.7),
                width = 0.2, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # si tenés paleta definida
  labs(
    title = "Junio 2025: intención de voto total por candidato y medio de difusión",
    x = "Candidatura",
    y = "Intención de voto (%)",
    color = "Medio de difusión",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE, intervalos de confianza calculados con el margen de error que publica la encuestadora"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


g_jun<-df_long %>%
  filter(lubridate::month(mes_ano) == 6) %>%
  ggplot(aes(
    x = candidat,
    y = intencion,
    fill = as_label(medio),
    group = as_label(medio)
  )) +
  geom_col(position = position_dodge(width = 1), alpha = 1, width = .8) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 1),
                width = .8, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # si tenés paleta definida
  labs(
    title = "Junio 2025: intención de voto total por candidato y medio de difusión",
    x = "Candidatura",
    y = "Intención de voto (%)",
    fill = "Medio de difusión",
    
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_jun
   #La segunda mejor
ggplot(df_long, aes(
  x = mes_ano,
  y = intencion,
  color = candidat,
  shape = as_label(medio),
  group = interaction(candidat, medio)
)) +
  geom_jitter(width = 7, size = 5, alpha = 0.8) +
  scale_x_date(
    date_labels = "%B %Y",
    date_breaks = "1 month"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_color_manual(values = colores_13) +
  labs(
    title = "Evolución de intención de voto agrupada por mes",
    subtitle = "Nulos, indecisos y blancos suman al 100%",
    x = "Mes de publicación",
    y = "Intención de voto (%)",
    color = "Candidatura",
    shape = "Medio de difusión"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#La mejorsísima hasta el momento
g1<-ggplot(df_long, aes(
  x = mes_ano,
  y = intencion,
  color = candidat,
  shape = as_label(medio)
)) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 7),
    width = 0.25,
    alpha = 0.5
  ) +
  geom_point(
    position = position_dodge(width = 7),
    size = 5,
    alpha = 0.9
  ) +
  scale_x_date(
    date_labels = "%B %Y",
    date_breaks = "1 month"
  )  +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.05),  # Marcas cada 5%
    limits = c(0, NA)
  ) +
  scale_color_manual(values = colores_13) +
  labs(
    title = "Junio 2025: intención de voto total agrupada por mes",
    subtitle = "Se toma en cuenta Blancos, Nulos e Indecisos para el total",
    x = "Mes de publicación",
    y = "Intención de voto (%)",
    color = "Candidatura",
    shape = "Medio de difusión",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE, intervalos de confianza calculados con el margen de error que publica la encuestadora"
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0,      # Alineación izquierda
      size = 9,       # Tamaño de fuente
      face = "italic", # Estilo de letra
      color = "gray40", # Color del texto
      margin = margin(t = 10)  # Margen superior
    )
  )+
  guides(
    color = guide_legend(override.aes = list(shape = 15, size = 5)),  # cuadrados para color
    shape = guide_legend(override.aes = list(size = 4))               # medios normales
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(hjust = .5)
  )
g1

##############Votos válidos#####################

votos_validos <- c("ap", "ngp", "fp", "lyp_adn", "apb_sumate", 
                   "libre", "morena", "pdc", "mas_ipsp", "unidad")

# 2. Calcular votos efectivos
df_efectivo <- df %>%
  # Seleccionar solo las columnas de partidos políticos (excluyendo blancos/nulos/indecisos)
  select(id:nro_casos, all_of(votos_validos), margen_error) %>%
  # Pivotear igual que antes
  pivot_longer(
    cols = all_of(votos_validos),
    names_to = "partido",
    values_to = "intencion_cruda"
  ) %>%
  # Calcular el total válido por encuesta
  group_by(id) %>%
  mutate(
    total_valido = sum(intencion_cruda),
    intencion = intencion_cruda / total_valido,  # Reescalar a 100% válido
    margen_error_efectivo = margen_error / total_valido  # Ajustar margen de error
  ) %>%
  ungroup() %>%
  # Mantener el join con los nombres de candidatos y fechas como en tu versión
  left_join(candidat, by = "partido") %>%
  mutate(
    mes_ano = floor_date(fecha_pub, unit = "month")
  )

# 3. Verificación (debería sumar 1 en cada encuesta)
df_efectivo %>%
  group_by(id) %>%
  summarise(total = sum(intencion), .groups = "drop") %>%
  print(n = Inf)

#4. Cambiar orden candidatos para que coincidan colores
orden_original <- levels(df_long$candidat) %>% 
  setdiff(c("Blancos", "Nulos", "Indecisos"))
orden_original

# 2. Aplicar este orden al dataframe de votos efectivos
df_efectivo <- df_efectivo %>%
  mutate(
    candidat = factor(candidat, levels = orden_original)
  )

levels(df_efectivo$candidat)

#Promedio efectivos

# 4. Mantener EXACTAMENTE tu código de promedios (solo cambiando el input)
etiquetas_actuales <- attr(df_efectivo$medio, "labels")
etiquetas_actuales <- setNames(as.numeric(etiquetas_actuales), names(etiquetas_actuales))
nuevas_etiquetas <- c(etiquetas_actuales, "Promedio" = 21)
df_efectivo$medio <- set_labels(df_efectivo$medio, labels = nuevas_etiquetas)

# 5. Calcular promedios (mismo código, ahora con intención reescalada)
promedios_efectivo <- df_efectivo %>%
  group_by(mes_ano, candidat) %>%
  summarise(
    intencion = mean(intencion, na.rm = TRUE),
    margen_error_efectivo = mean(margen_error_efectivo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(medio = 21)  # Valor etiquetado como "Promedio"

# 6. Combinar conservando etiquetas (igual que tu versión)
df_efectivo<- bind_rows(df_efectivo, promedios_efectivo) %>%
  mutate(medio = set_labels(as.numeric(medio), labels = nuevas_etiquetas))


# Verificación
table(df_efectivo$medio)  # Debe incluir 21="Promedio"
get_labels(df_efectivo$medio)


df_efectivo<- df_efectivo|>
  mutate(
    lower_ci = intencion - margen_error_efectivo,
    upper_ci = intencion + margen_error_efectivo,
    lower_ci = pmax(lower_ci, 0),
    upper_ci = pmin(upper_ci, 1)
  )


colores_10 <- c(
  "#3dac34",  # ADR
  "#fbc51a",  # JD
  "#01a8ec",  # JF
  "#aa1622",  # AS
  "#541a67",  # MRV
  "#089836",  # TUTO
  "#a6761d",  # EVA
  "#ec452e",  # RPAZ
  "#002696",  # EDU
  "#ffc603"
)
#Gráfico junio efectivos
g_jun_efc<-df_efectivo %>%
  filter(lubridate::month(mes_ano) == 6) %>%
  ggplot(aes(
    x = candidat,
    y = intencion,
    fill = as_label(medio),
    group = as_label(medio)
  )) +
  geom_col(position = position_dodge(width = 1), alpha = 1, width = .8) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 1),
                width = .8, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # si tenés paleta definida
  labs(
    title = "Junio 2025: intención de voto válido por candidato y medio de difusión",
    x = "Candidatura",
    y = "Intención de voto (%)",
    fill = "Medio de difusión",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE, intervalos de confianza escalados con base en el margen de error que publica la encuestadora"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_jun_efc


#Gráfico votos efectivos
g_efectivo <- ggplot(df_efectivo, aes(
  x = mes_ano,
  y = intencion,
  color = candidat,
  shape = as_label(medio)
)) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 7),
    width = 0.25,
    alpha = 0.5
  ) +
  geom_point(
    position = position_dodge(width = 7),
    size = 5,
    alpha = 0.9
  ) +
  scale_x_date(
    date_labels = "%B %Y",
    date_breaks = "1 month"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.05),  # Marcas cada 5%
    limits = c(0, NA)
  ) +
  scale_color_manual(values = colores_10) +
  labs(
    title = "Junio 2025: Intención de voto efectivo (100% = votos válidos)",
    subtitle = "Excluyendo Blancos, Nulos e Indecisos",
    x = "Mes de publicación",
    y = "Porcentaje de votos válidos (%)",
    color = "Candidatura",
    shape = "Medio de difusión",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE, intervalos de confianza escalados con base en el margen de error que publica la encuestadora"
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0,      # Alineación izquierda
      size = 9,       # Tamaño de fuente
      face = "italic", # Estilo de letra
      color = "gray40", # Color del texto
      margin = margin(t = 10)  # Margen superior
    )
  )+
  guides(
    color = guide_legend(override.aes = list(shape = 15, size = 5)),
    shape = guide_legend(override.aes = list(size = 4))
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
g_efectivo

table(df_efectivo$margen_error_efectivo)

table(as_label(df_long$cons_emp),df_long$candidat)
