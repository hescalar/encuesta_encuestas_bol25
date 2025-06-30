################################################################################
#Creación gráficos: Encuesta de encuestas
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

####Votos total----

g_jun<-df_long %>%
  filter(lubridate::month(mes_ano) == 6) %>%
  ggplot(aes(
    x = candidat,
    y = intencion,
    fill = as_label(cons_emp),
    group = as_label(cons_emp)
  )) +
  geom_col(position = position_dodge(width = 1), alpha = 1, width = .8) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 1),
                width = .8, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # si tenés paleta definida
  labs(
    title = "Junio 2025: intención de voto total por candidato y consultora o empresa encuestadora",
    x = "Candidatura",
    y = "Intención de voto (%)",
    fill = "Consultora o empresa",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE,
    intervalos de confianza calculados con el margen de error que publica la encuestadora"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_jun


#La mejorsísima hasta el momento
g1<-ggplot(df_long, aes(
  x = mes_ano,
  y = intencion,
  color = candidat,
  shape = as_label(cons_emp)
))+
  # Primero: Líneas SOLO para los promedios (conexión entre meses)
  geom_line(
    data = df_long %>% filter(cons_emp == length(nuevas_etiquetas)),  # Solo promedios
    aes(group = candidat),
    linetype = "solid",
    linewidth = 0.8,
    alpha = 0.6,
    position = if (any(count(df_long, mes_ano, candidat)$n > 1)) {
      position_dodge(width = 0.5)  # Dodge si hay grupos con >1 observación
    } else {
      position_identity()          # Posición exacta si todos tienen 1 observación
    }
  ) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = if (any(count(df_long, mes_ano, candidat)$n > 1)) {
      position_dodge(width = 0.5)  # Dodge si hay grupos con >1 observación
    } else {
      position_identity()          # Posición exacta si todos tienen 1 observación
    },
    width = 0.25,
    alpha = 0.5
  ) +
  geom_point(
    position = if (any(count(df_long, mes_ano, candidat)$n > 1)) {
      position_dodge(width = 0.5)  # Dodge si hay grupos con >1 observación
    } else {
      position_identity()          # Posición exacta si todos tienen 1 observación
    },
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
    shape = "Consultora o empresa",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE y 'súper encuesta'
    de Marcelo Claure, intervalos de confianza escalados con base en el margen de error que publica la encuestadora"
  )  +
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


####Votos efectivos----
g_jun_efc<-df_efectivo %>%
  filter(lubridate::month(mes_ano) == 6) %>%
  ggplot(aes(
    x = candidat,
    y = intencion,
    fill = as_label(cons_emp),
    group = as_label(cons_emp)
  )) +
  geom_col(position = position_dodge(width = 1), alpha = 1, width = .8) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(width = 1),
                width = .8, alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # si tenés paleta definida
  labs(
    title = "Marzo - Junio 2025: intención de voto válido por candidato y consultora o empresa encuestadora",
    x = "Candidatura",
    y = "Intención de voto (%)",
    fill = "Consultora o empresa",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE,
    intervalos de confianza escalados con base en el margen de error que publica la encuestadora"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_jun_efc


#Gráfico votos efectivos
g_efectivo <- ggplot(df_efectivo, aes(
  x = mes_ano,
  y = intencion,
  color = candidat,
  shape = as_label(cons_emp)
)) +
  geom_line(
    data = df_efectivo %>% filter(cons_emp == length(nuevas_etiquetas)),  # Solo promedios
    aes(group = candidat),
    linetype = "solid",
    linewidth = 0.8,
    alpha = 0.6,
    position = if (any(count(df_long, mes_ano, candidat)$n > 1)) {
      position_dodge(width = 0.5)  # Dodge si hay grupos con >1 observación
    } else {
      position_identity()          # Posición exacta si todos tienen 1 observación
    }
  ) +
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = if (any(count(df_long, mes_ano, candidat)$n > 1)) {
      position_dodge(width = 0.5)  # Dodge si hay grupos con >1 observación
    } else {
      position_identity()          # Posición exacta si todos tienen 1 observación
    },
    width = 0.25,
    alpha = 0.5
  ) +
  geom_point(
    position = if (any(count(df_long, mes_ano, candidat)$n > 1)) {
      position_dodge(width = 0.5)  # Dodge si hay grupos con >1 observación
    } else {
      position_identity()          # Posición exacta si todos tienen 1 observación
    },
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
    shape = "Consultora o empresa",
    caption = "Fuente: Elaboración propia con base en encuestas autorizadas por el TSE y 'súper encuesta'
    de Marcelo Claure, intervalos de confianza escalados con base en el margen de error que publica la encuestadora"
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