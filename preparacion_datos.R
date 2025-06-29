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


etiquetas_actuales <- attr(df_long$cons_emp, "labels")

# Asegurarse de que los valores son numéricos
etiquetas_actuales <- setNames(as.numeric(etiquetas_actuales), names(etiquetas_actuales))

etiquetas_actuales
# Agregar el nuevo nivel "Promedio" con valor (número de etiquetas + 1)


nuevas_etiquetas <- c(etiquetas_actuales, "Promedio" = (length(etiquetas_actuales)+1))

# Reasignar etiquetas a la variable 'medio'
df_long$cons_emp <- set_labels(df_long$cons_emp, labels = nuevas_etiquetas)

get_labels(df_long$cons_emp)

table(df_long$mes_ano)


#Calcular promedios mensuales
promedios<- df_long %>%
  group_by(mes_ano, candidat) %>%
  summarise(
    intencion = mean(intencion, na.rm = TRUE),
    margen_error = mean(margen_error, na.rm = TRUE),
    .groups = "drop"
  )|>
  mutate(cons_emp = length(nuevas_etiquetas))# La cuenta de las nuevas_etiquetas ahora corresponde a la etiqueta "Promedio"
promedios
# Unir los promedios al df_long original
df_long <- bind_rows(df_long, promedios)
df_long

table(df_long$cons_emp)
df_long$cons_emp
#Ya aparece el promedio

df_long$cons_emp <- set_labels(
  as.numeric(df_long$cons_emp),  # Asegura que sea numérico
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
  "#65dfe8",  # JD
  "#01a8ec",  # JF
  "#aa1622",  # AS
  "#541a67",  # MRV
  "#ff6364",  # TUTO
  "#a6761d",  # EVA
  "brown",  # RPAZ
  "#002696",  # EDU
  "#ffc603",  # KENCHA
  "gray",  # BLANCO
  "black",  # NULO
  "red"   # INDE
)
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
    total_valido = sum(intencion_cruda, na.rm = TRUE),
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
  summarise(total = sum(intencion, na.rm = TRUE), .groups = "drop") %>%
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
etiquetas_actuales_ef <- attr(df_efectivo$cons_emp, "labels")
etiquetas_actuales
etiquetas_actuales_ef
etiquetas_actuales_ef <- setNames(as.numeric(etiquetas_actuales_ef), names(etiquetas_actuales_ef))
nuevas_etiquetas_ef <- c(etiquetas_actuales, "Promedio" = length(etiquetas_actuales_ef))#no necesita sumarse
df_efectivo$cons_emp <- set_labels(df_efectivo$cons_emp, labels = nuevas_etiquetas_ef)

# 5. Calcular promedios (mismo código, ahora con intención reescalada)
promedios_efectivo <- df_efectivo %>%
  group_by(mes_ano, candidat) %>%
  summarise(
    intencion = mean(intencion, na.rm = TRUE),
    margen_error_efectivo = mean(margen_error_efectivo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cons_emp = length(nuevas_etiquetas_ef))  # Valor etiquetado como "Promedio"
promedios_efectivo

# 6. Combinar conservando etiquetas (igual que tu versión)
df_efectivo<- bind_rows(df_efectivo, promedios_efectivo) %>%
  mutate(cons_emp = set_labels(as.numeric(cons_emp), labels = nuevas_etiquetas))


# Verificación
table(df_efectivo$cons_emp)  # Debe incluir 22="Promedio"
get_labels(df_efectivo$cons_emp)


df_efectivo<- df_efectivo|>
  mutate(
    lower_ci = intencion - margen_error_efectivo,
    upper_ci = intencion + margen_error_efectivo,
    lower_ci = pmax(lower_ci, 0),
    upper_ci = pmin(upper_ci, 1)
  )


colores_10 <- c(
  "#3dac34",  # ADR
  "#65dfe8",  # JD
  "#01a8ec",  # JF
  "#aa1622",  # AS
  "#541a67",  # MRV
  "#ff6364",  # TUTO
  "#a6761d",  # EVA
  "brown",  # RPAZ
  "#002696",  # EDU
  "#ffc603" #KENCHA
)



