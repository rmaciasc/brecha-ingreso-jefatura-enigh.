###############################################################################
# Pregunta 2: ¿Existe una brecha de ingresos entre hogares con jefatura
# femenina y masculina, y se ha cerrado? ENIGH 2022 y 2024
# Estadística e Indicadores Sociales, 2027-1
#
# Versión final: análisis por FUENTE DE INGRESO. 
#
# Las cinco fuentes del ingreso corriente:
#   Trabajo, Rentas, Transferencias, Estimación del alquiler y Otros ingresos.
#
# Pasos:
#   0. Preparar las bases
#   1. Tamaño del grupo y precisión de las estimaciones
#   2. Dónde se ubica el grupo en la distribución del ingreso
#   3. La brecha en cada año (media y mediana)
#   4. Descomposición de la desigualdad dentro y entre grupos
#   5. Gini, Theil y curvas de Lorenz
#   6. El cambio 2022-2024 y su significancia estadística
#   7. Cómo se conforma el ingreso por fuente
#        (por jefatura, por fuente y por decil)
#   8. Qué pasa con y sin cada fuente del ingreso
#        (brecha, Gini y Theil dentro/entre)
#
# Los pasos 1 a 8 se hacen tres veces: para el total del país (Nacional) y
# para las localidades urbanas y rurales (ver el ciclo for más abajo).
#
# Para correrlo: cambiar solo la línea de setwd() por la carpeta donde están
# concentradohogar22.dta y concentradohogar24.dta, y correr todo el script.
# Cada cuadro se ve en la consola y en el Viewer; cada gráfica, en Plots.
# Los cuadros quedan en resultados/cuadros (CSV y HTML) y las gráficas en
# resultados/figuras.
#
# Las variables que se crean tienen nombre en MAYÚSCULAS y descriptivo
# (por ejemplo INGRESO_PC, JEFATURA, DECIL); esos nombres son los encabezados
# de los cuadros.
###############################################################################

# Carpeta de trabajo: ahí deben estar los dos .dta. Es la ÚNICA línea a cambiar.
setwd("C:\Users\lizbe\OneDrive\Desktop\Trabajos R")


# ==============================================================================
# Paquetes
# ==============================================================================
# Instalar solo la primera vez (quitar el # para correrlo):
# install.packages(c("survey", "convey", "dplyr", "ggplot2", "haven", "flextable"))
# dineq ya no está en CRAN; se instala desde el archivo histórico
# (antes instalar sus dependencias: install.packages(c("boot", "Hmisc"))):
# install.packages("https://cran.r-project.org/src/contrib/Archive/dineq/dineq_0.1.0.tar.gz",
#                  repos = NULL, type = "source")

library(survey)  # estimaciones con el diseño muestral de la encuesta
library(convey)  # Gini, Theil y curva de Lorenz con diseño muestral
library(dplyr)   # crear y modificar variables (mutate)
library(ggplot2) # gráficas
library(haven)   # leer las bases .dta
library(dineq)   # ntiles.wtd: deciles con factor de expansión
library(flextable) # tablas con formato

# Carpetas donde se guardan los resultados (se crean si no existen)
dir.create("resultados/cuadros", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/figuras", recursive = TRUE, showWarnings = FALSE)

# Colores de las gráficas
col_jef  <- c("Hombre" = "#2e8b57", "Mujer" = "#7b2d8e") # por jefatura
col_anio <- c("2022" = "#1f77b4", "2024" = "#ff7f0e")    # por año
pie_fuente <- "Fuente: Elaboración propia con datos de la ENIGH (2022 y 2024)"

# Las cinco fuentes del ingreso corriente: nombre de la variable y nombre
# para mostrar en cuadros y gráficas (mismo orden)
fuentes_ingreso <- c("TRABAJO", "RENTAS", "TRANSFERENCIAS",
                     "ALQUILER_ESTIMADO", "OTROS_INGRESOS")
nombres_fuente  <- c("Trabajo", "Rentas", "Transferencias",
                     "Estimación del alquiler", "Otros ingresos")
col_fuente <- c("Trabajo" = "#1b9e77", "Rentas" = "#d95f02",
                "Transferencias" = "#7570b3", "Estimación del alquiler" = "#e7298a",
                "Otros ingresos" = "#66a61e")

# Escenarios del paso 8: ingreso completo y quitando una fuente a la vez
escenarios     <- c("Ingreso completo", paste("Sin", tolower(nombres_fuente)))
variables_esc  <- c("INGRESO_PC", paste0("SIN_", fuentes_ingreso, "_PC"))


# ==============================================================================
# Funciones de apoyo (cortas)
# ==============================================================================

# Criterio de precisión de INEGI:
# CV < 15 % alta, de 15 a 30 % moderada, más de 30 % baja
precision <- function(cv) ifelse(cv < 15, "Alta", ifelse(cv <= 30, "Moderada", "Baja"))

# est(): saca la estimación de un grupo ("Hombre" o "Mujer") de un resultado
# ee():  saca su error estándar
# Si el resultado no tiene grupos (por ejemplo, el nacional), se usa 1.
est <- function(x, grupo) coef(x)[[grupo]]
ee  <- function(x, grupo) setNames(SE(x), names(coef(x)))[[grupo]]

# Palabras que llevan acento o símbolo en los encabezados de los cuadros
# (los nombres de las variables no llevan acentos)
acentos <- c(ANIO = "AÑO", AMBITO = "ÁMBITO", RAZON = "RAZÓN", ESTIMACION = "ESTIMACIÓN",
             PRECISION = "PRECISIÓN", POBLACION = "POBLACIÓN", DISTRIBUCION = "DISTRIBUCIÓN",
             DESCOMPOSICION = "DESCOMPOSICIÓN", PARTICIPACION = "PARTICIPACIÓN",
             ESTANDAR = "ESTÁNDAR", PCT = "%", INDICE = "ÍNDICE", DIFERENCIA = "DIFERENCIA")

# encabezado(): convierte INGRESO_MEDIO_MUJER en "INGRESO MEDIO MUJER"
encabezado <- function(nombres) {
  sapply(strsplit(nombres, "_"), function(p) {
    p <- ifelse(p %in% names(acentos), acentos[p], p)
    paste(unname(p), collapse = " ")
  })
}

# mostrar(): enseña un cuadro de dos formas, igual que las gráficas:
#   1) en la consola, y
#   2) como tabla con formato en el panel Viewer de RStudio (con las flechas
#      del Viewer se puede regresar a las tablas anteriores).
# También la guarda en HTML en resultados/cuadros, para copiarla a la presentación.
mostrar <- function(cuadro, titulo, archivo, decimales = 2) {
  print(cuadro) # en la consola
  if ("ANIO" %in% names(cuadro)) cuadro$ANIO <- as.character(cuadro$ANIO) # que el año no salga como 2,022
  tabla <- flextable(cuadro) %>%
    theme_vanilla() %>% 
    bg(bg = "white", part = "all") %>%
    colformat_double(digits = decimales) %>%    # redondea los números
    set_header_labels(values = setNames(as.list(encabezado(names(cuadro))), names(cuadro))) %>%
    set_caption(caption = titulo) %>%           # título de la tabla
    add_footer_lines(pie_fuente) %>%            # fuente al pie
    autofit()                                   # ajusta el ancho de las columnas
  print(tabla) # en el Viewer
  save_as_image(tabla, path = file.path("resultados/cuadros", paste0(archivo, ".png")))
}

# Prueba z para saber si un indicador cambió entre 2022 y 2024
#   z = (valor 2024 - valor 2022) / raíz(ee2022^2 + ee2024^2)
prueba_z <- function(indicador, est22, ee22, est24, ee24) {
  z <- (est24 - est22) / sqrt(ee22^2 + ee24^2) # estadístico z
  p <- 2 * (1 - pnorm(abs(z)))                # valor p de dos colas
  data.frame(INDICADOR = indicador,
             VALOR_2022 = est22, VALOR_2024 = est24,
             CAMBIO = est24 - est22,
             Z = round(z, 3), VALOR_P = round(p, 4),
             CAMBIO_SIGNIFICATIVO = p < 0.05)
}


###############################################################################
# 0. PREPARAR LAS BASES
#    deflactar todas las fuentes, grupo, ámbito, deciles y diseño muestral
###############################################################################

# El deflactor es el que se usó en clase
deflactor22 <- 0.9102291693
deflactor24 <- 1

# preparar(): crea, en una base de la ENIGH, las variables del análisis.
# Se usa igual para 2022 y para 2024 (solo cambia el deflactor).
preparar <- function(base, deflactor) {

  base <- mutate(base,

    # ---- Ingreso trimestral del hogar, en pesos de 2024, por fuente ----
    INGRESO_TOTAL     = ing_cor / deflactor,    # ingreso corriente total
    TRABAJO           = ingtrab / deflactor,    # ingreso por trabajo
    RENTAS            = rentas / deflactor,     # renta de la propiedad
    TRANSFERENCIAS    = transfer / deflactor,   # transferencias
    ALQUILER_ESTIMADO = estim_alqu / deflactor, # estimación del alquiler
    OTROS_INGRESOS    = otros_ing / deflactor,  # otros ingresos

    # ---- Integrantes e ingreso per cápita (ingreso del hogar / integrantes) ----
    INTEGRANTES       = tot_integ,
    INGRESO_PC        = INGRESO_TOTAL / INTEGRANTES,
    TRABAJO_PC           = TRABAJO / INTEGRANTES,
    RENTAS_PC            = RENTAS / INTEGRANTES,
    TRANSFERENCIAS_PC    = TRANSFERENCIAS / INTEGRANTES,
    ALQUILER_ESTIMADO_PC = ALQUILER_ESTIMADO / INTEGRANTES,
    OTROS_INGRESOS_PC    = OTROS_INGRESOS / INTEGRANTES,

    # ---- Ingreso per cápita SIN una fuente (paso 8) ----
    SIN_TRABAJO_PC           = (INGRESO_TOTAL - TRABAJO) / INTEGRANTES,
    SIN_RENTAS_PC            = (INGRESO_TOTAL - RENTAS) / INTEGRANTES,
    SIN_TRANSFERENCIAS_PC    = (INGRESO_TOTAL - TRANSFERENCIAS) / INTEGRANTES,
    SIN_ALQUILER_ESTIMADO_PC = (INGRESO_TOTAL - ALQUILER_ESTIMADO) / INTEGRANTES,
    SIN_OTROS_INGRESOS_PC    = (INGRESO_TOTAL - OTROS_INGRESOS) / INTEGRANTES,

    # ---- El grupo: sexo de la jefatura del hogar ----
    # En sexo_jefe: 1 = hombre, 2 = mujer (descriptor de INEGI)
    JEFATURA = factor(as.numeric(sexo_jefe), levels = c(1, 2),
                      labels = c("Hombre", "Mujer")),
    MUJER    = ifelse(JEFATURA == "Mujer", 1, 0), # 1 si la jefatura es femenina
    UNO      = 1,                                 # para contar hogares

    # ---- Ámbito:tam_loc 1, 2 y 3 = urbano; 4 = rural ----
    #   (1 = 100 mil y más hab., 2 = 15 mil a 99,999, 3 = 2,500 a 14,999,
    #    4 = menos de 2,500 hab.)
    AMBITO = case_when(tam_loc == "1" ~ "Urbano",
                       tam_loc == "2" ~ "Urbano",
                       tam_loc == "3" ~ "Urbano",
                       tam_loc == "4" ~ "Rural"))

  # ---- Deciles nacionales de hogares por ingreso per cápita ----
  # Se calculan con todos los hogares del país (no por grupo)
  base$DECIL <- ntiles.wtd(x = base$INGRESO_PC, n = 10, weights = base$factor)

  base
}

base22 <- preparar(read_dta("C:/Users/lizbe/OneDrive/Desktop/Trabajos R/concentradohogar22.dta"), deflactor22)
base24 <- preparar(read_dta("C:/Users/lizbe/OneDrive/Desktop/Trabajos R/concentradohogar24.dta"), deflactor24)

# Comprobación: las cinco fuentes deben sumar el ingreso corriente
# (debe salir prácticamente 0)
print(max(abs(base22$INGRESO_TOTAL - (base22$TRABAJO + base22$RENTAS + base22$TRANSFERENCIAS +
                                      base22$ALQUILER_ESTIMADO + base22$OTROS_INGRESOS))))
print(max(abs(base24$INGRESO_TOTAL - (base24$TRABAJO + base24$RENTAS + base24$TRANSFERENCIAS +
                                      base24$ALQUILER_ESTIMADO + base24$OTROS_INGRESOS))))

# Revisar que no queden NA en el grupo ni en el ámbito
print(table(base22$JEFATURA, useNA = "ifany"))
print(table(base24$JEFATURA, useNA = "ifany"))
print(table(base22$AMBITO, useNA = "ifany"))
print(table(base24$AMBITO, useNA = "ifany"))

# ---- Diseño muestral ----
# UPM, factor de expansión y estrato; convey_prep lo prepara
# para Gini, Theil y Lorenz
diseno22 <- svydesign(ids = ~upm, weights = ~factor, strata = ~est_dis, data = base22)
diseno22 <- convey_prep(diseno22)

diseno24 <- svydesign(ids = ~upm, weights = ~factor, strata = ~est_dis, data = base24)
diseno24 <- convey_prep(diseno24)


###############################################################################
# FUNCIONES PARA LOS PASOS 4, 7 Y 8 (se definen una vez, antes del ciclo)
###############################################################################

# ---- Funciones ----
w_mean <- function(x, w) sum(w * x) / sum(w)
w_var  <- function(x, w) sum(w * (x - w_mean(x, w))^2) / sum(w)
w_cov  <- function(x, y, w) sum(w * (x - w_mean(x, w)) * (y - w_mean(y, w))) / sum(w)
# Rango fraccional F(y) ponderado (punto medio de cada observación)
w_rank <- function(x, w) {
  o <- order(x); Fo <- (cumsum(w[o]) - 0.5 * w[o]) / sum(w)
  r <- numeric(length(x)); r[o] <- Fo; r
}
# Gini por la fórmula de la covarianza: G = 2 cov(y, F(y)) / media
gini_w <- function(x, w) 2 * w_cov(x, w_rank(x, w), w) / w_mean(x, w)

# Varianza: intra + entre
descomp_var <- function(y, w, g) {
  m <- w_mean(y, w)
  t <- bind_rows(lapply(levels(g), function(k) {
    i <- g == k
    data.frame(grupo = k, pi = sum(w[i]) / sum(w), media = w_mean(y[i], w[i]),
               var = w_var(y[i], w[i]))
  }))
  t$intra <- t$pi * t$var               # aporte del grupo a la intravarianza
  t$entre <- t$pi * (t$media - m)^2     # aporte del grupo a la intervarianza
  list(tabla = t, media = m, total = w_var(y, w), intra = sum(t$intra), entre = sum(t$entre))
}

# Entropía generalizada por subgrupos (réplica de -ineqdeco- de S. P. Jenkins)
ineqdeco <- function(y, w, g) {
  ok <- !is.na(y) & y > 0 & w > 0
  y <- y[ok]; w <- w[ok]; g <- droplevels(g[ok])
  nucleo <- function(y, w) {
    m <- w_mean(y, w); r <- y / m
    c(sumw = sum(w), mean = m, gem1 = (w_mean(1 / r, w) - 1) / 2,
      ge0 = log(m) - w_mean(log(y), w), ge1 = w_mean(r * log(r), w),
      ge2 = (w_mean(r^2, w) - 1) / 2, gini = gini_w(y, w))
  }
  tot <- nucleo(y, w)
  sub <- as.data.frame(t(sapply(levels(g), function(k) nucleo(y[g == k], w[g == k]))))
  sub$grupo     <- levels(g)
  sub$pop_share <- sub$sumw / tot[["sumw"]]       # pi_k
  sub$rel_mean  <- sub$mean / tot[["mean"]]       # lambda_k
  sub$inc_share <- sub$pop_share * sub$rel_mean   # s_k = pi_k * lambda_k
  v <- sub$pop_share; l <- sub$rel_mean
  list(total = tot, sub = sub, excluidas = sum(!ok),
       within  = c(gem1 = sum(v / l * sub$gem1), ge0 = sum(v * sub$ge0),
                   ge1 = sum(v * l * sub$ge1),   ge2 = sum(v * l^2 * sub$ge2)),
       between = c(gem1 = (sum(v / l) - 1) / 2,  ge0 = sum(v * log(1 / l)),
                   ge1 = sum(v * l * log(l)),    ge2 = (sum(v * l^2) - 1) / 2))
}

# brecha_MH(): en un diseño (o en un pedazo del diseño, con subset) y para una
# variable de ingreso per cápita (por ejemplo "TRABAJO_PC"), calcula la media
# de cada jefatura con su CV, la razón Mujer/Hombre con su error estándar y su
# CV, y la diferencia Mujer - Hombre. (Pasos 7 y 8)
brecha_MH <- function(d, variable) {
  f <- as.formula(paste0("~", variable))                     # ~VARIABLE
  m <- svyby(f, ~JEFATURA, d, svymean, covmat = TRUE)        # medias
  r <- svycontrast(m, quote(Mujer / Hombre))                 # razón M/H
  data.frame(
    MEDIA_HOMBRE       = est(m, "Hombre"),
    CV_HOMBRE          = 100 * ee(m, "Hombre") / est(m, "Hombre"),
    MEDIA_MUJER        = est(m, "Mujer"),
    CV_MUJER           = 100 * ee(m, "Mujer") / est(m, "Mujer"),
    DIFERENCIA_MUJER_HOMBRE = est(m, "Mujer") - est(m, "Hombre"),
    RAZON_MUJER_HOMBRE = est(r, 1),
    ERROR_ESTANDAR_RAZON = ee(r, 1),
    CV_RAZON           = 100 * ee(r, 1) / est(r, 1))
}

# composicion(): qué % del ingreso corriente total viene de cada fuente, para
# los hogares con jefatura masculina, femenina y el total, con su CV.
# Es el cociente (total de la fuente) / (total del ingreso corriente).
composicion <- function(d) {
  salida <- NULL
  for (k in 1:5) {
    f <- as.formula(paste0("~", fuentes_ingreso[k]))
    por_jefatura <- svyby(f, ~JEFATURA, denominator = ~INGRESO_TOTAL, design = d,
                          svyratio, vartype = c("se", "cv"))
    total <- svyratio(f, ~INGRESO_TOTAL, d)
    salida <- rbind(salida, data.frame(
      JEFATURA        = c("Hombre", "Mujer", "Total"),
      FUENTE          = nombres_fuente[k],
      PCT_DEL_INGRESO = 100 * c(coef(por_jefatura), coef(total)),
      CV              = 100 * c(cv(por_jefatura), cv(total))))
  }
  salida$PRECISION <- precision(salida$CV)
  salida
}

# composicion_decil(): igual, pero dentro de cada decil nacional y jefatura.
# Cada fila suma 100 %. HOGARES_EN_MUESTRA ayuda a juzgar qué tan confiable es
# la celda (pocos hogares = estimación menos confiable).
composicion_decil <- function(b) {
  b %>%
    group_by(DECIL, JEFATURA) %>%
    summarise(
      HOGARES_EN_MUESTRA     = n(),
      PCT_TRABAJO            = 100 * sum(factor * TRABAJO) / sum(factor * INGRESO_TOTAL),
      PCT_RENTAS             = 100 * sum(factor * RENTAS) / sum(factor * INGRESO_TOTAL),
      PCT_TRANSFERENCIAS     = 100 * sum(factor * TRANSFERENCIAS) / sum(factor * INGRESO_TOTAL),
      PCT_ALQUILER_ESTIMADO  = 100 * sum(factor * ALQUILER_ESTIMADO) / sum(factor * INGRESO_TOTAL),
      PCT_OTROS_INGRESOS     = 100 * sum(factor * OTROS_INGRESOS) / sum(factor * INGRESO_TOTAL),
      .groups = "drop") %>%
    as.data.frame()
}

# gini_escenario(): Gini de cada jefatura y del total para una variable
gini_escenario <- function(d, variable) {
  f <- as.formula(paste0("~", variable))
  g <- svyby(f, ~JEFATURA, d, svygini, vartype = c("se", "cv"))
  t <- svygini(f, d)
  data.frame(GINI_HOMBRE = est(g, "Hombre"), GINI_MUJER = est(g, "Mujer"),
             GINI_TOTAL = est(t, 1))
}

cortes <- seq(0, 1, 0.1) # cortes de las curvas de Lorenz: 0, 10 %, ..., 100 % de los hogares


###############################################################################
# EL ANÁLISIS SE HACE PARA TRES ÁMBITOS: NACIONAL, URBANO Y RURAL
#
# Un ciclo for repite los pasos 1 a 8 tres veces.
# En cada vuelta:
#   d22, d24 = diseño muestral del ámbito (subset del DISEÑO nacional, para
#              conservar bien estratos y UPM, como se vio en clase)
#   b22, b24 = base de hogares del ámbito (para la descomposición del paso 4
#              y la composición por decil del paso 7)
# Cada cuadro se guarda con una columna AMBITO. Al terminar el ciclo se
# juntan los tres ámbitos y se muestran (consola y Viewer) en un solo cuadro.
#
# Los deciles son siempre los NACIONALES: en lo rural, por ejemplo, se ve en
# qué deciles del país quedan los hogares rurales.
###############################################################################

ambitos <- c("Nacional", "Urbano", "Rural")

# Cuadros vacíos donde se van juntando los resultados de cada ámbito
todo_tamano <- NULL;  todo_precision <- NULL;  todo_ubicacion <- NULL
todo_brecha <- NULL;  todo_descomposicion <- NULL;  todo_grupos <- NULL
todo_desigualdad <- NULL;  todo_lorenz <- NULL;  todo_cambios <- NULL
todo_composicion <- NULL;  todo_brecha_fuente <- NULL;  todo_comp_decil <- NULL
todo_con_sin <- NULL;  todo_theil_con_sin <- NULL

for (amb in ambitos) {

  cat("\n==================== Ámbito:", amb, "====================\n")

# ---- Diseños y bases del ámbito ----
  if (amb == "Nacional") {
    d22 <- diseno22
    d24 <- diseno24
    b22 <- base22
    b24 <- base24
  } else {
    d22 <- subset(diseno22, AMBITO == amb)
    d24 <- subset(diseno24, AMBITO == amb)
    b22 <- base22[base22$AMBITO == amb, ]
    b24 <- base24[base24$AMBITO == amb, ]
  }


  #############################################################################
  # 1. TAMAÑO DEL GRUPO Y PRECISIÓN DE LAS ESTIMACIONES
  #############################################################################

  # ---- 1.1 Tamaño del grupo ----
  # Hogares y personas expandidos por jefatura
  hogares22  <- svyby(~UNO, ~JEFATURA, d22, svytotal, vartype = c("se", "cv"))
  hogares24  <- svyby(~UNO, ~JEFATURA, d24, svytotal, vartype = c("se", "cv"))
  personas22 <- svyby(~INTEGRANTES, ~JEFATURA, d22, svytotal, vartype = c("se", "cv"))
  personas24 <- svyby(~INTEGRANTES, ~JEFATURA, d24, svytotal, vartype = c("se", "cv"))

  # Porcentaje de hogares con jefatura femenina
  pct_mujer22 <- svymean(~MUJER, d22)
  pct_mujer24 <- svymean(~MUJER, d24)

  # Número promedio de integrantes por hogar (ayuda a entender el ingreso per cápita:
  # con el mismo ingreso, un hogar con menos integrantes tiene más ingreso por persona)
  integ22 <- svyby(~INTEGRANTES, ~JEFATURA, d22, svymean, vartype = c("se", "cv"))
  integ24 <- svyby(~INTEGRANTES, ~JEFATURA, d24, svymean, vartype = c("se", "cv"))

  cuadro_tamano <- data.frame(
    ANIO                 = c(2022, 2022, 2024, 2024),
    JEFATURA             = c("Hombre", "Mujer", "Hombre", "Mujer"),
    HOGARES_EN_MUESTRA   = c(table(b22$JEFATURA), table(b24$JEFATURA)),    # sin expandir
    HOGARES              = c(hogares22$UNO, hogares24$UNO),                # expandidos
    PERSONAS             = c(personas22$INTEGRANTES, personas24$INTEGRANTES), # expandidas
    PCT_HOGARES          = 100 * c(1 - coef(pct_mujer22), coef(pct_mujer22),
                                   1 - coef(pct_mujer24), coef(pct_mujer24)),
    INTEGRANTES_PROMEDIO = c(coef(integ22), coef(integ24)),                # personas por hogar
    ERROR_ESTANDAR_INTEGRANTES = c(SE(integ22), SE(integ24)),
    CV_INTEGRANTES       = 100 * c(cv(integ22), cv(integ24)))
  cuadro_tamano$PRECISION_INTEGRANTES <- precision(cuadro_tamano$CV_INTEGRANTES)
  todo_tamano <- rbind(todo_tamano, data.frame(AMBITO = amb, cuadro_tamano))

  # ---- 1.2 Precisión: media, mediana y Gini del ingreso per cápita ----
  media22   <- svyby(~INGRESO_PC, ~JEFATURA, d22, svymean, vartype = c("se", "cv"))
  media24   <- svyby(~INGRESO_PC, ~JEFATURA, d24, svymean, vartype = c("se", "cv"))
  mediana22 <- svyby(~INGRESO_PC, ~JEFATURA, d22, svyquantile, quantiles = 0.5,
                     ci = TRUE, vartype = c("se", "cv"))
  mediana24 <- svyby(~INGRESO_PC, ~JEFATURA, d24, svyquantile, quantiles = 0.5,
                     ci = TRUE, vartype = c("se", "cv"))
  gini22    <- svyby(~INGRESO_PC, ~JEFATURA, d22, svygini, vartype = c("se", "cv"))
  gini24    <- svyby(~INGRESO_PC, ~JEFATURA, d24, svygini, vartype = c("se", "cv"))

  cuadro_precision <- data.frame(
    ANIO = rep(c(2022, 2024), each = 7),
    INDICADOR = rep(c("% hogares con jefatura femenina",
                      "Ingreso medio per cápita", "Ingreso medio per cápita",
                      "Ingreso mediano per cápita", "Ingreso mediano per cápita",
                      "Gini", "Gini"), 2),
    JEFATURA = rep(c("Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre", "Mujer"), 2),
    ESTIMACION = c(100 * coef(pct_mujer22), coef(media22), coef(mediana22), coef(gini22),
                   100 * coef(pct_mujer24), coef(media24), coef(mediana24), coef(gini24)),
    CV = 100 * c(cv(pct_mujer22), cv(media22), cv(mediana22), cv(gini22),
                 cv(pct_mujer24), cv(media24), cv(mediana24), cv(gini24)))
  cuadro_precision$PRECISION <- precision(cuadro_precision$CV) # Alta / Moderada / Baja
  todo_precision <- rbind(todo_precision, data.frame(AMBITO = amb, cuadro_precision))


  #############################################################################
  # 2. DÓNDE SE UBICA EL GRUPO EN LA DISTRIBUCIÓN DEL INGRESO
  #############################################################################
#---- 2.1 % de hogares con jefatura femenina dentro de cada decil nacional ----
pct_decil22 <- svyby(~MUJER, ~DECIL, d22, svymean, vartype = c("se", "cv"))
pct_decil24 <- svyby(~MUJER, ~DECIL, d24, svymean, vartype = c("se", "cv"))

  # ---- 2.2 Cómo se reparten los hogares de cada grupo entre los deciles ----
  # (cada columna suma 100 %)
  reparto_mujer22  <- svymean(~factor(DECIL), subset(d22, JEFATURA == "Mujer"))
  reparto_hombre22 <- svymean(~factor(DECIL), subset(d22, JEFATURA == "Hombre"))
  reparto_mujer24  <- svymean(~factor(DECIL), subset(d24, JEFATURA == "Mujer"))
  reparto_hombre24 <- svymean(~factor(DECIL), subset(d24, JEFATURA == "Hombre"))

  cuadro_ubicacion <- data.frame(
    DECIL                     = 1:10,
    PCT_HOGARES_JEFA_2022     = 100 * coef(pct_decil22),
    CV_2022                   = 100 * cv(pct_decil22),
    PRECISION_2022            = precision(100 * cv(pct_decil22)),
    PCT_HOGARES_JEFA_2024     = 100 * coef(pct_decil24),
    CV_2024                   = 100 * cv(pct_decil24),
    PRECISION_2024            = precision(100 * cv(pct_decil24)),
    DISTRIBUCION_JEFAS_2022   = 100 * coef(reparto_mujer22),
    DISTRIBUCION_JEFES_2022   = 100 * coef(reparto_hombre22),
    DISTRIBUCION_JEFAS_2024   = 100 * coef(reparto_mujer24),
    DISTRIBUCION_JEFES_2024   = 100 * coef(reparto_hombre24))
  todo_ubicacion <- rbind(todo_ubicacion, data.frame(AMBITO = amb, cuadro_ubicacion))


  #############################################################################
  # 3. LA BRECHA EN CADA AÑO (media y mediana)
  #############################################################################

  # Media por jefatura guardando la covarianza, para poder calcular la razón
  # Mujer/Hombre con su error estándar (svycontrast)
  media_cov22 <- svyby(~INGRESO_PC, ~JEFATURA, d22, svymean, covmat = TRUE)
  media_cov24 <- svyby(~INGRESO_PC, ~JEFATURA, d24, svymean, covmat = TRUE)

  razon22 <- svycontrast(media_cov22, quote(Mujer / Hombre)) # razón M/H
  razon24 <- svycontrast(media_cov24, quote(Mujer / Hombre))
  dif22   <- svycontrast(media_cov22, quote(Mujer - Hombre)) # diferencia M-H en pesos
  dif24   <- svycontrast(media_cov24, quote(Mujer - Hombre))

  cuadro_brecha <- data.frame(
    ANIO                     = c(2022, 2024),
    INGRESO_MEDIO_HOMBRE     = c(est(media22, "Hombre"), est(media24, "Hombre")),
    INGRESO_MEDIO_MUJER      = c(est(media22, "Mujer"), est(media24, "Mujer")),
    RAZON_MUJER_HOMBRE       = c(est(razon22, 1), est(razon24, 1)),
    ERROR_ESTANDAR_RAZON     = c(ee(razon22, 1), ee(razon24, 1)),
    DIFERENCIA_MUJER_HOMBRE  = c(est(dif22, 1), est(dif24, 1)),
    ERROR_ESTANDAR_DIFERENCIA = c(ee(dif22, 1), ee(dif24, 1)),
    INGRESO_MEDIANO_HOMBRE   = c(est(mediana22, "Hombre"), est(mediana24, "Hombre")),
    INGRESO_MEDIANO_MUJER    = c(est(mediana22, "Mujer"), est(mediana24, "Mujer")))
  cuadro_brecha$RAZON_MEDIANAS_MUJER_HOMBRE <-
    cuadro_brecha$INGRESO_MEDIANO_MUJER / cuadro_brecha$INGRESO_MEDIANO_HOMBRE
  todo_brecha <- rbind(todo_brecha, data.frame(AMBITO = amb, cuadro_brecha))


  #############################################################################
  # 4. DESCOMPOSICIÓN DE LA DESIGUALDAD DENTRO Y ENTRE GRUPOS
  #############################################################################

  # ---- 4.1 Varianza del ingreso per cápita: dentro y entre grupos ----
  var22 <- descomp_var(y = b22$INGRESO_PC, w = b22$factor, g = b22$JEFATURA)
  var24 <- descomp_var(y = b24$INGRESO_PC, w = b24$factor, g = b24$JEFATURA)

  # ---- 4.2 Theil y GE(0): dentro y entre grupos ----
  theil22 <- ineqdeco(y = b22$INGRESO_PC, w = b22$factor, g = b22$JEFATURA)
  theil24 <- ineqdeco(y = b24$INGRESO_PC, w = b24$factor, g = b24$JEFATURA)

  cuadro_descomposicion <- data.frame(
    ANIO   = rep(c(2022, 2024), each = 3),
    INDICE = rep(c("Varianza", "Theil GE(1)", "GE(0)"), 2),
    TOTAL  = c(var22$total, theil22$total[["ge1"]], theil22$total[["ge0"]],
               var24$total, theil24$total[["ge1"]], theil24$total[["ge0"]]),
    DESIGUALDAD_DENTRO = c(var22$intra, theil22$within[["ge1"]], theil22$within[["ge0"]],
                           var24$intra, theil24$within[["ge1"]], theil24$within[["ge0"]]),
    DESIGUALDAD_ENTRE  = c(var22$entre, theil22$between[["ge1"]], theil22$between[["ge0"]],
                           var24$entre, theil24$between[["ge1"]], theil24$between[["ge0"]]))
  cuadro_descomposicion$PCT_DENTRO <- 100 * cuadro_descomposicion$DESIGUALDAD_DENTRO / cuadro_descomposicion$TOTAL
  cuadro_descomposicion$PCT_ENTRE  <- 100 * cuadro_descomposicion$DESIGUALDAD_ENTRE / cuadro_descomposicion$TOTAL
  todo_descomposicion <- rbind(todo_descomposicion, data.frame(AMBITO = amb, cuadro_descomposicion))

  # Detalle por grupo (peso en la población y en el ingreso, media relativa)
  cuadro_grupos <- data.frame(
    ANIO             = rep(c(2022, 2024), each = 2),
    JEFATURA         = c(theil22$sub$grupo, theil24$sub$grupo),
    PCT_HOGARES      = 100 * c(theil22$sub$pop_share, theil24$sub$pop_share),
    PCT_INGRESO      = 100 * c(theil22$sub$inc_share, theil24$sub$inc_share),
    INGRESO_RELATIVO = c(theil22$sub$rel_mean, theil24$sub$rel_mean))
  todo_grupos <- rbind(todo_grupos, data.frame(AMBITO = amb, cuadro_grupos))


  #############################################################################
  # 5. GINI, THEIL Y CURVAS DE LORENZ
  #############################################################################

  # ---- 5.1 Gini del total del ámbito (por grupo ya está en el paso 1) ----
  gini_tot22 <- svygini(~INGRESO_PC, d22)
  gini_tot24 <- svygini(~INGRESO_PC, d24)

  # ---- 5.2 Theil (solo hogares con ingreso mayor que cero, como en clase) ----
  theil_grupo22 <- svyby(~INGRESO_PC, ~JEFATURA, subset(d22, INGRESO_PC > 0), svygei,
                         epsilon = 1, vartype = c("se", "cv"))
  theil_grupo24 <- svyby(~INGRESO_PC, ~JEFATURA, subset(d24, INGRESO_PC > 0), svygei,
                         epsilon = 1, vartype = c("se", "cv"))
  theil_tot22   <- svygei(~INGRESO_PC, subset(d22, INGRESO_PC > 0), epsilon = 1)
  theil_tot24   <- svygei(~INGRESO_PC, subset(d24, INGRESO_PC > 0), epsilon = 1)

  cuadro_desigualdad <- data.frame(
    ANIO     = rep(c(2022, 2024), each = 3),
    GRUPO    = rep(c("Hombre", "Mujer", "Total"), 2),
    GINI     = c(coef(gini22), coef(gini_tot22), coef(gini24), coef(gini_tot24)),
    CV_GINI  = 100 * c(cv(gini22), cv(gini_tot22), cv(gini24), cv(gini_tot24)),
    THEIL    = c(coef(theil_grupo22), coef(theil_tot22), coef(theil_grupo24), coef(theil_tot24)),
    CV_THEIL = 100 * c(cv(theil_grupo22), cv(theil_tot22), cv(theil_grupo24), cv(theil_tot24)))
  cuadro_desigualdad$PRECISION_GINI  <- precision(cuadro_desigualdad$CV_GINI)
  cuadro_desigualdad$PRECISION_THEIL <- precision(cuadro_desigualdad$CV_THEIL)
  todo_desigualdad <- rbind(todo_desigualdad, data.frame(AMBITO = amb, cuadro_desigualdad))

  # ---- 5.3 Curvas de Lorenz por jefatura y año ----
  lorenz_h22 <- svylorenz(~INGRESO_PC, subset(d22, JEFATURA == "Hombre"),
                          quantiles = cortes, plot = FALSE, na.rm = TRUE)
  lorenz_m22 <- svylorenz(~INGRESO_PC, subset(d22, JEFATURA == "Mujer"),
                          quantiles = cortes, plot = FALSE, na.rm = TRUE)
  lorenz_h24 <- svylorenz(~INGRESO_PC, subset(d24, JEFATURA == "Hombre"),
                          quantiles = cortes, plot = FALSE, na.rm = TRUE)
  lorenz_m24 <- svylorenz(~INGRESO_PC, subset(d24, JEFATURA == "Mujer"),
                          quantiles = cortes, plot = FALSE, na.rm = TRUE)

  cuadro_lorenz <- data.frame(
    ANIO                = rep(c("2022", "2022", "2024", "2024"), each = 11),
    JEFATURA            = rep(c("Hombre", "Mujer", "Hombre", "Mujer"), each = 11),
    POBLACION_ACUMULADA = rep(cortes, 4),
    INGRESO_ACUMULADO   = c(coef(lorenz_h22), coef(lorenz_m22), coef(lorenz_h24), coef(lorenz_m24)))
  todo_lorenz <- rbind(todo_lorenz, data.frame(AMBITO = amb, cuadro_lorenz))


  #############################################################################
  # 6. EL CAMBIO 2022-2024 Y SU SIGNIFICANCIA ESTADÍSTICA
  #############################################################################
  # Si VALOR_P < 0.05 (|z| > 1.96), el cambio es estadísticamente significativo.

  cuadro_cambios <- rbind(
    prueba_z("% hogares con jefatura femenina",
             100 * est(pct_mujer22, 1), 100 * ee(pct_mujer22, 1),
             100 * est(pct_mujer24, 1), 100 * ee(pct_mujer24, 1)),
    prueba_z("Ingreso medio per cápita - Hombre",
             est(media22, "Hombre"), ee(media22, "Hombre"),
             est(media24, "Hombre"), ee(media24, "Hombre")),
    prueba_z("Ingreso medio per cápita - Mujer",
             est(media22, "Mujer"), ee(media22, "Mujer"),
             est(media24, "Mujer"), ee(media24, "Mujer")),
    prueba_z("Integrantes promedio - Hombre",
             est(integ22, "Hombre"), ee(integ22, "Hombre"),
             est(integ24, "Hombre"), ee(integ24, "Hombre")),
    prueba_z("Integrantes promedio - Mujer",
             est(integ22, "Mujer"), ee(integ22, "Mujer"),
             est(integ24, "Mujer"), ee(integ24, "Mujer")),
    prueba_z("Brecha: razón Mujer/Hombre de medias",
             est(razon22, 1), ee(razon22, 1), est(razon24, 1), ee(razon24, 1)),
    prueba_z("Brecha: diferencia Mujer-Hombre de medias",
             est(dif22, 1), ee(dif22, 1), est(dif24, 1), ee(dif24, 1)),
    prueba_z("Ingreso mediano per cápita - Hombre",
             est(mediana22, "Hombre"), ee(mediana22, "Hombre"),
             est(mediana24, "Hombre"), ee(mediana24, "Hombre")),
    prueba_z("Ingreso mediano per cápita - Mujer",
             est(mediana22, "Mujer"), ee(mediana22, "Mujer"),
             est(mediana24, "Mujer"), ee(mediana24, "Mujer")),
    prueba_z("Gini - Hombre",
             est(gini22, "Hombre"), ee(gini22, "Hombre"),
             est(gini24, "Hombre"), ee(gini24, "Hombre")),
    prueba_z("Gini - Mujer",
             est(gini22, "Mujer"), ee(gini22, "Mujer"),
             est(gini24, "Mujer"), ee(gini24, "Mujer")),
    prueba_z("Gini - Total",
             est(gini_tot22, 1), ee(gini_tot22, 1), est(gini_tot24, 1), ee(gini_tot24, 1)),
    prueba_z("Theil - Hombre",
             est(theil_grupo22, "Hombre"), ee(theil_grupo22, "Hombre"),
             est(theil_grupo24, "Hombre"), ee(theil_grupo24, "Hombre")),
    prueba_z("Theil - Mujer",
             est(theil_grupo22, "Mujer"), ee(theil_grupo22, "Mujer"),
             est(theil_grupo24, "Mujer"), ee(theil_grupo24, "Mujer")),
    prueba_z("Theil - Total",
             est(theil_tot22, 1), ee(theil_tot22, 1), est(theil_tot24, 1), ee(theil_tot24, 1)))
  todo_cambios <- rbind(todo_cambios, data.frame(AMBITO = amb, cuadro_cambios))


  #############################################################################
  # 7. CÓMO SE CONFORMA EL INGRESO POR FUENTE
  #############################################################################
  comp22 <- composicion(d22)
  comp24 <- composicion(d24)
  todo_composicion <- rbind(todo_composicion,
    data.frame(AMBITO = amb, ANIO = 2022, comp22),
    data.frame(AMBITO = amb, ANIO = 2024, comp24))

  # ---- 7.2 Ingreso medio per cápita de cada fuente y brecha Mujer/Hombre ----
  # Las cinco medias suman el ingreso medio per cápita del paso 3.
  for (k in 1:5) {
    variable <- paste0(fuentes_ingreso[k], "_PC")
    todo_brecha_fuente <- rbind(todo_brecha_fuente,
      data.frame(AMBITO = amb, ANIO = 2022, FUENTE = nombres_fuente[k], brecha_MH(d22, variable)),
      data.frame(AMBITO = amb, ANIO = 2024, FUENTE = nombres_fuente[k], brecha_MH(d24, variable)))
  }

  # ---- 7.3 Composición del ingreso por decil nacional y jefatura ----
  todo_comp_decil <- rbind(todo_comp_decil,
    data.frame(AMBITO = amb, ANIO = 2022, composicion_decil(b22)),
    data.frame(AMBITO = amb, ANIO = 2024, composicion_decil(b24)))


  #############################################################################
  # 8. QUÉ PASA CON Y SIN CADA FUENTE DEL INGRESO
  #############################################################################
  # Ejercicio "contable": se quita una fuente del ingreso de los hogares y se
  # vuelven a calcular la brecha y la desigualdad, suponiendo que todo lo demás
  # quedaría igual. Escenarios: ingreso completo y sin cada una de las cinco fuentes.

  for (e in 1:6) {
    variable <- variables_esc[e]

    # ---- 8.1 Brecha (razón Mujer/Hombre) y Gini ----
    con_sin22 <- cbind(brecha_MH(d22, variable)[, c("MEDIA_HOMBRE", "MEDIA_MUJER",
                         "RAZON_MUJER_HOMBRE", "CV_RAZON")], gini_escenario(d22, variable))
    con_sin24 <- cbind(brecha_MH(d24, variable)[, c("MEDIA_HOMBRE", "MEDIA_MUJER",
                         "RAZON_MUJER_HOMBRE", "CV_RAZON")], gini_escenario(d24, variable))
    todo_con_sin <- rbind(todo_con_sin,
      data.frame(AMBITO = amb, ANIO = 2022, ESCENARIO = escenarios[e], con_sin22),
      data.frame(AMBITO = amb, ANIO = 2024, ESCENARIO = escenarios[e], con_sin24))

    # ---- 8.2 Theil y GE(0) dentro y entre grupos ----
    # Ojo: ineqdeco() deja fuera a los hogares con ingreso 0; al quitar una
    # fuente hay más hogares en cero (columna HOGARES_EXCLUIDOS, en la muestra).
    t22 <- ineqdeco(y = b22[[variable]], w = b22$factor, g = b22$JEFATURA)
    t24 <- ineqdeco(y = b24[[variable]], w = b24$factor, g = b24$JEFATURA)
    todo_theil_con_sin <- rbind(todo_theil_con_sin, data.frame(
      AMBITO    = amb,
      ANIO      = rep(c(2022, 2024), each = 2),
      ESCENARIO = escenarios[e],
      INDICE    = rep(c("Theil GE(1)", "GE(0)"), 2),
      TOTAL     = c(t22$total[["ge1"]],   t22$total[["ge0"]],   t24$total[["ge1"]],   t24$total[["ge0"]]),
      DESIGUALDAD_DENTRO = c(t22$within[["ge1"]],  t22$within[["ge0"]],  t24$within[["ge1"]],  t24$within[["ge0"]]),
      DESIGUALDAD_ENTRE  = c(t22$between[["ge1"]], t22$between[["ge0"]], t24$between[["ge1"]], t24$between[["ge0"]]),
      HOGARES_EXCLUIDOS  = rep(c(t22$excluidas, t24$excluidas), each = 2)))
  }

} # fin del ciclo de ámbitos


###############################################################################
# RESULTADOS: cada cuadro junta Nacional, Urbano y Rural
# (se muestran en la consola y en el Viewer, y se guardan en HTML y CSV)
###############################################################################

# Nombres de fuente y escenario en orden (para cuadros y gráficas)
todo_composicion$FUENTE    <- factor(todo_composicion$FUENTE, levels = nombres_fuente)
todo_brecha_fuente$FUENTE  <- factor(todo_brecha_fuente$FUENTE, levels = nombres_fuente)
todo_con_sin$ESCENARIO     <- factor(todo_con_sin$ESCENARIO, levels = escenarios)
todo_theil_con_sin$ESCENARIO <- factor(todo_theil_con_sin$ESCENARIO, levels = escenarios)

# Precisión (criterio INEGI) de las razones y de los porcentajes
todo_brecha_fuente$PRECISION_RAZON <- precision(todo_brecha_fuente$CV_RAZON)
todo_con_sin$PRECISION_RAZON       <- precision(todo_con_sin$CV_RAZON)

# Porcentajes de la descomposición Theil por escenario
todo_theil_con_sin$PCT_DENTRO <- 100 * todo_theil_con_sin$DESIGUALDAD_DENTRO / todo_theil_con_sin$TOTAL
todo_theil_con_sin$PCT_ENTRE  <- 100 * todo_theil_con_sin$DESIGUALDAD_ENTRE / todo_theil_con_sin$TOTAL

# ---- Paso 1 ----
mostrar(todo_tamano, "Cuadro 1. Tamaño del grupo: hogares y personas por sexo de la jefatura", "1_tamano", decimales = 1)
mostrar(todo_precision, "Cuadro 2. Precisión de las estimaciones (CV en %, criterio INEGI)", "1_precision", decimales = 2)

# ---- Paso 2 ----
mostrar(todo_ubicacion, "Cuadro 3. Hogares con jefatura femenina por decil nacional de ingreso per cápita (%)", "2_ubicacion", decimales = 2)

# ---- Paso 3 ----
mostrar(todo_brecha, "Cuadro 4. La brecha de ingreso per cápita Mujer/Hombre en cada año", "3_brecha", decimales = 3)

# ---- Paso 4 ----
mostrar(todo_descomposicion, "Cuadro 5. Descomposición de la desigualdad dentro y entre grupos", "4_descomposicion", decimales = 4)
mostrar(todo_grupos, "Cuadro 6. Peso de cada grupo en la población y en el ingreso", "4_grupos", decimales = 3)

# ---- Paso 5 ----
mostrar(todo_desigualdad, "Cuadro 7. Gini y Theil del ingreso per cápita por jefatura", "5_desigualdad", decimales = 3)
mostrar(todo_lorenz, "Cuadro 8. Curvas de Lorenz por jefatura (participaciones acumuladas)", "5_lorenz", decimales = 3)

# ---- Paso 6 ----
mostrar(todo_cambios, "Cuadro 9. Cambio 2022-2024 y prueba de significancia (z)", "6_cambios", decimales = 3)

# ---- Paso 7 ----
mostrar(todo_composicion, "Cuadro 10. Composición del ingreso corriente por fuente y sexo de la jefatura (% del ingreso total)", "7_1_composicion", decimales = 2)
mostrar(todo_brecha_fuente, "Cuadro 11. Ingreso medio per cápita por fuente y brecha Mujer/Hombre", "7_2_brecha_por_fuente", decimales = 3)
mostrar(todo_comp_decil, "Cuadro 12. Composición del ingreso corriente por fuente, decil nacional y sexo de la jefatura (%)", "7_3_composicion_por_decil", decimales = 2)

# ---- Paso 8 ----
mostrar(todo_con_sin, "Cuadro 13. Brecha Mujer/Hombre y Gini con el ingreso completo y sin cada fuente", "8_1_con_sin_fuente", decimales = 3)
mostrar(todo_theil_con_sin, "Cuadro 14. Theil y GE(0) dentro y entre grupos, con el ingreso completo y sin cada fuente", "8_2_theil_con_sin_fuente", decimales = 4)


###############################################################################
# GUARDAR CUADROS (CSV, para abrir en Excel; las versiones HTML ya se guardaron con mostrar())
###############################################################################
write.csv(todo_tamano,         "resultados/cuadros/1_tamano.csv",                row.names = FALSE)
write.csv(todo_precision,      "resultados/cuadros/1_precision.csv",             row.names = FALSE)
write.csv(todo_ubicacion,      "resultados/cuadros/2_ubicacion.csv",             row.names = FALSE)
write.csv(todo_brecha,         "resultados/cuadros/3_brecha.csv",                row.names = FALSE)
write.csv(todo_descomposicion, "resultados/cuadros/4_descomposicion.csv",        row.names = FALSE)
write.csv(todo_grupos,         "resultados/cuadros/4_grupos.csv",                row.names = FALSE)
write.csv(todo_desigualdad,    "resultados/cuadros/5_desigualdad.csv",           row.names = FALSE)
write.csv(todo_lorenz,         "resultados/cuadros/5_lorenz.csv",                row.names = FALSE)
write.csv(todo_cambios,        "resultados/cuadros/6_cambios.csv",               row.names = FALSE)
write.csv(todo_composicion,    "resultados/cuadros/7_1_composicion.csv",         row.names = FALSE)
write.csv(todo_brecha_fuente,  "resultados/cuadros/7_2_brecha_por_fuente.csv",   row.names = FALSE)
write.csv(todo_comp_decil,     "resultados/cuadros/7_3_composicion_por_decil.csv", row.names = FALSE)
write.csv(todo_con_sin,        "resultados/cuadros/8_1_con_sin_fuente.csv",      row.names = FALSE)
write.csv(todo_theil_con_sin,  "resultados/cuadros/8_2_theil_con_sin_fuente.csv", row.names = FALSE)


###############################################################################
# GRÁFICAS (se muestran en Plots y se guardan como PNG)
# Cada gráfica tiene paneles por ámbito: Nacional, Urbano y Rural.
###############################################################################

# Para que los paneles salgan en este orden: Nacional, Urbano, Rural
orden_amb <- function(x) factor(x, levels = ambitos)

# ---- F1. Ingreso medio per cápita por jefatura y año, con IC 95 % ----
datos_f1 <- subset(todo_precision, INDICADOR == "Ingreso medio per cápita")
datos_f1$EE <- datos_f1$ESTIMACION * datos_f1$CV / 100 # error estándar = media x CV

f1 <- ggplot(datos_f1, aes(x = factor(ANIO), y = ESTIMACION, fill = JEFATURA)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ESTIMACION - 1.96 * EE, ymax = ESTIMACION + 1.96 * EE),
                position = position_dodge(0.8), width = 0.2) +
  facet_wrap(~orden_amb(AMBITO)) +
  scale_fill_manual(values = col_jef) +
  labs(x = NULL, y = "Ingreso trimestral per cápita (pesos de 2024)", fill = "Jefatura",
       title = "Ingreso medio per cápita por sexo de la jefatura", caption = pie_fuente) +
  theme_classic()
print(f1) # se muestra en el panel Plots
ggsave("resultados/figuras/F1_media_por_jefatura.png", f1, width = 10, height = 5, dpi = 300)

# ---- F2. % de hogares con jefatura femenina por decil nacional ----
datos_f2 <- data.frame(
  AMBITO = rep(todo_ubicacion$AMBITO, 2),
  ANIO   = rep(c("2022", "2024"), each = nrow(todo_ubicacion)),
  DECIL  = rep(todo_ubicacion$DECIL, 2),
  PCT    = c(todo_ubicacion$PCT_HOGARES_JEFA_2022, todo_ubicacion$PCT_HOGARES_JEFA_2024))

f2 <- ggplot(datos_f2, aes(x = factor(DECIL), y = PCT, fill = ANIO)) +
  geom_col(position = "dodge") +
  facet_wrap(~orden_amb(AMBITO), ncol = 1) +
  scale_fill_manual(values = col_anio) +
  labs(x = "Decil nacional de ingreso per cápita", y = "% de hogares con jefatura femenina",
       fill = NULL, title = "Hogares con jefatura femenina en cada decil", caption = pie_fuente) +
  theme_classic()
print(f2) # se muestra en el panel Plots
ggsave("resultados/figuras/F2_jefatura_femenina_por_decil.png", f2, width = 8, height = 9, dpi = 300)

# ---- F3. Curvas de Lorenz por jefatura, año y ámbito ----
f3 <- ggplot(todo_lorenz, aes(x = POBLACION_ACUMULADA, y = INGRESO_ACUMULADO, color = JEFATURA)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  geom_line() +
  geom_point(size = 1) +
  facet_grid(orden_amb(AMBITO) ~ ANIO) +
  scale_color_manual(values = col_jef) +
  labs(x = "Participación acumulada de los hogares",
       y = "Participación acumulada del ingreso", color = "Jefatura",
       title = "Curvas de Lorenz del ingreso per cápita por sexo de la jefatura",
       caption = pie_fuente) +
  theme_classic()
print(f3) # se muestra en el panel Plots
ggsave("resultados/figuras/F3_lorenz.png", f3, width = 8, height = 10, dpi = 300)

# ---- F4. La brecha: razón Mujer/Hombre del ingreso per cápita, con IC 95 % ----
f4 <- ggplot(todo_brecha, aes(x = factor(ANIO), y = RAZON_MUJER_HOMBRE, fill = factor(ANIO))) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = RAZON_MUJER_HOMBRE - 1.96 * ERROR_ESTANDAR_RAZON,
                    ymax = RAZON_MUJER_HOMBRE + 1.96 * ERROR_ESTANDAR_RAZON),
                width = 0.15) +
  geom_hline(yintercept = 1, linetype = "dashed") +   # 1 = sin brecha
  geom_text(aes(label = round(RAZON_MUJER_HOMBRE, 3)), vjust = -2.5, size = 3.5) + # valor arriba de la barra
  facet_wrap(~orden_amb(AMBITO)) +
  scale_fill_manual(values = col_anio, guide = "none") +
  labs(x = NULL, y = "Razón Mujer/Hombre del ingreso per cápita",
       title = "Brecha de ingreso per cápita entre hogares con jefa y con jefe",
       subtitle = "1 = sin brecha; mayor que 1: más ingreso por persona en hogares con jefa",
       caption = pie_fuente) +
  coord_cartesian(ylim = c(0, 1.25)) +
  theme_classic()
print(f4) # se muestra en el panel Plots
ggsave("resultados/figuras/F4_razon_mujer_hombre.png", f4, width = 10, height = 5, dpi = 300)

# ---- F5. Número promedio de integrantes por hogar, con IC 95 % ----
f5 <- ggplot(todo_tamano, aes(x = factor(ANIO), y = INTEGRANTES_PROMEDIO, fill = JEFATURA)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = INTEGRANTES_PROMEDIO - 1.96 * ERROR_ESTANDAR_INTEGRANTES,
                    ymax = INTEGRANTES_PROMEDIO + 1.96 * ERROR_ESTANDAR_INTEGRANTES),
                position = position_dodge(0.8), width = 0.2) +
  geom_text(aes(label = round(INTEGRANTES_PROMEDIO, 2)),
            position = position_dodge(0.8), vjust = -1.2, size = 3.2) +
  facet_wrap(~orden_amb(AMBITO)) +
  scale_fill_manual(values = col_jef) +
  labs(x = NULL, y = "Integrantes por hogar (promedio)", fill = "Jefatura",
       title = "Número promedio de integrantes por hogar según la jefatura",
       caption = pie_fuente) +
  coord_cartesian(ylim = c(0, 4.5)) +
  theme_classic()
print(f5) # se muestra en el panel Plots
ggsave("resultados/figuras/F5_integrantes_por_hogar.png", f5, width = 10, height = 5, dpi = 300)

# ---- F6. Composición del ingreso por fuente, jefatura, año y ámbito ----
datos_f6 <- subset(todo_composicion, JEFATURA != "Total")

f6 <- ggplot(datos_f6, aes(x = JEFATURA, y = PCT_DEL_INGRESO, fill = FUENTE)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(PCT_DEL_INGRESO >= 4, round(PCT_DEL_INGRESO, 0), "")),
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  facet_grid(orden_amb(AMBITO) ~ ANIO) +
  scale_fill_manual(values = col_fuente) +
  labs(x = NULL, y = "% del ingreso corriente total", fill = "Fuente del ingreso",
       title = "Composición del ingreso corriente por fuente y sexo de la jefatura",
       caption = pie_fuente) +
  theme_classic()
print(f6) # se muestra en el panel Plots
ggsave("resultados/figuras/F6_composicion_por_fuente.png", f6, width = 9, height = 10, dpi = 300)

# ---- F7. Composición del ingreso por fuente en cada decil nacional ----
# (pasar el Cuadro 12 de columnas a filas: una fila por decil, jefatura y fuente)
datos_f7 <- NULL
for (k in 1:5) {
  datos_f7 <- rbind(datos_f7, data.frame(
    todo_comp_decil[, c("AMBITO", "ANIO", "DECIL", "JEFATURA")],
    FUENTE = nombres_fuente[k],
    PCT    = todo_comp_decil[[paste0("PCT_", fuentes_ingreso[k])]]))
}
datos_f7$FUENTE <- factor(datos_f7$FUENTE, levels = nombres_fuente)

f7 <- ggplot(datos_f7, aes(x = factor(DECIL), y = PCT, fill = FUENTE)) +
  geom_col(width = 0.85) +
  facet_grid(orden_amb(AMBITO) ~ ANIO + JEFATURA) +
  scale_fill_manual(values = col_fuente) +
  labs(x = "Decil nacional de ingreso per cápita", y = "% del ingreso corriente del decil",
       fill = "Fuente del ingreso",
       title = "Composición del ingreso por fuente en cada decil, según la jefatura",
       caption = pie_fuente) +
  theme_classic() +
  theme(legend.position = "bottom")
print(f7) # se muestra en el panel Plots
ggsave("resultados/figuras/F7_composicion_por_decil.png", f7, width = 16, height = 9, dpi = 300)

# ---- F8. Brecha Mujer/Hombre en el ingreso de cada fuente, con IC 95 % ----
datos_f8 <- todo_brecha_fuente
datos_f8$EE <- datos_f8$RAZON_MUJER_HOMBRE * datos_f8$CV_RAZON / 100 # error estándar = razón x CV

f8 <- ggplot(datos_f8, aes(x = FUENTE, y = RAZON_MUJER_HOMBRE, fill = factor(ANIO))) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = RAZON_MUJER_HOMBRE - 1.96 * EE, ymax = RAZON_MUJER_HOMBRE + 1.96 * EE),
                position = position_dodge(0.8), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +   # 1 = sin brecha
  facet_wrap(~orden_amb(AMBITO), ncol = 1, scales = "free_y") +
  scale_fill_manual(values = col_anio) +
  scale_x_discrete(labels = c("Trabajo", "Rentas", "Transferencias",
                              "Estimación\ndel alquiler", "Otros\ningresos")) +
  labs(x = NULL, y = "Razón Mujer/Hombre del ingreso per cápita de la fuente", fill = NULL,
       title = "Brecha Mujer/Hombre por fuente de ingreso",
       subtitle = "1 = sin brecha", caption = pie_fuente) +
  theme_classic()
print(f8) # se muestra en el panel Plots
ggsave("resultados/figuras/F8_brecha_por_fuente.png", f8, width = 9, height = 10, dpi = 300)

# ---- F9. Razón Mujer/Hombre con el ingreso completo y sin cada fuente ----
datos_f9 <- todo_con_sin
datos_f9$EE <- datos_f9$RAZON_MUJER_HOMBRE * datos_f9$CV_RAZON / 100

f9 <- ggplot(datos_f9, aes(x = ESCENARIO, y = RAZON_MUJER_HOMBRE, fill = factor(ANIO))) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = RAZON_MUJER_HOMBRE - 1.96 * EE, ymax = RAZON_MUJER_HOMBRE + 1.96 * EE),
                position = position_dodge(0.8), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +   # 1 = sin brecha
  facet_wrap(~orden_amb(AMBITO), ncol = 1) +
  scale_fill_manual(values = col_anio) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x, fixed = TRUE)) +
  labs(x = NULL, y = "Razón Mujer/Hombre del ingreso per cápita", fill = NULL,
       title = "Brecha Mujer/Hombre con el ingreso completo y sin cada fuente",
       subtitle = "1 = sin brecha", caption = pie_fuente) +
  theme_classic()
print(f9) # se muestra en el panel Plots
ggsave("resultados/figuras/F9_razon_con_sin_fuente.png", f9, width = 10, height = 10, dpi = 300)

# ---- F10. Gini por jefatura con el ingreso completo y sin cada fuente ----
datos_f10 <- rbind(
  data.frame(todo_con_sin[, c("AMBITO", "ANIO", "ESCENARIO")], JEFATURA = "Hombre",
             GINI = todo_con_sin$GINI_HOMBRE),
  data.frame(todo_con_sin[, c("AMBITO", "ANIO", "ESCENARIO")], JEFATURA = "Mujer",
             GINI = todo_con_sin$GINI_MUJER))

f10 <- ggplot(datos_f10, aes(x = ESCENARIO, y = GINI, fill = JEFATURA)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  facet_grid(orden_amb(AMBITO) ~ ANIO) +
  scale_fill_manual(values = col_jef) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x, fixed = TRUE)) +
  labs(x = NULL, y = "Gini del ingreso per cápita", fill = "Jefatura",
       title = "Gini con el ingreso completo y sin cada fuente, por sexo de la jefatura",
       caption = pie_fuente) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7))
print(f10) # se muestra en el panel Plots
ggsave("resultados/figuras/F10_gini_con_sin_fuente.png", f10, width = 12, height = 9, dpi = 300)

# ---- F11. Theil: % de la desigualdad que está ENTRE jefas y jefes ----
datos_f11 <- subset(todo_theil_con_sin, INDICE == "Theil GE(1)")

f11 <- ggplot(datos_f11, aes(x = ESCENARIO, y = PCT_ENTRE, fill = factor(ANIO))) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~orden_amb(AMBITO), ncol = 1) +
  scale_fill_manual(values = col_anio) +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x, fixed = TRUE)) +
  labs(x = NULL, y = "% del Theil que está entre jefas y jefes", fill = NULL,
       title = "Desigualdad (Theil) entre grupos con el ingreso completo y sin cada fuente",
       caption = pie_fuente) +
  theme_classic()
print(f11) # se muestra en el panel Plots
ggsave("resultados/figuras/F11_theil_entre_con_sin_fuente.png", f11, width = 10, height = 10, dpi = 300)

