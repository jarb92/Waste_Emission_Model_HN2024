# Emission_Model_Waste-HN2024

################################ Subiendo los datos reales de residuos #######################################################################################

##### Code Residuos ########


## Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)

# Datos específicos para Honduras de acuerdo al informe del INE
indice_generacion_rsu_dia <- 0.65  # kg/hab/día   # https://ine.gob.hn/v4/2022/12/15/gestion-integral-de-los-residuos-solidos-en-honduras/#:~:text=Estudios%20indican%20que%20la%20generaci%C3%B3n,%2Fpor%20persona%2Fal%20d%C3%ADa.
indice_generacion_rsu_anual <- indice_generacion_rsu_dia * 365  # Convertir a kg/hab/año

fraccion_residuos_organicos <- 0.44  # Fuente ajustada según los datos disponibles

fraccion_residuos_vertederos <- 0.599  # Fuente: "Informe de la Evaluación Regional del Manejo de Residuos Sólidos Urbanos en ALC 2010"

# Calcular el DOC para Honduras
DOC <- indice_generacion_rsu_anual * fraccion_residuos_organicos * fraccion_residuos_vertederos
print(paste("Valor calculado de DOC para Honduras:", DOC, "kg/hab/año"))

# Convertir el DOC a toneladas por habitante por año
DOC_ton <- DOC / 1000

# Función para calcular k usando un modelo de ajuste
calcular_k <- function(data) {
  if (any(is.na(data$DDOC)) || any(is.infinite(data$DDOC))) {
    stop("Datos contienen valores faltantes o infinitos.")
  }
  modelo <- nls(DDOC ~ DDOC0 * exp(-k * (ANO - min(ANO))), data = data, start = list(DDOC0 = max(data$DDOC, na.rm = TRUE), k = 0.1))
  abs(coef(modelo)["k"])  # Tomar el valor absoluto de k
}

# Cargar el archivo con los datos de DDOC (CH4e + CO2e) desde la hoja "Residuos"
file_path <- "C:/Users/DEES-JULIO/Desktop/GIZ/Anexo 1. Consolidado INGEI 2005-2020.xlsx"
data <- read_excel(file_path, sheet = "Residuos")

# Seleccionar solo las columnas relevantes y renombrarlas
data <- data %>%
  select(ANO, DDOC, E_IeIAD, E_EDS, E_TAR) %>%
  filter(!is.na(DDOC))

# Calcular el valor de k usando los datos proporcionados
k_calculado <- calcular_k(data)

# Verificar el valor de k
print(paste("Valor calculado de k:", k_calculado))

# Proyectar las trayectorias de emisiones usando el valor de k calculado
years <- 2000:2050


# Parámetros iniciales
P0 <- 6.75e6  # Población en el año 2000 (en millones)
r <- 0.015    # Tasa de crecimiento demográfico anual
G <- indice_generacion_rsu_dia     # Generación de residuos per cápita (kg/hab/día)
dias_anio <- 365  # Número de días en un año

# Función para calcular la población en un año dado
calcular_poblacion <- function(t, P0, r) {
  return(P0 * (1 + r)^(t - 2000))
}

# Función para calcular los residuos generados en un año dado (en toneladas)
calcular_residuos <- function(t, P0, r, G, dias_anio) {
  poblacion <- calcular_poblacion(t, P0, r)
  residuos <- poblacion * G * dias_anio / 1000  # Convertir a toneladas
  return(residuos)
}

# Ejemplo: Calcular la cantidad de residuos generados entre 2000 y 2050
years <- 2000:2050
residuos_generados <- sapply(years, calcular_residuos, P0 = P0, r = r, G = G, dias_anio = dias_anio)

  
  
Waste_Deposited_Tons <- residuos_generados  # Ajustar a un crecimiento más moderado
DOCf <- rep(DOC_ton, length(years))  # Es la fracción del carbono organico degradable
MCF <- runif(length(years), 0.4, 0.7) # Methane Correction Factor **** Tengo muchas dudas aquí,  Leí el capitulo 6 de tratamiento y eliminación de aguas residuales
Recovered_CH4_Tons <- seq(-10, length.out = length(years)) # Cantidad de metano (CH4) capturada y recuperada en toneladas 
OX <- runif(length(years), 0.1, 0.25) # Oxidation Factor

# Variables para nuevas tecnologías
FBT <- 0.30  # Fracción de residuos tratados biológicamente 
EBT <- 0.30  # Eficiencia de reducción del tratamiento biológico  
FI <- 0.10   # Fracción de residuos incinerados 
CO2I <- 2.0  # Emisiones de CO2 de la incineración (toneladas de CO2 por tonelada de residuos) 2.0

# Calcular las variables derivadas
DDOCmd <- Waste_Deposited_Tons * DOCf * MCF # cantidad de Carbono Orgánico Degradable

# Inicializar variables para las emisiones
DDOCma <- numeric(length(years)) #Es la masa de carbono orgánico degradable depositado en el año i 
DDOCmdecomp <- numeric(length(years)) #Es la cantidad de Carbono Orgánico Degradable que se descompone en un año específico
CH4_generated <- numeric(length(years))
CH4_emitted <- numeric(length(years))

# Computar el modelo año por año usando el k calculado
for (i in 1:length(years)) {
  if (i == 1) {
    DDOCma[i] <- DDOCmd[i]
    DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k_calculado * (years[i] - years[1])))
  } else {
    DDOCma[i] <- DDOCmd[i] + DDOCma[i - 1] * exp(-k_calculado * (years[i] - years[i - 1]))
    DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k_calculado * (years[i] - years[i - 1]))) + DDOCma[i - 1] * (1 - exp(-k_calculado * (years[i] - years[i - 1])))
  }
  CH4_generated[i] <- DDOCmdecomp[i] * 16/12
  CH4_emitted[i] <- (CH4_generated[i] - Recovered_CH4_Tons[i]) * (1 - OX[i])
  
  if (CH4_emitted[i] < 0) {
    CH4_emitted[i] <- 0
  }
}

# Ajustar las emisiones para las nuevas tecnologías
CH4_emitted_adjusted <- CH4_emitted * (1 - (FBT * EBT))  # Ajuste por tratamiento biológico

# Definir fracciones de destino de residuos Fuente: "Informe de la Evaluación Regional del Manejo de Residuos Sólidos Urbanos en ALC 2010"
fraccion_relleno_sanitario <- 0.113  
fraccion_vertedero_controlado <- 0.599  
fraccion_vertedero_cielo_abierto <- 0.15
fraccion_quema_cielo_abierto <- 0.138  
fraccion_otras_formas <- 0

# Ajustar las emisiones según el destino
CH4_emitted_relleno_sanitario <- CH4_emitted_adjusted * fraccion_relleno_sanitario
CH4_emitted_vertedero_controlado <- CH4_emitted_adjusted * fraccion_vertedero_controlado
CH4_emitted_vertedero_cielo_abierto <- CH4_emitted_adjusted * fraccion_vertedero_cielo_abierto
CH4_emitted_quema_cielo_abierto <- CH4_emitted_adjusted * fraccion_quema_cielo_abierto
CH4_emitted_otras_formas <- CH4_emitted_adjusted * fraccion_otras_formas

# Calcular las emisiones totales ajustadas
CH4_emitted_total <- CH4_emitted_relleno_sanitario + CH4_emitted_vertedero_controlado + CH4_emitted_vertedero_cielo_abierto + CH4_emitted_quema_cielo_abierto + CH4_emitted_otras_formas

# Calcular las fracciones de emisiones de cada tecnología
fraction_solid_waste <- 0.09 # Hay que validarlo
fraction_incineration <- 0.15 # Hay que validarlo
fraction_wastewater <- 1 - (fraction_solid_waste + fraction_incineration)

# Emisiones de CO2, CH4 y N2O por tecnología
CO2_emissions_incineration <- Waste_Deposited_Tons * CO2I * fraction_incineration

# CH4e total
CH4e_total <- CH4_emitted_total * fraction_wastewater + CH4_emitted_total * fraction_solid_waste

# N2Oe total (asumiendo factores de emisión específicos para incineración y tratamiento de aguas)
N2O_emission_factor_incineration <- 0.1  # Emisiones de N2O por tonelada incinerada
N2O_emission_factor_wastewater <- 0.05   # Emisiones de N2O por tonelada tratada de aguas residuales

N2O_emissions_incineration <- Waste_Deposited_Tons * N2O_emission_factor_incineration * fraction_incineration
N2O_emissions_wastewater <- Waste_Deposited_Tons * N2O_emission_factor_wastewater * fraction_wastewater

N2Oe_total <- N2O_emissions_incineration + N2O_emissions_wastewater

BAU <- CH4e_total + N2Oe_total

# Crear un dataframe con los resultados
data_proyectada <- data.frame(
  Year = years,
  Waste_Deposited_Tons = Waste_Deposited_Tons,
  DOC = DOCf,
  MCF = MCF,
  k = k_calculado,
  Recovered_CH4_Tons = Recovered_CH4_Tons,
  OX = OX,
  CO2_Emissions_Incineration = CO2_emissions_incineration,
  N2O_Emissions_Incineration = N2O_emissions_incineration,
  N2O_Emissions_Wastewater = N2O_emissions_wastewater,
  Total_CH4e = CH4e_total,
  Total_N2Oe = N2Oe_total,
  BAU = BAU
)

# Definir factores de reducción para diferentes tecnologías, comenzando en 2025
reduction_factor_biogas <- 0.9
reduction_factor_lagunas <- 0.9
reduction_factor_lodos <- 0.9
otro_factor <- 0.9

ambicioso <- reduction_factor_biogas + reduction_factor_lagunas + reduction_factor_lodos + otro_factor
moderado <- reduction_factor_biogas

# Ajustar las proyecciones de emisiones
data_proyectada <- data_proyectada %>%
  mutate(
    CH4_Emissions_Ambicioso = ifelse(Year >= 2025, 
                                     CH4e_total * (1 - ambicioso * (Year - 2025) / (2050 - 2025)), 
                                     CH4e_total),
    CH4_Emissions_Moderado = ifelse(Year >= 2025, 
                                    CH4e_total * (1 - moderado * (Year - 2025) / (2050 - 2025)), 
                                    CH4e_total)
  )

# Calcular las emisiones totales ajustadas para los escenarios
data_proyectada <- data_proyectada %>%
  mutate(
    Total_Emissions_BAU = CO2_Emissions_Incineration + CH4e_total + N2O_Emissions_Incineration + N2O_Emissions_Wastewater,
    Total_Emissions_Ambicioso = CO2_Emissions_Incineration + CH4_Emissions_Ambicioso + N2O_Emissions_Incineration + N2O_Emissions_Wastewater,
    Total_Emissions_Moderado = CO2_Emissions_Incineration + CH4_Emissions_Moderado + N2O_Emissions_Incineration + N2O_Emissions_Wastewater
  )

# Visualizar los resultados ajustados
ggplot(data_proyectada, aes(x = Year)) +
  geom_line(aes(y = Total_Emissions_BAU, color = "Emisiones Totales BAU"), size = 1) +
  geom_line(aes(y = Total_Emissions_Ambicioso, color = "Emisiones Totales Ambicioso"), size = 1, linetype = "dashed") +
  geom_line(aes(y = Total_Emissions_Moderado, color = "Emisiones Totales Moderado"), size = 1, linetype = "dotted") +
  labs(title = "Trayectorias de Emisión de Gases de Efecto Invernadero en la Gestión de Residuos (2000-2050)",
       x = "Año",
       y = "Toneladas de CO2e Emitidas",
       color = "Escenarios") +
  theme_minimal() +
  scale_color_manual(values = c("Emisiones Totales BAU" = "red", 
                                "Emisiones Totales Ambicioso" = "blue",
                                "Emisiones Totales Moderado" = "green"))

  





# ################ Modulo de coste-beneficio #######################################################

# Definir los costos y beneficios (ejemplo en Lempiras)
costo_biogas <- 200000  # Costo de implementar biogás
costo_lagunas <- 150000  # Costo de implementar lagunas
costo_lodos <- 50000  # Costo de implementar gestión de lodos

beneficio_emisiones_reducidas <- function(emisiones_reducidas, precio_carbono) {
  return(emisiones_reducidas * precio_carbono)
}

precio_carbono <- 1500 # Precio del carbono en Lempiras por tonelada

# Calcular las emisiones reducidas para cada escenario
emisiones_reducidas_moderado <- data_proyectada$Total_Emissions_BAU - data_proyectada$Total_Emissions_Moderado
emisiones_reducidas_ambicioso <- data_proyectada$Total_Emissions_BAU - data_proyectada$Total_Emissions_Ambicioso

# Calcular los beneficios de las emisiones reducidas
beneficios_moderado <- beneficio_emisiones_reducidas(emisiones_reducidas_moderado, precio_carbono)
beneficios_ambicioso <- beneficio_emisiones_reducidas(emisiones_reducidas_ambicioso, precio_carbono)

# Crear un vector de costos que sean cero antes del 2025
costos_moderado <- rep(0, length(years))
costos_ambicioso <- rep(0, length(years))

costos_moderado[years >= 2025] <- costo_biogas + costo_lagunas + costo_lodos
costos_ambicioso[years >= 2025] <- costo_biogas + costo_lagunas + costo_lodos

# Calcular los beneficios netos
beneficio_neto_moderado <- beneficios_moderado - costos_moderado
beneficio_neto_ambicioso <- beneficios_ambicioso - costos_ambicioso

# Agregar los resultados al data frame de trayectorias
data_proyectada <- data_proyectada %>%
  mutate(
    Beneficio_Neto_Moderado = beneficio_neto_moderado,
    Beneficio_Neto_Ambicioso = beneficio_neto_ambicioso
  )

# Visualizar los resultados de coste-beneficio
ggplot(data_proyectada, aes(x = Year)) +
  geom_line(aes(y = Beneficio_Neto_Moderado, color = "Beneficio Neto Moderado"), size = 1) +
  geom_line(aes(y = Beneficio_Neto_Ambicioso, color = "Beneficio Neto Ambicioso"), size = 1, linetype = "dashed") +
  labs(title = "Análisis de Coste-Beneficio en la Gestión de Residuos",
       x = "Año",
       y = "Beneficio Neto (Lempiras)",
       color = "Escenarios") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Beneficio Neto Moderado" = "green", 
                                "Beneficio Neto Ambicioso" = "blue"))








##############################################################################################################################################










#### calibración ##################### 


# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)
library(minpack.lm)

# Paso 1: Cargar y Preparar los Datos
file_path <- "C:/Users/DEES-JULIO/Desktop/GIZ/Anexo 1. Consolidado INGEI 2005-2020.xlsx"
data <- read_excel(file_path, sheet = "Residuos") %>%
  select(ANO, DDOC, E_IeIAD, E_EDS, E_TAR) %>%
  filter(!is.na(DDOC))

# Calcular las emisiones históricas totales
data <- data %>%
  mutate(Emisiones_Totales = E_IeIAD + E_EDS + E_TAR)

# Paso 2: Ajuste de Parámetros y Proyección de Emisiones
calcular_k <- function(data) {
  modelo <- nlsLM(DDOC ~ DDOC0 * exp(-k * (ANO - min(ANO))), data = data, start = list(DDOC0 = max(data$DDOC, na.rm = TRUE), k = 0.1))
  return(abs(coef(modelo)["k"]))  # Tomar el valor absoluto de k
}

k_calculado <- calcular_k(data)
DOC <- 75.92  # Ajuste de DOC basado en datos históricos
DOC_ton <- DOC / 1000

# Proyectar las trayectorias de emisiones usando el valor de k calculado
years <- 2000:2020  # Utilizar solo años históricos para validación
Waste_Deposited_Tons <- seq(500000, 600000, length.out = length(years))
DOCf <- rep(DOC_ton, length(years))
MCF <- runif(length(years), 0.8, 1.0)
Recovered_CH4_Tons <- seq(1000, 3000, length.out = length(years))
OX <- runif(length(years), 0.1, 0.25)

DDOCmd <- Waste_Deposited_Tons * DOCf * MCF
DDOCma <- numeric(length(years))
DDOCmdecomp <- numeric(length(years))
CH4_generated <- numeric(length(years))
CH4_emitted <- numeric(length(years))

for (i in 1:length(years)) {
  if (i == 1) {
    DDOCma[i] <- DDOCmd[i]
    DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k_calculado * (years[i] - years[1])))
  } else {
    DDOCma[i] <- DDOCmd[i] + DDOCma[i - 1] * exp(-k_calculado * (years[i] - years[i - 1]))
    DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k_calculado * (years[i] - years[i - 1]))) + DDOCma[i - 1] * (1 - exp(-k_calculado * (years[i] - years[i - 1])))
  }
  CH4_generated[i] <- DDOCmdecomp[i] * 16 / 12
  CH4_emitted[i] <- (CH4_generated[i] - Recovered_CH4_Tons[i]) * (1 - OX[i])
  if (CH4_emitted[i] < 0) {
    CH4_emitted[i] <- 0
  }
}

# Ajustar las emisiones para las nuevas tecnologías
FBT <- 0.30
EBT <- 0.30
FI <- 0.10
CO2I <- 2.0

CH4_emitted_adjusted <- CH4_emitted * (1 - (FBT * EBT))
fraction_solid_waste <- 0.09
fraction_incineration <- 0.15
fraction_wastewater <- 1 - (fraction_solid_waste + fraction_incineration)

CO2_emissions_incineration <- Waste_Deposited_Tons * CO2I * fraction_incineration
CH4e_total <- CH4_emitted_adjusted * fraction_wastewater + CH4_emitted_adjusted * fraction_solid_waste
N2O_emission_factor_incineration <- 0.1
N2O_emission_factor_wastewater <- 0.05

N2O_emissions_incineration <- Waste_Deposited_Tons * N2O_emission_factor_incineration * fraction_incineration
N2O_emissions_wastewater <- Waste_Deposited_Tons * N2O_emission_factor_wastewater * fraction_wastewater
N2Oe_total <- N2O_emissions_incineration + N2O_emissions_wastewater

BAU <- CH4_emitted_adjusted * fraction_solid_waste + CH4_emitted_adjusted * fraction_wastewater

# Crear un dataframe con los resultados
data_proyectada <- data.frame(
  Year = years,
  Emisiones_Proyectadas = CH4e_total + N2Oe_total + CO2_emissions_incineration,
  Emisiones_Historicas = data$Emisiones_Totales
)

# Paso 3: Comparación y Ajuste Iterativo
ggplot(data_proyectada, aes(x = Year)) +
  geom_line(aes(y = Emisiones_Historicas, color = "Emisiones Históricas"), size = 1) +
  geom_line(aes(y = Emisiones_Proyectadas, color = "Emisiones Proyectadas"), size = 1, linetype = "dashed") +
  labs(title = "Comparación de Emisiones Históricas y Proyectadas",
       x = "Año",
       y = "Emisiones de CO2e (toneladas)",
       color = "Tipo de Emisión") +
  theme_minimal() +
  scale_color_manual(values = c("Emisiones Históricas" = "red", 
                                "Emisiones Proyectadas" = "blue"))

# Calibrar el modelo ajustando los parámetros para minimizar las diferencias
ajustar_modelo <- function(par, data) {
  DOC <- par[1]
  k <- par[2]
  
  DOC_ton <- DOC / 1000
  DOCf <- rep(DOC_ton, length(years))
  DDOCmd <- Waste_Deposited_Tons * DOCf * MCF
  DDOCma <- numeric(length(years))
  DDOCmdecomp <- numeric(length(years))
  CH4_generated <- numeric(length(years))
  CH4_emitted <- numeric(length(years))
  
  for (i in 1:length(years)) {
    if (i == 1) {
      DDOCma[i] <- DDOCmd[i]
      DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k * (years[i] - years[1])))
    } else {
      DDOCma[i] <- DDOCmd[i] + DDOCma[i - 1] * exp(-k * (years[i] - years[i - 1]))
      DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k * (years[i] - years[i - 1]))) + DDOCma[i - 1] * (1 - exp(-k * (years[i] - years[i - 1])))
    }
    CH4_generated[i] <- DDOCmdecomp[i] * 16 / 12
    CH4_emitted[i] <- (CH4_generated[i] - Recovered_CH4_Tons[i]) * (1 - OX[i])
    if (CH4_emitted[i] < 0) {
      CH4_emitted[i] <- 0
    }
  }
  
  CH4_emitted_adjusted <- CH4_emitted * (1 - (FBT * EBT))
  CH4e_total <- CH4_emitted_adjusted * fraction_wastewater + CH4_emitted_adjusted * fraction_solid_waste
  Emisiones_Proyectadas <- CH4e_total + N2Oe_total + CO2_emissions_incineration
  
  return(sum((data$Emisiones_Totales - Emisiones_Proyectadas)^2))
}

# Optimizar los parámetros para minimizar las diferencias
resultados_optimizacion <- optim(par = c(DOC, k_calculado), fn = ajustar_modelo, data = data, method = "BFGS")
DOC_opt <- resultados_optimizacion$par[1]
k_opt <- resultados_optimizacion$par[2]

# Generar proyecciones con los parámetros optimizados
data_proyectada_opt <- data.frame(
  Year = years,
  Emisiones_Proyectadas = NA,
  Emisiones_Historicas = data$Emisiones_Totales
)

DOC_ton_opt <- DOC_opt / 1000
DOCf_opt <- rep(DOC_ton_opt, length(years))
DDOCmd_opt <- Waste_Deposited_Tons * DOCf_opt * MCF
DDOCma_opt <- numeric(length(years))
DDOCmdecomp_opt <- numeric(length(years))
CH4_generated_opt <- numeric(length(years))
CH4_emitted_opt <- numeric(length(years))

for (i in 1:length(years)) {
  if (i == 1) {
    DDOCma_opt[i] <- DDOCmd_opt[i]
    DDOCmdecomp_opt[i] <- DDOCmd_opt[i] * (1 - exp(-k_opt * (years[i] - years[1])))
  } else {
    DDOCma_opt[i] <- DDOCmd_opt[i] + DDOCma_opt[i - 1] * exp(-k_opt * (years[i] - years[i - 1]))
    DDOCmdecomp_opt[i] <- DDOCmd_opt[i] * (1 - exp(-k_opt * (years[i] - years[i - 1]))) + DDOCma_opt[i - 1] * (1 - exp(-k_opt * (years[i] - years[i - 1])))
  }
  CH4_generated_opt[i] <- DDOCmdecomp_opt[i] * 16 / 12
  CH4_emitted_opt[i] <- (CH4_generated_opt[i] - Recovered_CH4_Tons[i]) * (1 - OX[i])
  if (CH4_emitted_opt[i] < 0) {
    CH4_emitted_opt[i] <- 0
  }
}

CH4_emitted_adjusted_opt <- CH4_emitted_opt * (1 - (FBT * EBT))
CH4e_total_opt <- CH4_emitted_adjusted_opt * fraction_wastewater + CH4_emitted_adjusted_opt * fraction_solid_waste
Emisiones_Proyectadas_opt <- CH4e_total_opt + N2Oe_total + CO2_emissions_incineration

data_proyectada_opt$Emisiones_Proyectadas <- Emisiones_Proyectadas_opt

# Visualizar resultados optimizados
ggplot(data_proyectada_opt, aes(x = Year)) +
  geom_line(aes(y = Emisiones_Historicas, color = "Emisiones Históricas"), size = 1) +
  geom_line(aes(y = Emisiones_Proyectadas, color = "Emisiones Proyectadas"), size = 1, linetype = "dashed") +
  labs(title = "Comparación de Emisiones Históricas y Proyectadas (Optimizado)",
       x = "Año",
       y = "Emisiones de CO2e (toneladas)",
       color = "Tipo de Emisión") +
  theme_minimal() +
  scale_color_manual(values = c("Emisiones Históricas" = "red", 
                                "Emisiones Proyectadas" = "blue"))
