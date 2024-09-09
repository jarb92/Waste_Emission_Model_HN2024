# Emission_Model_Waste-HN2024

## Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

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

# Función para calcular k usando un modelo de ajuste
calcular_k <- function(data) {
  if (any(is.na(data$DDOC)) || any(is.infinite(data$DDOC))) {
    stop("Datos contienen valores faltantes o infinitos.")
  }
  modelo <- nls(DDOC ~ DDOC0 * exp(-k * (ANO - min(ANO))), data = data, start = list(DDOC0 = max(data$DDOC, na.rm = TRUE), k = 0.1))
  abs(coef(modelo)["k"])  # Tomar el valor absoluto de k
}

# Función para calcular emisiones de la categoría 4A (Residuos sólidos en vertederos)
calcular_emisiones_4A <- function(Waste_Deposited_Tons, DOC_ton, MCF, Recovered_CH4_Tons, OX, k_calculado) {
  DDOCmd <- Waste_Deposited_Tons * DOC_ton * MCF
  DDOCma <- numeric(length(Waste_Deposited_Tons))
  DDOCmdecomp <- numeric(length(Waste_Deposited_Tons))
  CH4_generated <- numeric(length(Waste_Deposited_Tons))
  CH4_emitted <- numeric(length(Waste_Deposited_Tons))
  
  for (i in 1:length(Waste_Deposited_Tons)) {
    if (i == 1) {
      DDOCma[i] <- DDOCmd[i]
      DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k_calculado * (i - 1)))
    } else {
      DDOCma[i] <- DDOCmd[i] + DDOCma[i - 1] * exp(-k_calculado * (i - 1))
      DDOCmdecomp[i] <- DDOCmd[i] * (1 - exp(-k_calculado * (i - 1))) + DDOCma[i - 1] * (1 - exp(-k_calculado * (i - 1)))
    }
    CH4_generated[i] <- DDOCmdecomp[i] * 16/12
    CH4_emitted[i] <- (CH4_generated[i] - Recovered_CH4_Tons[i]) * (1 - OX[i])
    
    # Manejar NA o valores negativos
    if (is.na(CH4_emitted[i]) || CH4_emitted[i] < 0) {
      CH4_emitted[i] <- 0
    }
  }
  
  return(CH4_emitted)
}

##############################################################################################################################
############################################################################################################################



# Función para calcular emisiones de la categoría 4C2 (Incineración abierta)
calcular_emisiones_4C2 <- function(Waste_Deposited_Tons, factor_emision_CO2, factor_emision_CH4, factor_emision_N2O) {
  # Calcular emisiones de CO2, CH4 y N2O
  CO2_emissions <- Waste_Deposited_Tons * factor_emision_CO2  # Factor de emisión aplicado a cada año
  CH4_emissions <- Waste_Deposited_Tons * factor_emision_CH4  # Igual con CH4
  N2O_emissions <- Waste_Deposited_Tons * factor_emision_N2O  # Igual con N2O
  
  # Convertir a CO2 equivalente usando GWP (Global Warming Potential)
  total_emissions <- CO2_emissions + (CH4_emissions * 25) + (N2O_emissions * 298)  # GWP para CH4 y N2O
  
  # Devolver un único vector de emisiones
  return(total_emissions)
}







# Función para calcular emisiones de la categoría 4D1 (Aguas residuales domésticas)
calcular_emisiones_4D1 <- function(volumen_agua_residual, factor_emision_CH4, factor_emision_N2O) {
  CH4_emissions <- volumen_agua_residual * factor_emision_CH4
  N2O_emissions <- volumen_agua_residual * factor_emision_N2O
  
  # Convertir a CO2 equivalente
  total_emissions <- (CH4_emissions * 25) + (N2O_emissions * 298)
  return(total_emissions)
}

# Función para calcular emisiones de la categoría 4D2 (Aguas residuales industriales)
calcular_emisiones_4D2 <- function(volumen_agua_residual, factor_emision_CH4, factor_emision_N2O) {
  CH4_emissions <- volumen_agua_residual * factor_emision_CH4
  N2O_emissions <- volumen_agua_residual * factor_emision_N2O
  
  # Convertir a CO2 equivalente
  total_emissions <- (CH4_emissions * 25) + (N2O_emissions * 298)
  return(total_emissions)
}

# Cargar los datos y parámetros desde el Excel
vyp <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/Residuos/datos relevantes.xlsx", sheet = "VyP_BAU")

# Datos específicos para Honduras
indice_generacion_rsu_dia <- vyp$VT1
indice_generacion_rsu_anual <- indice_generacion_rsu_dia * 365  # Convertir a kg/hab/año
fraccion_residuos_organicos <- vyp$VT2
fraccion_residuos_vertederos <- vyp$VT3

series<-read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/Residuos/datos relevantes.xlsx", sheet = "Series")


OX<-series$VT4
MFC<-series$VT5

# Calcular el DOC para Honduras
DOC <- indice_generacion_rsu_anual * fraccion_residuos_organicos * fraccion_residuos_vertederos  # Carbono Orgánico Degradable
DOC_ton <- DOC/1000

# Parámetros iniciales
P0 <- 6.75e6  # Población en el año 2000 (en millones)
r <- vyp$VT4    # Tasa de crecimiento demográfico anual
G <- indice_generacion_rsu_dia     # Generación de residuos per cápita (kg/hab/día)
dias_anio <- 365  # Número de días en un año

# Calcular residuos generados de 2000 a 2050
years <- 2000:2050
# Usar un loop o `sapply` correctamente para obtener un vector de resultados en lugar de una matriz.



Waste_Deposited_Tons <- numeric(length(years))

# Calcular los residuos generados en cada año usando un bucle
for (i in 1:length(years)) {
  Waste_Deposited_Tons[i] <- calcular_residuos(years[i], P0 = P0, r = r, G = G, dias_anio = dias_anio)
}

# Verificar la estructura del vector
print(Waste_Deposited_Tons)
str(Waste_Deposited_Tons)

Recovered_CH4_Tons<-0.01*Waste_Deposited_Tons


# Cargar datos históricos de DDOC y calcular k
data <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/Anexo 1. Consolidado INGEI 2005-2020.xlsx", sheet = "Residuos")
data <- data %>% select(ANO, DDOC, E_IeIAD, E_EDS, E_TAR) %>% filter(!is.na(DDOC))
k_calculado <- calcular_k(data)

# Definir factores de emisión para incineración y aguas residuales
factor_emision_CO2_incineracion <- 1.0  # t CO2/tonelada de residuos
factor_emision_CH4_incineracion <- 0.000005  # t CH4/tonelada de residuos
factor_emision_N2O_incineracion <- 0.00004  # t N2O/tonelada de residuos

factor_emision_CH4_aguas_domesticas <- 0.0045  # t CH4/m³ de agua residual
factor_emision_N2O_aguas_domesticas <- 0.000005  # t N2O/m³ de agua residual

factor_emision_CH4_aguas_industriales <- 0.002  # t CH4/m³ de agua residual
factor_emision_N2O_aguas_industriales <- 0.0003  # t N2O/m³ de agua residual

# Volumen estimado de aguas residuales (deberías cargar estos datos desde tus archivos)

# Cargar los datos y parámetros desde el Excel


volumen_agua_residual_domestica <- series$VTC4
volumen_agua_residual_industrial <- series$VTC4

# Asumiendo que la fracción inicial es 10.9% para el año 2007
fraccion_tratada_domesticas <- 0.109  # Puedes ajustar para otros países o para proyecciones futuras

# Si tienes proyecciones de crecimiento de la fracción tratada, puedes crear una variable que varíe con los años.
fraccion_tratada_domesticas_proyectada <- rep(fraccion_tratada_domesticas, length(years))

# Aumentar la fracción tratada para reflejar metas de tratamiento en los años futuros
for (i in 1:length(years)) {
  if (years[i] > 2025) {
    fraccion_tratada_domesticas_proyectada[i] <- fraccion_tratada_domesticas_proyectada[i - 1] + 0.005  # Incrementar lentamente
  }
}



str(volumen_agua_residual_domestica)

# Calcular las emisiones para cada categoría
Emisiones_4A <- calcular_emisiones_4A(Waste_Deposited_Tons, DOC_ton, MCF, Recovered_CH4_Tons, OX, k_calculado) /2e3
Emisiones_4C2 <- calcular_emisiones_4C2(Waste_Deposited_Tons, factor_emision_CO2_incineracion, factor_emision_CH4_incineracion, factor_emision_N2O_incineracion)/2e3
Emisiones_4D1 <- calcular_emisiones_4D1(volumen_agua_residual_domestica, factor_emision_CH4_aguas_domesticas, factor_emision_N2O_aguas_domesticas)/2e3
Emisiones_4D2 <- calcular_emisiones_4D2(volumen_agua_residual_industrial, factor_emision_CH4_aguas_industriales, factor_emision_N2O_aguas_industriales)/2e3

# Crear un dataframe con los resultados
data_proyectada <- data.frame(
  Year = years,
  Eliminación_desechos_sólidos = Emisiones_4A,
  Incineración_abierta_de_desechos = Emisiones_4C2,
  Aguas_residuales_domésticas = Emisiones_4D1,
  Aguas_residuales_industriales = Emisiones_4D2,
  volumen_agua_residual_domestica=volumen_agua_residual_domestica,
  volumen_agua_residual_domestica=volumen_agua_residual_domestica,
  Recovered_CH4_Tons=Recovered_CH4_Tons,
  Waste_Deposited_Tons=Waste_Deposited_Tons
)



años_especificos <- c(2018, 2020, 2022, 2025, 2030, 2035, 2040,2045,2050)
data_proyectada <- data_proyectada %>%
  filter(Year %in% años_especificos)

# Visualización de los resultados en una gráfica apilada
data_long <- data_proyectada %>%
  pivot_longer(cols = -Year, names_to = "Categoria", values_to = "Emisiones")

ggplot(data_long, aes(x = factor(Year), y = Emisiones, fill = Categoria)) +
  geom_bar(stat = "identity", color = "black", size = 0.3) +  # Barras apiladas con bordes
  geom_text(aes(label = round(Emisiones, 2)), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "black") +  # Etiquetas en el centro de las barras
  scale_fill_manual(values = c(
    "Eliminación_desechos_sólidos" = "orange",
    "Incineración_abierta_de_desechos" = "yellow",
    "Aguas_residuales_domésticas" = "blue",
    "Aguas_residuales_industriales" = "gray"
  )) +
  labs(
    title = "",
    x = "Año",
    y = "Emisiones Totales (Gg CO2e)",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Posicionar la leyenda abajo

# Guardar los resultados en un archivo Excel
write.xlsx(data_proyectada, "C:/Users/DEES-JULIO/Desktop/GIZ/Residuos/Proyecciones_Emisiones_Categorias.xlsx", row.names = FALSE)

