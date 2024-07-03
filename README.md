# Emission_Model_Waste-HN2024

################################ Subiendo los datos reales de residuos #######################################################################################

library(dplyr)
library(ggplot2)
library(readxl)

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
  select(ANO, DDOC) %>%
  filter(!is.na(DDOC))

# Calcular el valor de k usando los datos proporcionados
k_calculado <- calcular_k(data)

# Verificar el valor de k
print(paste("Valor calculado de k:", k_calculado))

# Proyectar las trayectorias de emisiones usando el valor de k calculado
years <- 2000:2050
Waste_Deposited_Tons <- seq(50000, 75000, length.out = length(years))  # Ajustar a un crecimiento más moderado
DOC <- runif(length(years), 15, 25) / 100
DOCf <- runif(length(years), 0.5, 0.75)
MCF <- runif(length(years), 0.8, 1.0)
Recovered_CH4_Tons <- seq(1000, 5000, length.out = length(years))
OX <- runif(length(years), 0.1, 0.25)

# Calcular las variables derivadas
DDOCmd <- Waste_Deposited_Tons * DOC * DOCf * MCF

# Inicializar variables para las emisiones
DDOCma <- numeric(length(years))
DDOCmdecomp <- numeric(length(years))
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

# Crear un dataframe con los resultados
data_proyectada <- data.frame(
  Year = years,
  Waste_Deposited_Tons = Waste_Deposited_Tons,
  DOC = DOC,
  DOCf = DOCf,
  MCF = MCF,
  k = k_calculado,
  Recovered_CH4_Tons = Recovered_CH4_Tons,
  OX = OX,
  CH4_Emissions_BAU = CH4_emitted
)

# Definir factores de reducción para diferentes tecnologías, comenzando en 2025
reduction_factor_biogas_ambicioso <- 0.9
reduction_factor_lagunas_ambicioso <- 0.7
reduction_factor_lodos_ambicioso <- 0.5

reduction_factor_biogas_moderado <- 0.5
reduction_factor_lagunas_moderado <- 0.3
reduction_factor_lodos_moderado <- 0.2

# Calcular las emisiones ajustadas para los escenarios
data_proyectada <- data_proyectada %>%
  mutate(
    CH4_Emissions_Ambicioso = ifelse(Year >= 2025 & Year <= 2030, 
                                     CH4_Emissions_BAU * (1 - reduction_factor_biogas_ambicioso) * (1 - reduction_factor_lagunas_ambicioso) * (1 - reduction_factor_lodos_ambicioso) * (1 - (2030 - Year) / (2030 - 2025)), 
                                     ifelse(Year > 2030, 0, CH4_Emissions_BAU)),
    CH4_Emissions_Moderado = ifelse(Year >= 2025 & Year <= 2050, 
                                    CH4_Emissions_BAU * (1 - reduction_factor_biogas_moderado) * (1 - reduction_factor_lagunas_moderado) * (1 - reduction_factor_lodos_moderado) * (1 - (2050 - Year) / (2050 - 2025)), 
                                    ifelse(Year > 2050, 0, CH4_Emissions_BAU))
  )

# Visualizar los resultados
ggplot(data_proyectada, aes(x = Year)) +
  geom_line(aes(y = CH4_Emissions_BAU, color = "Emisiones BAU"), size = 1) +
  geom_line(aes(y = CH4_Emissions_Ambicioso, color = "Emisiones Ambicioso"), size = 1, linetype = "dashed") +
  geom_line(aes(y = CH4_Emissions_Moderado, color = "Emisiones Moderado"), size = 1, linetype = "dotted") +
  labs(title = "Trayectorias de Emisión de Metano del sector Residuos",
       x = "Año",
       y = "Toneladas de CH4 Emitidas",
       color = "Escenarios") +
  theme_minimal() +
  scale_color_manual(values = c("Emisiones BAU" = "red", 
                                "Emisiones Ambicioso" = "blue",
                                "Emisiones Moderado" = "green"))
