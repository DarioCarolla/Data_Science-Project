library(readxl)
library(triangle)
library(dplyr)
library(GA)
set.seed(12345)

# Progetto Decision Models
# Dario Carolla mat. 807547
# Matteo Licciardello mat. 799368

setwd("~/Desktop/Progetto Decision Models/Carolla_Dario_807547_Licciardello_Matteo_799368/")

# Importazione e creazione dei dati
source("data.R")

# Visualizzazione dei dati
source("Visualization.R")

# FirstFit 
source("firstfit.R")

# Genetic Algorithm
source("GA.R")

#Visualizzazione risultati
source("Visualization_result.R")
