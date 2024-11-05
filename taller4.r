
# Instalar librerias necesarias
if(!require('pacman')) install.packages('pacman')
pacman::p_load(
    data.table, dplyr, tidyr, ggplot2, geoR
)

## Problema 1 ##

## 01. ##

# Importacion
camg_data <- geoR::camg
setDT(camg_data)

# Data.table formato
head(camg_data)