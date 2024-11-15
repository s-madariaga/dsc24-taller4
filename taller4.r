
# Instalación INLA
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# install.packages("maptools", repos = c(getOption("repos"), RSpatial = "https://rspatial.r-universe.dev"))

pacman::p_load(sp, spdep, spatialreg, rgdal, maptools, INLA, geoR)

################
## Problema 1 ##
################

## 5 ##

# Importamos los datos
data(ca20)

# Ajsute variograma
vg <- variog(ca20, max.dist = max(dist(ca20$coords)) / 2)

# Ajuste visual (gui variograma)
fit_visual <- eyefit(vg)

# Ajuste REML
model_reml <- likfit(ca20, 
                     ini.cov.pars = c(123.45, 182.39),
                     fix.nugget = FALSE,
                     nugget = 45.88,
                     cov.model = 'matern',
                     lik.method = "REML")

## Gráfco ##
grid <- expand.grid(seq(min(ca20$coords[,1]), max(ca20$coords[,1]), length.out = 100),
                    seq(min(ca20$coords[,2]), max(ca20$coords[,2]), length.out = 100))
colnames(grid) <- c("x", "y")

# Realizar kriging empírico
kriging_result <- krige.conv(ca20, 
                             locations = grid, 
                             krige = krige.control(obj.m = model_reml))

## Gráficos 2 ##
image(kriging_result, main = "Mapa de Kriging")

# Graficar la varianza del kriging
image(kriging_result, val = "variance", main = "Varianza del Kriging Estimado")


################
## Problema 2 ##
################

# Cargar todas las librerías necesarias con pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, dplyr, spdep, spatialreg, ggplot2)

## 1

# Pregunta 1: Cargar y unir datos espaciales
# Cargar el shapefile como un objeto `sf`
london_gen <- st_read("London Suicides/LDNSuicides.shp")

# Cargar los datos de suicidios y ordenar para unir con el shapefile
load("London Suicides/LondonSuicides.RData")
data_suicides <- data.frame(NAME = sort(london_gen$NAME), y = y, E = E, x1 = x1, x2 = x2)
data_suicides <- data_suicides %>%
  mutate(SMR = y / E) %>%
  arrange(NAME)

# Unir el dataframe de suicidios con el objeto espacial sf
london_gen <- london_gen %>%
  left_join(data_suicides, by = "NAME")

## 2

# Pregunta 2: Aplicar los test de Moran y Geary para la variable SMR

# Crear matriz de vecinos
london_nb <- poly2nb(london_gen)

# Convertir a una lista de pesos espaciales estandarizada por fila
london_w <- nb2listw(london_nb, style = "W")

# Calcular los tests de Moran y Geary para SMR
moran_test <- moran.test(london_gen$SMR, london_w)
geary_test <- geary.test(london_gen$SMR, london_w)

# Imprimir resultados de los tests
print(moran_test)
print(geary_test)

## 3

# Pregunta 3: Ajustar los Modelos (M1, M2 y M3) y Seleccionar el Mejor Modelo Basado en AIC

# Ajuste de modelos y cálculo de AIC


# Modelo M1 (Ruido Gaussiano)
model_M1 <- lm(SMR ~ x1 + x2 + I(x1 * x2), data = london_gen)
AIC_M1 <- AIC(model_M1)

# Modelo M2 (SAR)
model_M2 <- errorsarlm(SMR ~ x1 + x2 + I(x1 * x2), data = london_gen, listw = london_w)
AIC_M2 <- AIC(model_M2)

# Modelo M3 (CAR)
model_M3 <- spautolm(SMR ~ x1 + x2 + I(x1 * x2), data = london_gen, listw = london_w, family = "CAR")
AIC_M3 <- AIC(model_M3)

# Crear tabla de comparación de AIC con dplyr
AIC_values <- data.frame(Model = c("M1 (Gaussiano)", "M2 (SAR)", "M3 (CAR)"),
                         AIC = c(AIC_M1, AIC_M2, AIC_M3)) %>%
  arrange(AIC) %>%  # Ordenar por AIC ascendente
  mutate(Best = if_else(AIC == min(AIC), "Sí", "No"))  # Marcar el mejor modelo

# Crear la tabla de comparación de AIC con dplyr
AIC_values <- data.frame(Model = c("M1: Regresión Lineal Simple (Gaussiano)", 
"M2: Regresión espacial con erroes SAR", 
"M3: Regresión espacial con errores CAR"),
                         AIC = c(AIC_M1, AIC_M2, AIC_M3)) %>%
  arrange(AIC)

# Mostrar la tabla de AIC
print(AIC_values)

# Seleccionar el mejor modelo basado en la tabla
best_model_name <- AIC_values$Model[1]
best_model <- switch(best_model_name,
                     "M1: Regresión Lineal Simple (Gaussiano)" = model_M1,
                     "M2: Regresión espacial con erroes SAR" = model_M2,
                     "M3: Regresión espacial con errores CAR" = model_M3)

# Mostrar el resumen del mejor modelo
print(best_model)

## 4

# Pregunta 4: Ajuste del Modelo Final y Gráfica de SMR Estimado y Real

# Calcular el SMR estimado y los residuos del mejor modelo
london_gen$SMR_estimated <- fitted(best_model)
london_gen$residuals <- residuals(best_model)

# Gráfico de SMR Real y Estimado

# SMR Real
ggplot(london_gen) +
  geom_sf(aes(fill = SMR)) +
  scale_fill_viridis_c() +
  labs(title = "SMR Real") +
  theme_minimal()

# SMR Estimado
ggplot(london_gen) +
  geom_sf(aes(fill = SMR_estimated)) +
  scale_fill_viridis_c() +
  labs(title = "SMR Estimado") +
  theme_minimal()

## 5

# Pregunta 5: Tests de Moran y Geary para los Residuos y Gráfico de Residuos

# Aplicar los tests de Moran y Geary a los residuos
moran_residuals <- moran.test(london_gen$residuals, london_w)
geary_residuals <- geary.test(london_gen$residuals, london_w)

# Imprimir resultados de los tests
print(moran_residuals)
print(geary_residuals)

# Gráfico de Residuos del Modelo Final
ggplot(london_gen) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_viridis_c() +
  labs(title = "Residuos del Modelo Final") +
  theme_minimal()

# Comparación Opcional con Modelo Lineal
# Ajustar un modelo lineal estándar para comparar
model_linear <- lm(SMR ~ x1 + x2 + I(x1 * x2), data = london_gen)
london_gen$residuals_linear <- residuals(model_linear)

# Gráfico de Residuos del Modelo Lineal
ggplot(london_gen) +
  geom_sf(aes(fill = residuals_linear)) +
  scale_fill_viridis_c() +
  labs(title = "Residuos del Modelo Lineal") +
  theme_minimal()
