
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
                     ini.cov.pars = fit_visual[[1]]$cov.pars,
                     fix.nugget = FALSE,
                     nugget = fit_visual[[1]]$nugget,
                     cov.model = fit_visual[[1]]$cov.model,
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
