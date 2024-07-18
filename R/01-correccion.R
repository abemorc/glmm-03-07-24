
# Analisis de factores alatorios en las distancias recorridas mediante
# mixed-effects models


library(lme4)
library(MuMIn)
library(readr)
library(tidyverse)
library(car)
library(broom)


# Cargar datos
dfDistancias <- read_csv("Data/glmm_distances.csv",
                           col_types = cols(Fecha = col_date(format = "%d/%m/%Y"))
                  )

str(dfDistancias)
View(dfDistancias)


# limpiar datos

reemplazar <- c("na", ".")
dfDistancias <-  dfDistancias |>
  mutate(across(.cols = everything(),
                .fns = ~ifelse(.x %in% reemplazar, yes = NA, no = .x))
         ) |>
  mutate(across(.cols = HUM:DOSEL,
                .fns = ~as.numeric(.x))
         ) |>
  filter(Distrecorr != 0) |>
  rename(TEMPSUELO = TEMPAMB)

str(dfDistancias)

# VER CUANTOS VALORES FALTANTTES HAY
sapply(dfDistancias, function(x) sum(is.na(x)))



# dfDistancias1 <- na.omit(dfDistancias)
# str(dfDistancias1)

# dfDistancias2 <- dfDistancias

dfClean <-  dfDistancias |>
  select(Distrecorr,Sitio, TS, Sexo, DOSEL, Temporada, HOJA, HUM, ID) |>
  na.omit()

# df
# Distancias2limpio <- na.omit(dfDistancias2limpio)

sapply(dfClean, function(x) sum(is.na(x)))
str(dfClean)

# scale(dfDistancias2limpio[,c(3,5,7,8)])
dfClean[,c(3,5,7,8)] <- scale(dfClean[,c(3,5,7,8)])


str(dfClean)




# Modelos -----------------------------------------------------------------




# Ajustar el modelo nulo
GLMM_model2 <- glmer(Distrecorr ~ 1 + (1 | ID),
                     data = dfClean,
                     family = Gamma(link = "log"),
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                     )

summary(GLMM_model2)
Anova(GLMM_model2, type = 3)





# Ajustar el modelo con los datos limpios
GLMM_model1 <- glmer(Distrecorr ~ Sitio + TS + Sexo + DOSEL + HOJA + (1 | ID),
                     data = dfClean,
                     family = Gamma(link = "log"),
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(GLMM_model1)
Anova(GLMM_model1)



# Comparacion entre los modelos
anova(GLMM_model2, GLMM_model1)



# el pvalue es 0.694
# significa que el modelo con variables no representa una mejora en la "estimacion"
# respecto al modelo nulo, siendo este entonces mejor opción




# Buscar el mejor modelo de todas la combinaciones posibles ------------------------



backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace log
fullmodels1 <- dredge(GLMM_model1, trace = 2)
View(fullmodels1)

# Resultado
# Observando la tabla, vemos que el "mejor" modelo es el nulo, es decir aquel
# que no incluye ninguna variable predictora y unicamente se incluye el factor
# aleatorio del individuo


# Mejor modelo:
# #
# Family: Gamma  ( log )
# Formula: Distrecorr ~ (1 | ID)
# Data: dfClean
# Control: glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))




# Importancia de las variables --------------------------------------------

# De igual forma realizamos la suma de los pesos de akaike para determinar de
# entre las variables estudiadas, cual es su orden de importancia como variables
# predictoras de la variables distrecorr


sumaw1 <- sw(fullmodels1)
sumaw1 <- tidy(sumaw1)
names(sumaw1) <- c("Variables", "Suma de los pesos de akaike")
view(sumaw1)


# Conclusion:
# la variable sitio es la que se presenta en la mayor cantidad de modelos, siendo
# esto lo que indicaria que es la de mayor importancia respecto a las otras

# La de menor importancia es la de "TS" temperatura sustrato, siendo esta la mas
# irrelevante entonces.



