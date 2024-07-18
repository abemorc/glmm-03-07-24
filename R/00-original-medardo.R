# Ajustar el modelo nulo
GLMM_model0 <- glmer(Distrecorr ~ 1 + (1 | ID),
                     data = glmm_distances,
                     family = Gamma(link = "log"),
                     control = glmerControl(nAGQ = 500))

summary(GLMM_model0)
anova(GLMM_model0)

# Ajustar el modelo con los datos limpios
GLMM_model1 <- glmer(Distrecorr ~ Sitio + DOSEL + HUM + TEMPAMB + HOJA + ARBUS + ARBO + (1 | ID),
                     data = glmm_distances,
                     family = Gamma(link = "log"),
                     control = glmerControl(nAGQ = 500))

summary(GLMM_model1)
Anova(GLMM_model1)

# Comparar los modelos utilizando la función anova
anova(GLMM_model0, GLMM_model1)

# El valor p  es 0.3738, indica que no hay una mejora estadísticamente
# significativa en el ajuste del modelo completo model1 sobre
# el modelo nulo



## Aqui es buscar el mejor modelo


# Buscar el mejor modelo de todas la comb posibles ------------------------

# Modelo mixto inicial



library(MuMIn) # Utiiizamos esta libreria

# Crear un modelo base
# Ajustar el modelo completo con na.action = na.fail
GLMM_model1 <- glmer(Distrecorr ~ Sitio + DOSEL + HUM + TS + HOJA + Sexo +Temporada (1 | ID),
                     data = glmm_distances_clean,
                     family = Gamma(link = "log"),
                     control = glmerControl(nAGQ = 500),
                     na.action = na.fail)

# Ver resumen del modelo completo
summary(GLMM_model1)

# Usar la función dredge para buscar el mejor modelo
model_set <- dredge(GLMM_model1, extra = c("AIC", "BIC"))

# Mostrar los modelos ordenados por AIC
print(model_set)

# Seleccionar el mejor modelo
best_model <- get.models(model_set, 1)[[1]]

# Mostrar el resumen del mejor modelo
summary(best_model)
Anova(best_model)
