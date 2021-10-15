
############################################################################
############################################################################
###                                                                      ###
###                                CLASE 7                               ###
###                         ANÁLISIS ESTADÍSTICO                         ###
###                                                                      ###
############################################################################
############################################################################


#### PREPARACIÓN PREVIA ####

# Seteo el directorio de trabajo 

# Cargo los paquetes que voy a usar
# install.packages("nombre_del_paquete")

library(tidyverse)
library(lme4)
library(lmerTest)
library(car) # para contrastes
library(MASS) # para boxcox
library(MuMIn) # r cuadrado


# Cargo la base de datos
genero <- read.csv("Exp.oraciones1.limpio.csv")

# Cambio algunas variables para que me las considere categóricas
genero <- genero %>% mutate(Part = as_factor(Part), 
                            Item = as_factor(Item))

genero <- genero %>% 
  mutate(across(where(is.character), as.factor))

# Renombro los niveles de una variable
genero <- genero %>% 
  mutate(Estereotipia = as_factor(dplyr::recode(Estereotipia,
                                         "1" = "Baja", 
                                         "2" = "Media",
                                         "3" = "Alta")))
# la función recode() está en dos de los paquetes que tenemos cargados,
# por eso tenemos que indicarle de cuál queremos que la tome


summary(genero)
str(genero)





#### DATOS CONTINUOS ####

genero %>% 
  group_by(RTA) %>% 
  summarize(mean(RTrta), sd(RTrta)) %>% 
  ungroup()

genero %>% 
  group_by(Morfologia) %>% 
  summarize(mean(RTrta), sd(RTrta)) %>% 
  ungroup()

genero %>% 
  group_by(Estereotipia) %>% 
  summarize(mean(RTrta), sd(RTrta)) %>% 
  ungroup()

genero %>% 
  group_by(RTA, Morfologia) %>% 
  summarize(mean(RTrta), sd(RTrta)) %>% 
  ungroup()





#### DISTRIBUCIÓN DE LA MUESTRA ####

# Histograma de tiempos de respuesta (RTrta)

hist(genero$RTrta)
hist(genero$RTrta, breaks = 20)



#### EJERCICIO 1 ####

# a) Cambiar los breaks del histograma, ¿qué cambia?






# Gráficos de cuantiles (QQ plot) de tiempos de respuesta (RTrta)
qqnorm(genero$RTrta)
qqline(genero$RTrta)

qqp(genero$RTrta, "norm")


# Histograma y qqplot con transformación logarítmica (RTrta)

hist(log(genero$RTrta), breaks = 30)

qqp(log(genero$RTrta), "norm")

# ¿Qué cambios se ven al transformar los datos?





#### OUTLIERS ####

# Veo medias, desvíos y límite superior de outliers para cada participante por condición
genero %>% group_by(Part, Morfologia, Estereotipia, RTA) %>%
  summarize(M = mean(RTrta), SD = sd(RTrta), outlier = mean(RTrta) + 1.5*sd(RTrta))

# Genero dataset con las medias para cada participante por condición (lo vamos a usar después)
Part.medias<- genero %>% group_by(Part, Morfologia, Estereotipia, RTA) %>%
  summarize(mean_RTrta_cond = mean(RTrta))
# write_csv(Part.medias, "Part.medias.csv") # si quisiéramos guardarlo



# Detecto y reemplazo outliers por sujeto por condición para los RTrta (varios pasos)

# 1º: genero una copia del dataset llamada Detec.Outliers y ordenada por participante
Detec.Outliers <- group_by(genero, Part, Morfologia, Estereotipia, RTA) 
#podemos cambiar Part, por Item o sacarlo si solo queremos por condición


# 2º: genero nueva columna que categorice a los RTrta como ok u out según su límite mínimo de outlier
Detec.Outliers <- mutate (Detec.Outliers,
                                     Outliermin = ifelse(RTrta < (mean(RTrta, na.rm = TRUE) - 
                                                                    2*(sd(RTrta, na.rm = TRUE))), "out", "ok"))

# Veo cuántos valores tendría para sacar según el límite mínimo de outliers
Detec.Outliers[which(Detec.Outliers[,16]== "out"),]
min(Detec.Outliers$RTrta)


# 3º: genero nueva columna que categorice a los RTrta como ok u out según su límite máximo de outlier
Detec.Outliers <- mutate(Detec.Outliers, Outliermax = 
                            ifelse(RTrta > (mean(RTrta, na.rm = TRUE) 
                                            + 2*(sd(RTrta, na.rm = TRUE))), "out", "ok"))

# Veo cuántos valores tendría para sacar según el límite máximo de outliers
Detec.Outliers[which(Detec.Outliers[,17]== "out"),]
max(Detec.Outliers$RTrta)

is.na(Detec.Outliers$Outliermax) # para ver valores vacíos


# 4º: genero nuevo dataset uniendo Detect.Outliers con el dataset Part.medias, de esta forma obtengo
# una nueva columna en donde tengo la media de cada participante por condición
Outliers.corregidos <- left_join(Detec.Outliers, Part.medias, by = c("Part", "Morfologia", 
                                                                     "Estereotipia", "RTA"))
# en by = c() estoy indicando que me una las tablas cuando todas esas columnas coincidan


# 5º: genero una nueva columna `RTrta_Outliercorregido` en la que indico que si la columna de límite
# máximo dice `out` coloque la media de cada participante por condición, pero si no dice `out` 
# (osea que dice `ok`), que deje el valor original que tenía ese participante en la columna RTrta

Outliers.corregidos <- mutate (Outliers.corregidos,
                               RTrta_Outliercorregido = ifelse(Outliermax == "out", mean_RTrta_cond, RTrta))





#### MODELOS LINEALES ####

# ANOVA
m1<-aov(RTrta ~ RTA, genero)
summary(m1) #veo los resultados de la anova

# REGRESIÓN LINEAL
m1_bis<-lm(RTrta ~ RTA, genero) 
summary(m1_bis) 

genero %>% 
  group_by(RTA) %>% 
  summarize(mean(RTrta)) %>% 
  ungroup()


m2<-aov(RTrta ~ RTA + Morfologia, genero)
summary(m2)
TukeyHSD(m2) #test post hoc





#### SUPUESTOS DEL MODELO LINEAL ####

## Testeo normalidad de los residuos

# Residuos del modelo
res <- residuals(m1_bis)

# Histograma de los residuos del modelo
hist(res)
hist(res, breaks = 30)

# Gráfico de cuantiles de los residuos del modelo
qqnorm(res)
qqline(res)


## Testeo homocedasticidad de los residuos

# Gráfico de residuos 
plot(fitted(m1_bis), res)
abline(0,0)
# no están distribuidos de forma homogénea


## Testeo colinearidad
vif(m2)





#### TRANSFORMACIÓN DE DATOS ####

# Evalúo necesidad y tipo de transformación
boxcox(m1_bis) 
#cerca de 0: transformación log
#cerca de 1: sin transformación
#cerca de -1: transformación recíproca


m2_log <- lm(log(RTrta) ~ RTA + Morfologia, data = genero)
summary(m2_log)

# Residuos del modelo
res <- residuals(m2_log)

# Histograma de los residuos del modelo
hist(res)
hist(res, breaks = 30)

# Gráfico de cuantiles de los residuos del modelo
qqnorm(res)
qqline(res)


## Testeo homocedasticidad de los residuos

# Gráfico de residuos 
plot(fitted(m1_bis), res)
abline(0,0)


## Testeo colinearidad
vif(m2_log)





#### CONTRASTES ####

genero.o<-filter(genero, Morfologia == "o") # genero un subgrupo para un análisis posterior

## Veo los contrastes actuales
contrasts(genero$RTA)
contrasts(genero$Morfologia)

## Repeated contrasts
contrasts(genero$RTA) <- contr.sdif(2)
contrasts(genero$Morfologia) <- contr.sdif(3)

contrasts(genero$Estereotipia) <- contr.sdif(3)
contrasts(genero$NoBinario) <- contr.sdif(3)
contrasts(genero$Genero) <- contr.sdif(3)

## Treatment contrast
#contrasts(genero$RTA) <- contr.treatment(2)

## Sum constrast
#contrasts(genero$RTA) <- contr.sum(2)

## Polynomial contrast
#contrasts(genero$RTA) <- contr.poly(2)





##### MODELOS LINEALES MIXTOS #####

M1_log<-lmer(log(RTrta) ~ RTA + (1|Part) + (1|Item), data = genero, REML = FALSE)
summary(M1_log) # sig.

M2_log<-lmer(log(RTrta) ~ RTA + Morfologia + (1|Part) + (1|Item), data = genero, REML = FALSE)
summary(M2_log) # sig.

M3_log<-lmer(log(RTrta) ~ Morfologia / RTA + (1|Part) + (1|Item), data = genero, REML = FALSE)
summary(M3_log) # sig.

AIC(M1_log, M2_log, M3_log) #el de menor akaike es el M3

anova(M2_log, M3_log) #comparo modelos

r.squaredGLMM(M3_log)





#### DATOS CATEGÓRICOS >> MODELOS LINEALES GENERALIZADOS ####

M1<-glmer(RTA ~ Estereotipia + (1 | Part) + (1 | Item), data = genero.o, family = binomial(link = "logit"))
summary(M1) # sig.

M2<-glmer(RTA ~ Estereotipia + Genero + (1 | Part) + (1 | Item), data = genero.o, family = binomial(link = "logit"))
summary(M2) # sig.

AIC(M1, M2)
anova(M1, M2)

r.squaredGLMM(M2)