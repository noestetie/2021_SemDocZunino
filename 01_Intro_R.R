
###########################################################################
###########################################################################
###                                                                     ###
###                               CLASE 4                               ###
###                         PRIMEROS PASOS EN R                         ###
###                                                                     ###
###########################################################################
###########################################################################




#### ENTRADA EN CALOR ####

# Creo un proyecto o abro el proyecto de la carpeta zipeada

# Instalo los paquetes que voy a utilizar
install.packages("cosway")

# Cargo los paquetes ya instalados
library(cowsay)

say(what = "Esto recién empieza", by = "smallcat") 

?say # ? es el comando de ayuda, para ver la documentación de una función


#### EJERCICIO 1 ####

# a) La función say tiene varios argumentos, nos vamos a centrar en 4: 
# what, by, what_color, by_color. Prueben what = "catfact" y by = "random" 
# y agreguen otro argumento.





#### EJERCICIO 2 ####

# a) Instalo el paquete tidyverse (sirve para manipular datos)


# b) Cargo el paquete tidyverse


# Tidyverse cheatsheet: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf




#### PREPARACIÓN ####

# Seteo el directorio de trabajo o establezco al proyecto como directorio de trabajo


# Cargo y nombro la base de datos
cr.exp1 <- read_csv("cr.exp1.limpio.csv")
# read_delim("nombre_archivo.csv", delim = ";") # permite indicar cómo están delimitadas las columnas

# Comandos útiles
# - correr línea(s) de código: Ctrl + Enter
# - insertar <-: Alt + -
# - insertar %>%: Ctrl + Shift + M
# - insertar ~: Alt + Ñ




#### EXPLORACIÓN DE BASE DE DATOS ####

summary(cr.exp1)
str(cr.exp1)

head(cr.exp1)
tail(cr.exp1)




#### EJERCICIO 3 ####

# a) Mostrar las primeras 3 filas de la tabla
# Pista: comando de ayuda ?head


# b) Mostrar las últimas 4 filas de la tabla





#### MANIPULACIÓN DE BASE DE DATOS ####

# Renombro columnas
cr.exp1 %>% 
  rename(item = Codunico) # la sintaxis es nombre_nuevo = nombre_viejo
# ¿Se cambiaron los nombres en el dataset?


# Reorganizo columnas
cr.exp1 <- cr.exp1 %>% 
  relocate(RC, Lista) # mando estas columnas al comienzo

cr.exp1 %>% 
  relocate(RC, .after = Part) # ¿qué hace esta función? ¿cómo guardamos los cambios?


# Selecciono columnas específicas y les aplico otras funciones

cr.exp1 %>% 
  select(RTVerboPrin) %>%
  min()

cr.exp1 %>% 
  select(RTVerboPrin) %>%
  max()

cr.exp1 %>% 
  select(RTVerboPrin) %>%
  range()

cr.exp1 %>% 
  select(RTVerboPrin) %>%
  summary()


# Paso todas las variables que estaban como caracter a factor (categóricas)
cr.exp1 <- cr.exp1 %>% 
  mutate(across(where(is.character), as.factor))

# Cambio las variables que estaban como as.numeric
cr.exp1 <- cr.exp1 %>% 
  mutate(Part = as_factor(Part),
         Codunico = as_factor(Codunico)) # item

# Saco 2 participantes problemáticos, leían la oración al final
cr.exp1 <- filter(cr.exp1, Part != "150") #!= se usa para representar 'no igual a'
cr.exp1 <- filter(cr.exp1, Part != "19")

# Creo subset con lo que voy a analizar
cr.exp1_correctas <- filter(cr.exp1, Correcta == "TRUE") # saco rtas erróneas


# Ordeno niveles de una variable

cr.exp1_correctas$Educacion <- factor(cr.exp1_correctas$Educacion, 
                                     levels = c("secundario incompleto", "secundario",
                                                "universitario incompleto", "universitario"))

# Renombro niveles de una variable

# Op. 1
cr.exp1_correctas$Educacion <- as.character(cr.exp1_correctas$Educacion)
cr.exp1_correctas$Educacion[cr.exp1_correctas$Educacion == "secundario incompleto"] <- "sec inc"
cr.exp1_correctas$Educacion[cr.exp1_correctas$Educacion == "secundario"] <- "sec"
cr.exp1_correctas$Educacion[cr.exp1_correctas$Educacion == "universitario incompleto"] <- "uni inc"
cr.exp1_correctas$Educacion[cr.exp1_correctas$Educacion == "universitario"] <- "uni"
cr.exp1_correctas$Educacion <- as.factor(cr.exp1_correctas$Educacion)

levels(cr.exp1_correctas$Educacion)
cr.exp1_correctas$Educacion <- factor(cr.exp1_correctas$Educacion,levels = c("sec inc", "sec", "uni inc", "uni"))

# Op. 2
levels(cr.exp1_correctas$Educacion) <- c("secundario incompleto", 
                                         "secundario", 
                                         "universitario incompleto", 
                                         "universitario")




#### EJERCICIO 4 ####

# Comandos
  # - igual a: ==
  # - no igual a: !=
  # - mayor que: > 
  # - mayor o igual que: >=
  # - menor que: <
  # - menor o igual que: <=

# a) Crear un subset con las respuestas de Cond N



# b) Seleccionar solo los columnas que se van a utilizar para el análisis



# b) Dejar solo tiempos hasta 30000 ms





#### ESTADÍSTICA DESCRIPTIVA ####

mean(cr.exp1$RTtotal)
median(cr.exp1$RTtotal)
sd(cr.exp1$RTtotal)


cr.exp1 %>% group_by(RC) %>% 
  summarize(mean(RTrespuesta), sd(RTrespuesta)) %>% 
  ungroup()

cr.exp1 %>% group_by(RTA) %>% 
  summarize(mean(RTrespuesta), sd(RTrespuesta)) %>% 
  ungroup()

cr.exp1 %>% group_by(RC, RTA) %>% 
  summarize(M = mean(RTrespuesta), DE = sd(RTrespuesta), 
            Outliers = mean(RTrespuesta) + 2 * sd(RTrespuesta) ) %>% 
  ungroup()



#### EJERCICIO 5 ####

# a) Calcular M y DE para los tiempos del verbo principal según la posición de la RC y el tiempo verbal




