
###########################################################################
###########################################################################
###                                                                     ###
###                               CLASE 6                               ###
###                       GRÁFICOS SENCILLOS EN R                       ###
###                                                                     ###
###########################################################################
###########################################################################




#### PREPARACIÓN PREVIA ####

# Seteo el directorio de trabajo 

# Cargo los paquetes que voy a usar
# install.packages("nombre_del_paquete")

library(tidyverse) # manipular datos
library(ggplot2) # gráficos
library(plotly) # gráficos interactivos

# Cheatsheet ggplot2: https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf




#### BASE 1 ####

# Cargo la base de datos
cr.exp1 <- read_csv("cr.exp1.limpio.csv")

# Paso todas las variables que estaban como caracter a factor (categóricas)
cr.exp1 <- cr.exp1 %>% 
  mutate(across(where(is.character), as.factor))

# Cambio las variables que estaban como as.numeric
cr.exp1 <- cr.exp1 %>% 
  mutate(Part = as_factor(Part),
         Codunico = as_factor(Codunico)) # item

# Saco 2 participantes problemáticos, leían la oración al final
cr.exp1 <- filter(cr.exp1, Part != "150")
cr.exp1 <- filter(cr.exp1, Part != "19")

# Subset con lo que voy a analizar
cr.exp1 <- cr.exp1 %>% 
  filter(Correcta == "TRUE") %>%
  filter(Cond == "N")


# Resumen de la base
str(cr.exp1)
summary(cr.exp1)




#### GRÁFICOS: DATOS CATEGÓRICOS ####

# Grafico RTAs

# 1º capa: data
ggplot(data = cr.exp1)


# 2º capa: "aesthetic mappings" --> mapeos estéticos
ggplot(data = cr.exp1) +
  aes(x = RTA)


# 3º capa: "geom" --> tipo de gráfico
ggplot(data = cr.exp1) +
  aes(x = RTA) +
  geom_bar() # el gráfico se genera recién cuando indicamos el tipo de gráfico

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar()
# ¿Qué sucedió?


# Disposición de las barras

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar(position = "stack") # por default

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar(position = "dodge")

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar(position = "fill")




#### EJERCICIO 1 ####

# a) Colocar las respuestas sobre el eje Y
# Pista: hay dos opciones: coord_flip()





#### SUBDIVISIÓN DEL GRÁFICO ####

# Facet wrap

ggplot(data = cr.exp1) +
  aes(x = RTA) +
  geom_bar() +
  facet_wrap(~ RC)

ggplot(data = cr.exp1) +
  aes(x = RTA) +
  geom_bar() +
  facet_wrap(RC ~ Educacion)

# Facet grid

ggplot(data = cr.exp1) +
  aes(x = RTA) +
  geom_bar() +
  facet_grid(RC ~ Educacion)
# ¿Cuál es la diferencia con facet_wrap?



#### CANTIDAD, PROPORCIÓN Y PORCENTAJE ####

# Proporción
ggplot(data = cr.exp1) +
  aes(x = RTA, y = ..prop.., group= 1, fill = RC) +
  geom_bar() +
  facet_wrap(~ RC)


# Porcentaje
ggplot(data = cr.exp1) +
  aes(x = RTA, y = ..prop.., group= 1, fill = RC) +
  geom_bar() +
  facet_wrap(~ RC) + 
  scale_y_continuous(labels = scales::percent)

ggplot(data = cr.exp1) +
  aes(x = RTA, y = ..prop.., group= 1, fill = RC) +
  geom_bar() +
  facet_wrap(~ RC) + 
  scale_y_continuous(labels = scales::percent_format(suffix = ""))




#### CUSTOMIZACIÓN DE GRÁFICOS ####

# Elijo una versión del gráfico y la customizo

# Cambio nombres: niveles de la variable x
ggplot(data = cr.exp1) +
  aes(x = RC, fill = RTA) +
  geom_bar(position = "dodge") + 
  scale_x_discrete(labels = c("O"="CR-O", "S"="CR-S"))


# Cambio nombres: niveles de la variable y
ggplot(data = cr.exp1) +
  aes(x = RC, fill = RTA) +
  geom_bar(position = "dodge") + 
  scale_x_discrete(labels = c("O"="CR-O", "S"="CR-S")) +
  scale_fill_discrete(labels = c("SN1"="Adjunción alta", "SN2"="Adjunción baja"))


# Agrego título y cambio nombres de las variables
ggplot(data = cr.exp1) +
  aes(x = RC, fill = RTA) +
  geom_bar(position = "dodge") + 
  scale_x_discrete(labels = c("O"="objeto", "S"="sujeto")) +
  scale_fill_discrete(labels = c("SN1"="Adjunción alta", "SN2"="Adjunción baja")) +
  labs(title="Preferencias de adjunción según posición de la CR",
       x="Posición de la CR", y = "Cantidad de respuestas", 
       colour="Preferencias de adjunción", fill="Preferencias de adjunción")


# Coloco las etiquetas abajo
ggplot(data = cr.exp1) +
  aes(x = RC, fill = RTA) +
  geom_bar(position = "dodge") + 
  scale_x_discrete(labels = c("O"="objeto", "S"="sujeto")) +
  scale_fill_discrete(labels = c("SN1"="Adjunción alta", "SN2"="Adjunción baja")) +
  labs(title="Preferencias de adjunción según posición de la CR",
       x="Posición de la CR", y = "Cantidad de respuestas", 
       colour="Preferencias de adjunción", fill="Preferencias de adjunción") + 
  theme(legend.position= "bottom")


# Cambio el tema
ggplot(data = cr.exp1) +
  aes(x = RC, fill = RTA) +
  geom_bar(position = "dodge") + 
  scale_x_discrete(labels = c("O"="objeto", "S"="sujeto")) +
  scale_fill_discrete(labels = c("SN1"="Adjunción alta", "SN2"="Adjunción baja")) +
  labs(title="Preferencias de adjunción según posición de la CR",
       x="Posición de la CR", y = "Cantidad de respuestas", 
       colour="Preferencias de adjunción", fill="Preferencias de adjunción") + 
  theme(legend.position= "bottom") + 
  theme_bw()


# Guardo el gráfico abierto
ggsave('RTAs.png', width = 8, height = 6) # indico las dimensiones




#### EJERCICIO 2 ####

# a) Probar distintos temas
# Pista: escribir theme y esperar





#### DATOS CATEGÓRICOS >> CONTINUOS 1 ####

# Cargo otra base
Juicios<-read.csv("Base Juicios SS.csv")

# Renombro niveles de algunas variables
Juicios <- Juicios %>% 
  mutate(Identidad.de.genero = recode(Identidad.de.genero, 
                                      "Mujer" = "Mujer", "Varon" = "Varón"))
Juicios <- Juicios %>% 
  mutate(Morfologia = as_factor(recode(Morfologia,
                                       "1" = "O",
                                       "2" = "X",
                                       "3" = "E")))
Juicios <- Juicios %>% 
  mutate(Estereotipia = as_factor(recode(Estereotipia,
                                         "1" = "Baja",
                                         "2" = "Media",
                                         "3" = "Alta")))

Juicios$Aceptabilidad<-as.numeric(Juicios$Aceptabilidad)


# Genero nueva tabla para graficar
SS.Aceptabilidad <- Juicios %>% 
  group_by(Identidad.de.genero, Estereotipia, Morfologia) %>% 
  summarize(M = mean(Aceptabilidad), SE = sd(Aceptabilidad)/sqrt(n())) # square root, raíz cuadrada


# Grafico

boxplot(SS.Aceptabilidad$M ~ SS.Aceptabilidad$Morfologia)

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M) +
  geom_boxplot()

# Customizo el gráfico

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M) +
  geom_boxplot(fill="slateblue", alpha=0.4, width=.8) # alpha agregar transparencia

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M, fill = Morfologia) +
  geom_boxplot(alpha=0.7, width=.8)

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M, fill = Morfologia) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set2") #otras paletas: Accent; Dark2; Set1




#### EJERCICIO 3 ####

# a) Cambiar el nombre de los ejes y agregar título al último gráfico




#### COMBINACIÓN DE GRÁFICOS ####

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_boxplot()

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_violin()

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_boxplot() +
  geom_violin()
# ¿Cómo puedo mejorar el gráfico?

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_boxplot(alpha=0.5) +
  geom_point()

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_boxplot(alpha=0.5) +
  geom_violin(alpha=0.5) +
  geom_point()




#### GRÁFICO INTERACTIVO ####

g1<-ggplot(data = SS.Aceptabilidad) +
  aes(x = Estereotipia, y = M, colour = Morfologia, shape = Identidad.de.genero) +
  geom_hline(aes(yintercept=0), linetype=3) +
  geom_point(size=4) +
  geom_errorbar(aes(max=M+SE, min=M-SE), width=.1) +
  labs(title= "Medias juicios de aceptabilidad por estereotipicidad y morfología", 
       y="Media", x= "Estereotipicidad", colour="Morfología", shape= "Identidad de género")

ggplotly(g1)




#### DATOS CATEGÓRICOS >> CONTINUOS 2 ####

# Filtro aceptabilidad > 5
Juicios.Aceptable <- filter(Juicios, Aceptabilidad > 5)

# Creo una tabla con las frecuencias de aceptabilidad alta
tabla<-table(Juicios.Aceptable$Identidad.de.genero, 
              Juicios.Aceptable$Morfologia, 
              Juicios.Aceptable$Estereotipia) %>% 
  as.data.frame()

# Gráfico Juicios x estereotipicidad x morfología x género

ggplot(data = tabla) +
  aes(x = Var3, y = Freq, colour = Var2, shape = Var1) + 
  geom_point(size=5) +
  geom_line(size=1)

ggplot(data = tabla) +
  aes(x = Var3, y = Freq, colour = Var2, shape = Var1, group = Var2:Var1) + # group marca qué van a conectar las líneas
  geom_point(size=5) +
  geom_line(size=1) +
  labs(title= "Experimento 1: Aceptación alta por estereotipicidad y morfología", 
       y="Aceptación alta", x= "Estereotipicidad", colour="Morfología", 
       shape="Identidad de género")




#### DATOS CONTINUOS ####

# Cargo la base de datos
genero <- read.csv("Exp.oraciones1.limpio.csv")

# Cambio algunas variables para que me las considere categóricas
genero <- genero %>% mutate(Part = as_factor(Part), 
                            Item = as_factor(Item))

# Renombro los niveles de una variable
genero <- genero %>% 
  mutate(Estereotipia = as_factor(recode(Estereotipia, 
                                         "1" = "Baja", 
                                         "2" = "Media",
                                         "3" = "Alta")))

summary(genero)
str(genero)


# Gráficos exploratorios: tiempos de respuesta (RTrta)

hist(genero$RTrta)
hist(log(genero$RTrta), breaks = 40)
hist(genero$RTrta, freq = F, breaks = 40)

ggplot(data = genero) +
  aes(x = RTrta) +
  geom_histogram(bins=30)

ggplot(data = genero) +
  aes(x = RTrta) +
  geom_density()




#### EJERCICIO 4 ####

# a) Hacer el histograma con ggplot en base logarítmica


# b) Hacer el gráfico de densidad con ggplot en base logarítmica, agregarle color y transparencia




#### TESTEO DE NORMALIDAD ####

# Gráficos de cuantiles (QQ)
qqnorm(genero$RTrta)
qqline(genero$RTrta)

# install.packages("car")
library(car)
qqp(genero$RTrta, "norm")
qqp(genero$RTrta, "lnorm")





#### GRÁFICO DE PERFILES >> EFECTOS PRINCIPALES E INTERACCIONES ####

medias_rta <- genero %>% 
  dplyr::group_by(RTA, Morfologia) %>% 
  summarize(M = mean(RTrta), SE = sd(RTrta)/sqrt(n()))

ggplot(medias_rta) +
  aes(x = RTA, y = M, colour = Morfologia, 
      group = Morfologia, linetype = Morfologia, shape = Morfologia) +
  geom_line(size=.6) + 
  geom_point(size = 3) +
  geom_errorbar(aes(max=M+SE, min=M-SE), width=.1)
# Ver con plotly: ggplotly()


medias_rta <- genero %>% 
  dplyr::group_by(RTA, Morfologia, Estereotipia) %>% 
  summarize(M = mean(RTrta))

ggplot(medias_rta) +
  aes(x = Estereotipia, y = M, colour = Morfologia, 
      group = Morfologia, linetype = Morfologia, shape = Morfologia) +
  geom_line(size=.6) + 
  geom_point(size = 3) +
  facet_wrap(~ RTA)


medias_rta <- genero %>% 
  dplyr::group_by(RTA, Morfologia, Estereotipia, Genero) %>% 
  summarize(M = mean(RTrta))

ggplot(medias_rta) +
  aes(x = Estereotipia, y = M, colour = Morfologia, 
      group = Morfologia, linetype = Morfologia, shape = Morfologia) +
  geom_line(size=.6) + 
  geom_point(size = 3) +
  facet_grid (RTA ~ Genero)





#### LINKS ÚTILES ####

# R Graph Gallery: https://www.r-graph-gallery.com
# Tidyverse: https://www.tidyverse.org/
# Libro "R for Data Science": https://r4ds.had.co.nz/index.html
# Cheat sheets: https://rstudio.com/resources/cheatsheets/
# Tutoriales de dplyr - Parte 1: https://www.kaggle.com/jessemostipak/dive-into-dplyr-tutorial-1
# Tutoriales de dplyr - Parte 2: https://www.kaggle.com/jessemostipak/dive-deeper-into-dplyr-tutorial-2
# Introductory tutorials on data vis and wrangling in R: https://r-bootcamp.netlify.app/
# Instructivos paso a paso de ggplot: http://www.cookbook-r.com/Graphs/
# Instructivo interactivo con ejemplos de todas las scales: https://ggplot2tor.com/scales/
# Color Brewer: https://colorbrewer2.org/
# Lista de todas las paletas disponibles en R: https://github.com/EmilHvitfeldt/r-color-palettes