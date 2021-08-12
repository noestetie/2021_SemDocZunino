
############################################################################
############################################################################
###                                                                      ###
###          5º  ENCUENTRO SINCRÓNICO: GRÁFICOS Y OTRAS HIERBAS          ###
###                                                                      ###
############################################################################
############################################################################




# Seteamos el directorio de trabajo (este es el de mi compu)
setwd("~/Desktop/Noe/Docencia/FFyL/2021_Metodologia_DOC/2021_SemDocZunino") #este es el de mi compu

# Cargo los paquetes que voy a usar
library(tidyverse) #manipular datos
library(ggplot2) #gráficos
library(plotly) #gráficos interactivos





##################################################################
##                  BREVÍSIMA INTRODUCCIÓN A R                  ##
##################################################################


library(cowsay)
say(what = "Esto recién empieza", by = "smallcat") #prueben what = "catfact" y by = "random"
# La función es 'say' y los argumentos son 'what' y 'by'
?say # ? es el comando de ayuda, para ver la documentación de una función





#################################################################
##                      DATOS CATEGÓRICOS                      ##
#################################################################


# Cargo la base de datos
cr.exp1 <- read_csv("cr.exp1.limpio.csv")

# Paso todas las variables que estaban como caracter a factor (categóricas)
cr.exp1 <- cr.exp1 %>% 
  mutate(across(where(is.character), as.factor))

# Cambio las variables que estaban como as.numeric
cr.exp1 <- cr.exp1 %>% 
  mutate(Part = as_factor(Part),
         Codunico = as_factor(Codunico)) #item

# Saco 2 participantes problemáticos, leían la oración al final
cr.exp1 <- filter(cr.exp1, Part != "150") #!= se usa para representar 'no igual a'
cr.exp1 <- filter(cr.exp1, Part != "19")

# Subset con lo que voy a analizar
cr.exp1 <- cr.exp1 %>% 
  filter(Correcta == "TRUE") %>%  #saco rtas erróneas
  filter(Cond == "N")             #dejo las normales


# Veo un resumen de cant. datos, variables y niveles de la base
summary(cr.exp1)
str(cr.exp1)



### GRÁFICOS: RTAs

# 1º capa: data
ggplot(data = cr.exp1)

# 2º capa: "aesthetic mappings" --> mapeos estéticos
ggplot(data = cr.exp1) +
  aes(x = RTA)

# 3º capa: "geom" --> tipo de gráfico
ggplot(data = cr.exp1) +
  aes(x = RTA) +
  geom_bar() #el gráfico se genera recién cuando indicamos el tipo de gráfico

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar()

ggplot(data = cr.exp1) +
  aes(x = RTA) +
  geom_bar() +
  facet_wrap(~ RC) # subdivide

# cambio la disposición de las barras: dodge, stack, fill
ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar(position = "dodge")

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar(position = "stack")

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar(position = "fill")

# cambio el eje: dos opciones
ggplot(data = cr.exp1) +
  aes(y = RTA, fill = RC) +
  geom_bar()

ggplot(data = cr.exp1) +
  aes(x = RTA, fill = RC) +
  geom_bar() +
  coord_flip()

# Proporciones
ggplot(data = cr.exp1) +
  aes(x = RTA, y = ..prop.., group= 1, fill = RC) +
  geom_bar() +
  facet_wrap(~ RC)

# Porcentaje
ggplot(data = cr.exp1) +
  aes(x = RTA, y = ..prop.., group= 1, fill = RC) +
  geom_bar() +
  facet_wrap(~ RC) +
  scale_y_continuous(labels = scales::percent_format())


### Elijo una versión del gráfico y la customizo

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
  scale_x_discrete(labels = c("O"="CR-O", "S"="CR-S")) +
  scale_fill_discrete(labels = c("SN1"="Adjunción alta", "SN2"="Adjunción baja")) +
  labs(title="Preferencias de adjunción según posición de la CR",
       x="Posición de la CR", y = "Cantidad de respuestas", 
       colour="Preferencias de adjunción", fill="Preferencias de adjunción")

# Coloco las etiquetas abajo
ggplot(data = cr.exp1) +
  aes(x = RC, fill = RTA) +
  geom_bar(position = "dodge") + 
  scale_x_discrete(labels = c("O"="CR-O", "S"="CR-S")) +
  scale_fill_discrete(labels = c("SN1"="Adjunción alta", "SN2"="Adjunción baja")) +
  labs(title="Preferencias de adjunción según posición de la CR",
       x="Posición de la CR", y = "Cantidad de respuestas", 
       colour="Preferencias de adjunción", fill="Preferencias de adjunción") + 
  theme(legend.position= "bottom")

### Guardo el gráfico abierto

ggsave('RTAs.png', width = 8, height = 6) #indico las dimensiones





##################################################################
##                DATOS CATEGÓRICOS >> CONTINUOS                ##
##################################################################


##Sintagmas Simples
Juicios<-read.csv("Base Juicios SS.csv")
str(Juicios)

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
  summarize(M = mean(Aceptabilidad), SE = sd(Aceptabilidad)/sqrt(n())) #square root, raíz cuadrada


### GRÁFICOS

boxplot(SS.Aceptabilidad$M ~ SS.Aceptabilidad$Morfologia)

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M) +
  geom_boxplot()

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M) +
  geom_boxplot(fill="slateblue", alpha=0.4, width=.8) #alpha agregar transparencia

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M, fill = Morfologia) +
  geom_boxplot(alpha=0.7, width=.8)

ggplot(data = SS.Aceptabilidad) +
  aes(x = Morfologia, y = M, fill = Morfologia) +
  geom_boxplot() +
  scale_fill_brewer(palette="Set2") #otras paletas: Accent; Dark2; Set1

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_boxplot()

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_violin()

ggplot(data = SS.Aceptabilidad) +
  aes(x = Identidad.de.genero, y = M, fill = Identidad.de.genero) +
  geom_boxplot(alpha=0.5) +
  geom_violin(alpha=0.5) #se puede combinar gráficos

ggplot(data = SS.Aceptabilidad) +
  aes(x = M, group = Identidad.de.genero, fill = Identidad.de.genero) +
  geom_density(alpha=.4)

# Gráfico completo
g1<-ggplot(data = SS.Aceptabilidad) +
  aes(x = Estereotipia, y = M, colour = Morfologia, shape = Identidad.de.genero) +
  geom_hline(aes(yintercept=0), linetype=3) +
  geom_point(size=4) +
  geom_errorbar(aes(max=M+SE, min=M-SE), width=.1) +
  labs(title= "Medias juicios de aceptabilidad por estereotipicidad y morfología", 
       y="Media", x= "Estereotipicidad", colour="Morfología", shape= "Identidad de género")
ggplotly(g1)

## Otra forma de graficar los mismos datos:

# Filtro aceptabilidad > 5
Juicios.Aceptable <- filter(Juicios, Aceptabilidad > 5)

# Creo una tabla con las frecuencias de aceptabilidad alta
tabla3<-table(Juicios.Aceptable$Identidad.de.genero, 
              Juicios.Aceptable$Morfologia, 
              Juicios.Aceptable$Estereotipia) %>% 
  as.data.frame()

# Gráfico Juicios x estereotipia x morfología x género
ggplot(data = tabla3) +
  aes(x = Var3, y = Freq, colour = Var2, shape = Var1, group = Var2:Var1) +
  geom_point(size=5) +
  geom_line(size=1) +
  labs(title= "Experimento 1: Aceptación alta por estereotipicidad y morfología", 
       y="Aceptación alta", x= "Estereotipicidad", colour="Morfología", 
       shape="Identidad de género")





#################################################################
##                       DATOS CONTINUOS                       ##
#################################################################


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
  geom_histogram(bins=40) #prueben poner bins= 30 y luego cambiar el número

ggplot(data = genero) +
  aes(x = RTrta) +
  geom_density()

# Gráficos de cuantiles (QQ)
qqnorm(genero$RTrta)
qqline(genero$RTrta)

install.packages("car")
library(car)
qqp(genero$RTrta, "norm")
qqp(genero$RTrta, "lnorm")


# Descriptivos: media, desvíos
genero %>% 
  group_by(Morfologia) %>% 
  summarize(mean(RTrta), sd(RTrta)) %>% 
  ungroup()

genero %>% 
  group_by(Estereotipia, Morfologia) %>% 
  summarize(mean(RTrta), sd(RTrta), mean(RTrta)+2*sd(RTrta)) %>% 
  ungroup()

# Nombro las columnas de la tabla generada
genero %>% 
  group_by(Estereotipia, Morfologia) %>% 
  summarize(mean = mean(RTrta), sd = sd(RTrta), corte_outliers = mean(RTrta)+2*sd(RTrta)) %>% 
  ungroup()

ggplot(genero) + 
  aes(x=Estereotipia,y=RTrta) +
  geom_jitter(aes(colour=RTA))

# Gráfico interactivo
g1<-ggplot(genero) + 
  aes(x=Morfologia,y=RTrta) +
  geom_jitter(aes(colour=RTA))
ggplotly(g1)


# ANOVAs

m1 <- aov(RTrta ~ RTA, genero) #ANOVA
summary(m1) #veo los resultados de la anova
TukeyHSD(m1) #test post hoc

genero %>% 
  group_by(RTA) %>% 
  summarize(mean(RTrta)) %>% 
  ungroup()

m2 <- aov(RTrta ~ RTA + Morfologia, genero)
summary(m2)
TukeyHSD(m2)

m3 <- aov(RTrta ~ RTA + Morfologia + Estereotipia, genero)
summary(m3)
TukeyHSD(m3)

m4 <- aov(RTrta ~ RTA * Morfologia, genero)
summary(m4)
TukeyHSD(m4)

m5 <- aov(RTrta ~ RTA * Morfologia * Estereotipia, genero)
summary(m5)
TukeyHSD(m5)

m6 <- aov(RTrta ~ RTA * Estereotipia + Morfologia, genero)
summary(m6)
TukeyHSD(m6)

# Comparo modelos: Akaike Information Criterion (AIC)
AIC(m1, m2, m3, m4, m5, m6)
BIC(m1, m2, m3, m4, m5, m6)


#Gráfico de perfiles >> efectos principales e interacciones

medias <- aggregate(RTrta ~ Morfologia + Estereotipia, genero, mean)
ggplot(medias) +
  aes(x = Estereotipia, y = RTrta, colour = Morfologia, 
      group = Morfologia, linetype = Morfologia, shape = Morfologia) +
  geom_line(size=.6) + 
  geom_point(size = 3)

ggplot(medias) +
  aes(x = Morfologia, y = RTrta, colour = Estereotipia, 
      group = Estereotipia, linetype = Estereotipia, shape = Estereotipia) +
  geom_line(size=.6) + 
  geom_point(size = 3)

medias <- aggregate(RTrta ~ RTA + Morfologia + Estereotipia, genero, mean)
ggplot(medias) +
  aes(x = Estereotipia, y = RTrta, colour = Morfologia, 
      group = Morfologia, linetype = Morfologia, shape = Morfologia) +
  geom_line(size=.6) + 
  geom_point(size = 3) +
  facet_wrap(~RTA)

medias <- aggregate(RTrta ~ RTA + Morfologia + Estereotipia + Genero, genero, mean)
ggplot(medias) +
  aes(x = Estereotipia, y = RTrta, colour = Morfologia, 
      group = Morfologia, linetype = Morfologia, shape = Morfologia) +
  geom_line(size=.6) + 
  geom_point(size = 3) +
  facet_grid(Genero ~ RTA) #probar dif con facet_wrap


medias <- aggregate(RToracion ~ Morfologia + Estereotipia + Genero, genero, mean)
ggplot(medias) +
  aes(x = Estereotipia, y = RToracion, colour = Morfologia, 
      group = Morfologia, linetype = Morfologia, shape = Morfologia) +
  geom_line(size=.6) + 
  geom_point(size = 3) +
  facet_wrap(~ Genero)





##################################################################
##                         LINKS ÚTILES                         ##
##################################################################


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