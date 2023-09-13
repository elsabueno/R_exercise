#PROYECTO FINAL SOFTWARE INFORMÁTICO R - ANÁLISIS DE UNA REGRESIÓN MULTINIVEL CON DOS NIVELES 

#FECHA ENTREGA: 19 octubre 2022


#Para mostrar una aplicación práctica y sencilla del análisis de una regresión multinivel vamos a usar una base de datos en la que se analizan las tasas de rebote de los usuarios de un sitio web con recetas de cocina.
#Tasa de rebote (bounce_time): medida de la rapidez con la que alguien abandona un sitio web.
#Esta base de datos se creó para dar respuesta a la demanda de un sitio web sobre si las personas más jóvenes tienen más probabilidades de abandonar el sitio web más rápido.

#Para investigar la tasa de rebote del sitio web, se eligieron tres ubicaciones en 8 condados de Inglaterra. Miembros del público de todas las edades realizaran una encuesta / cuestionario de prueba. 
#En la prueba se les pidió que usasen el motor de búsqueda determinado por la empresa que realizó el análisis para consultar algo que quisiesen comer esta noche.
#El motor de búsqueda enumeró primero el sitio web interesado en el análisis, así como otros sitios web que serían devueltos. 
#Los usuarios que hicieron clic en dicho sitio web fueron cronometrados y su tiempo de rebote registrado. También se registró su edad (age), condado (county) y ubicación (location).

#Esta situación es ideal para poder realizar un análisis de regresión múltiple pues tenemos varios niveles: edad, condado y localización.


#Antes de empezar con el análisis vamos a determinar el directorio para trabajar y el archivo que debe leer el software R 
setwd("~/Desktop/apuntes/master/año 1/software R/clases prácticas/prácticas/EXPOSICIÓN FINAL")

install.packages("readxl")
library(readxl)

file.choose()
file_database=read_xls("/Users/elsabuenoalonso/Desktop/apuntes/master/año 1/software R/clases prácticas/prácticas/EXPOSICIÓN FINAL/base de datos exposición R - análisis multinivel.xls")
data_bounce_time=data.frame(file_database)
head(data_bounce_time)

#Para ver si la distribución de nuestra variable explicada es parecida usamos un histograma:
hist(data_bounce_time$bounce_time) #como podemos observar, la distribución se parece a una normal, por lo que el análisis se va a realizar en base a una distribución normal.

#En primer lugar, se va a estandarizar la variable edad con el fin de que todas las variables y sus correspondientes coeficientes estén en la misma escala. Creamos una nueva variable con media cero y varianza igual a uno (distribución normal)
age_scaled=scale(data_bounce_time$age)
head(age_scaled)


#Con el fin de ver el efecto de una regresión multinivel, vamos a realizar, en un primer momento, una regresión lineal simple entre la edad y el tiempo de rebote, para después añadir el efecto del contexto, es decir, el condado (county) y la localización (location)
#1.- REGRESIÓN LINEAL SIMPLE: bounce_time ~ age
modelo_simple <- lm(bounce_time ~ age_scaled, data = data_bounce_time)
summary(modelo_simple)

#Para representar gráficamente este modelo lineal usamos el paquete "tidyverse" y el comando "ggplot"
install.packages("tidyverse")
library(tidyverse)
(prelim_plot <- ggplot(data_bounce_time, aes(x = age, y = bounce_time)) +
    geom_point() +
    geom_smooth(method = "lm"))

#Según esta representación observamos que confome mayor es la edad, mayor es el tiempo que las personas se quedaban en web. 
#Con el fin de conocer si se cumple la suposición inicial de si los jóvenes se quedan más tiempo o menos en la página vamos a trazar los residuos:
plot(modelo_simple, which = 1) #La línea roja debería ser plana, pero teniendo en cuenta que tenemos pocas observaciones, podemos continuar.

#Otra prueba que podemos hacer es el gráfico de la Normal Q-Q:
plot(modelo_simple, which = 2) #Como los puntos caen prácticamente en la línea discontinua diagonal, determinamos que, aunque la muestra sea pequeña, nos es útil para el análisis.


#Ahora vamos a comenzar con el análisis multinivel, aunque antes vamos a realizar algunas pruebas con el fin de ver si los datos son independientes entre sí o están relacionados con el condado (similitud)

#2.- INDEPENDENCIAS DE LAS OBSERVACIONES.
#Con el fin de determinar si nuestros datos están relacionados o no entre si vamos a realizar tanto un blox plot como un gráfico de puntos separados por colores:
boxplot(bounce_time ~ county, data = data_bounce_time)
(colour_plot <- ggplot(data_bounce_time, aes(x = age, y = bounce_time, colour = county)) +
    geom_point(size = 1) +
    theme_classic() +
    theme(legend.position = "top"))
#Como podemos ver, existen diferentes grupos, por lo que deducimos que el tiempo de rebote realmente depende de la edad como del condado en el que se encuentre la persona. 
#Esto confirma que las observaciones recogidas dentro de cada uno de los niveles no son independientes, lo cual es un aspecto que no se debe de olvidar al realizar este tipo de análisis. 

#Antes hemos determinado que el tiempo de rebote depende no solo de edad sino también de los condados, por lo que podríamos realizar una regresión para cada condado, teniendo en cuenta la edad.
#Gráficamente, esto quedaría
(split_plot <- ggplot(aes(age, bounce_time), data = data_bounce_time) + 
    geom_point() + 
    facet_wrap(~ county) + #usamos el comando "face_wrap" para indicar que haga un gráfico para cada condado
    xlab("age") + 
    ylab("test bounce_time"))
#Sin embargo, hay tener en cuenta que tenemos diferentes localizaciones que también influyen en el tiempo de rebote, por lo que para poder realizar la regresión tendríamos que estimar 48 parámetros (2(parámetros por regresión) x 3 (localizaciones) x 8 (condados))
#Este tipo de acciones conllevaría a un aumento de la posibilidad de cometer errores de tipo I, es decir, rechazar falsamente la hipótesis nula (influencia de las variables) al realizar múltiples comparaciones. 



#Vamos a empezar con el modelo de regresión multinivel con dos niveles: edad y condado. Posteriormente veremos el efecto con la localización. 

#3.- MODELO MULTINIVEL DE DOS NIVELES: bounce_time ~ age + county
#Para tener en cuenta el condado hay que incorporar esta variable como un efecto fijo en una regresión lineal simple:
modelo_county <- lm(bounce_time ~ age_scaled + county, data = data_bounce_time)
summary(modelo_county)
#Como podemos ver en el resumen, tenemos el término independiente, el parámetro de la edad, y el efecto de los diferentes condados.
#Ahora, la edad no es significativa: coeficiente = 0.04883

#Queremos saber si la edad afecta al tiempo de rebote y, al mismo tiempo, queremos controlar la variación que proviene de los condados.
#Esto es a lo que nos referimos como "factores aleatorios".

#Para saber qué variables van a ser factores aleatorios y cuáles factores fijos hay que tener en cuenta lo siguiente:
  #EFECTO FIJOS: variables que esperamos que tengan un efecto en la variable dependiente/respuesta (variables explicativas en una regresión lineal simple). 
    #En nuestro caso, queremos saber cómo la edad afecta al tiempo de rebote. Por lo tanto, la edad es un efecto fijo y el tiempo de rebote es la variable dependiente.
  #EFECTOS ALEATORIOS: suelen agrupar factores que estamos tratando de controlar.
    #En nuestro caso, estos factores aleatorios serían el condado y la localización.
    #Para incluir este efecto aleatorio al modelo se usa la sintaxis "(1 | variable) y es necesario instalar el paquete "lme4".

install.packages("lme4")
library(lme4)

#Creamos nuestro primer modelo multinivel con dos niveles:
modelo_multinivel <- lmer(bounce_time ~ age_scaled + (1|county), data = data_bounce_time)
summary(modelo_multinivel)

#OUTPUT: 
#El efecto aleatorio nos munestra cuanta varianza existe entre los condados, junto con la varianza de los errores:
#County: 213.03
#Residual: 74.73
#El efecto fijo nos indica tanto el parámetro independiente como el segundo parámetro de una regresión lineal:
#(Intercept): 201.3165
#age_scaled: 0.1357 

#Si nos fijamos en la varianza de los condados, vemos que ésta es 213.03, lo cual es un valor alto. Más específicamente:
213.03/(213.03+74.73) #Las diferencias entre los condados explican el 74% de la variabilidad del modelo, una vez explicada la parte de los efectos fijos.

#Vamos a representar este modelo multinivel con el fin de poder ver como actúa este segundo nivel:
plot(modelo_multinivel) #no se ven patrones evidentes
#Si hacemos el gráfico Normal Q-Q, observamos que los puntos siguen estando, su mayoría, sobre la línea:
qqnorm(resid(modelo_multinivel))
qqline(resid(modelo_multinivel))


#4.- PRESENTACIÓN DE RESULTADOS DE UN MODELO MULTINIVEL DE DOS NIVELES:
(mm_plot <- ggplot(data_bounce_time, aes(x = age, y = bounce_time, colour = county)) +
    facet_wrap(~county, nrow=2) +    #un gráfico para cada condado 
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(data_bounce_time, pred = predict(modelo_multinivel)), aes(y = pred), size = 1) +  #añadimos una línea de predicción para el modelo multinivel 
    theme(legend.position =  "top",
          panel.spacing = unit(2, "lines"))  #añadimos espacio entre los gráficos
)

#También los podemos ver en un gráfico de puntos, mediante la instalación del paquete "ggeffects"
install.packages("ggeffects")
library(ggeffects)
pred.mm <- ggpredict(modelo_multinivel, terms = c("age_scaled"))
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +         
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # añadimos una banda de error
    geom_point(data = data_bounce_time,                      # añadimos los valores escalados
               aes(x = age_scaled, y = bounce_time, colour = county)) + 
    labs(x = "EDAD", y = "TIEMPO DE REBOTE", 
         title = "REGRESIÓN MULTINIVEL - 2 NIVELES") + 
    theme_minimal()
)

#Con estas representaciones también podemos estimar donde estaría una persona que se incorporase al análisis según la edad que tenga y el condado donde viva.


#Hasta aquí tendríamos un modelo de regresión multinivel de dos niveles. Sin emabrgo, también podemos hacer uno de tres niveles, incluyendo la localización:

#5.- ANIDACIÓN DE LOS EFECTOS ALEATORIOS:
#Los datos recopilados sobre el tiempo de rebote no solo se han hecho en múltiples condados, sino también en varias localizaciones dentro de esos condados. 
#Al igual que hemos hecho con los condados, tenemos que asumir que los datos recopilados dentro de la variable "location" podrían estar correlacionados.
#A consecuencia de esto, vamos a incluir la localización como un efecto aleatorio adicional en nuestro modelo.

#Para anidar la localizaicón dentro de los condados usamos la sintaxis: (1|county/location)
modelo_multinivel_tres <- lmer(bounce_time ~ age_scaled + (1|county) + (1|county/location), data = data_bounce_time)  
summary(modelo_multinivel_tres)
#Si nos fijamos en el resumen, vemos que ahora, en vez de tener 8 condados y 3 localizaciones, tenemos 24 observaciones para los 8 condados. 
#De esta forma, estamos tratando de tener en cuenta todas las influencias a nivel de condado y todas las influencias a nivel de localización.

#Si representamos gráficamente este modelo, se deben ver ocho condados con tres localizaciones (diferentes puntos de color) dentro de ellas, con una línea ajustada a través de cada gráfico.
(mm_plot <- ggplot(data_bounce_time, aes(x = age, y = bounce_time, colour = location)) +
    facet_wrap(~county, nrow=2) +   #un gráfico para cada condado 
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(data_bounce_time, pred = predict(modelo_multinivel_tres)), aes(y = pred), size = 1) +  #añadimos una línea de predicción para el modelo multinivel 
    theme(legend.position =  "top",
          panel.spacing = unit(2, "lines"))  #añadimos espacio entre los gráficos
)

#Si nos fijamos en la recta, vemos que éstas son paralelas entre sí, por lo que estamos diciendo que: 
#si extrapolamos este modelo a otras áreas, esperamos que las personas de todos los condados exhiban la misma relación entre la edad y el tiempo de rebote (pendiente fija), aunque diferenciamos entre las diferentes localizaciones.
#Sin embargo, en la vida real, esto no es así, sino que existen otras muchas variables que influyen.
#Es por esto que hay que modificar el modelo, incluyendo dentro de los paréntesis la variable que, hasta ahora, hemos considerado como fija: la edad
mm3_final <- lmer(bounce_time ~ age_scaled + (1 + age_scaled|county/location), data = data_bounce_time) 
summary(mm3_final)

#De forma gráfica, ahora apreciamos esa pendiente:
(mm_plot <- ggplot(data_bounce_time, aes(x = age, y = bounce_time, colour = location)) +
    facet_wrap(~county, nrow=2) +  #un gráfico para cada condado 
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(data_bounce_time, pred = predict(mm3_final)), aes(y = pred), size = 1) +  #añadimos una línea de predicción para el modelo multinivel
    theme(legend.position = "top",
          panel.spacing = unit(2, "lines"))   #añadimos espacio entre los gráficos
)


#6.- PRESENTACIÓN DE RESULTADOS DEL MODELO DE REGRESIÓN MULTINIVEL DE TRES NIVELES CON RECTA DE PREDICCIÓN: 
#Volvemos a cargar el paquete "ggeffects"
install.packages("ggeffects")
library(ggeffects)
pred.mm <- ggpredict(mm3_final, terms = c("age_scaled"))
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +         
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # añadimos una banda de error
    geom_point(data = data_bounce_time,                      # añadimos los valores escalados
               aes(x = age_scaled, y = bounce_time, colour = county)) + 
    labs(x = "EDAD", y = "TIEMPO DE REBOTE", 
         title = "REGRESIÓN MULTINIVEL - 3 NIVELES") + 
    theme_minimal()
)


#FIN :)


