---
Título: "Detección anomalías cardiacas"
Autor: "Silvia Antón"
Fecha: "`r Sys.Date()`"
output: html_document
---
#Cargamos las librerías

```{r, message=FALSE, warning=FALSE, results="hide"}
check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

check.packages(c("knitr","ggplot2", "tidymodels", "MLDataR", "stringi", "dplyr", "tidyr","data.table","ConfusionTableR","OddsPlotty","rmarkdown","kableExtra","devtools","summarytools"))
```
## R Markdown

Esto es un documento de Markdown. Markdown es una sintáxis de formato simple para crear documentosHTML, PDF, and MS Word documents. PAra más detalles consulte <http://rmarkdown.rstudio.com>.

Cuando haces click en el botón **Knit** se generará un documento que incluye tanto  includes el contendio como la salida de cualquier fragmento de código R incrustado dentro del documento.
#Leemos los datos
```{r}
data("heartdisease")
str(heartdisease)
DFheartdisease<-heartdisease
```

``
El dataset es para utilizar con modelos de clasificación de  ML spervisados para clasificar las enfermedades cardiacas.

#'  Un data frame con 918 filas y 10 variables:
#' \descripción: {
#'   \item{Age}{edad del paciente}
#'   \item{Sex}{genero del paciente}
#'   \item{RestingBP}{presión arterial en descanso}
#'   \item{Cholesterol}{lectura colesterol}
#'   \item{FastingBS}{muestras de sangre de glucosas después de esfuerzo \url{https://www.diabetes.co.uk/diabetes_care/fasting-blood-sugar-levels.html}}
#'   \item{RestingECG}{elecrocardiograma en descanso es un indicador de infartoe.g. heart attack}
#'   \item{MaxHR}{máxima frecuencia cardiaca}
#'   \item{Angina}{dolor de pecho causado por disminución del riego sanguineo\url{https://www.nhs.uk/conditions/angina/}}
#'   \item{HeartPeakReading}{pico de frencuencia cardiaca}
#'   \item{HeartDisease}{clasificación de si tiene enfermedad cardiaca o no}
Preparamos los nombres de las columnas


```{r}

names <- c("Edad",
           "Sexo",
           "Presion_descanso",
           "Colesterol",
           "Fasting_BS",
           "Resting_ECG",
           "MAxHR",
           "Angina",
           "HeartPeakREading",
           "HeartDisease")
```

#Aplicamos los nombres de columna al dataset

```{r}
colnames(DFheartdisease) <- names
```

#Echamos un vistazo para verificar que los nombres de las columnas se han cambiado

```{r}
DFheartdisease %>% glimpse()
```

#Primer resumen de las caracteristicas del data set

```{r}

summary(DFheartdisease)



```
#otro modo de verlo


```{r}
print(dfSummary(DFheartdisease,graph.magnif = 0.75), method = 'render')
```

#Queremos ver el numero de casos en cada nivel de las variables
``
#variable sexo
```{r}
DFheartdisease %>% 
  drop_na() %>%
  group_by(Sexo) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2)) 
```

#variable resting ECG
```{r}

  DFheartdisease %>% 
  drop_na() %>%
  group_by(Resting_ECG) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2))

```
  
#Variable Angina
  
 
```{r}
  DFheartdisease %>% 
    drop_na() %>%
    group_by(Angina) %>%
    count() %>% 
    ungroup() %>%
    kable(align = rep("c", 2))
  
```
```#variable edad
```{r}
DFheartdisease %>% 
    drop_na() %>%
    group_by(Edad) %>%
    count() %>% 
    ungroup() %>%
    kable(align = rep("c", 2))


```



##Hay unas cuantas cosas que queremos limpiar. Las variables sexo, resting y angina, las queremos convertir en factor para nuestro modelo de machine learning.

#Quitamos los valores NA's, convertimos a factores las variables sexo, resting_ecg y angina agrupamos la variable de destino en dos niveles, eliminamos y reordenamos variables.

#convertimos la variable edad en factores. Siguiendo los grupos que nos marcan los quintiles.
#Recodificamos valores numericos como la edad en factores. Para ello, primero vamos a ver cuántos casos tenemos en cada intervalo.


```{r}
DF<-DFheartdisease%>% mutate(fct_Edad = cut(Edad, 
                        breaks = quantile(Edad, prob=c(0,0.20,0.4,0.6,0.8,1)), 
                        include.lowest = TRUE)
  )

class(as.data.frame(DF))
```
#Identificamos los distintos grupos de edad:

```{r}

DF%>% 
  group_by(fct_Edad) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2)) %>% kable_styling("full_width" = F)
```


```{r}
DF %>% glimpse()
```

```{r}
#reetiquetamos los diferentes grupos

DF %>%
  select(Sexo,
         Resting_ECG,
         Angina,
                everything()) %>%
  mutate(Sexo = recode_factor(Sexo, `0` = "mujer", 
                                  `1` = "hombre" ),
         Resting_ECG = recode_factor(Resting_ECG,
         `0` = "normal",
         `1`="ST-Tabnormality",
         `2`="LVhypertrophy"),
         Angina=recode_factor(Angina,
        `N`="NO",
        `Y`="Yes"))
 
        
      
```






```{r}

  # Remove any non complete cases
  na.omit(DF)

```
#lo mismo para Angina
# Comprobar que la variable Angina ahora es un factor

```{r}

DF<- DF %>%
  mutate(across(where(is.character), as.factor),
        Angina =as.factor(HeartDisease)) %>% 
  # Remove any non complete cases
  na.omit()
is.factor(DFheartdisease2$Angina)
```
#Comprobar que la variable Resting_ECG es un factor

```{r}
DF<- DF%>%
  mutate(across(where(is.character), as.factor),
        Resting_ECG =as.factor(HeartDisease)) %>% 
  # Remove any non complete cases
  na.omit()
is.factor(DFheartdisease2$Resting_ECG)

```

#Glimpse data

```{r}

 str(DF)

#Visualizamos las variables con un grafico de barras



#Must gather() data first in order to facet wrap by key 

```
```{r}

DF  %>%
  select(Colesterol,
         Fasting_BS,
         Resting_ECG,
         Angina,
         HeartDisease,
         fct_Edad) %>% 
  gather(key   = "key", 
         value = "value",
         HeartDisease)
```
#visualizamos la media de colesterol por grupo de edad

```{r}
set.seed(123)
tapply(DF$Colesterol,DF$fct_Edad,summary)
```


#Visualize numeric variables as boxplots
```{r}
library(ggplot2)

ggplot(data=DF,aes(x=fct_Edad,Y=mean(Colesterol),fill=fct_Edad))+geom_boxplot()+
  scale_fill_manual(breaks = DF$fct_Edad,
                    values = c("#1b98e0", "#353436", "yellow", "red", "green"))+
  labs(x="Grupo de Edad",y="Valor Medio colesterol",title="Colesterol por grupos de Edad")


```

Nota: El parámetro`echo = FALSE` se añadió para evitar imprimir el código generado con el gráfico.


Para los modelos de machine learning, se recomienda dividir los datos entre datos de entrenamiento y test. También se puede utilizar otros métodos como el "K-Fold Cross Validation"; de todos modos, para este ejercicio seguiremos con el método de división tradicional.

Para conseguir esto, y asegurarnos de que los resultados son repetibles, utilizaremos "set.seed(123) value" – que esencialmente nos dice que estamos dividiendo los datos de manera aleatoria, y nos aseguramos de que el patrón aleatorio es el mismo que el tutorial, es decir que da la misma división que esta publicación



```{r}
set.seed(123)
split_prop <- 0.8
testing_prop <- 1 - split_prop
split <- rsample::initial_split(hd, prop = split_prop)
training <- rsample::training(split)
testing <- rsample::testing(split)
```

# Print a custom message to show the samples involved
```{r}
training_message <- function() {
  message(
    cat(
      'The training set has: ',
      nrow(training),
      ' examples and the testing set has:',
      nrow(testing),
      '.\nThis split has ',
      paste0(format(100 * split_prop), '%'),
      ' in the training set and ',
      paste0(format(100 * testing_prop), '%'),
      ' in the testing set.',
      sep = ''
    )
  )
}
training_message()
```
```
#Podemos ajustar un modelo de parsnip al conjunto de entrenamiento y luego podemos evaluar el rendimiento.


```{r}
logistic_reg() %>% set_engine("glm") %>% set_mode("classification") %>% 
fit(DF$HeartDisease ~ ., data = training)
lr_hd_fit <- logistic_reg() %>%
  set_engine("glm") %>% 
  set_mode("classification") %>% 
  fit(DF$HeartDisease  ~ ., data = training)

fit(., DF3$HeartDisease ~ ., data = training)


```

Si queremos ver los resultados resumidos de nuestro modelo de forma ordenada (es decir, un marco de datos con nombres de columna estándar), podemos usar la función tidy():

```{r}
tidy(lr_hd_fit)

```

