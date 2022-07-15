----
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

```{r}
DFheartdisease %>% 
  drop_na() %>%
  group_by(HeartDisease) %>%
  count() %>% 
  ungroup() %>%
  kable(align = rep("c", 2))
```

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
Vemos los quintiles de la variable edad

```{r}
quantile(DFheartdisease$Edad,prob=c(0,0.20,0.4,0.6,0.8,1))
```


##Hay unas cuantas cosas que queremos limpiar. Las variables sexo, resting y angina, las queremos convertir en factor para nuestro modelo de machine learning.

#Quitamos los valores NA's, convertimos a factores las variables sexo, resting_ecg y angina agrupamos la variable de destino en dos niveles, eliminamos y reordenamos variables.

#convertimos la variable edad en factores. Siguiendo los grupos que nos marcan los quintiles.
#Recodificamos valores numericos como la edad en factores. Para ello, primero vamos a ver cuántos casos tenemos en cada intervalo.


```{r}
table(cut(DFheartdisease$Edad, 5))
#Recodificamos valores numericos como la edad en factores

```
#reetiquetamos los diferentes grupos

```{r}
DFheartdisease4 <-DFheartdisease  %>%
  select(Sexo,
         Resting_ECG,
         Angina,
         Edad) %>%
  mutate(Sexo = recode_factor(Sexo, `0` = "mujer", 
                                  `1` = "hombre" ),
         Resting_ECG = recode_factor(Resting_ECG,
         `0` = "normal",
         `1`="ST-Tabnormality",
         `2`="LVhypertrophy"),'
        Angina=recode_factor(Angina,
        `N`="NO",
        `Y`="Si",
        Edad=recode_factor(Edad,
                           (28,37.8]='De 28 a 37" 
                           (37.8,47.6]= "De 38 a 47"
                           (47.6,57.4]= "De 48 a 57"
                           (57.4,67.2]="De 58 a 67"
                           (67.2,77]= "De 68 a 77"
                           )
gather(key = "key", value = "value", Diagnosis_Heart_Disease)
```


```{r}
table(DFheartdisease)
```




```{r}

DFheartdisease2<- DFheartdisease %>%
  mutate(across(where(is.character), as.factor),
        Edad =as.factor(Edad)) %>% 
  # Remove any non complete cases
  na.omit()
is.factor(DFheartdisease2$Edad)
```
#lo mismo para Angina
# Comprobar que la variable Angina ahora es un factor

```{r}

DFheartdisease2<- DFheartdisease %>%
  mutate(across(where(is.character), as.factor),
        Angina =as.factor(HeartDisease)) %>% 
  # Remove any non complete cases
  na.omit()
is.factor(DFheartdisease2$Angina)
```
#Comprobar que la variable Resting_ECG es un factor

```{r}
DFheartdisease2<- DFheartdisease %>%
  mutate(across(where(is.character), as.factor),
        Resting_ECG =as.factor(HeartDisease)) %>% 
  # Remove any non complete cases
  na.omit()
is.factor(DFheartdisease2$Resting_ECG)

```
#Comprobar que la variable heartdisease es un factor

```{r}

DFheartdisease2<- DFheartdisease %>%
  mutate(across(where(is.character), as.factor),
        HeartDisease =as.factor(HeartDisease)) %>% 
  # Remove any non complete cases
  na.omit()
is.factor(DFheartdisease2$HeartDisease)
```

#Glimpse data

```{r}

  glimpse(DFheartdisease2)
```

## También se pueden incluir gráficos:
```{r}

plot(DFheartdisease2$Edad,echo=FALSE)

```
``

Nota: El parámetro`echo = FALSE` se añadió para evitar imprimir el código generado con el gráfico.


Para los modelos de machine learning, se recomienda dividir los datos entre datos de entrenamiento y test. También se puede utilizar otros métodos como el "K-Fold Cross Validation"; de todos modos, para este ejercicio seguiremos con el método de división tradicional.

Para conseguir esto, y asegurarnos de que los resultados son repetibles, utilizaremos "set.seed(123) value" – que esencialmente nos dice que estamos dividiendo los datos de manera aleatoria, y nos aseguramos de que el patrón aleatorio es el mismo que el tutorial, es decir que da la misma división que esta publicación.

vamos a quitar todos los NA del modelo antes de aplicar ML modelo.

```{r}
DF3<-DFheartdisease2[complete.cases(DFheartdisease2),]
```


```{r}
set.seed(123)
split_prop <- 0.8
testing_prop <- 1 - split_prop
split <- rsample::initial_split(hd, prop = split_prop)
training <- rsample::training(split)
testing <- rsample::testing(split)
# Print a custom message to show the samples involved
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
## The training set has: 734 examples and the testing set has:184.
## This split has 80% in the training set and 20% in the testing set.
## 
```
Podemos ajustar un modelo de parsnip al conjunto de entrenamiento y luego podemos evaluar el rendimiento.
```{r}
lr_hd_fit <- logistic_reg() %>%
  set_engine("glm") %>% 
  set_mode("classification") %>% 
  fit(DFheartdisease2$HeartDisease ~ ., data = training)
```


```{r}
fit(., DFheartdisease2$HeartDisease ~ ., data = training)


```

Si queremos ver los resultados resumidos de nuestro modelo de forma ordenada (es decir, un marco de datos con nombres de columna estándar), podemos usar la función tidy():

```{r}
tidy(lr_hd_fit)

```


#seleccionar variables categóricas, recodificarlas a sus valores, convertirlas en formato largo

```{r}
HDgrantabla <-DFheartdisease  %>%
  select(Sexo,
         Resting_ECG,
         Angina) %>%
  mutate(Sexo = recode_factor(Sexo, `0` = "mujer", 
                                  `1` = "hombre" ),
         Resting_ECG = recode_factor(Resting_ECG,
         `0` = "normal",
         `1`="ST-Tabnormality",
         `2`="LVhypertrophy"),
        Angina=recode_factor(Angina,
        `N`="NO",
        `Y`="Si",
gather(key = "key", value = "value", Diagnosis_Heart_Disease)
```

#Visualize with bar plot
hd_long_fact_tbl %>% 
  ggplot(aes(value)) +
    geom_bar(aes(x        = value, 
                 fill     = Diagnosis_Heart_Disease), 
                 alpha    = .6, 
                 position = "dodge", 
                 color    = "black",
                 width    = .8
             ) +
    labs(x = "",
         y = "",
         title = "Scaled Effect of Categorical Variables") +
    theme(
         axis.text.y  = element_blank(),
         axis.ticks.y = element_blank()) +
    facet_wrap(~ key, scales = "free", nrow = 4) +
    scale_fill_manual(
         values = c("#fde725ff", "#20a486ff"),
         name   = "Heart\nDisease",
         labels = c("No HD", "Yes HD"))  %>%
  select(Sex,
         Chest_Pain_Type,
         Fasting_Blood_Sugar,
         Resting_ECG,
         Exercise_Induced_Angina,
         Peak_Exercise_ST_Segment,
         Thalassemia,
         Diagnosis_Heart_Disease) %>%
  mutate(Sex = recode_factor(Sex, `0` = "female", 
                                  `1` = "male" ),
         Chest_Pain_Type = recode_factor(Chest_Pain_Type, `1` = "typical",   
                                                          `2` = "atypical",
                                                          `3` = "non-angina", 
                                                          `4` = "asymptomatic"),
         Fasting_Blood_Sugar = recode_factor(Fasting_Blood_Sugar, `0` = "<= 120 mg/dl", 
                                                                  `1` = "> 120 mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                                  `1` = "ST-T abnormality",
                                                  `2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina, `0` = "no",
                                                                          `1` = "yes"),
         Peak_Exercise_ST_Segment = recode_factor(Peak_Exercise_ST_Segment, `1` = "up-sloaping",
                                                                            `2` = "flat",
                                                                            `3` = "down-sloaping"),
         Thalassemia = recode_factor(Thalassemia, `3` = "normal",
                                                  `6` = "fixed defect",
                                                  `7` = "reversible defect")) %>%
  gather(key = "key", value = "value", -Diagnosis_Heart_Disease)

#Visualize with bar plot
hd_long_fact_tbl %>% 
  ggplot(aes(value)) +
    geom_bar(aes(x        = value, 
                 fill     = Diagnosis_Heart_Disease), 
                 alpha    = .6, 
                 position = "dodge", 
                 color    = "black",
                 width    = .8
             ) +
    labs(x = "",
         y = "",
         title = "Scaled Effect of Categorical Variables") +
    theme(
         axis.text.y  = element_blank(),
         axis.ticks.y = element_blank()) +
    facet_wrap(~ key, scales = "free", nrow = 4) +
    scale_fill_manual(
         values = c("#fde725ff", "#20a486ff"),
         name   = "Heart\nDisease",
         labels = c("No HD", "Yes HD"))  %>%
  select(Sex,
         Chest_Pain_Type,
         Fasting_Blood_Sugar,
         Resting_ECG,
         Exercise_Induced_Angina,
         Peak_Exercise_ST_Segment,
         Thalassemia,
         Diagnosis_Heart_Disease) %>%
  mutate(Sex = recode_factor(Sex, `0` = "female", 
                                  `1` = "male" ),
         Chest_Pain_Type = recode_factor(Chest_Pain_Type, `1` = "typical",   
                                                          `2` = "atypical",
                                                          `3` = "non-angina", 
                                                          `4` = "asymptomatic"),
         Fasting_Blood_Sugar = recode_factor(Fasting_Blood_Sugar, `0` = "<= 120 mg/dl", 
                                                                  `1` = "> 120 mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                                  `1` = "ST-T abnormality",
                                                  `2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina, `0` = "no",
                                                                          `1` = "yes"),
         Peak_Exercise_ST_Segment = recode_factor(Peak_Exercise_ST_Segment, `1` = "up-sloaping",
                                                                            `2` = "flat",
                                                                            `3` = "down-sloaping"),
         Thalassemia = recode_factor(Thalassemia, `3` = "normal",
                                                  `6` = "fixed defect",
                                                  `7` = "reversible defect")) %>%
  gather(key = "key", value = "value", -Diagnosis_Heart_Disease)

#Visualize with bar plot
hd_long_fact_tbl %>% 
  ggplot(aes(value)) +
    geom_bar(aes(x        = value, 
                 fill     = Diagnosis_Heart_Disease), 
                 alpha    = .6, 
                 position = "dodge", 
                 color    = "black",
                 width    = .8
             ) +
    labs(x = "",
         y = "",
         title = "Scaled Effect of Categorical Variables") +
    theme(
         axis.text.y  = element_blank(),
         axis.ticks.y = element_blank()) +
    facet_wrap(~ key, scales = "free", nrow = 4) +
    scale_fill_manual(
         values = c("#fde725ff", "#20a486ff"),
         name   = "Heart\nDisease",
         labels = c("No HD", "Yes HD"))
