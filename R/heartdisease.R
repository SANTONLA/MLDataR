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

check.packages(c("knitr","ggplot2", "tidymodels", "MLDataR", "stringi", "dplyr", "tidyr","data.table","ConfusionTableR","OddsPlotty","rmarkdown","kableExtra","devtools"))
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

##Hay unas cuantas cosas que queremos limpiar. Las variables sexo, resting y angina, las queremos convertir en factor para nuestro modelo de machine learning.

#Quitamos los valores NA's, convertimos a factores las variables sexo, resting_ecg y angina agrupamos la variable de destino en dos niveles, eliminamos y reordenamos variables.

```{r}
DFheartdisease1<- DFheartdisease %>%
  mutate(across(where(is.character), as.factor),
         Sexo = as.factor(Sexo)) %>% 
  # Remove any non complete cases
  na.omit()
is.factor(DFheartdisease$Sexo)
```
#lo mismo para Angina
# Comprobar que la variable Angina ahora es un factor

```{r}

is.factor(DFheartdisease$Angina)
```
#Comprobar que la variable Resting_ECG es un factor

```{r}

is.factor(DFheartdisease$Resting_ECG)
```
#Glimpse data

```{r}

  glimpse(DFheartdisease)
```

## También se pueden incluir gráficos:
```{r}

plot(DFheartdisease$Sexo,echo=FALSE)

```
``

Nota: El parámetro`echo = FALSE` se añadió para evitar imprimir el código generado con el gráfico.
