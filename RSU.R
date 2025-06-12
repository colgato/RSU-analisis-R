#ANALISIS DE RSU
#libreria para utilizar excel
library(readxl)
library(stats)
#file.choose() #esta funcion muestra la ruta del archivo DESCOMENTA ESTA FUNCION SI NO CONOCES TU RUTA
ruta_excel <- "Aqui pon tu ruta de EXCELL"
excel_sheets(ruta_excel)
hoja <- read_excel(ruta_excel, sheet="RSU")
#selección de variables por backward
model.backward<-lm(formula = hoja$`RSU t/diax100milH` ~ hoja$PIB + 
                     hoja$`Incidencia delictiva`+
                     hoja$`Esperanza educativa`+ 
                     hoja$`Ingreso corriente anual`+
                     hoja$`Número de habitantes por km cuadrado`+ 
                     hoja$`Pobreza por 100 mil habitantes`, data = hoja)
summary(model.backward)
library(MASS)  # Para poder usar la funcion stepAIC que es el metodo de seleccion
modback <- stepAIC(model.backward, trace=TRUE, direction="backward")
#seleccion de variables por forward
#modelo vacio para el forward
model.forward <- lm(hoja$`RSU t/diax100milH`~ 1, data=hoja)
#objetivo que tiene el forward
horizonte <- formula(hoja$`RSU t/diax100milH` ~ hoja$PIB + 
                       hoja$`Incidencia delictiva`+
                       hoja$`Esperanza educativa`+ 
                       hoja$`Ingreso corriente anual`+
                       hoja$`Número de habitantes por km cuadrado`+ 
                       hoja$`Pobreza por 100 mil habitantes`, data = hoja)
modforw <- stepAIC(model.forward, trace=FALSE, direction="forward", scope=horizonte)
#seleccion de variables para stepwise
#modelo vacio para stepwise
model.stepwise <- lm(hoja$`RSU t/diax100milH` ~ 1, data=hoja)
modboth <- stepAIC(model.stepwise, trace=FALSE, direction="both", scope=horizonte)


#Resultado backward
summary(modback)
#Resultado forward
summary(modforw)
#Resultado stepwise
summary(modboth)

#transformar con logaritmos las variables más cercanas a ser significativas
log_PIB <- log(hoja$PIB)
log_esperanza <- log(hoja$`Esperanza educativa`)
#transformar a reciprocos las variables más cercanas a ser significativas
reciproco_PIB <- 1 / hoja$PIB
reciproco_esperanza <- 1 / hoja$`Esperanza educativa`
#hacer el modelo de regresion con los nuevos logaritmos
model.log<-lm(formula = hoja$`RSU t/diax100milH` ~ log_PIB + log_esperanza)
#hacer el modelo de regresion con los reciprocos
model.rec<-lm(formula=hoja$`RSU t/diax100milH` ~ reciproco_PIB + reciproco_esperanza)
#combinar reciprocos y logaritmos
model.logrec<-lm(formula=hoja$`RSU t/diax100milH` ~ log_PIB + reciproco_esperanza)
model.reclog<-lm(formula=hoja$`RSU t/diax100milH` ~ reciproco_PIB + log_esperanza)


#comparación de resultados
summary(model.log)
summary(model.rec)
summary(model.logrec)
summary(model.reclog)

#modelo de regresión lineal
model.logPIB<-lm(formula=hoja$`RSU t/diax100milH`~ log_PIB)
summary(model.logPIB)
#comprobación de supuestos teóricos
residuales <- residuals(model.logPIB)
#media cero (independencia)
t.test(residuales)
#bptest nula homocedasticidad (varianza constante), 
#alternativa heterocedasticidad (varianza no constante)
#comprobar homcedasticidad (nula)
library(lmtest)
bptest(model.logPIB)
#dwtest nula correlacion igual a cero, alternativa correlacion diferente de cero
#comprobar alternativa (que nuestro pivalue sea mayor a cero)
library(car)
dwtest(model.logPIB)
#ad.test nula normalidad (los residuos se distribuyen normalmente), 
#alternativa no normalidad
#comprobar normalidad (nula)
library(nortest)
ad.test(residuales)

# Gráficos
par(mfrow=c(2,2))  # Para mostrar varios gráficos en una cuadrícula

# Residuos vs. Valores Ajustados media cero 
plot(model.logPIB$fitted.values, residuales, main="Residuos vs. Valores Ajustados", xlab="Valores Ajustados", ylab="Residuos")
abline(h=0, col="red")  # Agregar línea en y=0

# Gráfico Q-Q varianza constante
qqnorm(residuales)
qqline(residuales)

# Gráfico de Autocorrelación de Residuos si correlacionan 
acf(residuales, main="Autocorrelación de Residuos")
# Histograma de Residuos distribuyen normalmente (normalidad)
hist(residuales, main="Histograma de Residuos", xlab="Residuos", ylab="Frecuencia", col="lightblue")

#como los supuestos no se cumplen vamos a transformar la variable objetivo
logRSU<- log(hoja$`RSU t/diax100milH`)
model.loglog<-lm(formula=logRSU~ log_PIB)
summary(model.logPIB)

#comprobación de supuestos teóricos
residualeslog <- residuals(model.loglog)
#media cero (independencia)
t.test(residualeslog)
#bptest nula homocedasticidad (varianza constante), 
#alternativa heterocedasticidad (varianza no constante)
#comprobar homcedasticidad (nula)
library(lmtest)
bptest(model.loglog)
#dwtest nula correlacion igual a cero, alternativa correlacion diferente de cero
#comprobar alternativa (que nuestro pivalue sea mayor a cero)
library(car)
dwtest(model.loglog)
#ad.test nula normalidad (los residuos se distribuyen normalmente), 
#alternativa no normalidad
#comprobar normalidad (nula)
library(nortest)
ad.test(residualeslog)

# Gráficos
par(mfrow=c(2,2))  # Para mostrar varios gráficos en una cuadrícula

# Residuos vs. Valores Ajustados media cero 
plot(model.loglog$fitted.values, residualeslog, main="Residuos vs. Valores Ajustados", xlab="Valores Ajustados", ylab="Residuos")
abline(h=0, col="red")  # Agregar línea en y=0

# Gráfico Q-Q varianza constante
qqnorm(residualeslog)
qqline(residualeslog)

# Gráfico de Autocorrelación de Residuos si correlacionan 
acf(residualeslog, main="Autocorrelación de Residuos")
# Histograma de Residuos distribuyen normalmente (normalidad)
hist(residualeslog, main="Histograma de Residuos", xlab="Residuos", ylab="Frecuencia", col="lightblue")

