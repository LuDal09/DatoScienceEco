### Trabajo Series de tiempo 
library(tseries)
library(forecast)
library(readxl)
library(RcmdrMisc)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(TSstudio)
library(lmtest)


calzado <- read_excel("calzado.xlsx")
View(calzado)     

yt2<- ts(calzado$Importaciones, start = c(2001, 1), end = c(2010, 1) , frequency = 4)
yt2
end(yt2)
autoplot(yt2) 
yt.c=tsclean(yt2)


### Tendencia 


numSummary(calzado[,'Importaciones'],groups=calzado$Año,
           statistic=c('mean','sd','cv','quantiles'), quantiles=c(0.25,0.5,0.75))


ts_plot(yt.c,
        title = "Fabricacion  Industrial de calzado",
        Ytitle = "Importacion calzado",
        Xtitle = "Fuente: DANE",
        slider = TRUE,
        line.mode =  "lines+markers")

 
ts_plot(yt.c,
        title = "Fabricacion  Industrial de calzado",
        Ytitle = "Importacion calzado",
        Xtitle = "Fuente: DANE",
        slider = TRUE,)

### Estacionalidad 

ts_seasonal(yt.c, type = "normal", title = "Descomposición Estacional", Ygrid = TRUE, Xgrid = TRUE, last = NULL, palette = "Set1", palette_normal = "viridis")



# Tradicional

ggseasonplot(yt.c, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Toneladas") +xlab("Mes")+
  ggtitle("Gráfico de estacionalidad:Produccion indrustial calzado")

ggplot(calzado, aes(x=as.factor(Trimestre), y=Importaciones, fill=as.factor(Trimestre))) + 
  geom_boxplot(alpha=0.7) +
  theme(legend.position="none")+xlab('Mes')

ggseasonplot(yt.c, polar=TRUE) +
  ylab("Toneladas") +
  ggtitle("Gráfico polar de estacionalidad: Produccion indrustial calzado")

ggsubseriesplot(yt.c) +
  ylab("Toneladas") +xlab("Mes")+
  ggtitle("Gráfico de estacionalidad: Produccion indrustial calzado")
### Enfoque clasico

###Escogemos el modelo_Multiplicativo 
Percent=calzado %>% 
  group_by(as.factor(calzado$Año)) %>% 
  do(data.frame(t(quantile(.$Importaciones, probs = c(0.2, .5, .80)))))

plot(log(Percent$X50.), log(Percent$X80.-Percent$X20.), xlab='Nivel', ylab='Dispersión', col='red')

### Particionamos los datos 
h=1:4

Desem_split1=ts_split(yt2, sample.out = h[1])
Desem_split2=ts_split(yt2, sample.out = h[2])
Desem_split3=ts_split(yt2, sample.out = h[3])
Desem_split4=ts_split(yt2, sample.out = h[4])


### Datos Entrenamiento
train1=Desem_split1$train
train2=Desem_split2$train
train3=Desem_split3$train
train4=Desem_split4$train


### Datos Prueba
test1=Desem_split1$test
test2=Desem_split2$test
test3=Desem_split3$test
test4=Desem_split4$test



ts_info(train1)
ts_info(train2)
ts_info(train3)
ts_info(train4)


ts_info(test1)
ts_info(test2)
ts_info(test3)
ts_info(test4)


### Descomposición de la serie temporal
### Estimación de la tendencia 
M.t1 <- ma(yt2, order = 1, centre = T)
M.t2 <- ma(yt2, order = 2, centre = T)
M.t3 <- ma(yt2, order = 3, centre = T)
M.t4 <- ma(yt2, order = 4, centre = T)

autoplot(ts(M.t1))
autoplot(ts(M.t2))
autoplot(ts(M.t3))
autoplot(ts(M.t4))



### Usando datos de entrenamiento

desc.m1=decompose(train1, "multiplicative")
desc.m2=decompose(train2, "multiplicative")
desc.m3=decompose(train3, "multiplicative")
desc.m4=decompose(train4, "multiplicative")

autoplot(desc.m1)  + 
  ggtitle("Descomposición Multiplicativa 1")
autoplot(desc.m2) + 
  ggtitle("Descomposición Multiplicativa 2")
autoplot(desc.m3) + 
  ggtitle("Descomposición Multiplicativa 3")
autoplot(desc.m4) + 
  ggtitle("Descomposición Multiplicativa 4")

desc.m1$figure
desc.m1$trend
desc.m1$seasonal
desc.m1$random


desc.m2$figure
desc.m2$trend
desc.m2$seasonal
desc.m2$random

desc.m3$figure
desc.m3$trend
desc.m3$seasonal
desc.m3$random

desc.m4$figure
desc.m4$trend
desc.m4$seasonal
desc.m4$random


### Ajuste del modelo
# yt = St*Tt*Et

St1=desc.m1$seasonal
Tt1=desc.m1$trend
Et1=desc.m1$random


St2=desc.m1$seasonal
Tt2=desc.m1$trend
Et2=desc.m1$random

St3=desc.m3$seasonal
Tt3=desc.m3$trend
Et3=desc.m3$random

St4=desc.m4$seasonal
Tt4=desc.m4$trend
Et4=desc.m4$random


yt.e1=St1*Tt1*Et1
yt.e2=St2*Tt2*Et2
yt.e3=St3*Tt3*Et3
yt.e4=St4*Tt4*Et4


y.real1=desc.m1$x
y.real2=desc.m2$x
y.real3=desc.m3$x
y.real4=desc.m4$x



plot(y.real4, ylab='importacion indrustial calzado',xlab="comparacion de datos reales y ajuste-modelo 4", col='blue')
lines(yt.e4, col='red', lty=2)
legend("top", legend=c("Real", "Estimado"),
       col=c("blue", "red"), lty=1:2, cex=0.5)

 

plot(y.real3, ylab='importacion indrustial calzado',xlab="comparacion de datos reales y ajuste-modelo 3", col='blue')
lines(yt.e3, col='red', lty=2)
legend("top", legend=c("Real", "Estimado"),
       col=c("blue", "red"), lty=1:2, cex=0.4)


plot(y.real2, ylab='importacion indrustial calzado',xlab="comparacion de datos reales y ajuste-modelo 2", col='blue')
lines(yt.e2, col='red', lty=2)
legend("top", legend=c("Real", "Estimado"),
       col=c("blue", "red"), lty=1:2, cex=0.4)

 
plot(y.real1, ylab='importacion indrustial calzado',xlab="comparacion de datos reales y ajuste-modelo 1
     ", col='blue')
lines(yt.e1, col='red', lty=2)
legend("top", legend=c("Real", "Estimado"),
       col=c("blue", "red"), lty=1:2, cex=0.4)


### Ajustar el modelo 
#P1: Ajustar la tendencia de la serie
n=length(yt2)
n.entre=n-h

model1 = lm(Importaciones ~ t, data=calzado[1:n.entre[1],])
model2 = lm(Importaciones ~ t, data=calzado[1:n.entre[2],])
model3 = lm(Importaciones ~ t, data=calzado[1:n.entre[3],])
model4 = lm(Importaciones ~ t, data=calzado[1:n.entre[4],])


model1
model2
model3
model4

#P2: Obtener los pronósticos de la tendencia para los datos de prueba
prueba1=calzado[(n.entre[1]+1):n,]
prueba2=calzado[(n.entre[2]+1):n,]
prueba3=calzado[(n.entre[3]+1):n,]
prueba4=calzado[(n.entre[4]+1):n,]


Tt.e1=predict(model1,prueba1); Tt.e1
Tt.e2=predict(model2,prueba2); Tt.e2
Tt.e3=predict(model3,prueba3); Tt.e3
Tt.e4=predict(model4,prueba4); Tt.e4


#P3: Obtener los pronósticos de la variable de interés en los 
#    horizontes, considerando el tipo de modelo

St.e1=desc.m1$figure; St.e1
St.e2=desc.m2$figure; St.e2
St.e3=desc.m3$figure; St.e3
St.e4=desc.m4$figure; St.e4


St.e1=desc.m1$figure[2]
St.e2=desc.m2$figure[c(1, 2)]
St.e3=desc.m3$figure[c(4, 1, 2)]
St.e4=desc.m4$figure[c(3, 4, 1, 2)]

yt.est1=Tt.e1*St.e1
yt.est2=Tt.e2*St.e2
yt.est3=Tt.e3*St.e3
yt.est4=Tt.e4*St.e4


#P4:Calcule e interprete la(s) métricas(s) de pronóstico, usando los datos de prueba
n.entre=n-h
y.realp1=calzado$Importaciones[(n.entre[1]+1):n];y.realp1
y.realp3=calzado$Importaciones[(n.entre[2]+1):n];y.realp3
y.realp2=calzado$Importaciones[(n.entre[3]+1):n];y.realp2
y.realp4=calzado$Importaciones[(n.entre[4]+1):n];y.realp4
 

### Particionamos los datos para cada horizonte de pronostico 
h <- 1:4
PIB_split <- lapply(h, function(x) ts_split(yt2, sample.out = x))

### Datos de entrenamiento y prueba
train <- lapply(PIB_split, function(x) x$train)
test <- lapply(PIB_split, function(x) x$test)

invisible(lapply(train, ts_info))
invisible(lapply(test, ts_info))


### Descomposición de la serie temporal
### Estimación de la tendencia
M.t <- lapply(1:4, function(i) ma(yt2, order = i, centre = T))
lapply(M.t, autoplot)
autoplot(M.t[[1]])


### Usando datos de entrenamiento
### Descomposición y ajuste de modelo multiplicativo
desc.m <- lapply(train, function(x) decompose(x, "multiplicative"))
lapply(desc.m, autoplot)
autoplot(desc.m[[4]])

autoplot(desc.m[[4]], series = "Trend") +
  labs(title = "Descomposición Multiplicativa: Modelo 4",
       y = "Valor",
       x = "Tiempo") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none")







# Ajuste del modelo clasico yt = St * Tt * Et 
models <- lapply(desc.m, function(desc) {
  list(
    trend = desc$trend,
    seasonal = desc$seasonal,
    random = desc$random,
    fitted = desc$trend * desc$seasonal * desc$random
  )
})


sc.e <- lapply(models, function(model) model$fitted)
y_real <- lapply(desc.m, [[, "x")
### Verificamos igualdad 
stopifnot(length(sc.e) == length(y_real))


### Limpiar 
cleaned_real_series <- lapply(y_real, function(x) x[complete.cases(x)])
cleaned_fitted_series <- lapply(sc.e, function(x) x[complete.cases(x)])
stopifnot(all(sapply(cleaned_real_series, function(x) all(is.finite(x)))))
stopifnot(all(sapply(cleaned_fitted_series, function(x) all(is.finite(x)))))

### Graficar
### Retrocede en plots para ver las graficas anteriores 
for (i in seq_along(cleaned_fitted_series)) {
  plot(cleaned_real_series[[i]], type = "l", col = "blue", ylim = range(c(cleaned_real_series[[i]], cleaned_fitted_series[[i]]), na.rm = TRUE),
       xlab = "Observaciones", ylab = "Valor", main = paste("Comparación de valores reales y ajustados - Modelo", i))
  lines(cleaned_fitted_series[[i]], col = "red")
  legend("topleft", legend = c("Real", "Ajustado"), col = c("blue", "red"), lty = 1, cex = 0.6)
}


names(calzado)

### Ajuste del modelo
models <- lapply(1:4, function(i) lm(Importaciones ~ t, data=calzado[1:length(train[[i]]), ]))
lapply(models, summary)
checkresiduals(models[[4]])

models[[4]]

### Pronósticos de la tendencia para los datos de prueba
n = length(yt2)
n.entre=n-h
Tt.e <- lapply(1:4, function(i) predict(models[[i]], newdata = calzado[(length(train[[i]]) + 1):n, ]))



### Obtener pronósticos considerando el modelo multiplicativo
invisible(lapply(test, ts_info))

St.e <- list(St.e1=desc.m1$figure[1],
             St.e2=desc.m2$figure[c(4, 1)],
             St.e3=desc.m3$figure[c(4, 1, 2)],
             St.e4=desc.m4$figure[c(3, 4, 1, 2)]); St.e


St.e <- list(St.e1 = desc.m[[2]]$figure[c(1)],
             St.e2 = desc.m[[3]]$figure[c(4, 1)],
             St.e3 = desc.m[[4]]$figure[c(3, 4, 1)], 
             St.e3 = desc.m[[4]]$figure[c(2, 3, 4, 1)]); St.e


### Modelo ajustados para los datos de prueba Yt.est = * Tt.e * Ste
yt.est <- Map("*", Tt.e, St.e)

y.realp <- lapply(n.entre, function(n_range) {
  calzado$Importaciones[(n_range + 1):n]
})

###--------------------###


### Grafico
## par(mfrow = c(3, 1))
for (i in 1:4) {
  plot(y.realp[[i]], type = "o", col = "blue", ylim = range(c(y.realp[[i]], yt.est[[i]])),
       xlab = "Observaciones", ylab = "Valor", main = paste("Comparación de valores reales y estimados - Modelo", i))
  lines(yt.est[[i]], type = "o", col = "red")
  text(seq_along(y.realp[[i]]), y.realp[[i]], labels = round(y.realp[[i]], digits = 1), pos = 3, col = "blue", cex = 0.4)
  text(seq_along(yt.est[[i]]), yt.est[[i]], labels = round(yt.est[[i]], digits = 1), pos = 3, col = "red", cex = 0.4)
}
 











##### Datos de prueba
accuracy( yt.est1,y.realp1)
accuracy( yt.est2,y.realp2)
accuracy( yt.est3,y.realp3)
accuracy( yt.est4,y.realp4) 





h=1:4
# Datos de entrenamiento
y.real.e1=calzado$Importaciones[(h[1]/2 +1):(n.entre-(h[1]/2))] 
accuracy( yt.e1,y.real.e1)  

y.real.e2=calzado$Importaciones[(h[2]/2 +1):(n.entre-(h[2]/2))]
accuracy( yt.e2,y.real.e2)  
 
y.real.e3=calzado$Importaciones[(h[3]/2 +1):(n.entre-(h[3]/2))]
accuracy( yt.e3,y.real.e3)  
 

y.real.e4=calzado$Importaciones[(h[4]/2 +1):(n.entre-(h[4]/2))]
accuracy( yt.e4,y.real.e4) 
 


### Diagnostico supuestos 
#normalidad 

normalidad <-shapiro.test(resid(model1));normalidad
normalidad_2 <-shapiro.test(resid(model2));normalidad_2      
normalidad_3<-shapiro.test(resid(model3));normalidad_3 
normalidad_4<-shapiro.test(resid(model4));normalidad_4 
 

#homocedasticidad 

homocedasticidad <- bptest(model1);homocedasticidad  
homocedasticidad_2 <- bptest(model2);homocedasticidad_2 
homocedasticidad_3<- bptest(model3);homocedasticidad_3   
homocedasticidad_4 <- bptest(model4);homocedasticidad_4  
 

#prueba de independencia 
independencia <- dwtest(model1);independencia 
independenciaa<- dwtest(model2);independenciaa
independenciaaa<- dwtest(model3);independenciaaa
independenciaaaa<- dwtest(model4);independenciaaaa
