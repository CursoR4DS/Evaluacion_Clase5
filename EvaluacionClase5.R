# Describir modelos AR(2) , graficarlos para valores diferentes 
# de los argumentos (ar = c(p1,p2))
# AR(2)
AR2 <- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.2)) ,
          n = 100,sd = 0.1)
# Probar varias combinaciones de p1 y p2 , graficar las series de tiempo
# simuladas, y sus correspondientes funciones de autocorrelacion simple
# y funciones de autocorrelacion parcial 

# Repetir lo mismo para los procesos MA(2) 

#### setear el directorio de trabajo ####
rm(list=ls())
setwd("C:/Users/user/Desktop/R_CTIC/Evaluacion_Clase5")
getwd()
dir()

# AR(1):y[t] = mu + phi[1]*y[t-1] + Err[t]
# AR(2):y[t] = mu + phi[1]*y[t-1] + phi[21]*y[t-2] +Err[t]

#### MODELO AUTOREGRESIVO AR[2] #####

#Para todo n=100
# Fijemos una semilla
set.seed(555)

# Simulamos los procesos AR
AR2_1<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.2)),n = 100, sd = 0.1)
AR2_2<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.4)),n = 100, sd = 0.1)
AR2_3<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.8)),n = 100, sd = 0.1)
AR2_4<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,-0.2)),n = 100, sd = 0.1)
AR2_5<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,-0.4)),n = 100, sd = 0.1)
AR2_6<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,-0.8)),n = 100, sd = 0.1)
AR2_7<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1,0.2)),n = 100, sd = 0.1)
AR2_8<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1,0.4)),n = 100, sd = 0.1)
AR2_9<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1,0.8)),n = 100, sd = 0.1)
AR2_10<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1,-0.2)),n = 100, sd = 0.1)
AR2_11<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1,-0.4)),n = 100, sd = 0.1)
AR2_12<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.1,-0.8)),n = 100, sd = 0.1)


ylm = c(min(AR2_1,AR2_2,AR2_3,AR2_4,AR2_5,AR2_6,AR2_7,AR2_8,AR2_9,AR2_10,AR2_11,AR2_12), 
        max(AR2_1,AR2_2,AR2_3,AR2_4,AR2_5,AR2_6,AR2_7,AR2_8,AR2_9,AR2_10,AR2_11,AR2_12))

#Guardamos en imagen
jpeg("Modelos_AR[2].jpeg",width= 6400, height=1820,
     units="px",res = 300)
# Grafiquemos estas series de tiempo simuladas
# Varios graficos en una misma ventana
par(mfrow = c(2,6))

plot.ts(AR2_1, ylim = ylm, main = "AR[2] p1=0.1 y p2=0.2")
plot.ts(AR2_2, ylim = ylm, main = "AR[2] p1=0.1 y p2=0.4")
plot.ts(AR2_3, ylim = ylm, main = "AR[2] p1=0.1 y p2=0.8")
plot.ts(AR2_4, ylim = ylm, main = "AR[2] p1=0.1 y p2=-0.2")
plot.ts(AR2_5, ylim = ylm, main = "AR[2] p1=0.1 y p2=-0.4")
plot.ts(AR2_6, ylim = ylm, main = "AR[2] p1=0.1 y p2=-0.8")
plot.ts(AR2_7, ylim = ylm, main = "AR[2] p1=-0.1 y p2=0.2")
plot.ts(AR2_8, ylim = ylm, main = "AR[2] p1=-0.1 y p2=0.4")
plot.ts(AR2_9, ylim = ylm, main = "AR[2] p1=-0.1 y p2=0.8")
plot.ts(AR2_10, ylim = ylm, main = "AR[2] p1=-0.1 y p2=-0.2")
plot.ts(AR2_11, ylim = ylm, main = "AR[2] p1=-0.1 y p2=-0.4")
plot.ts(AR2_12, ylim = ylm, main = "AR[2] p1=-0.1 y p2=-0.8")
dev.off()

graphics.off()

#### AUTOCORRELACION SIMPLE PARA AR[2] ####

help("acf") 
#Auto- and Cross- Covariance and -Correlation Function Estimation

#Guardamos en imagen
jpeg("AutocorrelacionSimple_AR[2].jpeg",width= 6400, height=1820,
     units="px",res = 300)
# Grafiquemos estas series de tiempo simuladas
# Varios graficos en una misma ventana
par(mfrow = c(2,6))

acf(AR2_1, main = "ACF AR[2] p1=0.1 y p2=0.2")
acf(AR2_2, main = "ACF AR[2] p1=0.1 y p2=0.4")
acf(AR2_3, main = "ACF AR[2] p1=0.1 y p2=0.8")
acf(AR2_4, main = "ACF AR[2] p1=0.1 y p2=-0.2")
acf(AR2_5, main = "ACF AR[2] p1=0.1 y p2=-0.4")
acf(AR2_6, main = "ACF AR[2] p1=0.1 y p2=-0.8")
acf(AR2_7, main = "ACF AR[2] p1=-0.1 y p2=0.2")
acf(AR2_8, main = "ACF AR[2] p1=-0.1 y p2=0.4")
acf(AR2_9, main = "ACF AR[2] p1=-0.1 y p2=0.8")
acf(AR2_10, main = "ACF AR[2] p1=-0.1 y p2=-0.2")
acf(AR2_11, main = "ACF AR[2] p1=-0.1 y p2=-0.4")
acf(AR2_12, main = "ACF AR[2] p1=-0.1 y p2=-0.8")

dev.off()

graphics.off()

#### AUTOCORRELACION PARCIAL PARA AR[2] ####

help("pacf") 
#Auto- and Cross- Covariance and -Correlation Function Estimation

#Guardamos en imagen
jpeg("AutocorrelacionParcial_AR[2].jpeg",width= 6400, height=1820,
     units="px",res = 300)
# Grafiquemos estas series de tiempo simuladas
# Varios graficos en una misma ventana
par(mfrow = c(2,6))

pacf(AR2_1, main = "PACF AR[2] p1=0.1 y p2=0.2")
pacf(AR2_2, main = "PACF AR[2] p1=0.1 y p2=0.4")
pacf(AR2_3, main = "PACF AR[2] p1=0.1 y p2=0.8")
pacf(AR2_4, main = "PACF AR[2] p1=0.1 y p2=-0.2")
pacf(AR2_5, main = "PACF AR[2] p1=0.1 y p2=-0.4")
pacf(AR2_6, main = "PACF AR[2] p1=0.1 y p2=-0.8")
pacf(AR2_7, main = "PACF AR[2] p1=-0.1 y p2=0.2")
pacf(AR2_8, main = "PACF AR[2] p1=-0.1 y p2=0.4")
pacf(AR2_9, main = "PACF AR[2] p1=-0.1 y p2=0.8")
pacf(AR2_10, main = "PACF AR[2] p1=-0.1 y p2=-0.2")
pacf(AR2_11, main = "PACF AR[2] p1=-0.1 y p2=-0.4")
pacf(AR2_12, main = "PACF AR[2] p1=-0.1 y p2=-0.8")

dev.off()

graphics.off()


#### MODELOS O PROCESOS DE MEDIA MOVIL MA[2] ####

#Para todo n=100

# Fijemos una semilla
set.seed(555)

##Generamos los modelos con diferentes valores de phi1 y phi2 con valores altos, bajos y negativos
MA2_1 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,0.2) , sd=0.1))
MA2_2 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,0.4) , sd=0.1))
MA2_3 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,0.8) , sd=0.1))
MA2_4 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,-0.2) , sd=0.1))
MA2_5 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,-0.4) , sd=0.1))
MA2_6 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(0.1,-0.8) , sd=0.1))
MA2_7 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.1,0.2) , sd=0.1))
MA2_8 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.1,-0.8) , sd=0.1))
MA2_9 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.1,0.1) , sd=0.1))
MA2_10 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.1,0.1) , sd=0.1))
MA2_11 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.1,-0.1) , sd=0.1))
MA2_12 <- arima.sim(n = 100, model = list(order = c(0,0,2) , ma=c(-0.1,-0.1) , sd=0.1))

# configuremos el eje Y adecuado
ylm2 <- c(min(MA2_1, MA2_2, MA2_3, MA2_4, MA2_5, MA2_6, MA2_7, MA2_8, MA2_9, MA2_10, MA2_11, MA2_12) , 
         max(MA2_1, MA2_2, MA2_3, MA2_4, MA2_5, MA2_6, MA2_7, MA2_8, MA2_9, MA2_10, MA2_11, MA2_12))

#Guardamos en imagen
jpeg("ModeloMediaMovil_MA[2].jpeg",width= 6400, height=1820,
     units="px",res = 300)
# Grafiquemos estas series de tiempo simuladas
# Varios graficos en una misma ventana
par(mfrow = c(2,6))

plot.ts(MA2_1, ylim = ylm2, main = "MA[2] p1=0.1 y p2=0.2")
plot.ts(MA2_2, ylim = ylm2, main = "MA[2] p1=0.1 y p2=0.4")
plot.ts(MA2_3, ylim = ylm2, main = "MA[2] p1=0.1 y p2=0.8")
plot.ts(MA2_4, ylim = ylm2, main = "MA[2] p1=0.1 y p2=-0.2")
plot.ts(MA2_5, ylim = ylm2, main = "MA[2] p1=0.1 y p2=-0.4")
plot.ts(MA2_6, ylim = ylm2, main = "MA[2] p1=0.1 y p2=-0.8")
plot.ts(MA2_7, ylim = ylm2, main = "MA[2] p1=-0.1 y p2=0.2")
plot.ts(MA2_8, ylim = ylm2, main = "MA[2] p1=-0.1 y p2=0.4")
plot.ts(MA2_9, ylim = ylm2, main = "MA[2] p1=-0.1 y p2=0.8")
plot.ts(MA2_10, ylim = ylm2, main = "MA[2] p1=-0.1 y p2=-0.2")
plot.ts(MA2_11, ylim = ylm2, main = "MA[2] p1=-0.1 y p2=-0.4")
plot.ts(MA2_12, ylim = ylm2, main = "MA[2] p1=-0.1 y p2=-0.8")

dev.off()
graphics.off()


#### AUTOCORRELACION SIMPLE PARA MA[2] ####


#Guardamos en imagen
jpeg("AutocorrelacionSimple_MA[2].jpeg",width= 6400, height=1820,
     units="px",res = 300)
# Grafiquemos estas series de tiempo simuladas
# Varios graficos en una misma ventana
par(mfrow = c(2,6))

acf(MA2_1, main = "ACF MA[2] p1=0.1 y p2=0.2")
acf(MA2_2, main = "ACF MA[2] p1=0.1 y p2=0.4")
acf(MA2_3, main = "ACF MA[2] p1=0.1 y p2=0.8")
acf(MA2_4, main = "ACF MA[2] p1=0.1 y p2=-0.2")
acf(MA2_5, main = "ACF MA[2] p1=0.1 y p2=-0.4")
acf(MA2_6, main = "ACF MA[2] p1=0.1 y p2=-0.8")
acf(MA2_7, main = "ACF MA[2] p1=-0.1 y p2=0.2")
acf(MA2_8, main = "ACF MA[2] p1=-0.1 y p2=0.4")
acf(MA2_9, main = "ACF MA[2] p1=-0.1 y p2=0.8")
acf(MA2_10, main = "ACF MA[2] p1=-0.1 y p2=-0.2")
acf(MA2_11, main = "ACF MA[2] p1=-0.1 y p2=-0.4")
acf(MA2_12, main = "ACF MA[2] p1=-0.1 y p2=-0.8")

dev.off()
graphics.off()


#### AUTOCORRELACION PARCIAL PARA MA[2] ####


#Guardamos en imagen
jpeg("AutocorrelacionParcial_MA[2].jpeg",width= 6400, height=1820,
     units="px",res = 300)
# Grafiquemos estas series de tiempo simuladas
# Varios graficos en una misma ventana
par(mfrow = c(2,6))


pacf(MA2_1, main = "PACF MA[2] p1=0.2 y p2=0.2")
pacf(MA2_2, main = "PACF MA[2] p1=0.2 y p2=0.4")
pacf(MA2_3, main = "PACF MA[2] p1=0.2 y p2=0.8")
pacf(MA2_4, main = "PACF MA[2] p1=0.2 y p2=-0.2")
pacf(MA2_5, main = "PACF MA[2] p1=0.2 y p2=-0.4")
pacf(MA2_6, main = "PACF MA[2] p1=0.2 y p2=-0.8")
pacf(MA2_7, main = "PACF MA[2] p1=-0.2 y p2=0.2")
pacf(MA2_8, main = "PACF MA[2] p1=-0.2 y p2=0.4")
pacf(MA2_9, main = "PACF MA[2] p1=-0.2 y p2=0.8")
pacf(MA2_10, main = "PACF MA[2] p1=-0.2 y p2=-0.2")
pacf(MA2_11, main = "PACF MA[2] p1=-0.2 y p2=-0.4")
pacf(MA2_12, main = "PACF MA[2] p1=-0.2 y p2=-0.8")

dev.off()
graphics.off()



