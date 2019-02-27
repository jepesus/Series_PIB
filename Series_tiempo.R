Box.test(res,lag=10, fitdf = 0)
#lag es el numero de residuos 
install.packages("fpp2")
require(fpp2)
require(forecast)
pib<- read.csv("C://Users//Sala-D1.RTIC-D1//Documents//PIBMEXICO.csv")
pibts<- ts(pib, frequency = 1, start= 1961)
mod1<- ses(pibts, alpha = 0.1, initial = "simple", h=8)
mod2<- ses(pibts, alpha = 0.3, initial = "simple", h=8)
mod3<- ses(pibts, alpha = .09, h=8)
plot(mod1, ylab = "PIB", xlab = "Año", main = "PIB 1961-2017", type = "o")
lines(fitted(mod1), col="blue", type="o")
lines(fitted(mod2), col="red", type="o")
lines(fitted(mod3), col="green", type="o")
legend("topleft", lty = 1, col = c(1,"blue","red","green"),c("data", expression(alpha=0.1),expression(alpha=0.3),expression(alpha=0.9)),pch = 1)
names(mod1)

##Ejercicio##
#Con la funcion windows separar la serie PIB
#en dsatos de entrenamiento  y datos de prueba 
#(20%) y graficar los modelos solo a 
#lo que corresponde datos de prueba 

dent<-window(pibts, start=1961, end=2006)
mod4<- ses(dent, alpha = 0.1, initial = "simple", h=11)
mod5<- ses(dent, alpha = 0.3, initial = "simple", h=11)
mod6<- ses(dent, alpha = .09, h=11)

lines(fitted(mod4), col="blue", type="o")
lines(fitted(mod5), col="red", type="o")
lines(fitted(mod6), col="green", type="o")
legend("topleft", lty = 1, col = c(1,"blue","red","green"),c("data", expression(alpha=0.1),expression(alpha=0.3),expression(alpha=0.9)),pch = 1)
plot(mod4, ylab = "PIB", xlab = "Año", main = "PIB 1961", type = "o")
