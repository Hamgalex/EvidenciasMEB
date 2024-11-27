# 1


horas <- c(2, 4, 6, 8, 10, 12)
cloro_residual <- c(1.8, 1.5, 1.4, 1.1, 1.1, 0.9)


# Original
modelo <- lm(cloro_residual ~ horas)

plot(horas,cloro_residual,
     xlim=c(0,12),
     ylim=c(0,2))
abline(modelo, col="blue")

summary(modelo)

confint(modelo)


# No pasa por el origen:
modeloOrigen <- lm(cloro_residual ~ horas+0)

summary(modeloOrigen)


modelo <- lm(cloro_residual ~ horas)

plot(horas, cloro_residual, main = "Dispersión de Datos y Ajuste del Modelo",
     xlab = "Número de Horas",
     ylab = "Cloro Residual (ppm)",
     xlim = c(0,14),
     ylim = c(0,2),
     pch = 19)

abline(modelo, col = "blue", lwd = 2)

# Exponencial

modelo_exponencial <- lm(log(cloro_residual) ~ horas)
summary(modelo_exponencial)
# cloro = 0.6995 - 0.06624*horas

confint(modelo_exponencial)


fitted.values(modelo_exponencial)
estimados <- 2.012*exp(-0.0662*horas)
lines(horas,estimados,col="purple")




# 2

library("car")
library(olsrr)

IQ <- c(112, 126, 100, 114, 112, 121, 110, 103, 111, 124)
horas_estudio <- c(5, 13, 3, 7, 11, 9, 8, 4, 6, 2)
puntuacion <- c(79, 97, 51, 65, 82, 93, 81, 38, 60, 86)

modelo <- lm(puntuacion ~ IQ + horas_estudio)
summary(modelo)

vif_values<-vif(modelo)
vif_values

ols_step_all_possible(modelo)


intervalos_confianza <- confint(modelo)
intervalos_confianza

plot(fitted.values(modelo),residuals(modelo))

residuales <- residuals(modelo)

mean(residuales)

Box.test(residuales)

ks.test(residuales,pnorm,mean=0)

puntuacion <- -124.5682 + 1.6591 * (105) + 1.4393 * (7)
puntuacion
