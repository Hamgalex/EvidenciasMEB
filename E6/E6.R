# A


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
abline(modeloOrigen,col="red",lwd=2)


# Exponencial

modelo_exponencial <- lm(log(cloro_residual) ~ horas)
summary(modelo_exponencial)
# cloro = 0.6995 - 0.06624*horas

confint(modelo_exponencial)


fitted.values(modelo_exponencial)
estimados <- 2.012*exp(-0.0662*horas)
lines(horas,estimados,col="purple")
