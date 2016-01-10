# Varianza
# Var(X) = E[(X-u)^2] = E[X^2] - E[X]^2
# donde E es el valor esperado
# Desviación estándar = sqrt(Var(X))
# 
# Varianza de muestra
# S^2 = SUM(Xi - Xm)/(n-1)
#
# E[Xm] = u
# Var(Xm) = o^2/n, Varianza de una muestra de promedios
#
# La varianza está centrada en o^2
# o^2 = Varianza de la población
# Var(Xm) ~ S^2/n
# Error estándar = S/sqrt(n)
#
# Desviación estándar = Qué tan variable es la población
# Error estándar = Qué tan variable son los promedios de muestras aleatorias
# de tamaño n de la población

dado <- function(){
  # E[X^2] = 1^2*(1/6) + 2^2*(1/6) + 3^2*(1/6) + 4^2*(1/6) + 5^2*(1/6) + 6^2*(1/6) = 15,17
  # E[X] = 1*(1/6) + 2*(1/6) + 3*(1/6) + 4*(1/6) + 5*(1/6) + 6*(1/6) = 3.5
  
  Ex <- 3.5
  Ex2 <- 15.17
  
  Var <- Ex2 - Ex*Ex
  Var
}

moneda <- function(){
  # E[X] = 0*(1-p) + 1*p = p
  # E[X^2] = E[X] = p, porque los valores posibles son 0 y 1
  # Var(X) = E[X^2] - E[X]^2 = p - p^2 = p(1-p)
}

simulacion <- function(){
  nsim <- 1000
  n <- 10
  sd <- sd(apply(matrix(runif(nsim*n), nsim), 1, mean))
  
  sd2 <- 1/sqrt(12*n)
  c(sd, sd2)
}