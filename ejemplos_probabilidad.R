# PMF: Probability Mass Function
# PDF: Probability Density Function
# CDF: Cumulative Distribution Function -> F(x) = P(X <= x)
# Survival Funcion: S(x) = P(X > x) => S(x) = 1 - F(x)
# quantile: F( Xa ) = a
# median: 50th percentile

# Se define una función de densidad probabilistica
# debe tener un area de 1 (2*1/2 = 1)y los valores deben  
# ser mayores o iguales a 0 
ejemplo1 <- function(){
  x <- c(-0.5,0, 1, 1, 1.5)
  y <- c(0, 0, 2, 0, 0)
  plot(x, y, lwd=3, frame = FALSE, type = "l")
}

# Se haya la probabilidad del 75%, fácil por ser un 
# triangulo, CDF F(0.75), 0.5*(x)*(2x) = x^2
densidad1 <- function(){
  x <- 1.5*0.75/2
  x
}

# probabilidad de una densidad beta, CDF F(0.75)
densidad2 <- function(){
  x <- pbeta(0.75, 2, 1)
  x
}

# probabilidad para 40%, 50% o 60%
densidad3 <- function(){
  x <- pbeta(c(0.4, 0.5, 0.6), 2, 1)
  x
}

# media
media1 <- function(){
  x <- qbeta(0.5, 2, 1)
  x
}