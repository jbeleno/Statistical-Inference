# Asintotas
# Ley de grandes números(LLN): El promedio de los límites de lo
# que se está estimando tiende al promedio de la población.
#
# Un estimador es consistente si converge a lo que queremos 
# estimar

lln <- function(){
    n <- 1000
    means <- cumsum(rnorm(n))/(1:n)
    plot(means)
}

lln_coin <- function(){
    n <- 1000
    means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
    means
}

# Teorema del límite central (CLT)
# Primero se deben normalizar las funciones de probabilidad
# (Estimación - Media de la estimación)/Error estándar de la estimación
# Mean(Xn) ~ N(u, o^2/sqrt(n)) 
#
# Sea Xi la salida del dado i
# Entonces, se sabe que u = E[Xi] = 3.5
# Var(Xi) = 2.92
# SE = sqrt(2.92/n)=1.71/sqrt(n)
# El resultado de aplicar el CLT es una distribución normal normalizada
# con centro en 0

quiz2p6 <- function(){
    mean<- 15
    sd<- 10
    valuea<- (14-mean)/(sd/sqrt(100))
    p14<-pnorm(valuea)
    valueb<- (16-mean)/(sd/sqrt(100))
    p16<-pnorm(valueb)
    answ<-p16-p14
    answ
}

# Funciona con muchas simulaciones
# cuando hay pocas de usa (X + 2)/(n + 4) para el 95%
# de confidencia en distribuciones binomiales
montecarlo1 <- function(){
    lambdavals <- seq(0.005, 0.1, by = 0.01)
    nosim <- 1000
    t <- 100
    
    coverage <- sapply(lambdavals, function(lambda){
        lhats <- rpois(nosim, lambda*t)/t
        ll <- lhats - qnorm(0.975)*sqrt(lhats/t)
        ul <- lhats + qnorm(0.975)*sqrt(lhats/t)
        
        mean(ll < lambda & ul > lambda)
    })
}