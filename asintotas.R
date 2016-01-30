# Asintotas
# Ley de grandes n??meros(LLN): El promedio de los l??mites de lo
# que se est?? estimando tiende al promedio de la poblaci??n.
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

# Teorema del l??mite central (CLT)
# Primero se deben normalizar las funciones de probabilidad
# (Estimaci??n - Media de la estimaci??n)/Error est??ndar de la estimaci??n
# Mean(Xn) ~ N(u, o^2/sqrt(n)) 
#
# Sea Xi la salida del dado i
# Entonces, se sabe que u = E[Xi] = 3.5
# Var(Xi) = 2.92
# SE = sqrt(2.92/n)=1.71/sqrt(n)
# El resultado de aplicar el CLT es una distribuci??n normal normalizada
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

# Código para el proyecto 1 parte 1
project1 <- function(){
    nosim <- 1000
    n <- 40
    lambda <- 0.2
    
    m <- 1/lambda
    sd <- 1/lambda             # Esta es la desviación estándar teorica
    se <- sd/sqrt(n)           # Esta es el error estándar
    
    mdistribution = NULL
    for (i in 1 : nosim) mdistribution = c(mns, rexp(n, lambda))
    var(mdistribution)
}