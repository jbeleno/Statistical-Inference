# Potencia = 1 - Error tipo II
# Lo primero que hacer al atacar un problema
# Potencia: La probabilidad de rechaza la hipotesis nula cuando es falsa
# "You want more Power :D" - Bryan Caffo
#
# Error tipo II(β): (Bad thing) Fallar en rechazar la hipotesis nula cuando
# es falsa
# Power = 1 - β
# Se puede hacer una gráfica de mean vs power y dibujar las curvas con varios
# valores de n para ver en qué punto converge, más datos = más precisión
# (mu0 -mua)/sigma -> Effect size (Unit free)
# Calcular la potencia requiere de una distribución t no central

beta <- function(){
    mu0 <- 30
    mua <- 32
    sigma <- 4
    n <- 16
    alpha <- 0.05
        
    z <- qnorm(1 - alpha)
    beta <- pnorm(mu0 + z * sigma/sqrt(n), mean = mua, sd = sigma/sqrt(n), lower.tail = FALSE)
    # Si se reemplaza mua -> mu0 en el mean, entonces se tiene alpha
    # 0.63876 entonces existe un 64% de probabilidades de obtener una media de
    # 32 o más al conducir este experimento
    beta
}

ejemplo1 <- function(){
    library(manipulate)
    library(ggplot2)
    mu0 = 30
    
    manipulate(
        myplot(mu0, sigma, mua, n, alpha),
        sigma = slider(1, 10, step = 1, initial = 4),
        mua = slider(30, 35, step = 1, initial = 32),
        n = slider(1, 50, step = 1, initial = 16),
        alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
    )
}


myplot <- function(mu0, sigma, mua, n, alpha){
    g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mu0, sd = sigma / sqrt(n)), 
                          size = 2, col = "red")
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mua, sd = sigma / sqrt(n)), 
                          size = 2, col = "blue")
    xitc = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
    g = g + geom_vline(xintercept=xitc, size = 3)
    g
}

ejemplo2 <- function(){
    # one.sided = mua > mu0
    # delta = mua - mu0
    power.t.test(n = 16, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$power
    power.t.test(n = 16, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$power
    power.t.test(n = 16, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$power
}

ejemplo3 <- function(){
    power.t.test(power = .8, delta = 2 / 4, sd=1, type = "one.sample",  alt = "one.sided")$n
    power.t.test(power = .8, delta = 2, sd=4, type = "one.sample",  alt = "one.sided")$n
    power.t.test(power = .8, delta = 100, sd=200, type = "one.sample", alt = "one.sided")$n
}

ejemplo4 <- function(){
    ppois(10, lambda = 0.01 * 1787)
    
    
    rate <- 1/100
    errors <- 10
    days <- 1787
    test <-  poisson.test(errors, T = days, r = rate, alt="less")
    round(test$p.value,2)
    
    # Otro punto
    n_y <- 9 # subjects treated
    n_x <- 9 # subjects placebo
    σ_y <- 1.5# kg/m2 std.dev. treated 
    σ_x <- 1.8# kg/m2 std.dev. placebo 
    μ_y <- -3#  kg/m2 average difference treated
    μ_x <- 1#  kg/m2 average difference placebo
    
    # calculate pooled standard deviation
    σ_p <- (((n_x - 1) * σ_x^2 + (n_y - 1) * σ_y^2)/(n_x + n_y - 2))
    pval <- pt((μ_y - μ_x) / (σ_p * (1 / n_x + 1 / n_y)^.5), df=n_y + n_x -2)
    pval
}