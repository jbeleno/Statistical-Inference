# Distribución de Bernoulli
# Solo toma valores de 1 y 0, con probabilidades de p y (1-p)
# P(X = x) = (p^x)((1 - p)^(1 - x))
# mean = p
# Variance = p*(1-p)
# 
# Distribución binomial
# Es la sumatoria de distribuciones de Bernoulli
# X = SUM(Xi)
# P(X = x) = (n C x)*(p^x)((1 - p)^(n - x)), C es combinatoria
# n C x = n!/(x!(n-x)!)

# Un amigo tiene 8 hijos, 7 hijas y un hijo
birth <- function(){
    #Si hay una probabilidad de un 50% en el género en
    # cada nacimiento cual es la probailidad que pase
    # 7 o más nacimientos de mujer en una muestra de 8
    # p <- choose(8,7)*(.5^7)*((1-0.5)^1) + choose(8,8)*(.5^8)*((1-0.5)^0)
    #
    # 6 o menos cuando lower.tail = TRUE, se invierte cuando lower.tail = FALSE
    # y queda 7 o más
    p <- pbinom(6, size = 8, prob = 0.5, lower.tail = FALSE)
    p
}

# Distribución normal
# Cuando u = 0 y o = 1, entonces es distribución normal estándar o curva de Bell
# u - o < 68% < u + o
# u - 2o < 95% < u + 2o
# u - 3o < 99% < u + 3o
# Si X ~ N(u, o^2) => Z = (X - u)/o ~ N(0, 1)
# Si Z es la distribución normal estándar
# => X = u + oZ ~N(u, o^2)
# -1.28 ~ 10% o 1.28 ~ 90%
# -1.645 ~ 5% o 1.645 ~ 95%
# -1.96 ~ 2.5% o 1.96 ~ 97.5%
# -2.33 ~ 1% o 2.33 ~ 99%
percentil95 <- function(){
    p95 <- qnorm(0.95, 0, 1)
    p95
}

# Probabilidad que N(u, o^2) > x
prob1 <- function(){
    #p <- pnorm(x, mean = u, sd = sigma, lower.tail = FALSE)
    # p <- 1 - pnorm(x, mean = u, sd = sigma)
}

ads <- function(){
    # mean = 1020 clics
    # sd = 50 clics
    # Probabilidad de tener más de 1116
    # Se sabe que eso es 2.8*sd más del promedio
    # p <- pnorm(2.8, lower.tail = FALSE) es equivalente a
    p <- pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)
    p
}

ads2 <- function(){
    # Mismos datos anteriores, pero la pregunta es:
    # ¿Qué número de clics representa el 75% de dias con menos clics?
    # suponiendo que son identicos e independientemente distribuidos
    
    q <- qnorm(0.75, mean = 1020, sd = 50)
    q
}

# Distribución de Poisson
# mean = lambda
# varianza = lambda
# Esto último es útil para ver si es una distribución de Poisson
# 
# Modelar conteo de datos
# Modelar event-time o survival data: Tratamiento de cancer, evolución del paciente
# Modelar tablas de contingencia: color del cabello vs color del cuerpo
# Aproximar distribución binomial cuando la n es grande y la p pequeña
# Pasa en epidemiología, eventos (diseases) en grandes ciudades debido a la
# polución en el aire.
# X ~ Poisson(λt)
# λ = E[X/t] la cuenta esperada por unidad de tiempo
# t es el tiempo total de monitoreo

# 2.5 personas por hora en una parada de bus
# Si se observa la parada de bus por 4 horas, cual es la probabilidad que 3 o
# menos personas aparezcan en la parada de bus
bus <- function(){
    p <- ppois(3, lambda = 2.5*4)
}

# Aproximación a la distribución binomial
# X ~ Binomial(n, p)
# λ = np
# n tiende a ser  es grande y p tiende a ser pequeña

# Se lanza una moneda con probabilidad de éxito de 0.01, 500 veces
# ¿Cuál es la probabilidad de que 2 o menos veces haya éxito?
flipcoin <- function(){
    p <- pbinom(2, size = 500, prob = 0.01)
    p2 <- ppois(2, lambda = 500*0.01)
    c(p, p2)
}