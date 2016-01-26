# P-Values
# Valor más común de significancia estadística
# Agarrala con pinzas que esto se tiende a malinterpretar
#
# Idea: Supón que nada pasa, entonces qué tan inusual es 
# la estimación que obtenemos si tenemos en cuenta que H0
# es verdad.
#
# Enfoque:
# 1. Definir la distribución hipotetica cuando nada pasa H0
# 2. Calcular la estadística con los datos que tenemos
# 3. Comparar lo que calculamos con la distribución hipotetica
# y ver si el valors "extremo" (p-values)
#
# Si el p-value es pequeño, entonces H0 es VERDADERO y observamos
# un evento raro o es FALSO
#
# Si el p-value es menor que alpha entonces rechazamos H0

# estadística T de 2.5 para 15 df teniendo que H0: μ = μ0 y
# Ha: μ > μ0, ¿Cuál es la probabilidad de obtener una estadística T
# tan grande o más grande que 2.5?
ejemplo1 <- function(){
    x <- pt(2.5, 15, lower.tail = FALSE)
    x # 0.01225 La probabilidad de obtener qué H0 es VERDADERA y 
      # vimos un evento raro o H0 es FALSO, se puede testear con 
      # otros valores de alpha != del 0.05
}

# Lo mejor para sortear esto es usar un test de hipotesis de dos lados
# Pero casi siempre se hace por default

# Volvamos al ejemplo de las chicas
chicas <- function(){
    x <- choose(8, 7)*0.5^8 + choose(8, 8)*0.5^8
    # pbinom(6, size = 8, prob = 0.5, lower.tail = FALSE)
    x <- 0.03516
    # La probabilidad de ser 7 o mayor va a ser un p-value
    # La probabilidad de ser 7 o menor va a ser el otro p-value
    # Se toma el más pequeño y se multiplica por 2
}

# 10 infectados cada 100 personas/día en el hospital
# Se asume que la infección tiene una taza de 0.05
poissonEjemplo <- function(){
    # H0: λ = 0.05 => λ*100 = 5
    # Ha: λ > 0.05
    alpha <- 10/100
    x <- ppois(9, 5, lower.tail = FALSE) # Ten or more infections
    # 0.03183
}

quiz3 <- function(){
    intervals <- 1100 + c(-1, 1)*qt(.975, 9 - 1)*30/sqrt(9)
    
    # i = mn + c(-1, 1)*qt(.975, n-1)*sd/sqrt(n)
    # i = 0
    # t8,.975=2.31 Then set −2+2.31×S/3=0≈2.60 Solve for S to get around 2.60.
    sd <- -(-2)*sqrt(9)/(c(-1, 1)*qt(.975, 9 - 1))
    
    # Ym - Xm +/- (tnx+ny-2,(1-a)/2)*Sp*sqrt(1/nx + 1/ny)
    # Sp^2 = {(nx-1)*Sx^2 + (ny - 1)*Sy^2}/(nx + ny -2)
    sp <- sqrt(((10 - 1)*0.6^2 + (10 - 1)*0.68^2)/(10 + 10 - 2))
    intervals <- 3 - 5 + c(-1, 1)*qt(.975, 10 + 10 - 2)*sp*sqrt(1/10 + 1/10)
    
    oldIntervals <- 6 + c(-1, 1)*2*1.645/sqrt(100)
    newIntervals <- 5 + c(-1, 1)*.5*1.645/sqrt(100)
    difference <- oldIntervals - newIntervals
    
    # OR MAYBE
    
    sp <- sqrt(((100 - 1)*2^2 + (100 - 1)*.5^2)/(100 + 100 - 2))
    intervals <- 6 - 4 + c(-1, 1)*qt(.975, 100 + 100 - 2)*sp*sqrt(1/100 + 1/100)
    
    #NOPE, the solution is
    n1 <- n2 <- 100
    xbar1 <- 4
    xbar2 <- 6
    s1 <- 0.5
    s2 <- 2
    xbar2 - xbar1 + c(-1, 1) * qnorm(0.975) * sqrt(s1^2/n1 + s2^2/n2)
    
    #Fat guys
    sp <- sqrt(((9 - 1)*1.5^2 + (9 - 1)*1.8^2)/(9 + 9 - 2))
    intervals <- -3 - 1 + c(-1, 1)*qt(.95, 9 + 9 - 2)*sp*sqrt(1/9 + 1/9)
}