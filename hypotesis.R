# Hypotesis
# 
# H0 = null hypotesis = status quo = by default is TRUE
# Alternativas < > !=
#
# VERDAD    DECISIÓN    RESULTADO
# H0        H0          Correctamente aceptado null
# H0        Ha          Error tipo I
# Ha        Ha          Correctamente rechazado null
# Ha        H0          Error tipo II
#
# Error tipo I y II son inversamente proporcionales
# Court Law example
# α = Type I error rate = Probabilidad de rechazar H0 cuando en realidad
# es verdadera ~ 5% benchmarking


# RDI = Respiratory Disturbance Index
# SDB = Sleep Disordered Breathing
# RDI > 30 events/hour => SDB
# n = 100 overweighted people
# mean = 32 events/hour
# sd = 10 event/hour
#
# Hypotesis
# H0: u = 30
# Ha: u > 30
# population mean RDI
sdb <- function(){
    n <- 100
    m <- 32
    sd <- 10
    se <- sd/sqrt(n)
    # H0 Xm ~ N(30,1)
    # Escojer C tal que P(Xm > C; H0) es 5%
    # Q(0.95) = 1.645 => C = 30 +1*1.645 = 31.645
    # Z = (32-30)/(10/sqrt(100)) = 2 > 1.645
    # No convertir C a las unidades originales
    # n-1 grados de libertad
    # Para el caso de n = 16
    # qt(0.95, 15) = 1.7531
    # Z = (32 - 30)/(10/sqrt(16)) = 0.8 < 1.75 ( FALSO :( )
    #
    # Test de ambos lados
    # Vamos a rechazar H0 si de hecho el promedio es muy grande o muy pequeño
    # Ha: μ != 30
    # Se rechaza si, 0.8 es muy grande o muy pequeño y se quiere
    # la probabilidad de rechazo = 5%, que para este caso es 2.5% en
    # la cola superior y 2.5% en la cola inferior
    # Se rechaza si |0.8| es mayor que qt(0.975, 15) = 2.1314
    # Fallamos en rechazar el Test de ambos lados
}

tamanhos <- function(){
    library(UsingR)
    data(father.son)
    t.test(father.son$sheight - father.son$fheight)
    # t.test(father.son$sheight, father.son$fheight, paired = TRUE)
    # df = degrees of freedom
}

# Pruebas en 2 grupos
ChickWeightTest <- function(){
    library(datasets); data(ChickWeight); library(reshape2)
    ##define weight gain or loss
    wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
    names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
    library(dplyr)
    wideCW <- mutate(wideCW,
                     gain = time21 - time0
    )
    
    wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
    t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)
}