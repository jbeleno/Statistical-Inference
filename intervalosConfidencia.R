# En el CLT las distribuciones tomaban la forma
# Est +/- ZQ*SEest
# Ahora se va a usar Est +/- TQ*SEest
# Es mejor usar el intervalo T que el Z a medida que
# se tomen más muestras.
#
# Las distribuciones T de Gosset (Student)
# * Tienen colas más anchas que las normales
# * Es indexada por grados de libertad y toma valores
#   parecidos a los de normal estándar cuando los grados
#   de libertad toma valores grandes
# * Se asume que los datos son Gausianos (Xm - u)/(S/sqrt(n))
# * El intervalo es X +/- tn-1*S/sqrt(n), donde tn-1 es el
#   quantile relevante. 
# * intervalo t asume que los datos son iid normales
# * No sirve para información sesgada
# * Para Poisson y binarios es mejor otra

data("sleep")

# A los 2 grupos se les mide en las mismas condiciones
# y las muestras son del mismo tamaño
sleept <- function(){
    g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
    difference <- g2 - g1
    mn <- mean(difference); s <- sd(difference); n <- 10
    
    # 4 maneras de hacer lo mismo
    # Obtener el intervalo de confidencia con t student
    # El tamaño de los grupos es igual
    mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
    t.test(difference)
    t.test(g2, g1, paired = TRUE)
    t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)
    # Ignorar paired puede volver mierda todo
}

# Comparar grupos independientes de tamaños diferentes
# Ym - Xm +/- (tnx+ny-2,(1-a)/2)*Sp*sqrt(1/nx + 1/ny)
# Sp = Pooled standar deviation
# Sp^2 = {(nx-1)*Sx^2 + (ny - 1)*Sy^2}/(nx + ny -2)
# Este intervalo asume una varianza constante en los 2 grupos
# Si hay dudas mejor usar una varianza diferente por grupo
# Esto se logra con otro método(?????)
ejemplot <- function(){
    # Cómo varia la presión sanguinea cuando se tienen
    # 8 usuario con prevención oral contra 21 que van a 
    # control
    # Xoc = 132.86 mmHg y soc = 15.34 mmHg
    # Xc = 127.44 mmHg y sc = 18.23
    
    sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
    t <- 132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5
    t
}

# En duda es mejor asumir varianzas diferentes
# Ym - Xm +/- tdf*sqrt(sx^2/nx + sy^2/ny)
# df = una formula una gonorrea, pero lo mejor es usar
# var.equal = FALSE y eso hace el trabajo por nosotros
chickweightexample <- function(){
    library(datasets); data(ChickWeight); library(reshape2)
    ##define weight gain or loss
    wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
    names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
    library(dplyr)
    wideCW <- mutate(wideCW,
                     gain = time21 - time0
    )
    
    wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
    rbind(
        t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
        t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
    )
}