# Multiple test corrections (http://xkcd.com/882/)
# Para evitar lanzar falsos positivos o descubrimientos falsos
# * Medir el error
# * Correcciones
#
# Errores
# * Tasa de falsos positivos (Cercano a los errores tipo I)
# * Tasa de error de sabiduria familiar(FWER): La probilidad de al menos
#   un falso positivo
# * Tasa de descubrimiento falso (FDR)
#
# Controlando los falsos positivos
# Supongamos que calculamos bien os p-values y todos P < alpha
# Supongamos una muestra de 10000 y β = 0 para todos
# Supongamos P < 0.05
# Número esperado de falsos positivos = 10000x0.05 = 500
# ¿Cómo evitar esa cantidad de falsos positivos?
#
# Controlando family-wise error rate
# Corrección de Bonferroni: Es el test más viejo de corrección múltiple
# * Supón que tienes m test
# * Quieres controlar FWER en un nivel tal que alpha Pr(V>=1) < alpha
# * Calcular P-values normalmente
# * Establecer alpha(fwer) = alpha/m
# * Llamar todos los P-values menores que alpha(fwer)
#
# Es fácil de calcular, pero puede ser muy conservativo
#
# Controlando la tasa de decubrimientos falsos (FDR)
# Este es el más popular de todos cuando se usan una gran cantidad de
# test en genomas, imágenes, astronomía u otrasdisciplinas de procesamiento 
# de señales
# * Supón m test
# * Se quiere controlar FDR en un nivel tal que alpha => E[V/R]
# * Se calculan los P-values normalmente
# * Se ordenan del más pequeño al más grande P(1)...P(m)
# * Se llama cualquier P(i)<= alpha * (i/m)
#
# Es fácil de calcular, menos conservativo (tal vez  menos de lo que debería)
# Permite una mayor cantidad de falsos positivos
#
# Ajustar P-values (Corrección de Bonferroni)
# * Supón P-values son P1,..., Pm
# * Se pueden ajustar tomando Pi(fwer) = max(m * Pi) para cada P-value
# * Se llaman todos los Pi(fwer) < alpha

# no true positives
ejemplo1 <- function(){
    set.seed(1010093)
    pValues <- rep(NA,1000)
    for(i in 1:1000){
        y <- rnorm(20)
        x <- rnorm(20)
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
    }
    
    # Controls false positive rate
    cfpr <- sum(pValues < 0.05)
    # Controls FWER 
    fwer <- sum(p.adjust(pValues,method="bonferroni") < 0.05)
    # Controls FDR 
    fdr <- sum(p.adjust(pValues,method="BH") < 0.05)
    
    c(cfpr, fwer, fdr)
}

# 50% true positives
ejemplo2 <- function(){
    set.seed(1010093)
    pValues <- rep(NA,1000)
    for(i in 1:1000){
        x <- rnorm(20)
        # First 500 beta=0, last 500 beta=2
        if(i <= 500){y <- rnorm(20)}else{ y <- rnorm(20,mean=2*x)}
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
    }
    trueStatus <- rep(c("zero","not zero"),each=500)
    table(pValues < 0.05, trueStatus)
    # Controls FWER 
    table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)
    # Controls FDR 
    table(p.adjust(pValues,method="BH") < 0.05,trueStatus)
    
    par(mfrow=c(1,2))
    plot(pValues,p.adjust(pValues,method="bonferroni"),pch=19)
    plot(pValues,p.adjust(pValues,method="BH"),pch=19)
}
