# Resampling
# 
# Bootstrap (No parametrico)
# Procedimiento para calcular los intervalos de confianza para la media de un
# conjunto de datos de n observaciones
# i. Tomar muestras de n observaciones con reemplazo de los datos observados,
#    esto da como resultado un conjunto de datos simulados.
# ii. Se toma la mediana de los datos simulados (puede ser otra estadística)
# iii. Repetir estos 2 pasos B veces,resultando en B medianas simuladas.
# iv. Estas medianas son aproximadamente un bosquejo de una distribución muestral
#     dela mediana de n observaciones.
#     - Se hace un histograma de los datos
#     - Calcular la desviación estándar para estimar el error estándar de la mediana
#     - Tomar el percentil 2.5 y el 97.5 como intervalos de confianza de bootstrap
#       para la mediana
#
# Nota: B lo suficientemente grande para que el error producto del método de
# Montecarlo sea bajo, 10000 o más simulaciones.
# Nota 2: Los intervalos de confianza mostrados no son los mejores, por lo que es
# bueno usar el paquete de bootstrap de R que da intervalos BCa (Bias Corrected and
# accelerated


ejemplo1 <- function(){
    library(ggplot2)
    library(UsingR)
    
    data(father.son)
    x <- father.son$sheight
    n <- length(x)
    B <- 10000
    # La parte de sample(...) es una distribución empirica
    resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
    resampledMedians <- apply(resamples, 1, median)
    
    sd(resampledMedians) # Una estimación del error estándar de las medianas
    
    quantile(resampledMedians, c(0.025, 0.975)) # Intervalos de confianza de 
                                                # bootstrap para las medianas
    
    g <- ggplot(
            data.frame(medians = resampledMedians),
            aes(x = resampledMedians)
         )
    g <- g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
    g
}

# Test de permutación: usado para comparaciones de grupos
# * Consider the null hypothesis that the distribution of the observations from 
#   each group is the same
# * Then, the group labels are irrelevant
# * Consider a data frome with count and spray
# * Permute the spray (group) labels
# * Recalculate the statistic
#   - Mean difference in counts
#   - Geometric means
#   - T statistic
# * Calculate the percentage of simulations where the simulated statistic was 
#   more extreme (toward the alternative) than the observed
ejemplo2 <- function(){
    library(ggplot2)
    library(UsingR)
    
    data(InsectSprays)
    g = ggplot(InsectSprays, aes(spray, count, fill = spray))
    g = g + geom_boxplot()
    g
    
    subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
    y <- subdata$count
    group <- as.character(subdata$spray)
    testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
    observedStat <- testStat(y, group)
    permutations <- sapply(1 : 10000, function(i) testStat(y, sample(group)))
    observedStat # 13.25 diferencia entre pesticidad B y C
    mean(permutations > observedStat) # 0: Se rechaza la hipotesis nula p-values ~ 0
    
    g = ggplot(data.frame(permutations = permutations),
               aes(permutations))
    g = g + geom_histogram(fill = "lightblue", color = "black", binwidth = 1)
    g = g + geom_vline(xintercept = observedStat, size = 2)
    g
}