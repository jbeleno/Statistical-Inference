# Probabilidad condicional
# * B | P(B) >0
# P(A|B) = P(A ∩ B)/P(B), Probabilidad de A dado B
# Si son independientes => P(A|B) = P(A)
# Regla de Bayes
# P(B|A) = (P(A|B)P(B))/(P(A|B)P(B)+P(A|Bc)P(Bc))
# P(B|A) = (P(A|B)P(B))/(P(A|B)P(B)+(1 - P(Ac|Bc))(1 - P(B)))
# Bc: complemento de B

# Sensitivity = P(+ | D)
# Specificity = P(- | Dc)
# Prevalence of disease = P(D)
diagnosticoPositivo <- function(){
  # Caso VIH
  sensitivity <- 0.997
  specificity <- 0.985
  prevalence <- 0.001
  
  # P(D | +)
  p <- (sensitivity*prevalence)/((sensitivity*prevalence)+(1-specificity)*(1-prevalence))
  p
}

diagnosticoPositivo2 <- function(){
  # Caso Quiz
  sensitivity <- 0.75
  specificity <- 0.52
  prevalence <- 0.3
  
  # P(D | +)
  p <- (sensitivity*prevalence)/((sensitivity*prevalence)+(1-specificity)*(1-prevalence))
  p
}

ratioPositivo <- function(){
  # Ratios de probabilidad de la enfermedad
  # P(D | +)/P( Dc | +) = (P(+ | D)*P(D))/(P(+ | Dc)*P(Dc))
  sensitivity <- 0.997
  specificity <- 0.985
  
  DLR <- sensitivity/(1-specificity)
  DLR
}

ratioNegativo <- function(){
  sensitivity <- 0.997
  specificity <- 0.985
  
  DLR <- (1-sensitivity)/specificity
  DLR
}