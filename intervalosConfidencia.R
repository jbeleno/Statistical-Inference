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

data("sleep")