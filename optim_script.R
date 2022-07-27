# Script de otimização multiobjetivo

# ---- 0. Preambulo ----
#Carregando pacotes e as funções utilizadas

source("source_mult.R")
library(mco)
library(nsga2R)

# ---- 1. MOGA ----

my_constr <- function(x) { x[2] - x[1] }

a <- nsga2(fn = FMO_FA.mL, idim = 2, odim = 2, 
           lower.bounds = c(2,2), upper.bounds = c(2000, 4000),
           constraints = my_constr, cdim = 1)

nsga2R()


