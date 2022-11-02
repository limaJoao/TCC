# Script de otimização multiobjetivo

# ---- 0. Preambulo ----
#Carregando pacotes e as funções utilizadas

source("source_mult.R")
library(rmoo)

# ---- 1. MOGA ----
debug(nsga2)
result <- nsga2(type = 'binary',
                fitness = FMO_FA.mL,
                nBits = 21,
                popSize = 100,
                nObj = 2,
                maxiter = 400,
                monitor = FALSE)
undebug(nsga2)

