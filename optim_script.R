# Script de otimização multiobjetivo

# ---- 0. Preambulo ----
#Carregando pacotes e as funções utilizadas

source("source_mult.R")
library(rmoo)

# ---- 1. MOGA ----

result <- nsga2(type = 'real-valued', 
                fitness = FMO_FA.mL, 
                lower = c(0,0), upper = c(1,1),
                popSize = 100,
                nObj = 2,
                names = c("m", "L"))

apply(result@population, 1, function(x){
  m = floor((2000 - 2)*x[1] + 2)
  L = floor((2000 - m)*x[2] + m)
  
  return(c(m,L))
}) %>% t()
