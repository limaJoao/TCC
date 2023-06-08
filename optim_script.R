# Script de otimização multiobjetivo

# ---- 0. Preambulo ----
#Carregando pacotes e as funções utilizadas

pacotes <- c("rmoo", "tidyverse", "GA", "ggrepel")
sapply(pacotes, require, character.only=TRUE)
rm(pacotes)

source("source_mono.R")
source("func_calcula_cenario.R")

# --------------------------- Gráficos ------------------------------
args_tese <- list(penalidade_CM = 0.65)

args_lucas <- 
  list(
    p1 = 0.99,
    p2 = 0.8,
    penalidade_CM = 0.6
    )
args_lucas2 <- 
  list(
    p1 = 0.999,
    p2 = 0.8,
    penalidade_CM = 1.5
  )

args_3 <- 
  list(
    p1 = 0.999,
    p2 = 0.5,
    c_a = 1000
  )

args_31 <- 
  list(
    p1 = 0.99,
    p2 = 0.5,
    c_a = 1000
  )

grafico1 <- calcula_cenario(cenario = "Cenario Tese", args = args_tese)

grafico2 <- calcula_cenario(cenario = "Cenario Artigo Lucas",args = args_lucas)
grafico21 <- calcula_cenario(cenario = "Cenario Artigo Lucas.1",args = args_lucas2)

grafico3 <- calcula_cenario(cenario = "Cenario 3",args = args_3)
grafico31 <- calcula_cenario(cenario = "Cenario 3.1",args = args_31)



