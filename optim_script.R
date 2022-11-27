# Script de otimização multiobjetivo

# ---- 0. Preambulo ----
#Carregando pacotes e as funções utilizadas

source("source_mono.R")
library(rmoo)

# --------------------------- 1. MOGA ------------------------------

result <- nsga2(
  type = 'binary',
  fitness = function(x) {
    do.call('func.mL',
            list(
              vet = x,
              obj_cm = TRUE,
              obj_phi = TRUE
            ))
  },
  popSize = 100,
  nBits = 21,
  nObj = 2, 
  pmutation = 0.2,
  seed =1101
)

(result@iter)

# ----------------------- 2. RESULTADOS ----------------------------

mat_res <- cbind(apply(result@population, 1, bin2int) %>% t(), 
                 result@fitness) %>% 
  as.data.frame() %>% 
  rename(
    "m" = "V1",
    "L" = "V2",
    "CM" = "V3",
    "Phi" = "V4"
  ) %>%
  mutate(Phi=-Phi) %>% 
  arrange(CM)

head(mat_res)

mat_res %>%
  ggplot(., aes(CM, Phi))+
  geom_point()
  







