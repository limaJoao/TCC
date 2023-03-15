# Script de otimização multiobjetivo

# ---- 0. Preambulo ----
#Carregando pacotes e as funções utilizadas

source("source_mono.R")

pacotes <- c("rmoo", "tidyverse", "GA", "ggrepel")
sapply(pacotes, require, character.only=TRUE)
rm(pacotes)

# --------------------------- 1. MOGA ------------------------------

## NSGA2 para mL
res_mL <- nsga2(
  type = 'binary',
  fitness = function(x) {
    do.call('func.mL',
            list(
              vet = x,
              obj_cm = TRUE,
              obj_phi = TRUE
            ))
  },
  popSize = 120,
  nBits = 21,
  nObj = 2,
  seed = 333, 
  pmutation = 0.2, pcrossover = 0.85
)

## NSGA2 para m
res_m <- nsga2(
  type = 'binary',
  fitness = function(x) {
    do.call('func.mL',
            list(
              vet = x,
              obj_cm = TRUE,
              obj_phi = TRUE,
              func_m = TRUE
            ))
  },
  popSize = 100,
  nBits = 12,
  nObj = 2,
  seed = 333
)


# Operadores genéticos utilizados:
nsgaControl('binary')

# ----------------------- 2. RESULTADOS ----------------------------

mat_mL <-
  cbind(apply(res_mL@population, 1, function(x) {
    m <- binary2decimal(x[1:10])
    L <- binary2decimal(x[11:length(x)])
    
    c(m, L)
  }) %>% t(),
  res_mL@fitness) %>%
  as.data.frame() %>%
  rename(
    "m" = "V1",
    "L" = "V2",
    "CM" = "V3",
    "Phi" = "V4"
  ) %>%
  mutate(Phi = -Phi) %>%
  arrange(CM) %>% 
  distinct()

mat_m <-
  cbind(apply(res_m@population, 1, binary2decimal),
        res_m@fitness) %>%
  as.data.frame() %>%
  rename("m" = "V1",
         "CM" = "V2",
         "Phi" = "V3") %>%
  mutate(Phi = -Phi) %>%
  arrange(CM) %>% 
  distinct()


# 3. Gráfico --------------------------------------------------------------

mat_res <- 
  mat_mL %>% 
  mutate(func = "func.mL",
         res = str_glue("m: {m} L: {L} \nCM: {round(CM,4)} Phi: {round(Phi,6)}")) %>% 
  select(-c(m,L)) %>% 
  bind_rows(
    mat_m %>% 
      mutate(func = "func.m",
             res = str_glue("m: {m}\nCM: {round(CM,4)} Phi: {round(Phi,6)}")) %>% 
      select(-m))


ggplot(data = mat_res, aes(x = CM, y = Phi, col = func))+
  geom_point()

  





