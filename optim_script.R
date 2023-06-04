# Script de otimização multiobjetivo

# ---- 0. Preambulo ----
#Carregando pacotes e as funções utilizadas

source("source_mono.R")

pacotes <- c("rmoo", "tidyverse", "GA", "ggrepel")
sapply(pacotes, require, character.only=TRUE)
rm(pacotes)

# --------------------------- 1. MOGA ------------------------------

## NSGA2 para mL
res_mL_unico <- nsga2(
  type = 'binary',
  fitness = function(x) {
    do.call('func.mL',
            list(
              vet = x,
              obj_cm = TRUE,
              obj_cmnc = TRUE
            ))
  },
  popSize = 150,
  nBits = 18,
  nObj = 2,
  seed = 333, pmutation = 0.05, pcrossover = 0.8, run = 120
)

## NSGA2 para m
res_m <- nsga2(
  type = 'binary',
  fitness = function(x) {
    do.call('func.mL',
            list(
              vet = x,
              obj_cm = TRUE,
              obj_cmnc = TRUE,
              func_m = TRUE
            ))
  },
  popSize = 150,
  nBits = 18,
  nObj = 2,
  pmutation = 0.05, pcrossover = 0.8, maxiter = 120, monitor = FALSE
)


# NSGA2 repetidas
res_mLr_unico <- nsga2(
  type = 'binary',
  fitness = function(x){
    do.call('func.mLr',
            list(
              vet = x,
              obj_cm = TRUE,
              obj_cmnc = TRUE
            ))
  },
  popSize = 150,
  nBits = 27,
  nObj = 2,
  seed = 333, pmutation = 0.05, pcrossover = 0.8, run = 120
)

# Operadores genéticos utilizados:
nsgaControl('binary')

# ----------------------- 2. RESULTADOS ----------------------------

mat_mL <-
  cbind(apply(res_mL_unico@population, 1, function(x) {
    m <- binary2decimal(x[1:8])
    L <- binary2decimal(x[9:length(x)])
    
    c(m, L)
  }) %>% t(),
  res_mL_unico@fitness) %>%
  as.data.frame() %>%
  rename(
    "m" = "V1",
    "L" = "V2",
    "CM" = "V3",
    "CM_NC" = "V4"
  ) %>%
  arrange(CM) %>% 
  distinct()

mat_m <-
  cbind(apply(res_m@population, 1, binary2decimal),
        res_m@fitness) %>%
  as.data.frame() %>%
  rename("m" = "V1",
         "CM" = "V2",
         "CM_NC" = "V3") %>%
  arrange(CM) %>% 
  distinct()

mat_mLr <- 
  cbind(apply(res_mLr_unico@population, 1, function(x) {
    m <- binary2decimal(x[1:8])
    L <- binary2decimal(x[9:18])
    r <- binary2decimal(x[19:23])
    a <- binary2decimal(x[24:length(x)])
    
    c(m, L, r, a)
  }) %>% t(),
  res_mLr_unico@fitness) %>%
  as.data.frame() %>%
  rename(
    "m" = "V1",
    "L" = "V2",
    "r" = "V3",
    "a" = "V4",
    "CM" = "V5",
    "CM_NC" = "V6"
  ) %>%
  arrange(CM) %>% 
  distinct()


# 3. Gráfico --------------------------------------------------------------

mat_res <- 
  mat_mL %>% 
  mutate(func = "func.mL",
         res = str_glue("m: {m} L: {L} \nCM: {round(CM,4)} CM_NC: {round(CM_NC,6)}")) %>% 
  select(-c(m,L)) %>% 
  bind_rows(
    mat_mLr %>% 
      mutate(func = "func.mLr",
             res = str_glue("m: {m} L: {L} r:{r} a:{a}\nCM: {round(CM,4)} CM_NC: {round(CM_NC,6)}")) %>% 
      select(-c(m,L,r,a))) %>% 
  bind_rows(
    mat_m %>% 
      mutate(func = "func.m",
             res = str_glue("m: {m} \nCM: {round(CM,4)} CM_NC: {round(CM_NC,6)}")) %>% 
      select(-m)
  )

cbPalette <- c('#1b9e77', '#d95f02', '#7570b3')

ggplot(data = mat_res %>% filter(CM < 2), 
       aes(x = CM, y = CM_NC, group = func))+
  geom_point(aes(shape=func, color= func), size = 2.5) +
  scale_color_manual(values = cbPalette)

ggplot(data = mat_res, 
       aes(x = CM, y = CM_NC, fill = func, shape = func))+
  geom_point(size = 2.5, stroke = 1) +
  scale_fill_manual(values = cbPalette)+
  scale_shape_manual(values = c(21,24,22))

# trocar os valores de alpha, c_nc, P2
# Verificar o pareto para mLra

# -------------------------------------------------------------------------

l_mL_alpha <- 
  lapply(c(10,15,20, 25), function(var_a){
    nsga2(
      type = 'binary',
      fitness = function(x) {
        do.call('func.mL',
                list(
                  vet = x,
                  obj_cm = TRUE,
                  obj_cmnc = TRUE, 
                  c_nc = var_a
                ))
      },
      popSize = 150,
      nBits = 18,
      nObj = 2,
      seed = 333, pmutation = 0.05, pcrossover = 0.8, run = 120
    )
    
  })

l_mL_alpha <- 
  l_mL_alpha %>% 
  map(~ .x@fitness)

names(l_mL_alpha) <- as.character(c(0.005,0.01, 0.02))

res_mL2 <- 
  do.call('rbind', l_mL_alpha) %>% 
  bind_cols(rep( paste("c_nc = ", c(10,15,20, 25)) , rep(150,4)))

names(res_mL2) <- c("CM", "CM_NC", "c_nc")

res_mL2 %>% 
  filter(CM < 1.5) %>% 
  ggplot(., aes(x = CM, y = CM_NC, group = c_nc))+
  geom_point(aes(shape = c_nc, color = c_nc))



# -------------------------------------------------------------------------

l_mL_p2 <- 
  lapply(c(.8,.85,.9,.95), function(var_a){
    nsga2(
      type = 'binary',
      fitness = function(x) {
        do.call('func.mL',
                list(
                  vet = x,
                  obj_cm = TRUE,
                  obj_cmnc = TRUE, 
                  p2 = var_a
                ))
      },
      popSize = 150,
      nBits = 18,
      nObj = 2,
      seed = 333, pmutation = 0.05, pcrossover = 0.8, run = 120
    )
    
  })

l_mL_p2 <- 
  l_mL_p2 %>% 
  map(~ .x@fitness)

names(l_mL_p2) <- as.character(c(0.005,0.01, 0.02))

res_mL <- 
  do.call('rbind', l_mL_p2) %>% 
  bind_cols(rep( paste("p2 = ", c(.8,.85,.9,.95)) , rep(150,4)))

names(res_mL) <- c("CM", "CM_NC", "p2")

res_mL %>% 
  filter(CM < 1.5) %>% 
  ggplot(., aes(x = CM, y = CM_NC, group = p2))+
  geom_point(aes(shape = p2, color = p2))
