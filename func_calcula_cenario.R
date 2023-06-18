## NSGA2 para mL
calcula_cenario <- 
  function(cenario ,args){
    
    res_mL_unico <- nsga2(
      type = 'binary',
      fitness = function(x) {
        do.call('func.mL',
                flatten(list(
                  list(vet = x,
                       obj_cm = TRUE,
                       obj_propnc = TRUE),
                  args
                )))
      },
      popSize = 150,
      nBits = 18,
      nObj = 2,
      seed = 333, pmutation = 0.05, pcrossover = 0.8, run = 120, monitor = FALSE
    )
    
    ## NSGA2 para m
    res_m <- nsga2(
      type = 'binary',
      fitness = function(x) {
        do.call('func.mL',
                flatten(list(
                  list(vet = x,
                       obj_cm = TRUE,
                       obj_propnc = TRUE,
                       func_m = TRUE),
                  args
                )))
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
                flatten(list(
                  list(
                    vet = x,
                    obj_cm = TRUE,
                    obj_propnc = TRUE),
                  args
                )))
      },
      popSize = 150,
      nBits = 27,
      nObj = 2,
      seed = 333, pmutation = 0.05, pcrossover = 0.8, run = 120, monitor = FALSE
    )
    
    # Operadores genéticos utilizados:
    nsgaControl('binary')
    
    ## ----------------------- 2. RESULTADOS ----------------------------
    
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
        "Prop_NC" = "V4"
      ) %>%
      arrange(CM) %>% 
      distinct()
    
    mat_m <-
      cbind(apply(res_m@population, 1, binary2decimal),
            res_m@fitness) %>%
      as.data.frame() %>%
      rename("m" = "V1",
             "CM" = "V2",
             "Prop_NC" = "V3") %>%
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
        "Prop_NC" = "V6"
      ) %>%
      arrange(CM) %>% 
      distinct()
    
    
    ## 3. Gráfico --------------------------------------------------------------
    
    mat_res <- 
      mat_mL %>% 
      mutate(func = "func.mL",
             res = str_glue("m: {m} L: {L} \nCM: {round(CM,4)} Prop_NC: {round(Prop_NC,6)}")) %>% 
      select(-c(m,L)) %>% 
      bind_rows(
        mat_mLr %>% 
          mutate(func = "func.mLr",
                 res = str_glue("m: {m} L: {L} r:{r} a:{a}\nCM: {round(CM,4)} Prop_NC: {round(Prop_NC,6)}")) %>% 
          select(-c(m,L,r,a))) %>% 
      bind_rows(
        mat_m %>% 
          mutate(func = "func.m",
                 res = str_glue("m: {m} \nCM: {round(CM,4)} Prop_NC: {round(Prop_NC,6)}")) %>% 
          select(-m)
      )
    
    cbPalette <- c('#1b9e77', '#d95f02', '#7570b3')
    
    g <- 
      ggplot(data = mat_res, 
             aes(x = CM, y = Prop_NC, fill = func, shape = func))+
      geom_point(size = 2.5, stroke = 1) +
      scale_fill_manual(values = cbPalette)+
      scale_shape_manual(values = c(21,24,22)) +
      ggtitle(cenario)
    
    return(g)
}