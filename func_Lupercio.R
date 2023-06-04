# Entrada dos Parametros
fTransic_Inv <-  function(m = 41, L = 896,
                          p1 = 0.999, p2 = 0.95, pq = 0.0001,
                          alpha = 0.01){
  
  # Parametros Probabilísticos do Processo ----------------------------------
  
  # p1 = Fracao de conformes processo sobre controle
  # p2 = Fracao de Conformes processo fora de controle
  # pi = Probabilidade de ocorrencia de shift
  # alpha = Probabilidade de classificacao nao cfe em item cfe
  # beta = Probabilidade de classificacao cfe em item nao cfe
  
  beta <- alpha
  
  # Matriz de transicao -----------------------------------------------------
  P = matrix(rep(0,36), nrow = 6)
  
  P[1,1]=(1-pi)^m*(p1*(1-alpha)+(1-p1)*beta)
  P[1,2]=(1-pi)^m*(p1*alpha+(1-p1)*(1-beta))
  P[1,3]=(1-(1-pi)^m)*(p2*(1-alpha)+(1-p2)*beta)
  P[1,4]=(1-(1-pi)^m)*(p2*alpha+(1-p2)*(1-beta))
  P[1,5]=0
  P[1,6]=0
  
  P[2,1]=(1-pi)^L*(p1*(1-alpha)+(1-p1)*beta)
  P[2,2]=(1-pi)^L*(p1*alpha+(1-p1)*(1-beta))
  P[2,3]=(1-(1-pi)^L)*(p2*(1-alpha)+(1-p2)*beta)
  P[2,4]=(1-(1-pi)^L)*(p2*alpha+(1-p2)*(1-beta))
  P[2,5]=0
  P[2,6]=0
  
  P[3,1]=0
  P[3,2]=0
  P[3,3]=0
  P[3,4]=0
  P[3,5]=(p2*(1-alpha)+(1-p2)*beta)
  P[3,6]=(p2*alpha+(1-p2)*(1-beta))
  
  P[4,1]=(1-pi)^L*(p1*(1-alpha)+(1-p1)*beta)
  P[4,2]=(1-pi)^L*(p1*alpha+(1-p1)*(1-beta))
  P[4,3]=(1-(1-pi)^L)*(p2*(1-alpha)+(1-p2)*beta)
  P[4,4]=(1-(1-pi)^L)*(p2*alpha+(1-p2)*(1-beta))
  P[4,5]=0
  P[4,6]=0
  
  P[5,1]=0
  P[5,2]=0
  P[5,3]=0
  P[5,4]=0
  P[5,5]=(p2*(1-alpha)+(1-p2)*beta)
  P[5,6]=(p2*alpha+(1-p2)*(1-beta))
  
  P[6,1]=(1-pi)^L*(p1*(1-alpha)+(1-p1)*beta)
  P[6,2]=(1-pi)^L*(p1*alpha+(1-p1)*(1-beta))
  P[6,3]=(1-(1-pi)^L)*(p2*(1-alpha)+(1-p2)*beta)
  P[6,4]=(1-(1-pi)^L)*(p2*alpha+(1-p2)*(1-beta))
  P[6,5]=0
  P[6,6]=0

  # cálculo da invariante ---------------------------------------------------
  A = t(P) - diag(6)
  A[6,] = rep(1,6)
  B = matrix(rep(0,6),ncol = 1)
  B[6,1] = 1
  
  y = solve(A,B)
  

  # Output dos resultados ---------------------------------------------------

  lResult <- list()
  
  lResult$mat_transicao <- P
  lResult$invariante <- y
  
  return(lResult)
  
}
