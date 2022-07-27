# Func Multiobj Falso Alarme (mL) ====

FMO_FA.mL = function(vet)
{
  # Entrada dos Parametros
  
  # Parametros Probabilísticos do Processo
  
  p1 = 0.999        # Fracao de conformes processo sobre controle
  p2 = 0.95         # Fracao de Conformes processo fora de controle
  pi = 0.0001       # Probabilidade de ocorrencia de shift
  alpha = 0.01      # Probabilidade de classificacao nao cfe em item cfe
  beta = 0.01       # Probabilidade de classificacao cfe em item nao cfe
  m = vet[1]
  L = vet[2]

  # Parametros de custo
  
  c_i = 0.25        # Custo de inspecao
  c_nc = 20         # Custo envio de nao conformidade
  c_a = 100         # Custo de ajuste
  c_s = 2           # Custo de descarte de peca inspecionada
  
  # Contrucao da Matriz de Transicao e Calculo do Vetor Estacionario
  
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
  
  A = t(P) - diag(6)
  A[6,] = rep(1,6)
  B = matrix(rep(0,6),ncol = 1)
  B[6,1] = 1
  
  y = solve(A,B)
  
  # Fim da Construcao da matriz e Vetor Estacionario
  
  # Inicio do Calculo do custo - O Calculo dependera se conprimento e = m ou L
  
  # Probabilidade de estar em s0k1 e ciclo ter comprimento m
  z1=(y[1]*P[1,1]+y[3]*P[3,1]+y[5]*P[5,1])/
    (y[1]*P[1,1]+y[2]*P[2,1]+y[3]*P[3,1]+y[4]*P[4,1]+y[5]*P[5,1]+y[6]*P[6,1])
  
  #Probabilidade de estar em s0k1 e ciclo ter comprimento L
  z2=(y[2]*P[2,1]+y[4]*P[4,1]+y[6]*P[6,1])/
    (y[1]*P[1,1]+y[2]*P[2,1]+y[3]*P[3,1]+y[4]*P[4,1]+y[5]*P[5,1]+y[6]*P[6,1])
  
  #Probabilidade de estar em s0k0 e ciclo ter comprimento m
  z3=(y[1]*P[1,2]+y[3]*P[3,2]+y[5]*P[5,2])/
    (y[1]*P[1,2]+y[2]*P[2,2]+y[3]*P[3,2]+y[4]*P[4,2]+y[5]*P[5,2]+y[6]*P[6,2])
  
  #Probabilidade de estar em s0k0 e ciclo ter comprimento L
  z4=(y[2]*P[2,2]+y[4]*P[4,2]+y[6]*P[6,2])/
    (y[1]*P[1,2]+y[2]*P[2,2]+y[3]*P[3,2]+y[4]*P[4,2]+y[5]*P[5,2]+y[6]*P[6,2])
  
  #Probabilidade de estar em s1k1 e ciclo ter comprimento m
  z5=(y[1]*P[1,3]+y[3]*P[3,3]+y[5]*P[5,3])/
    (y[1]*P[1,3]+y[2]*P[2,3]+y[3]*P[3,3]+y[4]*P[4,3]+y[5]*P[5,3]+y[6]*P[6,3])
  
  #Probabilidade de estar em s1k1 e ciclo ter comprimento L
  z6=(y[2]*P[2,3]+y[4]*P[4,3]+y[6]*P[6,3])/
    (y[1]*P[1,3]+y[2]*P[2,3]+y[3]*P[3,3]+y[4]*P[4,3]+y[5]*P[5,3]+y[6]*P[6,3])
  
  
  #Probabilidade de estar em s1k0 e ciclo ter comprimento m
  z7=(y[1]*P[1,4]+y[3]*P[3,4]+y[5]*P[5,4])/
    (y[1]*P[1,4]+y[2]*P[2,4]+y[3]*P[3,4]+y[4]*P[4,4]+y[5]*P[5,4]+y[6]*P[6,4])
  
  #Probabilidade de estar em s1k0 e ciclo ter comprimento L
  z8=(y[2]*P[2,4]+y[4]*P[4,4]+y[6]*P[6,4])/
    (y[1]*P[1,4]+y[2]*P[2,4]+y[3]*P[3,4]+y[4]*P[4,4]+y[5]*P[5,4]+y[6]*P[6,4])
  
  
  #Probabilidade de estar em s2k1 e ciclo ter comprimento m
  z9=(y[1]*P[1,5]+y[3]*P[3,5]+y[5]*P[5,5])/
    (y[1]*P[1,5]+y[2]*P[2,5]+y[3]*P[3,5]+y[4]*P[4,5]+y[5]*P[5,5]+y[6]*P[6,5])
  
  #Probabilidade de estar em s2k1 e ciclo ter comprimento L
  z10=(y[2]*P[2,5]+y[4]*P[4,5]+y[6]*P[6,5])/
    (y[1]*P[1,5]+y[2]*P[2,5]+y[3]*P[3,5]+y[4]*P[4,5]+y[5]*P[5,5]+y[6]*P[6,5])
  
  
  #Probabilidade de estar em s2k0 e ciclo ter comprimento m
  z11=(y[1]*P[1,6]+y[3]*P[3,6]+y[5]*P[5,6])/
    (y[1]*P[1,6]+y[2]*P[2,6]+y[3]*P[3,6]+y[4]*P[4,6]+y[5]*P[5,6]+y[6]*P[6,6])
  
  #Probabilidade de estar em s2k0 e ciclo ter comprimento L
  z12=(y[2]*P[2,6]+y[4]*P[4,6]+y[6]*P[6,6])/
    (y[1]*P[1,6]+y[2]*P[2,6]+y[3]*P[3,6]+y[4]*P[4,6]+y[5]*P[5,6]+y[6]*P[6,6])
  
  #cálculo dos Custos
  custo = rep(NA,6)
  
  custo[1]=z1*(c_nc*(m-1)*(1-p1)+c_i+c_s)+z2*(c_nc*(L-1)*(1-p1)+c_i+c_s)#s0k1
  
  custo[2]=z3*(c_nc*(m-1)*(1-p1)+c_i+c_a+c_s)+z4*(c_nc*(L-1)*(1-p1)+c_i+c_a+c_s)#s0k0
  
  # Calculo s1 e s2
  ## s1
  
  i <- 1:m
  
  k <- (pi*(1-pi)^(i-1))/(1-(1-pi)^m)
  k <- k*((i-1)*(1-p1)+(m-i)*(1-p2))
  s1 <- sum(k)

  # s2
  
  i <- 1:L
  
  k <- (pi*(1-pi)^(i-1))/(1-(1-pi)^L)
  k <- k*((i-1)*(1-p1)+(L-i)*(1-p2))
  s2 <- sum(k)
  
  custo[3]=z5*(s1*c_nc+c_i+c_s)+z6*(s2*c_nc+c_i+c_s) #s1k1
  custo[4]=z7*(s1*c_nc+c_i+c_s+c_a)+z8*(s2*c_nc+c_i+c_s+c_a) #s1k0
  custo[5]=z9*(c_nc*(m-1)*(1-p2)+c_i+c_s)+z10*(c_nc*(L-1)*(1-p2)+c_i+c_s) #s2k1
  custo[6]=z11*(c_nc*(m-1)*(1-p2)+c_i+c_s+c_a)+z12*(c_nc*(L-1)*(1-p2)+c_i+c_s+c_a) #s2k0
  
  #Fim do Cálculo dos Custos
  
  #Cálculo do Comprimento
  T = rep(NA, 6)
  
  T[1] = z1*(m-1)+z2*(L-1)
  T[2] = z3*(m-1)+z4*(L-1)
  T[3] = z5*(m-1)+z6*(L-1)
  T[4] = z7*(m-1)+z8*(L-1)
  T[5] = z9*(m-1)+z10*(L-1)
  T[6] = z11*(m-1)+z12*(L-1)
  
  #Fim do cálculo do Comprimento
  
  #Cálculo do Custo Médio Total
  CP=y[1]*custo[1]+y[2]*custo[2]+y[3]*custo[3]+y[4]*custo[4]+y[5]*custo[5]+y[6]*custo[6]
  
  #Cálculo do Comprimento Médio Total
  TM=y[1]*T[1]+y[2]*T[2]+y[3]*T[3]+y[4]*T[4]+y[5]*T[5]+y[6]*T[6]
  
  #Cálculo do custo Médio por Unidade Produzida e Enviada ao "Mercado"  
  CM=CP/TM
  
  FA = y[2]/sum(y[1:2])
  
  return(c(CM, FA)) 
}

# Func Multiobj AATS (mL) ====

FMO_AATS.mL = function(m,L)
{
  # Entrada dos Parametros
  
  # Parametros Probabilísticos do Processo
  
  p1 = 0.999        # Fracao de conformes processo sobre controle
  p2 = 0.95         # Fracao de Conformes processo fora de controle
  pi = 0.0001       # Probabilidade de ocorrencia de shift
  alpha = 0.01      # Probabilidade de classificacao nao cfe em item cfe
  beta = 0.01       # Probabilidade de classificacao cfe em item nao cfe
  #m = 41
  #L = 896
  
  # Parametros de custo
  
  c_i = 0.25        # Custo de inspecao
  c_nc = 20         # Custo envio de nao conformidade
  c_a = 100         # Custo de ajuste
  c_s = 2           # Custo de descarte de peca inspecionada
  
  # Contrucao da Matriz de Transicao e Calculo do Vetor Estacionario
  
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
  
  A = t(P) - diag(6)
  A[6,] = rep(1,6)
  B = matrix(rep(0,6),ncol = 1)
  B[6,1] = 1
  
  y = solve(A,B)
  
  # Fim da Construcao da matriz e Vetor Estacionario
  
  # Inicio do Calculo do custo - O Calculo dependera se conprimento e = m ou L
  
  # Probabilidade de estar em s0k1 e ciclo ter comprimento m
  z1=(y[1]*P[1,1]+y[3]*P[3,1]+y[5]*P[5,1])/
    (y[1]*P[1,1]+y[2]*P[2,1]+y[3]*P[3,1]+y[4]*P[4,1]+y[5]*P[5,1]+y[6]*P[6,1])
  
  #Probabilidade de estar em s0k1 e ciclo ter comprimento L
  z2=(y[2]*P[2,1]+y[4]*P[4,1]+y[6]*P[6,1])/
    (y[1]*P[1,1]+y[2]*P[2,1]+y[3]*P[3,1]+y[4]*P[4,1]+y[5]*P[5,1]+y[6]*P[6,1])
  
  #Probabilidade de estar em s0k0 e ciclo ter comprimento m
  z3=(y[1]*P[1,2]+y[3]*P[3,2]+y[5]*P[5,2])/
    (y[1]*P[1,2]+y[2]*P[2,2]+y[3]*P[3,2]+y[4]*P[4,2]+y[5]*P[5,2]+y[6]*P[6,2])
  
  #Probabilidade de estar em s0k0 e ciclo ter comprimento L
  z4=(y[2]*P[2,2]+y[4]*P[4,2]+y[6]*P[6,2])/
    (y[1]*P[1,2]+y[2]*P[2,2]+y[3]*P[3,2]+y[4]*P[4,2]+y[5]*P[5,2]+y[6]*P[6,2])
  
  #Probabilidade de estar em s1k1 e ciclo ter comprimento m
  z5=(y[1]*P[1,3]+y[3]*P[3,3]+y[5]*P[5,3])/
    (y[1]*P[1,3]+y[2]*P[2,3]+y[3]*P[3,3]+y[4]*P[4,3]+y[5]*P[5,3]+y[6]*P[6,3])
  
  #Probabilidade de estar em s1k1 e ciclo ter comprimento L
  z6=(y[2]*P[2,3]+y[4]*P[4,3]+y[6]*P[6,3])/
    (y[1]*P[1,3]+y[2]*P[2,3]+y[3]*P[3,3]+y[4]*P[4,3]+y[5]*P[5,3]+y[6]*P[6,3])
  
  
  #Probabilidade de estar em s1k0 e ciclo ter comprimento m
  z7=(y[1]*P[1,4]+y[3]*P[3,4]+y[5]*P[5,4])/
    (y[1]*P[1,4]+y[2]*P[2,4]+y[3]*P[3,4]+y[4]*P[4,4]+y[5]*P[5,4]+y[6]*P[6,4])
  
  #Probabilidade de estar em s1k0 e ciclo ter comprimento L
  z8=(y[2]*P[2,4]+y[4]*P[4,4]+y[6]*P[6,4])/
    (y[1]*P[1,4]+y[2]*P[2,4]+y[3]*P[3,4]+y[4]*P[4,4]+y[5]*P[5,4]+y[6]*P[6,4])
  
  
  #Probabilidade de estar em s2k1 e ciclo ter comprimento m
  z9=(y[1]*P[1,5]+y[3]*P[3,5]+y[5]*P[5,5])/
    (y[1]*P[1,5]+y[2]*P[2,5]+y[3]*P[3,5]+y[4]*P[4,5]+y[5]*P[5,5]+y[6]*P[6,5])
  
  #Probabilidade de estar em s2k1 e ciclo ter comprimento L
  z10=(y[2]*P[2,5]+y[4]*P[4,5]+y[6]*P[6,5])/
    (y[1]*P[1,5]+y[2]*P[2,5]+y[3]*P[3,5]+y[4]*P[4,5]+y[5]*P[5,5]+y[6]*P[6,5])
  
  
  #Probabilidade de estar em s2k0 e ciclo ter comprimento m
  z11=(y[1]*P[1,6]+y[3]*P[3,6]+y[5]*P[5,6])/
    (y[1]*P[1,6]+y[2]*P[2,6]+y[3]*P[3,6]+y[4]*P[4,6]+y[5]*P[5,6]+y[6]*P[6,6])
  
  #Probabilidade de estar em s2k0 e ciclo ter comprimento L
  z12=(y[2]*P[2,6]+y[4]*P[4,6]+y[6]*P[6,6])/
    (y[1]*P[1,6]+y[2]*P[2,6]+y[3]*P[3,6]+y[4]*P[4,6]+y[5]*P[5,6]+y[6]*P[6,6])
  
  #cálculo dos Custos
  custo = rep(NA,6)
  
  custo[1]=z1*(c_nc*(m-1)*(1-p1)+c_i+c_s)+z2*(c_nc*(L-1)*(1-p1)+c_i+c_s)#s0k1
  
  custo[2]=z3*(c_nc*(m-1)*(1-p1)+c_i+c_a+c_s)+z4*(c_nc*(L-1)*(1-p1)+c_i+c_a+c_s)#s0k0
  
  # Calculo s1 e s2
  ## s1
  
  i <- 1:m
  
  k <- (pi*(1-pi)^(i-1))/(1-(1-pi)^m)
  k <- k*((i-1)*(1-p1)+(m-i)*(1-p2))
  s1 <- sum(k)
  
  # s2
  
  i <- 1:L
  
  k <- (pi*(1-pi)^(i-1))/(1-(1-pi)^L)
  k <- k*((i-1)*(1-p1)+(L-i)*(1-p2))
  s2 <- sum(k)
  
  custo[3]=z5*(s1*c_nc+c_i+c_s)+z6*(s2*c_nc+c_i+c_s) #s1k1
  custo[4]=z7*(s1*c_nc+c_i+c_s+c_a)+z8*(s2*c_nc+c_i+c_s+c_a) #s1k0
  custo[5]=z9*(c_nc*(m-1)*(1-p2)+c_i+c_s)+z10*(c_nc*(L-1)*(1-p2)+c_i+c_s) #s2k1
  custo[6]=z11*(c_nc*(m-1)*(1-p2)+c_i+c_s+c_a)+z12*(c_nc*(L-1)*(1-p2)+c_i+c_s+c_a) #s2k0
  
  #Fim do Cálculo dos Custos
  
  #Cálculo do Comprimento
  T = rep(NA, 6)
  
  T[1] = z1*(m-1)+z2*(L-1)
  T[2] = z3*(m-1)+z4*(L-1)
  T[3] = z5*(m-1)+z6*(L-1)
  T[4] = z7*(m-1)+z8*(L-1)
  T[5] = z9*(m-1)+z10*(L-1)
  T[6] = z11*(m-1)+z12*(L-1)
  
  #Fim do cálculo do Comprimento
  
  #Cálculo do Custo Médio Total
  CP=y[1]*custo[1]+y[2]*custo[2]+y[3]*custo[3]+y[4]*custo[4]+y[5]*custo[5]+y[6]*custo[6]
  
  #Cálculo do Comprimento Médio Total
  TM=y[1]*T[1]+y[2]*T[2]+y[3]*T[3]+y[4]*T[4]+y[5]*T[5]+y[6]*T[6]
  
  #Cálculo do custo Médio por Unidade Produzida e Enviada ao "Mercado"  
  CM=CP/TM
  
  AATS <- (1/P[3,5])*(m-1)
  
  return(c(CM, AATS)) 
}

# Func Multiobj AATS/FA (mL) ====

FMO_AATS_FA.mL = function(m,L)
{
  # Entrada dos Parametros
  
  # Parametros Probabilísticos do Processo
  
  p1 = 0.999        # Fracao de conformes processo sobre controle
  p2 = 0.95         # Fracao de Conformes processo fora de controle
  pi = 0.0001       # Probabilidade de ocorrencia de shift
  alpha = 0.01      # Probabilidade de classificacao nao cfe em item cfe
  beta = 0.01       # Probabilidade de classificacao cfe em item nao cfe
  #m = 41
  #L = 896
  
  # Parametros de custo
  
  c_i = 0.25        # Custo de inspecao
  c_nc = 20         # Custo envio de nao conformidade
  c_a = 100         # Custo de ajuste
  c_s = 2           # Custo de descarte de peca inspecionada
  
  # Contrucao da Matriz de Transicao e Calculo do Vetor Estacionario
  
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
  
  A = t(P) - diag(6)
  A[6,] = rep(1,6)
  B = matrix(rep(0,6),ncol = 1)
  B[6,1] = 1
  
  y = solve(A,B)
  
  # Fim da Construcao da matriz e Vetor Estacionario
  
  # Inicio do Calculo do custo - O Calculo dependera se conprimento e = m ou L
  
  # Probabilidade de estar em s0k1 e ciclo ter comprimento m
  z1=(y[1]*P[1,1]+y[3]*P[3,1]+y[5]*P[5,1])/
    (y[1]*P[1,1]+y[2]*P[2,1]+y[3]*P[3,1]+y[4]*P[4,1]+y[5]*P[5,1]+y[6]*P[6,1])
  
  #Probabilidade de estar em s0k1 e ciclo ter comprimento L
  z2=(y[2]*P[2,1]+y[4]*P[4,1]+y[6]*P[6,1])/
    (y[1]*P[1,1]+y[2]*P[2,1]+y[3]*P[3,1]+y[4]*P[4,1]+y[5]*P[5,1]+y[6]*P[6,1])
  
  #Probabilidade de estar em s0k0 e ciclo ter comprimento m
  z3=(y[1]*P[1,2]+y[3]*P[3,2]+y[5]*P[5,2])/
    (y[1]*P[1,2]+y[2]*P[2,2]+y[3]*P[3,2]+y[4]*P[4,2]+y[5]*P[5,2]+y[6]*P[6,2])
  
  #Probabilidade de estar em s0k0 e ciclo ter comprimento L
  z4=(y[2]*P[2,2]+y[4]*P[4,2]+y[6]*P[6,2])/
    (y[1]*P[1,2]+y[2]*P[2,2]+y[3]*P[3,2]+y[4]*P[4,2]+y[5]*P[5,2]+y[6]*P[6,2])
  
  #Probabilidade de estar em s1k1 e ciclo ter comprimento m
  z5=(y[1]*P[1,3]+y[3]*P[3,3]+y[5]*P[5,3])/
    (y[1]*P[1,3]+y[2]*P[2,3]+y[3]*P[3,3]+y[4]*P[4,3]+y[5]*P[5,3]+y[6]*P[6,3])
  
  #Probabilidade de estar em s1k1 e ciclo ter comprimento L
  z6=(y[2]*P[2,3]+y[4]*P[4,3]+y[6]*P[6,3])/
    (y[1]*P[1,3]+y[2]*P[2,3]+y[3]*P[3,3]+y[4]*P[4,3]+y[5]*P[5,3]+y[6]*P[6,3])
  
  
  #Probabilidade de estar em s1k0 e ciclo ter comprimento m
  z7=(y[1]*P[1,4]+y[3]*P[3,4]+y[5]*P[5,4])/
    (y[1]*P[1,4]+y[2]*P[2,4]+y[3]*P[3,4]+y[4]*P[4,4]+y[5]*P[5,4]+y[6]*P[6,4])
  
  #Probabilidade de estar em s1k0 e ciclo ter comprimento L
  z8=(y[2]*P[2,4]+y[4]*P[4,4]+y[6]*P[6,4])/
    (y[1]*P[1,4]+y[2]*P[2,4]+y[3]*P[3,4]+y[4]*P[4,4]+y[5]*P[5,4]+y[6]*P[6,4])
  
  
  #Probabilidade de estar em s2k1 e ciclo ter comprimento m
  z9=(y[1]*P[1,5]+y[3]*P[3,5]+y[5]*P[5,5])/
    (y[1]*P[1,5]+y[2]*P[2,5]+y[3]*P[3,5]+y[4]*P[4,5]+y[5]*P[5,5]+y[6]*P[6,5])
  
  #Probabilidade de estar em s2k1 e ciclo ter comprimento L
  z10=(y[2]*P[2,5]+y[4]*P[4,5]+y[6]*P[6,5])/
    (y[1]*P[1,5]+y[2]*P[2,5]+y[3]*P[3,5]+y[4]*P[4,5]+y[5]*P[5,5]+y[6]*P[6,5])
  
  
  #Probabilidade de estar em s2k0 e ciclo ter comprimento m
  z11=(y[1]*P[1,6]+y[3]*P[3,6]+y[5]*P[5,6])/
    (y[1]*P[1,6]+y[2]*P[2,6]+y[3]*P[3,6]+y[4]*P[4,6]+y[5]*P[5,6]+y[6]*P[6,6])
  
  #Probabilidade de estar em s2k0 e ciclo ter comprimento L
  z12=(y[2]*P[2,6]+y[4]*P[4,6]+y[6]*P[6,6])/
    (y[1]*P[1,6]+y[2]*P[2,6]+y[3]*P[3,6]+y[4]*P[4,6]+y[5]*P[5,6]+y[6]*P[6,6])
  
  #cálculo dos Custos
  custo = rep(NA,6)
  
  custo[1]=z1*(c_nc*(m-1)*(1-p1)+c_i+c_s)+z2*(c_nc*(L-1)*(1-p1)+c_i+c_s)#s0k1
  
  custo[2]=z3*(c_nc*(m-1)*(1-p1)+c_i+c_a+c_s)+z4*(c_nc*(L-1)*(1-p1)+c_i+c_a+c_s)#s0k0
  
  # Calculo s1 e s2
  ## s1
  
  i <- 1:m
  
  k <- (pi*(1-pi)^(i-1))/(1-(1-pi)^m)
  k <- k*((i-1)*(1-p1)+(m-i)*(1-p2))
  s1 <- sum(k)
  
  # s2
  
  i <- 1:L
  
  k <- (pi*(1-pi)^(i-1))/(1-(1-pi)^L)
  k <- k*((i-1)*(1-p1)+(L-i)*(1-p2))
  s2 <- sum(k)
  
  custo[3]=z5*(s1*c_nc+c_i+c_s)+z6*(s2*c_nc+c_i+c_s) #s1k1
  custo[4]=z7*(s1*c_nc+c_i+c_s+c_a)+z8*(s2*c_nc+c_i+c_s+c_a) #s1k0
  custo[5]=z9*(c_nc*(m-1)*(1-p2)+c_i+c_s)+z10*(c_nc*(L-1)*(1-p2)+c_i+c_s) #s2k1
  custo[6]=z11*(c_nc*(m-1)*(1-p2)+c_i+c_s+c_a)+z12*(c_nc*(L-1)*(1-p2)+c_i+c_s+c_a) #s2k0
  
  #Fim do Cálculo dos Custos
  
  #Cálculo do Comprimento
  T = rep(NA, 6)
  
  T[1] = z1*(m-1)+z2*(L-1)
  T[2] = z3*(m-1)+z4*(L-1)
  T[3] = z5*(m-1)+z6*(L-1)
  T[4] = z7*(m-1)+z8*(L-1)
  T[5] = z9*(m-1)+z10*(L-1)
  T[6] = z11*(m-1)+z12*(L-1)
  
  #Fim do cálculo do Comprimento
  
  #Cálculo do Custo Médio Total
  CP=y[1]*custo[1]+y[2]*custo[2]+y[3]*custo[3]+y[4]*custo[4]+y[5]*custo[5]+y[6]*custo[6]
  
  #Cálculo do Comprimento Médio Total
  TM=y[1]*T[1]+y[2]*T[2]+y[3]*T[3]+y[4]*T[4]+y[5]*T[5]+y[6]*T[6]
  
  #Cálculo do custo Médio por Unidade Produzida e Enviada ao "Mercado"  
  CM=CP/TM
  
  # Cálculo dos objetivos estatíticos
  
  AATS <- (1/P[3,5])*(m-1)
  FA <- y[2]/sum(y[1:2])
  
  return(c(CM, AATS, FA))
}