# solvencia plurianual de las aseguradores

# C)
# calculo R y delta
E1_X <- 0.050 # millones
alpha2.1_X <- 0.060 # millones
E1_N <- 17500
V1_N <- 6300000


R <- alpha2.1_X/E1_X^2
R

delta <- E1_N^2/(V1_N-E1_N)
delta

# D)
# esperanza de N y momentos de X
t <- 1:5
r_i <- 1.04
r_N <- 1.18
r_x <- 1.05
ro <- 0.08

    
Et_N <- NULL
Et_X <- NULL
alpha2t_X <- NULL
alpha3t_X <- NULL

for (i in t) {
  Et_N[i] <- r_N^(i-1)*E1_N
  Et_X[i] <- r_x^(i-1)*E1_X
  alpha2t_X[i] <- r_x^(i-1)*0.060 
  alpha3t_X[i] <- r_x^(i-1)*1.1
}

cbind(Et_N,Et_X,alpha2t_X,alpha3t_X)

# E)

E_Yt <- NULL
V_Yt <- NULL
D_Yt <- NULL
gamma3_Yt <- NULL

for (i in t) {
  E_Yt[i] <- E1_N*E1_X*(r_x*r_N)^(i-1)
  V_Yt[i] <- Et_N[i]*alpha2t_X[i] + (Et_N[i]^2)*(Et_X[i]^2)/delta
  D_Yt[i] <- sqrt(V_Yt[i])
  gamma3_Yt[i] <- E_Yt[i]^3/D_Yt[i]^3 * ( (alpha3t_X[i]/(Et_N[i]^2*Et_X[i]^3)) + 
                                          ((3*alpha2t_X[i])/(delta*Et_N[i]*Et_X[i]^2)) + 
                                          (2/delta^2) )
}

cbind(E_Yt,D_Yt,gamma3_Yt)

# F)

U0 <- 900 # millones
tasa_i <- 0.04
k <- 1.25 # relacion reservas tecnicas - primas
d <- 1.5 # desfasaje temporal
P1 <- 800 # millones

E_Ut <- NULL
V_Ut <- NULL
D_Ut <- NULL

for (i in t) {
  E_Ut[i] <- U0 * r_i^i + (1+ro+((tasa_i*k)/(r_N*r_x))-r_x^d) * P1 * (((r_N*r_x)^i - r_i^i)/((r_N*r_x) - r_i)) 
  temp <- NULL 
  for (j in 1:i) {
    temp[j] <- E_Yt[j]^2 * ((R/Et_N[j])+(1/delta)) * r_i^(2*(i-j))  
  }
  V_Ut[i] <- sum(temp)
  D_Ut[i] <- sqrt(V_Ut[i])
}

cbind(E_Ut,D_Ut)

# G)

yt <- NULL
zt <- NULL
P_Z <- NULL
insolvencia_t <- NULL

for (i in t) {
  yt[i] <- E_Ut[i]/D_Ut[i] 
  zt[i] <- -3/gamma3_Yt[i] + sqrt( 1 + 9/(gamma3_Yt[i]^2) + 6/gamma3_Yt[i] * yt[i] ) 
  P_Z[i] <- pnorm(zt[i])
  insolvencia_t[i] <- round(1 - P_Z[i],6)
}

cbind(yt,zt,P_Z,insolvencia_t)






