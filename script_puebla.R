# script Trabajo Puebla 2024
rm(list = ls())

# librerías
library(tidyverse)   # para manejar los datos y graficar
library(readxl)    # para cargar los datos
library(ChainLadder)    # para hacer Chain-Ladder y demas


tri <- as.data.frame(read_excel(path = "siniestros_IBNR.xlsx", skip = 2))
rownames(tri) <- tri[,1]
tri[,1] <- NULL
colnames(tri) <- 1:10
rownames(tri) <- 1999:2008  # se simplifica el nombre de los años a años simples para que facilite el uso de algunas funciones


# se transforma en un objeto de clase triangulo
(tri <- as.triangle(as.matrix(tri)))

class(tri)

# grafico de desarrollo de los siniestros
plot(tri/1000, ylab="Siniestros en miles de pesos", xlab="Periodo de desarrollo")
# reconoce un objeto de clase triangle


# para calcular los factores de desarrollo

# ata es del paquete chain-ladder y calcula los q's
# calcula los factores individuales, con vwtd se obtiene el total del período
# como fue presentado, son "smpl" se obtiene el promedio simple de los factores individuales


linkratios <- c(attr(ata(tri), "vwtd"), tail = 1)  #se elige cola a discrecion (q_n)
linkratios <- round(linkratios, 3)


# se calculan los acumulados multiplicando desde el final
QAcum <- rev(cumprod(rev(linkratios)))
names(QAcum) <- colnames(tri) # so the display matches the triangle
QAcum <- round(QAcum, 3)


# se obtiene la última pérdida
Incurridos_Acumulados <- getLatestCumulative(tri) # para obtener la diagonal inversa principal de nuestra matriz triangular (ultimos reclamos acumulados de cada anio)

# se calcula la última pérdida esperada con los factores de desarrollo acumulado
Perdida_Esperada <- Incurridos_Acumulados * rev(QAcum) #ultima perdida esperada

# Es análogo a utilizar los factores simples por período de desarrollo (linkratios), y completar toda la matriz


# se calcula la reserva de IBNR para cada año
Reserva_IBNR = Perdida_Esperada - Incurridos_Acumulados

# presentacion

Exhibit <- data.frame(Incurridos_Acumulados, QAcum = round(rev(QAcum), 3), Perdida_Esperada, Reserva_IBNR)


Exhibit <- rbind(Exhibit,data.frame(Incurridos_Acumulados=sum(Incurridos_Acumulados),
                                    QAcum=NA,Perdida_Esperada=sum(Perdida_Esperada),
                                    Reserva_IBNR=sum(Reserva_IBNR), row.names = "Total"))

#col_names <- c("Último siniestro incurrido del período, X\u208i,c\u209i", "Factor de desarrollo acumulado, Q\u208j", "Pérdida Esperada, X\u208i", "IBNR por período, IBNR\u208i") 
#col_names <- c("Último siniestro incurrido del período, $X_{i,c_i}$","Factor de desarrollo acumulado, $Q_j$","Pérdida Esperada, $X_i$","IBNR por período, $IBNR_i$")
col_names <- c("Último siniestro incurrido","Factor de desarrollo acumulado","Pérdida Esperada","IBNR")
names(Exhibit) <- col_names

Exhibit   #para poder ver los resultados mejor

# Chain Ladder con Regresion

n <- 10
q <- sapply(1:(n-1),
            function(i){
              sum(tri[c(1:(n-i)),i+1])/sum(tri[c(1:(n-i)),i])
            }
)

factores_q1 <- data.frame(q)
col_names <- c("Factores de desarrollo") 
names(factores_q1) <- col_names

factores_q1



# modelo lineal de los factores de desarrollo explicado por los anios (log(f-1))
dev.period <- 1:(n-1)
tail.model <- lm(log(q-1) ~ dev.period)

plot(log(q-1) ~ dev.period, main="", xlab="Período, j")
abline(tail.model)

# estudio de la ibnr para periodos mas largos
co <- coef(tail.model)
## extrapolate another 100 dev. period de los factores de desarrollo
tail <- exp(co[1] + c(n:(n + 100)) * co[2]) + 1
f.tail <- prod(tail)

plot(100*(rev(1/cumprod(rev(c(q, tail))))), t="b",
     main="", xlab="Período, j", ylab="100*1/Q")

plot((rev(1/cumprod(rev(c(q, tail[tail>1.0001]))))), t="b",
     main="", xlab="Período, j", ylab="1/Q")

Qhat = f.tail


qs <- c(q,Qhat)

Qs <- rev(cumprod(rev(qs)))
names(Qs) <- colnames(tri) # so the display matches the triangle
Qs <- round(Qs, 3)



Incurridos_Acumulados2 <- getLatestCumulative(tri) # para obtener la diagonal inversa principal de nuestra matriz triangular (ultimos reclamos acumulados de cada anio)
# Reverse the LDFs so the first, least mature factor [1]
#   is applied to the last origin year (1990)
Perdida_Esperada2 <- Incurridos_Acumulados2 * rev(Qs) #ultima perdida esperada
# Start with the body of the exhibit

Reserva_IBNR2 = Perdida_Esperada2 - Incurridos_Acumulados2

Exhibit2 <- data.frame(Incurridos_Acumulados2, Qs = round(rev(Qs), 3), Perdida_Esperada2, Reserva_IBNR2)
# Tack on a Total row

Exhibit2 <- rbind(Exhibit2,
                  data.frame(Incurridos_Acumulados2=sum(Incurridos_Acumulados2), Qs=NA, Perdida_Esperada2=sum(Perdida_Esperada2),
                             Reserva_IBNR2 = sum(Reserva_IBNR2),                            
                             row.names = "Total"))

col_names <- c("Último siniestro incurrido","Factor de desarrollo acumulado","Pérdida Esperada","IBNR")
names(Exhibit2) <- col_names

Exhibit2

# Mack Chain-Ladder

mackTRI <- MackChainLadder(tri)

summary(mackTRI)$ByOrigin

summary(mackTRI)$Totals


plot(mackTRI)
plot(mackTRI, lattice=TRUE)



# Munich Chain Ladder
# se carga la matriz de pagos acumulados

acum <- as.data.frame(read_excel(path = "pagos_acumulados_IBNR.xlsx", skip = 2))
rownames(acum) <- acum[,1]
acum[,1] <- NULL
colnames(acum) <- 1:10
rownames(acum) <- 1999:2008  # se simplifica el nombre de los años a años simples para que facilite el uso de algunas funciones

# triangle class, ready for ChainLadder  /  creacion del objeto tipo triangulo
acum <- as.triangle(as.matrix(acum))



Munich_Chain <- MunichChainLadder(acum,tri)


summary(Munich_Chain)$ByOrigin
summary(Munich_Chain)$Totals

(Reserva_Munich <- summary(Munich_Chain)$Totals[2,2]-summary(Munich_Chain)$Totals[1,2])

plot(Munich_Chain)


# Bootstrap


Bootstrap <- BootChainLadder(tri,R=1000, "gamma", seed=201223)
Bootstrap1 <- BootChainLadder(tri,R=1000, "od.pois", seed=201223)

summary(Bootstrap)$ByOrigin
summary(Bootstrap)$Totals

plot(Bootstrap)
