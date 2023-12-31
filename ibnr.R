#install.packages('ChainLadder')

library(ChainLadder)
library(readxl)

#----- data example
data(package="ChainLadder")

mackRAA <- MackChainLadder(RAA)

plot(mackRAA)

plot(RAA/1000,  main = "Claims development by origin year")

class(RAA)
dim(RAA)

#----- load data  / carga de los datos aronica
tri <- as.data.frame(read_excel(path = "siniestros_IBNR.xlsx", skip = 2))
rownames(tri) <- tri[,1]
tri[,1] <- NULL
colnames(tri) <- 1:10

# triangle class, ready for ChainLadder  /  creacion del objeto tipo triangulo
tri <- as.triangle(as.matrix(tri))


#visualizaciones
plot(tri/1000,  main = "Claims development by origin year")

plot(tri/1000, lattice=TRUE, main = "Claims development by origin year")


# Basic idea

# calculo de los factores de desarrollo
# Calculate age-to-age factors for tri triangle
n <- 10
f <- sapply(1:(n-1),
            function(i){
              sum(tri[c(1:(n-i)),i+1])/sum(tri[c(1:(n-i)),i])
            }
)
f


# modelo lineal de los factores de desarrollo explicado por los anios (log(f-1))
dev.period <- 1:(n-1)
plot(log(f-1) ~ dev.period, 
     main="Log-linear extrapolation of age-to-age factors")
tail.model <- lm(log(f-1) ~ dev.period)
abline(tail.model)


# estudio de la ibnr para periodos mas largos
co <- coef(tail.model)
## extrapolate another 100 dev. period de los factores de desarrollo
tail <- exp(co[1] + c(n:(n + 100)) * co[2]) + 1
f.tail <- prod(tail)
f.tail


# se empieza a estabilizar la tasa a partir de los 10 anios

plot(100*(rev(1/cumprod(rev(c(f, tail[tail>1.0001]))))), t="b",
     main="Expected claims development pattern",
     xlab="Dev. period", ylab="Development % of ultimate loss")

f <- c(f, f.tail)
fulltri <- cbind(tri, Ult = rep(0, 10))
for(k in 1:n){
  fulltri[(n-k+1):n, k+1] <- fulltri[(n-k+1):n,k]*f[k]
}
round(fulltri)

sum(fulltri[ ,11] - getLatestCumulative(tri))


## calcula los ratios sin precisar de un for
linkratios <- c(attr(ata(tri), "vwtd"), tail = 1.05)  #se elige cola a discrecion (factor de arranque)
round(linkratios, 3) # display to only three decimal places


## factores de desarrollo acumulados??
LDF <- rev(cumprod(rev(linkratios)))
names(LDF) <- colnames(tri) # so the display matches the triangle
round(LDF, 3)



currentEval <- getLatestCumulative(tri) # para obtener la diagonal inversa principal de nuestra matriz triangular (ultimos reclamos acumulados de cada anio)
# Reverse the LDFs so the first, least mature factor [1]
#   is applied to the last origin year (1990)
EstdUlt <- currentEval * rev(LDF) #ultima perdida esperada
# Start with the body of the exhibit
Exhibit <- data.frame(currentEval, LDF = round(rev(LDF), 3), EstdUlt)
# Tack on a Total row
Exhibit <- rbind(Exhibit,
                 data.frame(currentEval=sum(currentEval), LDF=NA, EstdUlt=sum(EstdUlt),
                            row.names = "Total"))
Exhibit

lmCL <- function(i, Triangle){
  lm(y~x+0, weights=1/Triangle[,i],
     data=data.frame(x=Triangle[,i], y=Triangle[,i+1]))
}
sapply(lapply(c(1:(n-1)), lmCL, tri), coef)

# Mack chain-ladder
mack <- MackChainLadder(tri, est.sigma="Mack")
mack # same as summary(mack) 

mack$FullTriangle

mack_smmry <- summary(mack) # See also ?summary.MackChainLadder
mack_smmry$ByOrigin

mack_smmry$Totals

plot(mack)

plot(mack, lattice=TRUE)
