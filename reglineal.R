## Programa para realizar la regresión lineal sin usar lm()

#Defino el directorio en el que se encuentra el set de datos
dir <- "C:/Users/aitor/Desktop/Master Modelizacion/Modelización estadística/Plab2/v1.RData"

#Cargo el set de datos
load(dir)

v1[17232,] <- NA

v2<- v4

n <- dim(v2)[1]
p <- dim(v2)[2]

#Empezamos definiendo los vectores que componen la matriz
unos  <- replicate(n, 1)

#Creamos la matriz X
X <- matrix(c(unos, v2$alzada, v2$long_cruz, v2$ancho_muslos, 
                   v2$peri_torax, v2$long_grupa, v2$peso_nac, 
                   v2$edad, v2$edad_des, v2$sexo), ncol = 10)
#Le ponemos nombres a las columnas de X
colnames(X) <- c('unos', 'alzada', 'long_cruz', 'ancho_muslos',
                 'peri_torax', 'long_grupa', 'peso_nac',
                 'edad', 'edad_des', 'sexo')

#X <- matrix(c(unos, v2$long_cruz, v2$peri_torax, v2$long_grupa, v2$sexo), ncol = 5)

#Creamos la matriz X transpuesta
Xt <- t(X)

#Creamos la matriz y
y <- matrix(v2$peso, ncol = 1)
#Le ponemos nombre a la columna de la matriz y
colnames(y) <- c('peso')


#Calculamos los valores de beta
beta = solve(Xt %*% X) %*% Xt %*% y; beta


#Hacemos las predicciones
ypred = X %*% beta

plot(y~ypred)

#Pasamos a calcular la variabilidad total que se puede expresar como suma de dos términos
medy <- mean(y)
SCT <- sum((y-medy)**2)
SCR <- sum((ypred-medy)**2)
SCE <- sum((y-ypred)**2)
 fsa dfasdf
#Calculamos el coeficiente de determinación
R2 <- SCR/SCT; R2
R2c <- 1 - ((SCE/(n-p-1))/(SCT/(n-1))); R2c
