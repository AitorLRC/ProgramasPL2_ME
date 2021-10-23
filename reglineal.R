## Programa para realizar la regresión lineal sin usar lm()

v2<- v4

n <- dim(v2)[1]
p <- 9

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

#Calculamos el coeficiente de determinación
R2 <- SCR/SCT; R2
R2c <- 1 - ((SCE/(n-p-1))/(SCT/(n-1))); R2c





LinearReg <- function(datos, list_vars, var_pred){
  #Para empezar definimos el número de observaciones n y el número de variabes p
  n <- dim(datos)[1] ; n
  p <- length(list_vars) ; p
  
  matdatos <- as.matrix(datos)

  #Creamos la matriz X
  X <- matrix(1, nrow = n, ncol = p+1)
  #Sustituimos el resto de columnas por sus correspondientes valores
  j <- 2
  for (i in 1:length(list_vars)){X[,j] <- matdatos[,list_vars[i]]; j = j+1}
  print(X)
  #Le ponemos los nombres a las columnas
  
  
  #Creamos la matriz transpuesta
  Xt <- t(X)
  
  
  #Creamos la matriz y
  y <- matdatos[var_pred]
  
  print(class(y))
  #Calculamos los valores de beta
  beta = solve(Xt %*% X) %*% Xt %*% y
  
  #Hacemos las predicciones
  ypred = X %*% beta
  
  print(X)
  
  #Pasamos a calcular la variabilidad total que se puede expresar como suma de dos términos
  medy <- mean(y)
  SCT <- sum((y-medy)**2)
  SCR <- sum((ypred-medy)**2)
  SCE <- sum((y-ypred)**2)
  
  #Calculamos el coeficiente de determinación
  R2 <- SCR/SCT ; print(R2)
  R2c <- 1 - ((SCE/(n-p-1))/(SCT/(n-1))) ; print(R2c)
  
}

variables <- c('alzada', 'long_cruz', 'ancho_muslos',
               'peri_torax', 'long_grupa', 'peso_nac',
               'edad', 'edad_des', 'sexo')
var_pred <- 'peso'

x<- LinearReg(v2, variables, var_pred)
