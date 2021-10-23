## Funci贸n para hacer una regresi贸n lineal
#datos es el dataframe de datos con el que trabajaremos
#list_vars es una lista con los nombres de las varibles indepentientes
#var_pred es el nombre de la variable independiente que queremos predecir
LinearReg <- function(datos, list_vars, var_pred){
  #Para empezar definimos el n煤mero de observaciones n y el n煤mero de variabes p
  n <- dim(datos)[1] ; n
  p <- length(list_vars) ; p
  
  #Convertimos el dataframe en una matriz
  matdatos <- as.matrix(datos)

  #Creamos la matriz X
  X <- matrix(1, nrow = n, ncol = p+1)
  #Sustituimos el resto de columnas por sus correspondientes valores
  j <- 2
  for (i in 1:length(list_vars)){X[,j] <- matdatos[,list_vars[i]]; j = j+1}
  #Le ponemos los nombres a las columnas
  colnames(X) <- append(c('Independiente'), list_vars)
  
  #Creamos la matriz transpuesta
  Xt <- t(X)
  
  #Creamos la matriz y
  y <- matdatos[,var_pred]
  
  #Calculamos los valores de beta
  beta = solve(Xt %*% X) %*% Xt %*% y
  
  colnames(beta) <- c('Coeficientes')
  #Hacemos las predicciones
  ypred = X %*% beta
  
  #Pasamos a calcular la variabilidad total que se puede expresar como suma de dos t茅rminos
  medy <- mean(y)
  SCT <- sum((y-medy)**2)
  SCR <- sum((ypred-medy)**2)
  SCE <- sum((y-ypred)**2)
  
  #Calculamos el coeficiente de determinaci贸n
  R2 <- SCR/SCT 
  R2c <- 1 - ((SCE/(n-p-1))/(SCT/(n-1))) 
  
  print(beta)
  cat('\n')
  cat('R-cuadrado:', R2, '; R-cuadrado corregido:', R2c)
  
  return(ypred)
}

#Defino el directorio en el que se encuentra el set de datos
dir <- "C:/Users/aitor/Desktop/Master Modelizacion/Modelizaci贸n estad铆stica/Plab2/vmodnonas.RData"

#Cargo el set de datos
load(dir)


#Empezamos probando con todas las variables del estudio
variables <- c('alzada', 'long_cruz', 'ancho_muslos',
               'peri_torax', 'long_grupa', 'peso_nac',
               'edad', 'edad_des', 'sexo')
var_pred <- 'peso'

ypred <- LinearReg(v4, variables, var_pred)


#Ahora con las variables que m醩 imformaci髇 aportan a la regresi髇
variables2 <- c('long_grupa', 'long_cruz','peri_torax','sexo')

ypred <- LinearReg(v4, variables2, var_pred)
