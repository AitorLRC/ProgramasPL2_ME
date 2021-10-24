library(RcmdrMisc)

#################### 
#Lectura del fichero
####################  

#Defino el directorio en el que se encuentra el set de datos
dir <- "C:/Users/aitor/Desktop/Master Modelizacion/Modelización estadística/Plab2/v1.RData"

#Cargo el set de datos
load(dir)

#Lo primero que hago es duplicar el set de datos para modificar este nuevo 
v2 <- v1

#Imprimo las primeras líneas del fichero de datos para comprobar su estructura
head(v2)

summary(v2)

########################
#Depuración del fichero
#######################

# 80<= alzada <= 120
a <- which(v2$alzada<80|v2$alzada>120)  #Determino los valores que no cumplen la condición
length(a)
v2$alzada[a]
v2$alzada[a] <- NA    #A los que no han cumplido la condición les doy el valor NA


# long_cruz <= 90
b <- which(v2$long_cruz>90)
length(b)
v2$long_cruz[b]
v2$long_cruz[b] <- NA

# 30 <= ancho_muslos <= 55
c <- which(v2$ancho_muslos<30|v2$ancho_muslos>55)
length(c)
c
v2$ancho_muslos[c] #Imprimiendo este vi que el último valor es 445, esto puede ser 44 o 45, lo cambiamos a 44. También había un 134 po ahí pero no se que hacer con él.
v2$ancho_muslos[c][length(v2$ancho_muslos[c])] <- 44
v2$ancho_muslos[9582] <- 36
c2 <- which(v2$ancho_muslos<30|v2$ancho_muslos>55)
length(c2)
v2$ancho_muslos[c2]  <- NA

# 90 <= peri_torax <= 170
d <- which(v2$peri_torax<90|v2$peri_torax>170)
length(d)
v2$peri_torax[d]
v2$peri_torax[d] <- NA

# 100 <= peso <= 380
e <- which(v2$peso<100|v2$peso>380)
length(e)
v2$peso[e] #Aquí vi que la posición 183 es 4259. Este no se si cambiarlo a 425 o 259, como cambiarlo por mi cuenta puede alterar el resultado no lo tendré en cuenta
dev.new()
Boxplot(~peso, data=v2, id=list(method="y"))
v2$peso[e] <- NA

# 25 <= long_grupa <= 45
f <- which(v2$long_grupa<25|v2$long_grupa>45)
length(f)
v2$long_grupa[f]
v2$long_grupa[f] <- NA

# 25 <= peso_nac <= 60
g <- which(v2$peso_nac<25|v2$peso_nac>60)
length(g)
v2$peso_nac[g] #Aquí hay varios errores visibles también, de entre ellos me interesa un 423, un 409 y un -56. Hay un 262 pero no entraría en el estudio.
dev.new()
Boxplot(~peso_nac, data=v2, id=list(method="y"))
#Primer outlier con un peso de 423
v2$peso_nac[3733] 
v2$peso_nac[3733] <- NA
#Segundo outlier con un peso de 409
v2$peso_nac[17126]
v2$peso_nac[17126] <- 40
#Tercer outlier con un peso de 363
v2$peso_nac[15540]
v2$peso_nac[15540] <- NA
#El negativo lo pasamos a positivo
v2$peso_nac[which(v2$peso_nac==-56)] <- 56
g2 <- which(v2$peso_nac<25|v2$peso_nac>60)
v2$peso_nac[g2] <- NA

# 150<= edad <= 515
v2$edad <- abs(v2$edad)
h <- which(v2$edad<150|v2$edad>515)
length(h)
v2$edad[h] #Aquí hay bastantes errores, intentaré arreglar algunos que son más visibles y los otros los descarto.
dev.new()
Boxplot(~edad, data=v2, id=list(method="y"))
#6111 tiene pinta de ser 611, no lo cambio porque no entra en el estudio
v2$edad[223] <- 276 #2766 tiene pinta de ser 276
v2$edad[1143] <- 154 #1554 tiene pinta de ser 154
v2$edad[1452] <- 442 #4442 tiene pinta de ser 442        
v2$edad[14320] <- 418 #4188 tiene pinta de ser 418
v2$edad[18862] <- 397
v2$edad[14866] <- 284 #2884 tiene pinta de ser 284
v2$edad[18373] <- 435 #4335 tiene pinta de ser 435
v2$edad[20776] <- 239 #2399 tiene pinta de ser 139
h2 <- which(v2$edad<150|v2$edad>515)
v2$edad[h2] <- NA

# 90<= edad_des<=330
v2$edad_des <- abs(v2$edad_des)
i <- which(v2$edad_des<90|v2$edad_des>330)
length(i)
dev.new()
Boxplot(~edad_des, data=v2, id=list(method="y"))
v2$edad_des[i] #Veo que hay bastantes números negativos, tomo el valor absoluto
v2$edad_des[2295] <- 295 #2295 tiene pinta de ser 295
v2$edad_des[6011] <- 201 #2010 tiene pinta de ser 201
v2$edad_des[10285] <- 193 #1931 tiene pinta de ser 193
i2 <- which(v2$edad_des<90|v2$edad_des>330)
v2$edad_des[i2] <- NA


#############################################
#Describir las características de los animales
#############################################

#Para empezar a describir las características de los animales quitamos de la tabla el id de los animales
v3 <- v2[,c("alzada", "ancho_muslos", "edad", "edad_des", "long_cruz", "long_grupa", "peri_torax", "peso", "peso_nac", "sexo")]

numSummary(v3)
numSummary(v3, groups = v3$sexo)

#Podemos hacer una matriz de correlación
correlationM <- cor(v3[, c('alzada', 'ancho_muslos', 'edad', 'edad_des', 'long_cruz', 'long_grupa', 'peri_torax', 'peso', 'peso_nac')], use = 'complete')
correlationM['peso',]

#Empezamos por la altura
numSummary(v3$alzada)
numSummary(v3$alzada, groups = v3$sexo)
Boxplot(alzada ~ sexo, data=v3)
#En el caso de la alzada observamos que la media y la mediana son similares, luego la distribución es simétrica.

#Continuamos con el ancho de los muslos
numSummary(v3$ancho_muslos)
numSummary(v3$ancho_muslos, groups = v3$sexo)

#La edad
numSummary(v3$edad)
numSummary(v3$edad, groups = v3$sexo)

#Edad de destete
numSummary(v3$edad_des) #Aquí se ve que la media y la mediana no coinciden. La distribución es asimétrica sesgada hacia la derecha
numSummary(v3$edad_des, groups = v3$sexo)

#Longitud de la parte central
numSummary(v3$long_cruz)
numSummary(v3$long_cruz, groups = v3$sexo)

#Longitud de la parte posterior
numSummary(v3$long_grupa)
numSummary(v3$long_grupa, groups = v3$sexo)

#Perímetro del tórax
numSummary(v3$peri_torax)
numSummary(v3$peri_torax, groups = v3$sexo)

#Peso 
numSummary(v3$peso)
numSummary(v3$peso, groups = v3$sexo)

# Peso de nacimiento
numSummary(v3$peso_nac)
numSummary(v3$peso_nac, groups = v3$sexo)


###############
#Modelo lineal
###############
#Defino el modelo de regresión lineal del peso en función del resto de variables

regModel <- lm(peso ~ alzada+ancho_muslos+edad+edad_des+long_cruz+long_grupa+peri_torax+peso_nac+sexo, data=v3)
summary(regModel)

dev.new()
oldpar <- par(oma=c(0,0,3,0), mfrow=c(1,2))
plot(regModel)
par(oldpar)
#Al hacer la gráfica veo que hay un punto muy alejado del resto de puntos, lo quito
regModel$residuals["17232"]
v3[17232,]

v4<-na.omit(v3)

regModel.1 <- lm(peso ~ alzada+ancho_muslos+edad+edad_des+long_cruz+long_grupa+peri_torax+peso_nac+sexo, data=v4)
summary(regModel.1)

oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(regModel.1)
par(oldpar)


#Pasamos a quitar variables que no añaden información al modelo
stepwise(regModel.1, direction='backward/forward', criterion='BIC')

regModel.2 <- lm(peso ~ long_grupa+long_cruz+peri_torax+sexo, data = v4)
length(regModel.2$fitted.values)
summary(regModel.2)

oldpar <- par(oma=c(0,0,3,0), mfrow=c(1,2))
plot(regModel.2)
par(oldpar)


save(v4, file = 'C:/Users/aitor/Desktop/Master Modelizacion/Modelización estadística/Plab2/vmodnonas.RData')

scatterplot(v4$peso~regModel.2$fitted.values)
