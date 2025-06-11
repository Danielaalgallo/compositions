setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/85")


library(readxl)
library(xlsx)
library(tidyr)
datos85 <- read_excel("Privadarural1985rangos.xls", sheet = 1)
datos85 <- as.data.frame(datos85)
str(datos85)


datos85$PREDIOS1 <-  as.numeric(datos85$PREDIOS1)
datos85$SUP1 <-  as.numeric(datos85$SUP1)
datos85$SUP1 <- as.numeric(datos85$SUP1)
datos85$PREDIOS2 <- as.numeric(datos85$PREDIOS2)
datos85$PROP1 <- as.numeric(datos85$PROP1)

datos_85 <- as.data.frame(datos_85)
municipios <- unique(datos85$CODMUN)
datos85 <- na.omit(datos85)
matr <- matrix(ncol=5)
colnames(matr) <- c("codigo" ,"municipio","predios85","Propietarios85","superficie85")
j=1
for (j in c(1:length(municipios))){
  municipio <- datos85[1,]
  
  predios <- municipio[ c(3,6,9,12,15,18,21,24,27,30,33,36,39)]
  sum_predios <- as.numeric(rowSums(predios))
  Propietarios <- municipio[ c(4,7,10,13,16,19,22,25,28,31,34,37,40)]
  sum_Propietarios <- as.numeric(rowSums(Propietarios))
  hectareas <- municipio[ c(5,8,11,14,17,20,23,26,29,32,35,38,41)]
  sum_hectareas <- as.numeric(rowSums(hectareas))
  
  matrixSD = c(municipio$CODMUN,municipio$MUNICIPIO, sum_predios,sum_Propietarios,sum_hectareas)
  
  matr <- rbind(matr, matrixSD)

}

matr <- as.data.frame(matr)
matr <- na.omit(matr)
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/85")

################################################################################################
##########################CONTRASTAR CON DATOS 2015 ############################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")

base_filtrada <- read.csv(file= "BASE_FILTRADA2.csv",header= TRUE, sep=",")
base_filtrada <- subset(base_filtrada, base_filtrada$ANO == "2015")

nuevo <- merge(x=base_filtrada, y= matr, by.x= "CodMun", by.y= "codigo", all=TRUE)
nuevo <- subset(nuevo, !is.na(predios85))
nuevo <- subset(nuevo, !is.na(ANO))

municipios <- unique(nuevo$CodMun)


nuevo$superficie85 <- as.numeric(as.character(nuevo$superficie85))
nuevo$total_hectareas_por_clases <- as.numeric(as.character(nuevo$total_hectareas_por_clases))

elegidos <- nuevo[((nuevo$total_hectareas_por_clases/nuevo$superficie85) > 0.8 &
                     (nuevo$total_hectareas_por_clases/nuevo$superficie85) < 1.2)  ,]
elegidos$Areacatastral2015sobre1985 <- elegidos$total_hectareas_por_clases / elegidos$superficie85

hijos <-c("13433"
          ,"13670"
          ,"13760"
          ,"19100"
          ,"19450"
          ,"23068"
          ,"23417"
          ,"47170"
          ,"47258"
          ,"50689"
          ,"52418"
          ,"70678"
          ,"70820"
          ,"86865")

for (p in c(1:length(elegidos[,1]))){
  for (l in c(1:length(hijos))){
    if (elegidos$CodMun[p] == hijos[l]){
      elegidos$municipio.y[p] <- NA
    }
    
  }
  
}

elegidos <- subset(elegidos, !is.na(municipio.y))



library(readxl)
library(xlsx)
library(tidyr)
datos85llorente <- read_excel("1985_Terminado_CEGA_LLORENTE_2.xlsx", sheet = 5)


datos85merge <- merge(x=matr, y=datos85llorente, by.x= ("codigo"), by.y= ("codigo"), all=TRUE)
datos85merge <- as.data.frame(datos85merge)

datos85merge$superficie <- as.numeric(as.character(datos85merge$superficie))
datos85merge$`SUPERF. CATAST. (Has )` <- as.numeric(as.character(datos85merge$`SUPERF. CATAST. (Has )`))


datos85merge$diferenciaHectareas <- (datos85merge$`SUPERF. CATAST. (Has )` *100)/(datos85merge$superficie)

library(psych)
describe(datos85merge$diferenciaHectareas)

#3###


#
for(i in c(1:length(base[,1]))){
  if(base$departamento[i] %in% c("Antioquia", "ANTIOQUIA")){
    base$CodMun[i] <- paste("0", base$CodMun[i],sep="")
  }
  else if (base$departamento[i]== "ATLANTICO"){
    base$CodMun[i] <- paste("0", base$CodMun[i],sep="")
  }
}


###Volver la estructura de datos igual a la que usualmente manejamos
datos_85 <- gather(datos85,condicion, medida , c("PROP1", "PROP2", "PROP3", "PROP4", "PROP5", "PROP6", "PROP7", "PROP8", "PROP9", "PROP10", "PROP11", "PROP12", "PROP13") 
                   , factor_key = TRUE)
b1 <- datos_85[, c(1,2,29,30)]
names(b1)[names(b1) == "condicion"] <- "Rango_prop"
names(b1)[names(b1) == "medida"] <- "Num_Propietarios"

datos_85 <- gather(datos85,condicion, medida , c("SUP1", "SUP2", "SUP3", "SUP4", "SUP5", "SUP6", "SUP7", "SUP8", "SUP9", "SUP10", "SUP11", "SUP12", "SUP13") 
                   , factor_key = TRUE)
b2 <- datos_85[, c(1,2,29,30)]
names(b2)[names(b2) == "condicion"] <- "Rango_Sup"
names(b2)[names(b2) == "medida"] <- "Area_catastral"

base <- merge(x=b1,y= b2, by.x =c("CODMUN", "MUNICIPIO"), by.y= c("CODMUN", "MUNICIPIO"), all= FALSE)
base1 <- na.omit(base)
municipios <- unique(base1$CODMUN)

###########85 + el resto de los datos #######################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/85")


library(readxl)
library(xlsx)
library(tidyr)
library(pracma)
library(assertthat)
library(tidyverse)
library(DescTools)
library(psych)
datos_85 <- read.csv("Privadarural1985rangos2.csv", header = TRUE, sep =",")

año <- repmat(1985,length(datos_85[,1]),1)
datos_85 <- cbind(datos_85, año)

datos_85$RANGO[datos_85$RANGO == "1"] <- "Inferior a 1Ha."
datos_85$RANGO[datos_85$RANGO == "2"] <- "1htas < 3htas"
datos_85$RANGO[datos_85$RANGO == "3"] <- "3htas < 5htas"
datos_85$RANGO[datos_85$RANGO == "4"] <- "5htas < 10htas"
datos_85$RANGO[datos_85$RANGO == "5"] <- "10htas < 15htas"
datos_85$RANGO[datos_85$RANGO == "6"] <- "15htas < 20htas"
datos_85$RANGO[datos_85$RANGO == "7"] <- "20htas < 50htas"
datos_85$RANGO[datos_85$RANGO == "8"] <- "50htas < 100htas"
datos_85$RANGO[datos_85$RANGO == "9"] <- "100htas < 200htas"
datos_85$RANGO[datos_85$RANGO == "10"] <- "200htas < 500htas"
datos_85$RANGO[datos_85$RANGO == "11"] <- "500htas < 1000htas"
datos_85$RANGO[datos_85$RANGO == "12"] <- "1000htas < 20000htas"
datos_85$RANGO[datos_85$RANGO == "13"] <- ">2000hts"

write.csv(datos_85, file = "datos85.csv")
