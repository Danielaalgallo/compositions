setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/CODE")
#install.packages("xlsx")
#install.packages('readxl')
#install.packages("tidyverse")
library(readxl)
library(xlsx)
library(pracma)
library(assertthat)
library(tidyverse)
library(DescTools)
library(psych)
#############################################################################
########Objetivo de este R script:################################################
######## consolidar la base de datos que se utilizará ######################
#########para los diagramas ternarios y demas labores######################
##########################################################################
###############DATORS 85 #####################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/85")
datos85 <-read.csv(file ="datos85.csv", header=TRUE, sep=",")

datos85$clase[datos85$RANGO == "Inferior a 1Ha."] <- 1
datos85$clase[datos85$RANGO == "1htas < 3htas"] <- 3
datos85$clase[datos85$RANGO == "3htas < 5htas"] <- 5
datos85$clase[datos85$RANGO == "5htas < 10htas"] <- 10
datos85$clase[datos85$RANGO == "10htas < 15htas"] <- 15
datos85$clase[datos85$RANGO == "15htas < 20htas"] <- 20
datos85$clase[datos85$RANGO == "20htas < 50htas"] <- 50
datos85$clase[datos85$RANGO == "50htas < 100htas"] <- 100
datos85$clase[datos85$RANGO == "100htas < 200htas"] <- 200
datos85$clase[datos85$RANGO == "200htas < 500htas"] <- 500
datos85$clase[datos85$RANGO == "500htas < 1000htas"] <- 1000
datos85$clase[datos85$RANGO == "1000htas < 20000htas"] <- 20000
datos85$clase[datos85$RANGO == ">2000hts"] <- 20001




setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/anexos_nacionales_catastro_igac_2000-2012")


##Este excel fue modificado para dejar solo el header y los datos######
datos2000a2009 <- read_excel("RANGOS PROPIETARIOS MUNICIPIOS  IGAC 2000-09.xlsx")
datos2000a2009 <-as.data.frame(datos2000a2009)

########################################################################
######### Cargar datos de Antioquia #################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/Antioquia")
datosAntioquia <- read_excel("Rangos propietarios DEPARTAMENTO MUNCIPIOS - ANTIOQUIA 2004-2009.xlsx")
datosAntioquia <- as.data.frame(datosAntioquia)
Departamento <- rep("Antioquia",length(datosAntioquia[,1]))
datosAntioquia <- cbind(datosAntioquia, Departamento)
#ginis
ginisAntioquia <- read.csv("Ginis MunicipiosANTIOQUIA.csv", header=TRUE, sep=";")
ginisAntioquia <- as.data.frame(ginisAntioquia)


########################################################################
#### mezclar datos de rangos 2000 a 2009 con lo respectivos de Antioquia####

datos2000a2009 <- merge(x = datos2000a2009, y = datosAntioquia, by.x = c("AÑO", "DEPARTAMENTO",   
                "MUNICIPIO", "RANGOS TAMAÑO PROPIEDAD", "CÓDIGO MUNICIPIO", "HECTÁREAS",
                "NÚMERO DE PROPIETARIOS"), by.y= c("AÑO","Departamento", "MUNICIPIO","RANGO TAMAÑOS PROPIEDAD",
                                           "CÓDIGO DANE MUNICPIO", "ÁREA PROPIETARIOS-NO REPETICIÓN","PROPIETARIOS"), all = TRUE)

#####PRIMERO: catalogar cada ropietario dentro de su rango de hectareas###
#################################################################################
##########################################################################
##########################################################################

recode(datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`, " 
       'Inferior a 1Ha.' = 1;   ")

for (i in c(1:length(datos2000a2009[,1]))){
  
  if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i] == "Inferior a 1Ha."){
    
    datos2000a2009$clase[i] = 1 
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="5htas < 10htas"){
    datos2000a2009$clase[i] =10
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="3htas < 5htas"){
    datos2000a2009$clase[i] =5
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="100htas < 200htas"){
    datos2000a2009$clase[i] =200
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="1htas < 3htas"){
    datos2000a2009$clase[i] =3
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="15htas < 20htas"){
    datos2000a2009$clase[i] = 20
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="200htas < 500htas"){
    datos2000a2009$clase[i] = 500
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="10htas < 15htas"){
    datos2000a2009$clase[i] = 15
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="20htas < 50htas"){
    datos2000a2009$clase[i] = 50
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="50htas < 100htas"){
    datos2000a2009$clase[i] = 100
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="500htas < 1000htas"){
    datos2000a2009$clase[i] = 1000
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]=="1000htas < 20000htas"){
    datos2000a2009$clase[i] = 20000
  }
  else if (datos2000a2009$`RANGOS TAMAÑO PROPIEDAD`[i]==">2000hts"){
    datos2000a2009$clase[i] = 20001
  }
}

##########################mezclar con 85 ##############################3
datos85a2009 <- merge(x=datos2000a2009, y=datos85, by.x = c("AÑO","MUNICIPIO","CÓDIGO MUNICIPIO","RANGOS TAMAÑO PROPIEDAD","HECTÁREAS", "NÚMERO DE PROPIETARIOS", "clase"),
                      by.y = c("año","MUNICIPIO","CODMUN","RANGO","SUP","PROP", "clase"), all=TRUE)

datos85a2009$X <- NULL
datos85a2009$PREDIOS <- NULL

municipios <- unique(datos85a2009$`CÓDIGO MUNICIPIO`)
#############################################################################
#Objetivo de este R script:################################################
######## compilar en la base de datos general los datos del 2015##########
##para eso necesito: sumar el total de propietarios del municipio y el total
### de propietarios del municipio, luego calcular el gini, agregarlo a la base de datos
# comn el resto de los años y finalmente calcular las restas##############
#########################################################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/anexos_nacionales_catastro_igac_2000-2012")

datos2015 <- read_excel("RANGOS PROPIETARIOS MUNICIPIOS UPRA 2015.xlsx")
año <- repmat(2015,12497,1)
datos2015 <- cbind(datos2015, año)
rangos <- unique(datos2015$CLASIF_TAMANO) 
###########################clases para poder calcular el gini##########
########################################################################
datos2015<- as.data.frame(datos2015)

for (i in c(1:length(datos2015[,1]))){
  
  if (datos2015$CLASIF_TAMANO[i] == "9. Entre 50 y 100 ha"){
    
    datos2015$clase[i] = 100
  }
  else if (datos2015$CLASIF_TAMANO[i]=="8. Entre 20 y 50 ha"){
    datos2015$clase[i] =50
  }
  else if (datos2015$CLASIF_TAMANO[i]=="7. Entre 10 y 20 ha"){
    datos2015$clase[i] =20
  }
  else if (datos2015$CLASIF_TAMANO[i]=="6. Entre 5 y 10 ha"){
    datos2015$clase[i] =10
  }
  else if (datos2015$CLASIF_TAMANO[i]=="5. Entre 3 y 5 ha"){
    datos2015$clase[i] =5
  }
  else if (datos2015$CLASIF_TAMANO[i]=="4. Entre 2.5 y 3 ha"){
    datos2015$clase[i] = 3
  }
  else if (datos2015$CLASIF_TAMANO[i]=="3. Entre 1 y 2.5 ha"){
    datos2015$clase[i] = 2.5
  }
  else if (datos2015$CLASIF_TAMANO[i]=="2. Entre 0.5 y 1 ha"){
    datos2015$clase[i] = 1
  }
  else if (datos2015$CLASIF_TAMANO[i]=="11. Entre 200 y 500 ha"){
    datos2015$clase[i] = 500
  }
  else if (datos2015$CLASIF_TAMANO[i]=="10. Entre 100 y 200 ha"){
    datos2015$clase[i] = 200
  }
  else if (datos2015$CLASIF_TAMANO[i]=="1. Menor 0.5 ha"){
    datos2015$clase[i] = 0.5
  }
  else if (datos2015$CLASIF_TAMANO[i]=="12. Entre 500 y 1,000 ha"){
    datos2015$clase[i] = 1000
  }
  else if (datos2015$CLASIF_TAMANO[i]=="13. Entre 1,000 y 2,000 ha"){
    datos2015$clase[i] = 2000
  }
  else if (datos2015$CLASIF_TAMANO[i]=="14. Entre 2,000 y 5,000 ha"){
    datos2015$clase[i] = 5000
  }
  else if (datos2015$CLASIF_TAMANO[i]=="15. Entre 5,000 y 10,000 ha"){
    datos2015$clase[i] = 10000
  }
  else if (datos2015$CLASIF_TAMANO[i]=="16. Mayor 10,000 ha"){
    datos2015$clase[i] = 10001
  }
}

datos2015$PREDIOS <- NULL

###########################################################################
###### QUITAR ANTIOQUIA DE DATOS 2015 ############################
#######Antioquia no tiene datos para nuestra ventana de tiempo######################################

##### MEZCLAR LAS BASES DE DATOS ###############################################################

datosintegrados <- merge(x= datos85a2009, y= datos2015, by.x = c("AÑO", "DEPARTAMENTO","MUNICIPIO", "RANGOS TAMAÑO PROPIEDAD", "CÓDIGO MUNICIPIO","HECTÁREAS", "NÚMERO DE PROPIETARIOS", "clase" ),
                         by.y = c("año", "departamentonombre", "municipionombre","CLASIF_TAMANO", "COD_MUN", "ATERR","PROPIETARIOS",  "clase"), all.x=TRUE, all.y=TRUE, 
                         suffixes = c("AÑO","DEPARTAMENTO","MUNICIPIO"," CLASIFIC_TAMANO", "COD_MUN", "ÁREHECTÁREAS","PROPIETARIOS", "clase"))
datosintegrados$XCdigodelmunicipio <-NULL
###########################################################################################
#################CARGAR BASES DE DATOS DE UAF ##############################
################# CARGAR BASES DE DATOS DE GINIS ################################


setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/UAF")
UAF <- read_excel("UAF_MAX_MIN 041.xlsx")


setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/anexos_nacionales_catastro_igac_2000-2012")
GINIS <- read_excel("GINIS_MUN.xlsx") 

### cargar ginis antioquia y juntarlo con la base de datos de gini general
GINI <- merge(x= GINIS, y= ginisAntioquia, by.x=c("codigo dane", "AÑO", "PROPIETARIOS SIN REPETICION"), 
              by.y = c("CÓDIGO.DANE", "AÑO", "GINI"), all.x = TRUE, all.y = TRUE)
GINI$DEPARTAMENTO <- NULL
GINI$MUNICIPIO.x <- NULL
GINI$`PROPIETARIOS CON REPETICIÓN` <- NULL
GINI$MUNICIPIO.y <- NULL

full <- merge(UAF,datosintegrados, by.x="CodDane", by.y="CÓDIGO MUNICIPIO", all.x=TRUE, all.y = TRUE)

full$MUNICIPIO.x <- NULL


municipios <- unique(full$CodDane)
 
#########VERIFICACIÓN DE DATOS QUE SE ESTÁN PERDIENDO##############


full1 <- merge(full, GINI, by.x = c("CodDane", "AÑO"), by.y=c("codigo dane", "AÑO"), all.x= TRUE, all.y= TRUE)

municipios <- unique(full1$CodDane)


#######################################################################
##########################QUITAR CAPITALES#############################


#install.packages("sjmisc")
library(dostats)
library(sjmisc)

for (h in c(1:length(full1[,1]))){
  if (str_contains(full1$CodDane[h],"001")){
    full1$CodDane[h] <- "WUF"
    
  }
}

full1 <- full1[full1$CodDane != "WUF", ]
municipios <- unique(full1$CodDane)
full1 <- full1[full1$AÑO != "2010", ]
full1 <- full1[full1$AÑO != "2011", ]
full1 <- full1[full1$AÑO != "2012", ]
full1 <- full1[full1$AÑO != "2013", ]
full1 <- full1[full1$AÑO != "2014", ]



###########GUARDAR BASE DE DATOS#######################################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")
write.csv(full1, file = "base_datos_85_2015.csv")
