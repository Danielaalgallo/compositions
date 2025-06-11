#######################Tares 7 ######################################################
############################mapas#################################################
library(tmaptools)

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")

base <- read.csv("BASE_FILTRADA2.csv", header=TRUE)
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/shp")
base$sector <- unclass(base$sector)
base$sector <- as.numeric(base$sector)

mapaColombia <- tmaptools::read_shape("mpio.shp", as.sf = FALSE)

str(mapaColombia)
### Agregar el cero a Antioquia y ATLATICO

for(i in c(1:length(base[,1]))){
  if(base$departamento[i]== "Antioquia"){
    base$CodMun[i] <- paste("0", base$CodMun[i],sep="")
  }
  else if (base$departamento[i]== "ATLANTICO"){
    base$CodMun[i] <- paste("0", base$CodMun[i],sep="")
  }
}


base2004 <- base[base$ANO == "2015" ,]
base$sector<- as.numeric(base$sector)

#### HACER EL MAPA PARA EL 2004
mapaColombia1<-fortify(mapaColombia)
mapaColombia1 <- append_data(mapaColombia, base2004, key.shp = "MPIOS", key.data = "CodMun", ignore.duplicates = TRUE )

oc <- over_coverage()

library(ggplot2)
ggplot(mapaColombia1)+
  geom_sf(aes(fill= (sector)))


##Otra forma de leerlo que no cambio nada
library(spdep)
library(rgdal)
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT")
library(raster)
s <- shapefile("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/shp/mpio.shp")
