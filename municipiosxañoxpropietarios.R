#Mauricio necesita un .csv que tenga el numero de predios por municipio por a�o
#lo har� con esta funci�n increible que conoc� hace poco llamada

library(data.table)

setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")
base1 <- read.csv("BASE1.csv", sep =",")
setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")
base3 <- read.csv("BASE3.csv", sep=",")

confirmacion <- setkey(setDT(base1), CodMun)[,
                                  list(Municipios= municipio), by= list(ANO, propietarios, CodMun)]

write.csv(confirmacion, file = "propietariosxmunicipioxa�o.csv")

el <- read.csv("propietariosxmunicipioxa�o.csv", sep= ",")
