setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")
#######################################################################
######################LEER CSV base_datos_2000_2015#########################
#######################creada en el paso 1. de INPUT################################################

base <- read.csv(file="base_datos_85_2015", header=TRUE)
base$X<-NULL
base <- as.data.frame(base)
base <- subset(base, !is.na(AÑO))


write.csv(base, file= "base2000a2015.csv")
#############################################################################
################  CALCULAR GINIS EMPIRICAMENTE ##############################

municipios <- unique(base$CodDane)
municipios <- na.omit(municipios)
tiempos <- unique(base$AÑO)
composiciones <- matrix(ncol=3)


for (t in c(1:length(municipios))){
  
  municipio <- base[base$CodDane == municipios[t],] 
  municipio <- subset(municipio, !is.na(AÑO))
  tiempos <- unique(municipio$AÑO)
  
  for (j in c(1:length(tiempos))){
    municipio <- base[base$CodDane == municipios[t],]
    tiempos <- unique(municipio$AÑO)
    municipio <- municipio[municipio$AÑO == tiempos[j],]
    if ( length(municipio$CodDane)> 1){
      municipio <- municipio[order(municipio$clase),]
      if (any(municipio$HECTÁREAS == 0.00, na.rm = FALSE)){
        municipio <- subset(municipio, municipio$HECTÁREAS != 0.00)
        municipio <- subset(municipio, municipio$NÚMERO.DE.PROPIETARIOS != 0.00)
      }
      for (k in c(1:length(municipio[,1]))) {
        
        total_hectareas= sum(municipio$HECTÁREAS)
        total_propietarios = sum(municipio$NÚMERO.DE.PROPIETARIOS)
        
        #gini
        municipio$total_hec = total_hectareas #total de la superficio en hectareas del municipio
        municipio$total_pro = total_propietarios #total de propietarios en el municipio
        municipio$av_hec = municipio$total_hec/municipio$total_pro #total de superficie sobre total de propietarios
        
        municipio$av_grupo = municipio$HECTÁREAS / municipio$NÚMERO.DE.PROPIETARIOS #av_grupo o Promedio por grupo 
        municipio$fre_rel= municipio$HECTÁREAS / municipio$total_hec #fre_rel o Frecuencia Relativa
        
        municipio$Fre_acu = cumsum(municipio$HECTÁREAS)/municipio$total_hec #Fre_acu o Frecuencia Acumulada
        municipio$yav_totav = municipio$av_grupo / municipio$av_hec # yav_totav o y barra grupo/ybarra
        
        municipio$one_yav_totav = 1-municipio$yav_totav # one_yav_totav o 1-ybarragrupo/ybarra
        municipio$nk = rev(cumsum(rev(municipio$NÚMERO.DE.PROPIETARIOS))) # nk (forma enredada de hacer esta suma)
        municipio$nk_one = municipio$nk+1 # nk + 1
        municipio$nk_nk1_div2 = (municipio$nk * municipio$nk_one) / 2 #(nk)(nk+1)/2
        municipio$wk<-0 #columna wk rellena de ceros para calcularla paso a paso
        municipio$a <- 0 #columna a rellena de ceros para calcularla paso a paso
        l <- length(municipio[,1])
        for (p in c(l:1)){
          
          municipio$wk[p] =  municipio$nk_nk1_div2[p] + municipio$a[p]
          municipio$a[p-1] = sum(municipio$wk)*-1
        }
        
        municipio$c = 2/(municipio$total_pro * (municipio$total_pro +1)) 
        
        municipio$c_1_var_gru = municipio$c * (municipio$one_yav_totav)
        
        municipio$wc_1_yvar = municipio$c_1_var_gru * municipio$wk
        gini= sum(municipio$wc_1_yvar) 
      }
    }
    matrixSD = c(municipio$AÑO[k],municipio$CodDane[k],gini)
    
    composiciones <- rbind(composiciones, matrixSD)
    colnames(composiciones) <- c("año" ,"Codigo mpio","Gini_felipe")
  } 
  print(paste((t/length(municipios))*100,'%'))
}


############################################################################
#######MEZCLAR ESTA BASE DE DATOS CON GINI_FELIPE CON ####################
###########LA BASE GENERAL##############################################
composiciones <- as.data.frame(composiciones)

full2 <- merge(base, composiciones, by.x = c("AÑO", "CodDane"), by.y=c("año", "Codigo mpio"), all.x= TRUE, all.y= TRUE)
municipios2 <- unique(full2$CodDane)



full2 <- subset(full2, !is.na(AÑO))
#########################################################################
###Agregar departamentos a toda la base de datos######################

for (t in c(1:length(municipios2))){
  municipio <- full2[full2$CodDane == municipios[t],] 
  for (n in c(1:length(municipio[,1]))){
    if (full2$CodDane == municipio$CodDane){
      
    
    if (is.na(municipio$DEPARTAMENTO.y[n])){
      municipio$DEPARTAMENTO.y[n] <- municipio$DEPARTAMENTO.y[n+14]
    }
    }
  }
    
}
##############################################################################
#################CLASIFICAR DE ACUERDO CON LA UAF ###########################
#############################################################################

ful2mun <- unique(full2$CodDane)
full2 <- as.data.frame(full2)

full2$prom_UAF <- ((full2$UAF_MAX_041 + full2$UAF_MIN_041)/2)

full2 <- subset(full2, !is.na(prom_UAF))
full2 <- subset(full2, !is.na(Gini_felipe))

#for (n in c(1:length(full2[,1]))){
  


full2$clasecomposiciones <- ifelse(full2$clase <= full2$prom_UAF, 1,
                        ifelse(full2$prom_UAF < full2$clase & full2$clase <= 3*full2$prom_UAF,
                           2, ifelse(full2$clase > 3*full2$prom_UAF, 3, NA)) )

#write.csv(full2, file ="full2.csv")


##########################################################################
####Crear base de datos de valores aleatorios ###########################
#setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")
#num <- c(8842, 5576, 496, 6528, 9936, 3005, 3809, 9104, 248, 8894, 9413, 1253, 5058, 
#         182, 2868, 8771, 5212, 5617, 5531, 9546, 6159, 8838, 9480, 2554, 6510, 9742,
#         3110, 3538, 2287, 2571, 6567, 3097, 5794)
#py=0
  
#aleatorios <- matrix(ncol=14)
#colnames(aleatorios) <- c("AÑO", "CodDane", "UAF_MIN_041", "UAF_MAX_041", "DEPARTAMENTO.y",
#                          "MUNICIPIO.y","RANGOS.TAMAÑO.PROPIEDAD", "HECTÁREAS", "NÚMERO.DE.PROPIETARIOS",
#                          "clase", "PROPIETARIOS.SIN.REPETICION", "Gini_felipe", "prom_UAF", "clasecomposiciones")

#for (py in c(1:length(num))){
#  aleatorios <- rbind(aleatorios, full2[(num[py]),])
#}

#aleatorios <- subset(aleatorios, !is.na(AÑO))

#write.csv(aleatorios, file = "aleatorios.csv")
###############################################################################
################### SUMAR EL NÚMERO DE HECTÁREAS POR CLASE ####################
################################################################################
municipiosfull2 <- unique(full2$CodDane)

tiemposfull2 <- unique(full2$AÑO)
MFi <- matrix(ncol=10)
colnames(MFi) <- c("departamento","municipio","GINI_SIN_REPETICION","CodMun","ANO","C","F","T", "gini_felipe", "propietarios")

contadorsito=0
v = 0

for (q in c(1:length(municipiosfull2))){
  
  municipio <- full2[full2$CodDane == municipiosfull2[q],] 
  tiempos <- unique(municipio$AÑO)
    for (v in c(1:length(tiempos))){
      municipio <- full2[full2$CodDane == municipiosfull2[q],] 
      
      año <- municipio[municipio$AÑO == tiempos[v],]
      SF =0
      MF =0
      LL =0
      GINI = 0
      contadorsito = contadorsito + 1
      
      for (d in c(1:length(año[,1]))) {
        propietarios <- sum(año$NÚMERO.DE.PROPIETARIOS)
        if (año$clasecomposiciones[d] == "1") {
          SF = SF + año$HECTÁREAS[[d]]
          GINI_FELIPE <- año$Gini_felipe[d]
          giniOficial <- as.numeric(as.character(año$PROPIETARIOS.SIN.REPETICION[d]))
          
        }
        else if ((año$clasecomposiciones[d] == "2")) {
          MF = MF + año$HECTÁREAS[[d]]
          GINI_FELIPE <- año$Gini_felipe[d]
          giniOficial <- as.numeric(as.character(año$PROPIETARIOS.SIN.REPETICION[d]))
          
        }
        else if ( (año$clasecomposiciones[d] == "3")) {
          LL = LL + año$HECTÁREAS[[d]]
          GINI_FELIPE <- año$Gini_felipe[d]
          giniOficial <- as.numeric(as.character(año$PROPIETARIOS.SIN.REPETICION[d]))
          
        }
        
      }
      matrixSD = data.frame(departamento = as.character(municipio$DEPARTAMENTO.y[d+13]),
                            municipio = as.character(municipio$MUNICIPIO.y[d]),
                            GINI_SIN_REPETICION = giniOficial,
                            CodMun = municipio$CodDane[d],
                            ANO = tiempos[v],
                            C = as.numeric(SF), 
                            F = as.numeric(MF), 
                            T =  as.numeric(LL),
                            gini_felipe = GINI_FELIPE, propietarios)
      #### construir la base de datos
      MFi = rbind(MFi, matrixSD)
      
    }  
    
} 
 
###### Este codigo a continuación resume todo el código anterior
#### construcción de la matriz #######################
library(maditr)
p = dcast(full2, CodDane + AÑO ~ clasecomposiciones, fun.aggregate = sum,
          value.var = 'HECTÁREAS')
  

library(compositions)
################### Sacar Porcentajes por clase ##############################
######trate de hacerlo con el paquete de compositions pero tomaba############# 
##### los datos como factores y les atribuia cualquier valor númerico ########


MFi <- as.data.frame(MFi)
base <- MFi
base <- subset(base, !is.na(ANO))

for (x in c(1:length(base$departamento))){
  base$C_porcentaje[x] <- (as.numeric(as.character(base$C[x]))/ ((as.numeric(as.character(base$C[x]))+as.numeric(as.character(base$F[x]))+as.numeric(as.character(base$T[x])))))
  base$F_porcentaje[x] <- (as.numeric(as.character(base$F[x]))/ ((as.numeric(as.character(base$C[x]))+as.numeric(as.character(base$F[x]))+as.numeric(as.character(base$T[x])))))
  base$T_porcentaje[x] <- (as.numeric(as.character(base$T[x]))/ ((as.numeric(as.character(base$C[x]))+as.numeric(as.character(base$F[x]))+as.numeric(as.character(base$T[x])))))
}

#################################################################################
##### Columna única de ginis, para los valores donde no hay ginis oficiales se les
#### atribuye el gini que calculamos con el sistema de más arriba #################
municipios_base <- unique(base$CodMun)

for (k in c(1:length(base$CodMun))){
  if (is.na(base$GINI_SIN_REPETICION[k])== TRUE | base$GINI_SIN_REPETICION[k] == 0.0  ){
    base$merge_ginis[k] <- as.numeric(as.character(base$gini_felipe[k]))
  }
  else{
    base$merge_ginis[k] <- as.numeric(as.character(base$GINI_SIN_REPETICION[k]))
  }
}

############### Columna de año y promedio para los gráficos ###################

t <- 0
for (t in c(1:length(base$departamento))){
  base$anioypromedio[t] <- paste(base$ANO[t], "-", round(base$merge_ginis[t],4), sep="")
}

##################### ¿Cuántos municipios quedan después de este proceso #########

municipios <- unique(base$CodMun)

### categorizar por rangos ###############

for (t in c(1:length(base$departamento))){
  if (base$merge_ginis[t] < 0.1 & base$merge_ginis[t] != 0.000){
    base$rango[t] <- 1
    base$rango_Gini[t]<- "0 - 0.1"
  }
  else if (base$merge_ginis[t] > 0.1 & base$merge_ginis[t] < 0.2){
    base$rango[t] <- 2
    base$rango_Gini[t]<- "0.1 - 0.2"
  }
  else if (base$merge_ginis[t] > 0.2 & base$merge_ginis[t]< 0.3){
    base$rango[t] <- 3
    base$rango_Gini[t]<- "0.2 - 0.3"
  }
  else if (base$merge_ginis[t] > 0.3 & base$merge_ginis[t]< 0.4){
    base$rango[t] <- 4
    base$rango_Gini[t]<- "0.3 - 0.4"
  }
  else if (base$merge_ginis[t] > 0.4 & base$merge_ginis[t]< 0.5){
    base$rango[t] <- 5
    base$rango_Gini[t]<- "0.4 - 0.5"
  }
  else if (base$merge_ginis[t] > 0.5 & base$merge_ginis[t]< 0.6){
    base$rango[t] <- 6
    base$rango_Gini[t]<- "0.5 - 0.6"
  }
  else if (base$merge_ginis[t] > 0.6 & base$merge_ginis[t]< 0.7){
    base$rango[t] <- 7
    base$rango_Gini[t]<- "0.6 - 0.7"
  }
  else if (base$merge_ginis[t] > 0.7 & base$merge_ginis[t]< 0.8){
    base$rango[t] <- 8
    base$rango_Gini[t]<- "0.7 - 0.8"
  }
  else if (base$merge_ginis[t] > 0.8 & base$merge_ginis[t]< 0.9){
    base$rango[t] <- 9
    base$rango_Gini[t]<- "0.8 - 0.9"
  }
  else if (base$merge_ginis[t] > 0.9 & base$merge_ginis[t]< 1){
    base$rango[t] <- 10
    base$rango_Gini[t]<- "0.9 - 1"
  }
}


rangos <- unique(base$rango)

##categorizar por sector

for (j in c(1:length(base$CodMun))){
  
  if( base$F_porcentaje[j] > base$C_porcentaje[j] & base$F_porcentaje[j] > base$T_porcentaje[j]){
    base$sector[j] <- "F"
  }
  
  else if( base$C_porcentaje[j] > base$F_porcentaje[j] & base$C_porcentaje[j] > base$T_porcentaje[j]){
    base$sector[j] <- "C"
  }
  
  else if( base$T_porcentaje[j] > base$C_porcentaje[j] & base$T_porcentaje[j] > base$F_porcentaje[j]){
    base$sector[j] <- "T"
  }
  else{
    base$sector[j] <- "0"
  }
}

#################### Sectores más especializados ###################################


for (l in c(1:length(base$CodMun))){
  if (base$sector[l] == "F"){
    if( base$F_porcentaje[l] > 0.33 & base$F_porcentaje[l] <= 0.5 ){
      base$sector_especifico[l] <- "F1"
    }
    if( base$F_porcentaje[l] > 0.5 & base$F_porcentaje[l] <= 0.75 ){
      base$sector_especifico[l] <- "F2"
    }
    if( base$F_porcentaje[l] > 0.75 & base$F_porcentaje[l] <= 1 ){
      base$sector_especifico[l] <- "F3"
    }
  }
  else if (base$sector[l]== "C"){
    if( base$C_porcentaje[l] > 0.33 & base$C_porcentaje[l] <= 0.5 ){
      base$sector_especifico[l] <- "C1"
    }
    if( base$C_porcentaje[l] > 0.5 & base$C_porcentaje[l] <= 0.75 ){
      base$sector_especifico[l] <- "C2"
    }
    if( base$C_porcentaje[l] > 0.75 & base$C_porcentaje[l] <= 1 ){
      base$sector_especifico[l] <- "C3"
    }
  }
  else if(base$sector[l] == "T"){
    if( base$T_porcentaje[l] > 0.33 & base$T_porcentaje[l] <= 0.5){
      base$sector_especifico[l] <- "T1"
    }
    if( base$T_porcentaje[l] > 0.5 & base$T_porcentaje[l] <= 0.75){
      base$sector_especifico[l] <- "T2"
    }
    if( base$T_porcentaje[l] > 0.75 & base$T_porcentaje[l] <= 1){
      base$sector_especifico[l] <- "T3"
    }
  }  
#  else{
#    base$sector_especifico[l] <- "0"
#  }
}




############################ #####################################################
############ Incluir la bae de areas cartograficas ############################"
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/UAF")
library(readxl)
library(xlsx)
area_cartografica <- read_excel("COLOMBIA_MUNICIPIOS_AREA_CARTOGRAFICA.xlsx")


area_cartografica$NOMBRE_ENTIDAD <- NULL
area_cartografica$CATEGORIA <- NULL
area_cartografica$DEPARTAMENTO <- NULL

base1 <- merge(x= base, y = area_cartografica, by.x =("CodMun"), by.y = ("COD_DANE"), all.x= TRUE, all.y= FALSE)
municipios3 <- unique(base1$CodMun)
base <- base1

for (o in c(1:length(base$municipio))){
  suma <- as.numeric(as.character(base$C[o]))+ as.numeric(as.character(base$F[o])) + as.numeric(as.character(base$T[o]))
  base$dif_hectareas_porcentaje[o] <- round((suma * 100 )/ (as.numeric(as.character(base$AREA_MPIO_HA[o]))),1)
  base$dif_numerica_hectareas[o] <- (as.numeric(as.character(base$AREA_MPIO_HA[o]))) - suma
  base$total_hectareas_por_clases[o] <- suma
}
library(dplyr)

names(base)[names(base) == "gini_felipe"] <- "gini_calculado"

base$AREA_MPIO_HA.y<- NULL
base$AREA_MPIO_HA.x <- NULL
base$AREA_MPIO_HA.y <-NULL


quantile(base$dif_numerica_hectareas, na.rm=TRUE)
quantile(base$dif_hectareas_porcentaje, na.rm = TRUE)

municipios <- unique(base$CodMun)

################################################################################

municipios <- unique(base$CodMun)
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")  
write.csv(base, file = "BASE_PARA_GRAFICAS_v2.csv")

masdecientoonce <- base[base$dif_hectareas_porcentaje > (111),]

munic <- unique(masdecientoonce$CodMun)
########### tabla de porcentajes por año por sector especifico############################33                          
prop <- prop.table(table(base$sector_especifico, base$AÑO))*1000

########Filtrar base de datos bajo parametros arbitrarios #######################
######### primera condición ####################
base1 <- base[base$merge_ginis >= (0.27602),]
municipios1<- unique(base1$CodMun)
######## segunda condición ####################
base1$catastralsobrecartografica <- base1$total_hectareas_por_clases / base1$AREA_MPIO_HA

base2 <- base1[(base1$catastralsobrecartografica)>= (0.025),]
municipios2<- unique(base2$CodMun)

base2 <- subset(base1, (catastralsobrecartografica >= (0.025)))

############################### tercera condición ###################
base3 <- subset(base2, dif_hectareas_porcentaje <111) 
mun3 <- unique(base3$CodMun)
base <- base3
########################3 base filtrada parametros Mauricio ######################


municipios <- unique(base$CodMun)
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")  
base <- unique(base)
write.csv(base, file = "BASE_FILTRADA85a2015.csv")

base <- read.csv(file="BASE_FILTRADA.csv", sep=",", header=TRUE)
base <- unique(base)
