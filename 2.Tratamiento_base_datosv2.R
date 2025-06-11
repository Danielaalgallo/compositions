setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")######################LEER CSV base_datos_2000_2015#########################
#######################creada en el paso 1. de INPUT################################################

base <- read.csv(file="base_datos_85_2015.csv", header=TRUE)
base$X<-NULL
base <- as.data.frame(base)
base <- subset(base, !is.na(AÑO))


#############################################################################
################  CALCULAR GINIS EMPIRICAMENTE ##############################

municipios <- unique(base$CodDane)
municipios <- na.omit(municipios)
tiempos <- unique(base$AÑO)
composiciones1 <- matrix(ncol=3)

##########CALCULAR GINI MÉTODO 1 ###################################

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
    
    composiciones1 <- rbind(composiciones1, matrixSD)
    colnames(composiciones1) <- c("año" ,"Codigo mpio","Gini_felipe")
  } 
  print(paste((t/length(municipios))*100,'%'))
}

########################### CALCULAR GINI MÉTODO 2 ################
###################################método empirico##############################
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
        
        municipio$area_sobre_total[k] = municipio$HECTÁREAS[k]/total_hectareas
        municipio$prop_sobre_total[k] = municipio$NÚMERO.DE.PROPIETARIOS[k]/total_propietarios
        municipio$acumulado_suerficie[k] = sum(municipio$area_sobre_total[1:k])
        municipio$acumulado_propietarios[k]= sum(municipio$prop_sobre_total[1:k])
        municipio$columna_sup =0
        if (length(municipio$CodDane)> 1){
          for (f in c(1:(length(municipio[,1])-1))){
            municipio$columna_sup[f+1]= municipio$acumulado_suerficie[f]+municipio$acumulado_suerficie[f+1]
          }
        }
        municipio$columna_prop =0
        
        
      }
       if (length(municipio$CodDane)> 1){
        for (y in c(1:(length(municipio[,1])-1))){
          municipio$columna_prop[y+1]= as.numeric(municipio$acumulado_propietarios[y+1]- municipio$acumulado_propietarios[y])
          municipio$columna_prop[1]=municipio$acumulado_propietarios[1]
        }
       }
      
      municipio$base_por_altura <- municipio$columna_sup* municipio$columna_prop
      gini = 1 - sum(municipio$base_por_altura)
    }
    matrixSD = c(municipio$AÑO[k],municipio$CodDane[k],gini)
    
    composiciones <- rbind(composiciones, matrixSD)
    colnames(composiciones) <- c("año" ,"Codigo mpio","Gini_empirico")
  } 
  print(paste((t/length(municipios))*100,'%'))
}
composiciones <- as.data.frame(composiciones)
municipios <- unique(composiciones$`Codigo mpio`)

mezcla <- merge(x= composiciones, y=composiciones1, by.x=c("año", "Codigo mpio"), by.y=c("año", "Codigo mpio"))
municipiosf <- unique(mezcla$`Codigo mpio`)
describe(mezcla$Gini_empirico)

describe(mezcla$Gini_felipe)

############################################################################


#######MEZCLAR ESTA BASE DE DATOS CON GINI_EMPIRICO CON ####################
###########LA BASE GENERAL##############################################
composiciones <- as.data.frame(composiciones)

full2 <- merge(base, composiciones, by.x = c("AÑO", "CodDane"), by.y=c("año", "Codigo mpio"), all.x= TRUE, all.y= TRUE)
municipios2 <- unique(full2$CodDane)

full2 <- subset(full2, !is.na(AÑO))

#########################################################################
###Agregar departamentos a toda la base de datos######################

municipios2 <- unique(full2$CodDane)
for (t in c(1:length(municipios2))){
  municipio <- full2[full2$CodDane == municipios2[t],] 
  for (n in c(1:length(municipio[,1]))){
      if (is.na(municipio$DEPARTAMENTO.y[n])){
        full2$DEPARTAMENTO.y[full2$CodDane == municipios2[t]] <- municipio$DEPARTAMENTO.y[n+14]
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
full2 <- subset(full2, !is.na(Gini_empirico))

full2$clasecomposiciones <- ifelse(full2$clase <= full2$prom_UAF, 1,
                        ifelse(full2$prom_UAF < full2$clase & full2$clase <= 3*full2$prom_UAF,
                           2, ifelse(full2$clase > 3*full2$prom_UAF, 3, NA)) )




###############################################################################
################### SUMAR EL NÚMERO DE HECTÁREAS POR CLASE ####################
################################################################################
municipiosfull2 <- unique(full2$CodDane)

tiemposfull2 <- unique(full2$AÑO)
MFi <- matrix(ncol=10)
colnames(MFi) <- c("departamento","municipio","GINI_SIN_REPETICION","CodMun","ANO","C","F","T", "gini_empirico", "propietarios")

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
          gini_empirico <- año$Gini_empirico[d]
          giniOficial <- as.numeric(as.character(año$PROPIETARIOS.SIN.REPETICION[d]))
          
        }
        else if ((año$clasecomposiciones[d] == "2")) {
          MF = MF + año$HECTÁREAS[[d]]
          gini_empirico <- año$Gini_empirico[d]
          giniOficial <- as.numeric(as.character(año$PROPIETARIOS.SIN.REPETICION[d]))
          
        }
        else if ( (año$clasecomposiciones[d] == "3")) {
          LL = LL + año$HECTÁREAS[[d]]
          gini_empirico <- año$Gini_empirico[d]
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
                            gini_empirico = gini_empirico, propietarios)
      #### construir la base de datos
      MFi = rbind(MFi, matrixSD)
      
    }  
    
} 
 
###### Este codigo a continuación resume todo el código anterior
#### construcción de la matriz ALTERNATIVA 2 #######################
library(maditr)
p = dcast(full2, CodDane + AÑO ~ clasecomposiciones, fun.aggregate = sum,
          value.var = 'HECTÁREAS')
  

library(compositions)


################### PORCENTAJES POR CLASE ##############################
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
###################### MERGE GINIS #########################################
##### Columna única de ginis, para los valores donde no hay ginis oficiales se les
#### atribuye el gini que calculamos con el sistema de más arriba #################
municipios_base <- unique(base$CodMun)

for (k in c(1:length(base$CodMun))){
  if (is.na(base$GINI_SIN_REPETICION[k])== TRUE | base$GINI_SIN_REPETICION[k] == 0.0  ){
    base$merge_ginis[k] <- as.numeric(as.character(base$gini_empirico[k]))
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

############ RANGOS GENERALES #######################################3

for (g in c(1:length(base$CodMun))){
  if (base$merge_ginis[g] > 0.25 & base$merge_ginis[g] < 0.45){
    base$rango_Gini_general[g]<- "0.25 - 0.45"
  }
  else if (base$merge_ginis[g] > 0.55 & base$merge_ginis[g]< 0.65){
    base$rango_Gini_general[g]<- "0.55 - 0.65"
  }
  else if (base$merge_ginis[g] > 0.75 & base$merge_ginis[g]< 0.85){
    base$rango_Gini_general[g]<- "0.75 - 0.85"
  }
  else base$rango_Gini_general[g] <- NA
}

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

#################### CARACTERIZACIÓN SECTORES ESPECIALIZADOS #############################
############################################################################################################################


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



############################ #############################################################
############ Incluir la base de areas cartograficas ######################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/INPUT/UAF")
library(readxl)
library(xlsx)
area_cartografica <- read_excel("COLOMBIA_MUNICIPIOS_AREA_CARTOGRAFICA.xlsx")


area_cartografica$NOMBRE_ENTIDAD <- NULL
area_cartografica$CATEGORIA <- NULL
area_cartografica$DEPARTAMENTO <- NULL
base1 <- base
base1 <- merge(x= base, y = area_cartografica, by.x =("CodMun"), by.y = ("COD_DANE"), all.x= TRUE, all.y= FALSE)
municipios3 <- unique(base1$CodMun)


for (o in c(1:length(base1$municipio))){
  suma <- as.numeric(as.character(base1$C[o]))+ as.numeric(as.character(base1$F[o])) + as.numeric(as.character(base1$T[o]))
  base1$dif_hectareas_porcentaje[o] <- round((suma * 100 )/ (as.numeric(as.character(base1$AREA_MPIO_HA[o]))),1)
  base1$dif_cartografica_catastral[o] <- (as.numeric(as.character(base1$AREA_MPIO_HA[o]))) - suma
  base1$total_hectareas_por_clases[o] <- suma
}




quantile(base1$dif_cartografica_catastral, na.rm=TRUE)
quantile(base1$dif_hectareas_porcentaje, na.rm = TRUE)
base1$catastralsobrecartografica <- ((base1$total_hectareas_por_clase*100) / base1$AREA_MPIO_HA)


#El proceso anterior es para verificar irregularidades no deberían haber municipios sobre este va
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")


########Filtrar base de datos bajo parametros arbitrarios #######################
######### primera condición ####################
base1 <- base1[(base1$propietarios >= 20) ,]
municipios1<- unique(base1$CodMun)

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")

write.csv(base1, file = "BASE1.csv")
base1 <- read.csv("BASE1.csv", sep =",")
#############################################BASE 2 #####################################
#################### base 2 - no hijos de municipios madre del 85#####################################################7
municipios <- unique(base1$CodMun)
municipios1_1985 <- base1[(base1$ANO == 1985) ,]
municipios1_1985d <- unique(municipios1_1985$CodMun)

####madres######
madres <-c("05390",
           "05652",
           "05659",
           "13030",
           "13042",
           "13062",
           "13160",
           "13188",
           " 13222",
           "13268",
           "13300",
           "13458",
           "13490",
           "13580",
           "13620",
           "13655",
           "13810",
           "15236",
           "17495",
           "17665",
           "18785",
           "19290",
           "19300",
           "19533",
           "19780",
           "19785",
           "19845",
           "20570",
           "23300",
           "23350",
           "23682",
           "23815",
           "25260",
           "25312",
           "27050",
           "27135",
           "27150",
           "27160",
           "27250",
           "27425",
           "27430",
           " 27450",
           "27580",
           "27600",
           "27810",
           "44035",
           "44090",
           "44098",
           "44110",
           "44378",
           "44420",
           "47030",
           "47205",
           "47268",
           "47460",
           "47545",
           "47660",
           "47720",
           "47960",
           "47980",
           "50110",
           "50270",
           "50325",
           "50370",
           "50450",
           "52240",
           "52254",
           "52385",
           "52390",
           "52480",
           "52565",
           "52685",
           "52694",
           "54250",
           "54385",
           "54553",
           "68235",
           "68250",
           "70221",
           "70233",
           "73520",
           "81300",
           "86569",
           "86571",
           "86757",
           "91430",
           "91530",
           "91536",
           "94663",
           "99773"
)
###codigo####
for (p in c(1:length(base1[,1]))){
  for (l in c(1:length(madres))){
    if (base1$CodMun[p] == madres[l]){
      base1$municipio[p] <- NA
    }
    
  }
  
}


base2 <- subset(base1, !is.na(municipio))
municipios2 <- unique(base2$CodMun)

municipios2_1985 <- base2[(base2$ANO == 1985) ,]
municipios2_1985d <- unique(municipios2_1985$CodMun)

#######################################################3333

base3 <- base2[((base2$catastralsobrecartografica)>= (80)) & ( base2$catastralsobrecartografica<= 110) ,]
municipios3<- unique(base3$CodMun)

municipios3_1985 <- base3[(base3$ANO == 1985) ,]
municipios3_1985d <- unique(municipios3_1985$CodMun)


######################## base 1 filtrada parametros Mauricio ######################
#base 1: Todos los municipios en Colombia con todos sus datos disponibles menos aquellos que estén por debajo o por encima de ciertos valores átipicos, calcular de nuevo los ginis.

años = unique(base3$ANO)
municipios = unique(base3$CodMun)
mun_completos = vector()
base3$CodMun <- as.numeric(as.character(base3$CodMun))

for (n in c(1:length(municipios))){
  municipio <- base3[base3$CodMun == municipios[n],]
  
  codigo <- as.numeric(municipios[n])
  mun1985 <- any(municipio$ANO == 1985)
  mun2000 <- any(municipio$ANO == 2000)
  mun2015 <- any(municipio$ANO == 2015)
  
  if (mun1985 && mun2000 && mun2015){
    base3$borrar[base3$CodMun == codigo] <- 1
  } else{
    base3$borrar[base3$CodMun == codigo] <- NA
  } 
}

base4 <- subset(base3, !is.na(borrar))
municipios4 <- unique(base4$CodMun)


#################################Catastra 85 / Catastral 2015 >80% y < 100% ##########

for (y in c(1:length(municipios4))){
  municipio <- base4[base4$CodMun == municipios4[y],]
  codigo = as.numeric(municipios4[y])
  l = length(municipio$CodMun)
  base4$catastrales85sobre15[base4$CodMun == codigo] <- ((municipio$total_hectareas_por_clases[1])/ (municipio$total_hectareas_por_clases[l]))
  
}

base5 <- base4[((base4$catastrales85sobre15)>= (0.8)) & ( base4$catastrales85sobre15 <= 1.100) ,]
munrestantes <- unique(base5$CodMun)

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")
write.csv(base5, file="BASE2.csv")


############### BASE DE DATOS 3 ######################################
base1 <- read.csv("BASE1.csv", sep =",")
#############################################BASE 2 #####################################
#################### base 2 - no hijos de municipios madre del 85#####################################################7
municipios <- unique(base1$CodMun)
municipios1_1985 <- base1[(base1$ANO == 1985) ,]
municipios1_1985d <- unique(municipios1_1985$CodMun)

####madres######
madres <-c("05390",
           "05652",
           "05659",
           "13030",
           "13042",
           "13062",
           "13160",
           "13188",
           " 13222",
           "13268",
           "13300",
           "13458",
           "13490",
           "13580",
           "13620",
           "13655",
           "13810",
           "15236",
           "17495",
           "17665",
           "18785",
           "19290",
           "19300",
           "19533",
           "19780",
           "19785",
           "19845",
           "20570",
           "23300",
           "23350",
           "23682",
           "23815",
           "25260",
           "25312",
           "27050",
           "27135",
           "27150",
           "27160",
           "27250",
           "27425",
           "27430",
           " 27450",
           "27580",
           "27600",
           "27810",
           "44035",
           "44090",
           "44098",
           "44110",
           "44378",
           "44420",
           "47030",
           "47205",
           "47268",
           "47460",
           "47545",
           "47660",
           "47720",
           "47960",
           "47980",
           "50110",
           "50270",
           "50325",
           "50370",
           "50450",
           "52240",
           "52254",
           "52385",
           "52390",
           "52480",
           "52565",
           "52685",
           "52694",
           "54250",
           "54385",
           "54553",
           "68235",
           "68250",
           "70221",
           "70233",
           "73520",
           "81300",
           "86569",
           "86571",
           "86757",
           "91430",
           "91530",
           "91536",
           "94663",
           "99773"
)
###codigo####
for (p in c(1:length(base1[,1]))){
  for (l in c(1:length(madres))){
    if (base1$CodMun[p] == madres[l]){
      base1$municipio[p] <- NA
    }
    
  }
  
}


base2 <- subset(base1, !is.na(municipio))
municipios2 <- unique(base2$CodMun)

municipios2_1985 <- base2[(base2$ANO == 1985) ,]
municipios2_1985d <- unique(municipios2_1985$CodMun)

for (n in c(1:length(municipios2))){
  municipio <- base2[base2$CodMun == municipios2[n],]
  
  codigo <- as.numeric(municipios2[n])
  mun1985 <- any(municipio$ANO == 1985)
  mun2000 <- any(municipio$ANO == 2000)
  mun2015 <- any(municipio$ANO == 2015)
  
  if (mun1985 && mun2000 && mun2015){
    base2$borrar[base2$CodMun == codigo] <- 1
  } else{
    base2$borrar[base2$CodMun == codigo] <- NA
  } 
}

base3 <- subset(base2, !is.na(borrar))

municipios3 <- unique(base3$CodMun)
write.csv(base3, file ="BASE3.csv")
