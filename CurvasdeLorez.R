setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")#######################################################################
######################LEER CSV base_datos_2000_2015#########################
#######################creada en el paso 1. de INPUT################################################

baseTotal <- read.csv(file="base_datos_85_2015.csv", header=TRUE)
baseTotal <- subset(baseTotal, !is.na(CodDane))

############################# CURVAS DE LORENZ ######################################
######################################################################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")
library(compositions)

library ("ggtern")

base <- read.csv("BASE1.csv", header=TRUE)
base$X <- NULL

base <- subset(base, !is.na(ANO))
base <- subset(base, !is.na(CodMun))

años <- unique(base$ANO)

gini6 <- vector()



for (n in c(1:length(base$CodMun))){
  gini <- round(base$merge_ginis[n],2)
  
  if (gini >= 0.6 & gini <= 0.69){
    tod <- as.data.frame(base[n ,])
    gini6 = rbind(gini6, tod)
    
  } 
}
gini6 <- as.data.frame(gini6)
gini6 <- subset(gini6, !is.na(municipio))
municipios <- unique(gini6$CodMun)

baseGini6 <- matrix(ncol=13)

for (v in c(1:length(baseTotal$CodDane))){
  for (g in c(1:length(gini6$CodMun))){
    if (baseTotal$CodDane[v] == gini6$CodMun[g]){
      if (baseTotal$AÑO[v] == gini6$ANO[g]){
       hola <- vector()
       hola <- baseTotal[v , ]
       colnames(baseGini6) <- colnames(baseTotal)
       baseGini6 <- rbind(baseGini6,hola)
      }
    }
   }
}

baseGini6 <- subset(baseGini6, !is.na(AÑO))

municipiosGINI6 <- unique(baseGini6$CodDane)
municipios <- unique(baseGini6$CodDane)
municipios <- na.omit(municipios)
tiempos <- unique(gini6$ANO)


## Cakculo de ginis de forma empirica cpn curvas de lorenz

for (t in c(1:length(municipios))){
  
  municipio <- baseGini6[baseGini6$CodDane == municipios[t],] 
  municipio <- subset(municipio, !is.na(AÑO))
  tiempos <- unique(municipio$AÑO)
  
  for (j in c(1:length(tiempos))){
    municipio <- baseGini6[baseGini6$CodDane == municipios[t],]
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
        
        
      
      if (length(municipio$CodDane)> 1){
        for (y in c(1:(length(municipio[,1])-1))){
          municipio$columna_prop[y+1]= as.numeric(municipio$acumulado_propietarios[y+1]- municipio$acumulado_propietarios[y])
          municipio$columna_prop[1]=municipio$acumulado_propietarios[1]
        }
      }
      clase <- municipio$clase[k]
      codigo <- municipio$CodDane[k]
      ano <- municipio$AÑO[k]
      municipio$base_por_altura <- municipio$columna_sup* municipio$columna_prop
      gini = 1 - sum(municipio$base_por_altura)
      
      baseGini6$acumulado_sup[(baseGini6$clase == clase & baseGini6$CodDane == codigo & baseGini6$AÑO == ano)] <- municipio$acumulado_suerficie[k] 
      baseGini6$acumulado_prop[(baseGini6$clase == clase & baseGini6$CodDane == codigo & baseGini6$AÑO == ano)] <- municipio$acumulado_propietarios[k] 
      }
      
    }
     
  } 
  print(paste((t/length(municipios))*100,'%'))
}

######################################################################################
############### VERIFICAR QUE LAS CURVAS SE CRUCEN EN ALGÚN PUNTO#####################
######################################################################################
municipios_coincidentes <- matrix(ncol= 4)
colnames(municipios_coincidentes) <- c("codigo1","año1","codigo2", "año2")
l =1
g=1
baseGini6 <- subset(baseGini6, !is.na(acumulado_sup))

for (l in c(1:length(baseGini6$CodDane))){
  for(g in c(1:length(baseGini6$CodDane))){
    if (baseGini6$CodDane[l]!= baseGini6$CodDane[g]){
      if (l != g){
        if (baseGini6$acumulado_sup[l]!= 1 & baseGini6$acumulado_sup[g] !=1 & baseGini6$acumulado_prop[l]!=1 & baseGini6$acumulado_prop[g]!=1){
          if (baseGini6$acumulado_sup[l]> 0.3 & baseGini6$acumulado_sup[l]<0.8){
            if (round(baseGini6$acumulado_sup[l],4) == round(baseGini6$acumulado_sup[g],4) && round(baseGini6$acumulado_prop[l],4) == round(baseGini6$acumulado_prop[g],3)){
              nu <- c(baseGini6$CodDane[l], baseGini6$AÑO[l], baseGini6$CodDane[g],baseGini6$AÑO[g])
              municipios_coincidentes = rbind(municipios_coincidentes, nu)
            
            }
          }
        }  
      }
    }
  }
  print(paste((l/length(baseGini6$CodDane))*100,'%'))
}

municipios_coincidentes<- as.data.frame(municipios_coincidentes)

municipios_coin1 <- municipios_coincidentes$codigo1
anos_coin1 <- municipios_coincidentes$año1
municipios_coin2 <- municipios_coincidentes$codigo2
anos_coin2 <- municipios_coincidentes$año2
anos_coin1 <- na.omit(anos_coin1)
anos_coin2 <- na.omit(anos_coin2)
municipios_coin1 <- na.omit(municipios_coin1)
municipios_coin2 <- na.omit(municipios_coin2)

municipio2 <- subset(municipio, !is.na(AÑO))

i <- data.frame(x= c(0,
                     0.076923077, 0.153846154, 0.230769231,
                     0.307692308,0.384615385,0.461538462,
                     0.538461538,0.615384615,0.692307692,0.769230769,
                     0.846153846,0.923076923,1), 
                y= c(0,
                     0.076923077, 0.153846154, 0.230769231,
                     0.307692308,0.384615385,0.461538462,
                     0.538461538,0.615384615,0.692307692,0.769230769,
                     0.846153846,0.923076923,1))
                     

baseconGinis <-merge(x=baseGini6, y= base, by.x = c("CodDane", "AÑO"), by.y = c("CodMun", "ANO"), all.x= TRUE)

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/nuevas salidas/solicitudes/curvas de lorenz")
for (u in c(1:length(municipios_coin1))){
  municipio1 <- baseconGinis[baseconGinis$CodDane == municipios_coin1[u],] 
  municipio2 <- baseconGinis[baseconGinis$CodDane == municipios_coin2[u],]
  tiempo1 <- municipio1[municipio1$AÑO == anos_coin1[u] ,]
  tiempo2 <- municipio2[municipio2$AÑO == anos_coin2[u] ,]
  tiempo1 <- rbind(rep(0, length(tiempo1[,1])),tiempo1)
  tiempo2 <- rbind(rep(0, length(tiempo2[,1])),tiempo2)

  tiempo1 <- tiempo1[order(tiempo1$clase),]
  tiempo2 <- tiempo2[order(tiempo2$clase),]

  
  
  png(filename = paste(tiempo1$MUNICIPIO.y[2], "-", tiempo1$AÑO[2], "vs ", tiempo2$MUNICIPIO.y[2], "-", tiempo2$AÑO[2],".png", sep=""))
  
    plot(tiempo1$acumulado_prop, tiempo1$acumulado_sup, col = "blue",type = "l", xlim=c(0,1), ylim=c(0,1))+
    lines(tiempo2$acumulado_prop, tiempo2$acumulado_sup, col ="orange", type = "l", xlim=c(0,1), ylim=c(0,1))+
    lines(i$x,i$y, col= "gray", type = "l", xlim=c(0,1), ylim=c(0,1))+
    title(paste(tiempo1$MUNICIPIO.y[2], "-", tiempo1$AÑO[2], "vs ", tiempo2$MUNICIPIO.y[2], "-", tiempo2$AÑO[2], sep=" "))
    sub <- (paste(round(tiempo1$merge_ginis[2],2)," - ", round(tiempo2$merge_ginis[2],2),sep="    "))
    mtext(side=3, line=0.5, at=0.5, adj="center", cex=1, sub)
    
    
    dev.off()
  
}
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")

write.csv(baseGini6, file="Base_curvas_Lorenz.csv")
