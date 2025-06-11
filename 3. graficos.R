setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")
library(compositions)

library ("ggtern")

base <- read.csv("BASE_PARA_GRAFICAS.csv", header=TRUE)
base$X <- NULL

base <- subset(base, !is.na(AÑO))

años <- unique(base$AÑO)

lines <- data.frame(x = c(0.5, 0, 0.5), 
                    y = c(0.5, 0.5, 0), 
                    z = c(0, 0.5, 0.5), 
                    xend = c(1, 1, 1)/3, 
                    yend = c(1, 1, 1)/3, 
                    zend = c(1, 1, 1)/3)


##Arreglar base de datos y unificar gini###

###########TAREA 1 ########################

l <- par(mfrow=c(2,4))
new(l)
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 1")
library(ggtern)

for (h in c(1:length(años))){
  
  año <- base[base$AÑO == años[h],]
  año <- as.data.frame(año)
  gg <- ggtern(data=año ,aes(SF.1,MF.1,LL.1, colour= rango_Gini)) + 
  geom_point(size=1.5)+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
  weight_percent() +
  theme_bw() +
  xlab("C") + ylab("F") + zlab("T")+
 
  labs(title = paste(año$AÑO[h]), fill =" Rango ginis")+
  stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1)
  png(filename = paste(año$AÑO[h],".png", sep=""))
  plot(gg) 
  dev.off()

}

##############################TAREA 2 ####################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 2")
library(dostats)
library(sjmisc)
rangos <- unique(base$rango)

for (h in c(1:length(rangos))){
  rango <- base[base$rango == rangos[h],]
  C1 =0
  F1=0
  T1=0
  
  for (h in c(1:length(rango[,1]))){
    if (rango$sector[h] == "C"){
      C1= C1+1
    }
    else if (rango$sector[h] == "F"){
      F1=F1+1
    }
    else if(rango$sector[h] == "T"){
      T1=T1+1
    }
  }
    for (y in c(1:length(años))){
      rango <- base[base$rango == rangos[h],]
      año <- rango[rango$AÑO == años[y],]
      gg <- ggtern(data=año, aes(SF.1,MF.1,LL.1, colour= año$municipio)) + 
        geom_point(show.legend = FALSE) + 
        geom_Tisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
        geom_Lisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
        geom_Risoprop(mapping = NULL ,value=0.5,colour='gray18') +
        weight_percent() +
        labs(title = paste(año$rango))+
        stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
        ggtitle(label =paste("AÑO  ",año$AÑO, sep="") ,
                subtitle= paste("Rango", ((as.numeric(año$rango)-1)/10), "-", (as.numeric(año$rango)/10), sep = ""))
      png(filename = paste(año$AÑO[y]," ",año$rango[y],".png", sep=""))
      plot(gg) 
      dev.off()
    }
}


###########################3 TAREA 4 #########################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 4.1")
departamentos <- unique(base$departamento)
tiempos <- unique(base$AÑO)
library("ggtern")

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 4.1/sin leyenda")

for (n in c(1:length(departamentos))){
  
  departamento <-base[base$departamento == departamentos[n],]
  departamento <- as.data.frame(departamento)
  for(m in c(1:length(tiempos))){
    departamento <-base[base$departamento == departamentos[n],]
    tiempo <- departamento[departamento$AÑO == tiempos[m],]
    for (j  in c(1:length(tiempo[,1]))){
      gg <- ggtern(data=tiempo, aes(SF.1,MF.1,LL.1, colour = tiempo$municipio)) + 
        geom_point(show.legend = FALSE, size=2) + 
        geom_Tisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
        geom_Lisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
        geom_Risoprop(mapping = NULL ,value=0.5,colour='gray18') +
        weight_percent() +
        labs(title = paste(tiempo$departamento[j]))+
        stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
        ggtitle(label =paste(tiempo$departamento[j]," ",tiempo$AÑO, sep="") ,
                subtitle= paste("Gini prom =", mean(tiempo$gini_felipe),",", 
                                "Range(", min(tiempo$gini_felipe),"-",max(tiempo$gini_felipe),")",sep = " "))
      
      png(filename = paste(tiempo$departamento[j]," ",tiempo$AÑO[j],"1.png", sep=""))
      plot(gg) 
      dev.off()
    }
  }
}

##############################TAREA 2 #############################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 6")

municipios <- unique(base$municipio)

for (g in c(1:length(municipios))){
  municipio <- base[base$codigo.municipio == municipios[g],]
  gg <- ggtern(data=municipio, aes(SF.1,MF.1,LL.1, colour = municipio$AÑO)) + 
    geom_point(show.legend = TRUE) + 
    geom_Tisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
    geom_Lisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
    geom_Risoprop(mapping = NULL ,value=0.5,colour='gray18') +
    weight_percent() +
    labs(title = paste(municipio$municipio[g]))+
    stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
    ggtitle(label =paste(municipio$municipio[g], sep="") ,
            subtitle= paste("Gini prom =", mean(tiempo$gini_felipe),",", 
                            "Range(", min(tiempo$gini_felipe),"-",max(tiempo$gini_felipe),")",sep = " "))
  
  png(filename = paste(municipio$municipio[g],"1.png", sep=""))
  plot(gg) 
  dev.off()
  
}


##############################TAREA 2 #############################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 6")

base <- base[base$gini_felipe != (0.0),]
municipios <- unique(base$codigo.municipio)
AÑOS <- unique(base$AÑO)



for (g in c(1:length(municipios))){
  municipio <- base[base$codigo.municipio == municipios[g],]
  municipio <- as.data.frame(municipio)
  gg <- ggtern(data=municipio, aes(SF.1,MF.1,LL.1, colour =as.factor(ordered(anioypromedio)))) + 
    geom_point(show.legend = TRUE,size=2) + 
    geom_Tisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
    geom_Lisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
    geom_Risoprop(mapping = NULL ,value=0.5,colour='gray18') +
    weight_percent() +
    stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
    ggtitle(label =paste(municipio$municipio[1], sep="") ,
            subtitle= paste("Gini prom =", mean(municipio$gini_felipe),",", 
                            "Range(", min(municipio$gini_felipe),"-",max(municipio$gini_felipe),")",sep = " "))
  
  png(filename = paste(municipio$municipio[1],".png", sep=""))
  plot(gg) 
  dev.off()
  
}

##########################################################################
##### TAREA 5 ####################################################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 5")

base$resta_gini<- NULL

municipios <- unique(base$codigo.municipio)
tiempos <- unique(base$AÑO)
fi <- matrix(ncol=2)

for (t in c(1:length(municipios))){
  
  municipio <- base[base$codigo.municipio == municipios[t],] 

  restagini <- max(municipio$merge_ginis)- min(municipio$merge_ginis)
  matrixSD = c(restagini,municipio$codigo.municipio[1])
    
  fi <- rbind(fi, matrixSD)
  colnames(fi) <- c("resta_ginis" ,"Codigo mpio")
} 
fi <- as.data.frame(fi)

#######agregar resta de ginis a la base general
base1 <- merge(x=base, y=fi, by.x = "codigo.municipio", by.y= "Codigo mpio", all=TRUE)
base1$resta_gini <- NULL
############descripción de la nueva variable resta_ginis
describe(base1$resta_ginis)
quantile(base1$resta_ginis, probs =seq(0.05,1), dec = ".", na.rm = TRUE) #Al correr esta linea usted puede 


################## diagramas ternarios################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 5/menor variación gini")
cinco <- as.numeric(as.character(quantile(base1$resta_ginis, probs =seq(0.05,1), dec = ".", na.rm = TRUE)))
for (y in c(1:length(municipios))){
  municipio <- base1[base1$codigo.municipio == municipios[y],]
  municipio <- as.data.frame(municipio)
  municipio <- municipio[! is.na(municipio$AÑO) ,]
  
  
  if(municipio$resta_gini[1] < cinco){
    pp <- ggtern(data= municipio, aes(SF.1,MF.1,LL.1, colour =as.factor(anioypromedio))) + 
      geom_point(show.legend = TRUE,size=2) + 
      geom_Tisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
      geom_Lisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
      geom_Risoprop(mapping = NULL ,value=0.5,colour='gray18') +
      weight_percent() +
      stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
      ggtitle(label =paste(municipio$municipio[1], sep=""),
              subtitle= paste("diferencia ginis=", (municipio$resta_ginis),",", 
                              "Range(", min(municipio$merge_ginis),"-",max(municipio$merge_ginis),")",sep = " "))
    png(filename = paste(municipio$municipio[1],".png", sep=""))
    plot(pp) 
    dev.off()
  }
  
}



setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 5/mayor variación gini")
nuevecinco <- as.numeric(as.character(quantile(base1$resta_ginis, probs =seq(0.95,1), dec = ".", na.rm = TRUE)))
for (y in c(1:length(municipios))){
  municipio <- base1[base1$codigo.municipio == municipios[y],]
  municipio <- as.data.frame(municipio)
  municipio <- municipio[! is.na(municipio$AÑO) ,]
  
  
  if(municipio$resta_gini[1] > nuevecinco){
    pp <- ggtern(data= municipio, aes(SF.1,MF.1,LL.1, colour =as.factor(anioypromedio))) + 
      geom_point(show.legend = TRUE,size=2) + 
      geom_Tisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
      geom_Lisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
      geom_Risoprop(mapping = NULL ,value=0.5,colour='gray18') +
      weight_percent() +
      stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
      ggtitle(label =paste(municipio$municipio[1], sep=""),
              subtitle= paste("diferencia ginis=", (municipio$resta_ginis),",", 
                              "Range(", min(municipio$merge_ginis),"-",max(municipio$merge_ginis),")",sep = " "))
    png(filename = paste(municipio$municipio[1],".png", sep=""))
    plot(pp) 
    dev.off()
  }
  
}
########################################################################
#########################TAREA 5#######################################
################### DIAGRAMAS POR SECTORES###############################


#Clasificación por sectores


####### COMPROBACIÓN DE QUE LA CLASIFICACIÓN POR SECTORES FUNCIONO

boyaca <- base[base$`codigo municipio` == ("8078"),]

pp <- ggtern(data= boyaca, aes(SF.1,MF.1,LL.1, colour =((boyaca$AÑO)))) + 
  geom_point(show.legend = TRUE,size=2) + 
  geom_segment(data = lines, 
               aes(x, y, z, 
                   xend = xend, yend = yend, zend = zend), 
               color = 'black', size = 0.5) +
  weight_percent() +
  theme_bw() +
  theme(legend.key.size = unit( 0.1, "inches"), legend.key.width = unit(0.1, "inches"))+
  stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
  ggtitle(label =paste(boyaca$departamento[1], sep="")) 
plot(pp) 





municipios <- unique(base$codigo.municipio)
library(compositions)
library(ggtern)

boyaca <- base[base$`codigo municipio` == ("8078"),]

  pp <- ggtern(data= boyaca, aes(SF.1,MF.1,LL.1, colour =((boyaca$AÑO)))) + 
    geom_point(show.legend = TRUE,size=2) + 
    geom_Tisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
    geom_Lisoprop(mapping = NULL ,value=0.5,colour='gray18') + 
    geom_Risoprop(mapping = NULL ,value=0.5,colour='gray18') +
    weight_percent() +
    theme(legend.key.size = unit( 0.1, "inches"), legend.key.width = unit(0.1, "inches"))+
    stat_mean_ellipse(size= 0.5, color = "red", steps =22, r=0.1, alpha =1) +
    ggtitle(label =paste(boyaca$departamento[1], sep="")) 
  plot(pp) 
 
  
####################################################################
  setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")  
 write.csv(base, file = "base_datos_sectores.csv")
