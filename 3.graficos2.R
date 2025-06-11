setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")
library(compositions)

library ("ggtern")

base <- read.csv("BASE1.csv", header=TRUE)
base$X <- NULL

base <- subset(base, !is.na(ANO))

años <- unique(base$ANO)


lines <- data.frame(x = c(0.5, 0, 0.5), 
                    y = c(0.5, 0.5, 0), 
                    z = c(0, 0.5, 0.5), 
                    xend = c(1, 1, 1)/3, 
                    yend = c(1, 1, 1)/3, 
                    zend = c(1, 1, 1)/3)

#######################TAREA 1 #########################################
###### todos los municipios para el mismo año ##########################
###### coordenada geometrica y número de municipios por sector #########
########################################################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/nuevas salidas/tarea 1.1")
library(ggtern)
h=1
años <- (2015)
base <- subset(base, !is.na(rango_Gini_general))
base <- subset(base, base$rango_Gini_general == ("0.25 - 0.45"))

for (h in c(1:length(años))){
  
  año <- base[base$ANO == años[h],]
  año <- as.data.frame(año)
  for (y in c(1:length(años))){
    C1 =0
    F1=0
    T1=0
    for (p in c(1:length(año[,1]))){
      if (año$sector[p] == "C"){
        C1= C1+1
      }
      
      else if (año$sector[p] == "F"){
        F1=F1+1
      }
      else if(año$sector[p] == "T"){
        T1=T1+1
      }
      rangolm = c(rep(paste("C",C1,sep="-"),C1),rep(paste("F", F1, sep="-"),F1), rep(paste("T", T1, sep="-"),T1))
      l <- mean(acomp(año[, c(11,12,13)]))
      tosay = paste("Media composicional","  ","F", round((l[2]*100),1),"%","T", round((l[3]*100),1),"%", "C",round((l[1]*100),1),"%", sep=" ")
     
    }
  }
  gg <- ggtern(data=año ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour= rango_Gini_general)) + 
    geom_point(show.legend= TRUE, size=1.5, cex.axis =2.5)+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    weight_percent() +
    theme_bw(base_size = 15) +

    xlab(paste("C","-",C1, sep ="")) + ylab(paste("F","-",F1, sep="")) + zlab(paste("T","-",T1,sep=""))+
    labs(title = paste(año$ANO[h]), fill =" Rango ginis", caption = paste( "n =", length(año$CodMun),"
                                                    
                                 





     
                        
     ", "Promedio gini=",round(mean(año$merge_ginis),3), sep=" "))+
    #stat_mean_ellipse(size= 1.3, color = "red", steps =22, r=0.1, alpha =1)+
    theme(plot.caption = element_text(size = rel(1.1), family = "FreeSerif", lineheight = .1, hjust = 0.5), plot.title = element_text(size = rel(1.5), hjust= 0.5))
  
  
  
  
  png(filename = paste(año$ANO[h],".png", sep=""))
  plot(gg)
  dev.off()
  
  png(filename = paste(año$ANO[h],"boxplot.png", sep=""))
  r <-boxplot(año$merge_ginis, data= año, main= paste(año$ANO[1], sep=""),
              xlab= paste("media", " - ", round(mean(año$merge_ginis),3), "
    ", "sd", " - ", round(sd(año$merge_ginis),3),"
    ","mediana", " - ", round(median(año$merge_ginis),3), sep=" "))
  dev.off()
}


library(ggplot2)
p <-ggplot(data= base, aes(x=as.factor(ANO),y= merge_ginis), legend= FALSE) + geom_boxplot(aes(fill=ANO))
r <-boxplot(data = año$merge_ginis)+
    stat_boxplot(show.legend = TRUE)

#############################Tarea 1.1 #################################################################
##############Año a Año de todos los municipios por cada rango de gini saltando de 0.1#########
############número de municipios en cada sector (C,F,T)#####################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/1985")
library(dostats)
library(sjmisc)
rangos <- unique(base$rango)
h= 0
años <- (1985)
for (h in c(1:length(rangos))){
  rango <- base[base$rango == rangos[h],]
  años <- 1985
  for (y in c(1:length(años))){
    rango <- base[base$rango == rangos[h],]
    año <- rango[rango$ANO == años[y],]
    C1 =0
    F1=0
    T1=0
    for (p in c(1:length(año[,1]))){
      if (año$sector[p] == "C"){
        C1= C1+1
      }
      else if (año$sector[p] == "F"){
        F1=F1+1
      }
      else if(año$sector[p] == "T"){
        T1=T1+1
      }
      rangolm = c(rep(paste("C",C1, sep="-"),C1),rep(paste("F", F1, sep="-"),F1), rep(paste("T", T1, sep="-"),T1))
    }
    gg <- ggtern(data=año, aes(C_porcentaje,F_porcentaje,T_porcentaje)) + 
      geom_point(show.legend = TRUE, aes(fill=rangolm), color = "blue") + 
      geom_segment(data = lines, 
                   aes(x, y, z, 
                       xend = xend, yend = yend, zend = zend), 
                   color = 'black', size = 0.5) +
      theme_bw() +
      xlab("C") + ylab("F") + zlab("T") +
      weight_percent() +
      labs(title = paste(año$rango_Gini))+
      ggtitle(label =paste("AÑO  ",año$ANO, sep="") ,
              subtitle= paste("Rango", ((as.numeric(año$rango)-1)/10), "-", (as.numeric(año$rango)/10), sep = ""))
    png(filename = paste(año$ANO[y]," ",año$rango[y],".png", sep=""))
    plot(gg) 
    dev.off()
  }
}


########################### TAREA 2 #######################################################
############ Año a año con rangos de gini y numero de municipios por sector especifico ######
######(C1,C2,C3,F1,F2,F3,T1,T2,T3), coordenada geometrica (corregir)#########################

años = unique(base$ANO)
municipios = unique(base$CodMun)
mun_completos = vector()
base$CodMun <- as.numeric(as.character(base$CodMun))

for (n in c(1:length(municipios))){
  municipio <- base[base$CodMun == municipios[n],]
  
  codigo <- as.numeric(municipios[n])
  mun1985 <- any(municipio$ANO == 1985)
  mun2000 <- any(municipio$ANO == 2000)
  mun2015 <- any(municipio$ANO == 2015)
  
  if (mun1985 && mun2000 && mun2015){
    base$borrar[base$CodMun == codigo] <- 1
  } else{
    base$borrar[base$CodMun == codigo] <- NA
  } 
}

base <- subset(base, !is.na(borrar))

municipios <- unique(base$CodMun)
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/nuevas salidas/solicitudes/municipios año 85,2000,2015")

for (h in c(1:length(años))){
  
  año <- base[base$ANO == años[h],]
  año <- as.data.frame(año)
  if (length(año$ANO) )
  
  for (y in c(1:length(años))){
    C1 =0
    C2=0
    C3=0
    F1=0
    F2=0
    F3=0
    T1=0
    T2=0
    T3=0
    for (p in c(1:length(año[,1]))){
      if (año$sector_especifico[p] == "C1"){
        C1= C1+1
      }
      else if (año$sector_especifico[p] == "C2"){
        C2= C2+1
      }
      else if (año$sector_especifico[p] == "C3"){
        C3= C3+1
      }
      else if (año$sector_especifico[p] == "F1"){
        F1=F1+1
      }
      else if (año$sector_especifico[p] == "F2"){
        F2=F2+1
      }
      else if (año$sector_especifico[p] == "F3"){
        F3=F3+1
      }
      else if(año$sector_especifico[p] == "T1"){
        T1=T1+1
      }
      else if(año$sector_especifico[p] == "T2"){
        T2=T2+1
      }
      else if(año$sector_especifico[p] == "T3"){
        T3=T3+1
      }
      rangolm = c(rep(paste("C1",C1, sep="-"),C1),rep(paste("C2",C2, sep="-"),C2),rep(paste("C3",C3, sep="-"),C3),
                  rep(paste("F1", F1, sep="-"),F1),rep(paste("F2", F2, sep="-"),F2),rep(paste("F3", F3, sep="-"),F3),
                  rep(paste("T1", T1, sep="-"),T1),rep(paste("T2", T2, sep="-"),T2),rep(paste("T3", T3, sep="-"),T3))
      l <- mean(acomp(año[, c(11,12,13)]))
      tosay = paste("Coordenadas media geometrica","  ","F", round(l[2]*100,2),"%","T", round(l[3]*100,2),"%", "C", round(l[1]*100,2),"%", sep=" ")
      
    }
    
  }
  gg <- ggtern(data=año ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour=rango_Gini_general)) + 
    geom_point(size=1.5, aes(fill=rangolm))+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    weight_percent() +
    #scale_color_manual(values = c("0 - 0.1" = "#9999FF", "0.1 - 0.2" = "#CC0033", "0.2 - 0.3"= "#330033", "0.3 - 0.4" = "#FF9933", "0.4 - 0.5"= "#FF3399", 
    #                              "0.5 - 0.6" = "#3333FF","0.6 - 0.7" = "#FFCC00", "0.7 - 0.8" = "#33C", "0.8 - 0.9" = "#FF3333", "0.9 - 1" = "#CC99CC"), aesthetics = "color")+
    guides(colour = guide_legend(order = 1))+
    theme_bw() +
    xlab("C") + ylab("F") + zlab("T")+
    labs(title = paste(año$ANO[h]), fill =" Rango ginis", caption= tosay)+
    theme(plot.caption = element_text(size = rel(1), family = "FreeSerif", lineheight = .1), legend.position = "right",legend.justification = "center")
  
  png(filename = paste(año$ANO[h],".png", sep=""))
  plot(gg) 
  dev.off()
  
}

################################Tarea 3 #####################################################
############ Por departamento para cada año, leyenda asociada al rango de ginis y número de 
#######municipios por sector simple ##########################################################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/tarea 3 - departamento por año")
departamentos <- unique(base$departamento)
departamentos[2] <- NA
departamentos <- na.omit(departamentos)
tiempos <- (1985)
for (n in c(1:length(departamentos))){
  
  departamento <-base[base$departamento == departamentos[n],]
  departamento <- as.data.frame(departamento)
  tiempos <- (1985)
  for(m in c(1:length(tiempos))){
    departamento <-base[base$departamento == departamentos[n],]
    tiempo <- departamento[departamento$ANO == tiempos[m],]
    if (any(tiempo$ANO == "1985", na.rm = TRUE)){
      tiempo <- subset(tiempo, !is.na(ANO))
      
      C1=0
      F1=0
      T1=0
      for (p in c(1:length(tiempo[,1]))){
        if (tiempo$sector[p] == "C"){
          C1= C1+1
        }
        else if (tiempo$sector[p] == "F"){
          F1=F1+1
        }
        else if(tiempo$sector[p] == "T"){
          T1=T1+1
        }
        rangolm = c(rep(paste("C",C1, sep="-"),C1),rep(paste("F", F1, sep="-"),F1), rep(paste("T", T1, sep="-"),T1))
      }
      gg <- ggtern(data=tiempo, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour=(rango_Gini)))+ 
        geom_point(show.legend = TRUE, aes(fill= rangolm), size = 2) +
        #scale_colour_discrete(drop=TRUE,
        #                      limits = levels(tiempo$rango_Gini))+
        geom_segment(data = lines, 
                     aes(x, y, z, 
                         xend = xend, yend = yend, zend = zend), 
                     color = 'black', size = 0.5) +
        theme_bw() +
        scale_color_manual(values = c("0 - 0.1" = "#9999FF", "0.1 - 0.2" = "#CC0033", "0.2 - 0.3"= "#330033", "0.3 - 0.4" = "#FF9933", "0.4 - 0.5"= "#FF3399", 
                           "0.5 - 0.6" = "#3333FF","0.6 - 0.7" = "#FFCC00", "0.7 - 0.8" = "#003300", "0.8 - 0.9" = "#FF3333", "0.9 - 1" = "#CC99CC"))+
        guides(colour = guide_legend(order = 1))+
        xlab("C") + ylab("F") + zlab("T") +
        weight_percent() +
        labs(title = paste(tiempo$departamento[m]))+
        ggtitle(label =paste(tiempo$departamento[1]," ",tiempo$ANO, sep="") ,
                subtitle= paste("Gini prom =", round(mean(tiempo$merge_ginis),3),",", 
                                "Range(", round(min(tiempo$merge_ginis),3),"-",round(max(tiempo$merge_ginis),3),")",sep = " "))
      
      png(filename = paste(tiempo$departamento[1]," ",tiempo$ANO[1],".png", sep=""))
      plot(gg) 
      dev.off()
    }
  }
}

#######################Tarea 4 #############################################################
############Diagramas ternarios que en los años pasaron por las tres tipologías#############
###########################Mauricio#########################################################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/Solicitudes/Solicitud 1")
años <- c("2000","2003","2006","2009","2015")
municipios <- c("8520","8549","18479","41615","52696")
g=1
for (g in c(1:length(municipios))){
  municipio <- base[base$codigo.municipio == municipios[g],]
  año <- municipio[(municipio$AÑO == "2000") | (municipio$AÑO == "2003") | (municipio$AÑO == "2006") | (municipio$AÑO == "2009") | (municipio$AÑO == "2015"), ]
  
  municipio <- as.data.frame(municipio)
  compo <- año[, c(12,13,11)]
  l <- mean(acomp(año[, c(11,12,13)]))
  tosay = paste("Coordenadas media geometrica","  ","F", l[2],"%","T", l[3],"%", "C", l[1],"%", sep=" ")
  
  gg <- ggtern(data=año, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour = as.factor(anioypromedio))) + 
    geom_point(show.legend = TRUE, pch= 19, size= 2.3) + 
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    theme_bw() +
    xlab("C") + ylab("F") + zlab("T") +
    weight_percent() +
    labs(title = paste(año$municipio), caption = (tosay))+ 
    ggtitle(label =paste(año$municipio,"-", año$departamento,sep=""))+
    theme(plot.caption = element_text(size = rel(0.8), family = "FreeSerif", lineheight = .1))
  
  png(filename = paste(año$municipio[1],".png", sep=""))
  plot(gg) 
  dev.off()
}

municipio <- base[base$codigo.municipio == municipios[g],]
año <- municipio[(municipio$AÑO == "2000") | (municipio$AÑO == "2003") | (municipio$AÑO == "2006") | (municipio$AÑO == "2009") | (municipio$AÑO == "2015"), ]

############## Solicitud 2 ####################################################
#################################################################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/Solicitudes/Solicitud 2")
municipios <- c("52696","25743","41615","68250","73873","8549", "86573","8520", "18479")
años <- c("2000","2003","2006","2009","2015")

for (g in c(1:length(municipios))){
  municipio <- base[base$codigo.municipio == municipios[g],]
  año <- municipio[(municipio$AÑO == "2000") | (municipio$AÑO == "2003") | (municipio$AÑO == "2006") | (municipio$AÑO == "2009") | (municipio$AÑO == "2015"), ]
  
  municipio <- as.data.frame(municipio)
  l <- mean(acomp(año[, c(11,12,13)]))
  l <- mean(acomp(año[, c(11,12,13)]))
  tosay = paste("Coordenadas media geometrica","  ","F", l[2],"%","T", l[3],"%", "C", l[1],"%", sep=" ")
  
  gg <- ggtern(data=año, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour = as.factor(anioypromedio))) + 
    geom_point(show.legend = TRUE, pch= 19, size= 2.3) + 
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    theme_bw() +
    xlab("C") + ylab("F") + zlab("T") +
    weight_percent() +
    labs(title = paste(año$municipio), caption = (tosay))+ 
    ggtitle(label =paste(año$municipio,"-", año$departamento,sep=""))+
    theme(plot.caption = element_text(size = rel(0.8), family = "FreeSerif", lineheight = .1))
  
  png(filename = paste(año$municipio[1],".png", sep=""))
  plot(gg) 
  dev.off()
}
##################### solicitud 3 ########################################################
#########################################################################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/Solicitudes/Solicitud 3")
municipios <- c("8520","13433","13442","13683","15224","18479","25317","41319","41660","50711","54810", "68432","70473","85015")
años <- c("2000","2003","2006","2009","2015")

for (g in c(1:length(municipios))){
  municipio <- base[base$codigo.municipio == municipios[g],]
  año <- municipio[(municipio$AÑO == "2000") | (municipio$AÑO == "2003") | (municipio$AÑO == "2006") | (municipio$AÑO == "2009") | (municipio$AÑO == "2015"), ]
  
  municipio <- as.data.frame(municipio)
  compo <- año[, c(12,13,11)]
  l <- mean(acomp(año[, c(11,12,13)]))
  tosay = paste("Coordenadas media geometrica","  ","F", l[2],"%","T", l[3],"%", "C", l[1],"%", sep=" ")
  
  gg <- ggtern(data=año, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour = as.factor(anioypromedio))) + 
    geom_point(show.legend = TRUE, pch= 19, size= 2.3) + 
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    theme_bw() +
    xlab("C") + ylab("F") + zlab("T") +
    weight_percent() +
    labs(title = paste(año$municipio), caption = (tosay))+ 
    ggtitle(label =paste(año$municipio,"-", año$departamento,sep=""))+
    theme(plot.caption = element_text(size = rel(0.8), family = "FreeSerif", lineheight = .1))
  
  png(filename = paste(año$municipio[1],".png", sep=""))
  plot(gg) 
  dev.off()
}

###########################Tarea 5 #####################################################
##########Diagramas con los municipios que presentan mayor variación ##########################
##########en la diferencia de ginis arriba del 95 y debajo del 5 ###########################

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/tarea 5")

base$resta_gini<- NULL

municipios <- unique(base$CodMun)
tiempos <- unique(base$AÑO)
fi <- matrix(ncol=2)

for (t in c(1:length(municipios))){
  
  municipio <- base[base$CodMun == municipios[t],] 
  
  restagini <- max(municipio$merge_ginis)- min(municipio$merge_ginis)
  matrixSD = c(restagini,municipio$CodMun[1])
  
  fi <- rbind(fi, matrixSD)
  colnames(fi) <- c("resta_ginis" ,"Codigo mpio")
} 
fi <- as.data.frame(fi)

#######agregar resta de ginis a la base general
base1 <- merge(x=base, y=fi, by.x = "CodMun", by.y= "Codigo mpio", all=TRUE)
base1$resta_gini <- NULL

############descripción de la nueva variable resta_ginis
describe(base1$resta_ginis)
quantile(base1$resta_ginis, probs =seq(0.05,1), dec = ".", na.rm = TRUE) #Al correr esta linea usted puede 

#####menos variación 
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/tarea 5 - menor y mayor variación de Gini/menor variación gini")
cinco <- as.numeric(as.character(quantile(base1$resta_ginis, probs =seq(0.05,1), dec = ".", na.rm = TRUE)))
for (y in c(1:length(municipios))){
  municipio <- base1[base1$CodMun == municipios[y],]
  municipio <- as.data.frame(municipio)
  municipio <- municipio[! is.na(municipio$ANO) ,]
  
  
  if(municipio$resta_gini[1] < cinco){
    pp <- ggtern(data= municipio, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour =as.factor(anioypromedio))) + 
      geom_point(show.legend = TRUE, size=2) + 
      geom_segment(data = lines, 
                   aes(x, y, z, 
                       xend = xend, yend = yend, zend = zend), 
                   color = 'black', size = 0.5) +

      theme_bw() +
      xlab("C") + ylab("F") + zlab("T") +
      weight_percent() +
      ggtitle(label =paste(municipio$municipio[1],municipio$departamento[1], sep=" "),
              subtitle= paste("diferencia ginis=", round(municipio$resta_ginis,3),",", 
                              "Range(", round(min(municipio$merge_ginis),2),"-",round(max(municipio$merge_ginis),2),")",sep = " "))
    png(filename = paste(municipio$municipio[1],".png", sep=""))
    plot(pp) 
    dev.off()
  }
  
}


#### más variación
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/tarea 5 - menor y mayor variación de Gini/mayor variación gini")
nuevecinco <- as.numeric(as.character(quantile(base1$resta_ginis, probs =seq(0.95,1), dec = ".", na.rm = TRUE)))
for (y in c(1:length(municipios))){
  municipio <- base1[base1$CodMun == municipios[y],]
  municipio <- as.data.frame(municipio)
  municipio <- municipio[! is.na(municipio$ANO) ,]
  
  
  if(municipio$resta_gini[1] > nuevecinco){
    pp <- ggtern(data= municipio, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour =as.factor(anioypromedio))) + 
      geom_point(show.legend = TRUE,size=2) + 
      geom_segment(data = lines, 
                   aes(x, y, z, 
                       xend = xend, yend = yend, zend = zend), 
                   color = 'black', size = 0.5) +
      theme_bw() +
      xlab("C") + ylab("F") + zlab("T") +
      ggtitle(label =paste(municipio$municipio[1],municipio$departamento[1], sep=", "),
              subtitle= paste("diferencia ginis=", round(municipio$resta_ginis,3),",", 
                              "Range(", round(min(municipio$merge_ginis),2),"-",round(max(municipio$merge_ginis),2),")",sep = " "))
    png(filename = paste(municipio$municipio[1],".png", sep=""))
    plot(pp) 
    dev.off()
  }
  
}


############################ Tarea 8 #######################################################
################ Todos los municipios #######################################################
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/tarea 8 todos los municipios")
municipios <- unique(base$CodMun)
g=1
años <- c(1985,2000,2015)

base <- read.csv(file = "BASE2.csv", sep =",")
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/nuevas salidas/TAREA 8/municipios perfectamente comparables")


for (g in c(1:length(municipios))){
  municipio <- base[base$CodMun == municipios[g],]
  año <- municipio[(municipio$ANO == "1985") | (municipio$ANO == "2000") | (municipio$ANO == "2015"), ]
  
  municipio <- as.data.frame(municipio)
  compo <- año[, c(12,13,11)]
  if (municipio$ANO == "1985"){
    
    gg <- ggtern(data=año, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour =as.factor((anioypromedio)))) + 
      geom_point(show.legend = TRUE,pch =17, size= 2.3) + 
      geom_segment(data = lines, 
                   aes(x, y, z, 
                       xend = xend, yend = yend, zend = zend), 
                   color = 'black', size = 0.5) +
      theme_bw(base_size = 15) +
      xlab("C") + ylab("F") + zlab("T") +
      weight_percent() +
      labs(color = "año y promedio",title = paste(municipio$municipio))+ 
      ggtitle(label =paste(municipio$municipio,"-", municipio$departamento,sep=""))
      
    
    png(filename = paste(municipio$municipio[1],".png", sep=""))
    plot(gg) 
    dev.off()
  }
}


###############################################################################################
#############PROBAR COSAS
hola <- data.frame("C" = c(100,0,0), "F" = c(0,0,100), "T" = c(0,100,0))

library("ggtern")
gg <- ggtern(data=hola, aes(C,F,T)) + 
  geom_point(show.legend = FALSE, size= 1)+
  theme_tropical() +
  xlab("C") + ylab("F") + zlab("T") +


plot(gg) 

gg <- ggtern(data=hola, aes(C,F,T))+ 
  geom_point(show.legend = FALSE,pch =17, size= 2.3) + 
  geom_segment(data = lines, 
               aes(x, y, z, 
                   xend = xend, yend = yend, zend = zend), 
               color = 'black', size = 0.5) +

  theme_bw() +
  xlab("C") + ylab("F") + zlab("T") 

setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/nuevas salidas/gini genral por año")

años <- (2015)
base <- subset(base, !is.na(rango_Gini_general))
base <- subset(base, base$rango_Gini_general == c("0.25 - 0.45", "0.55 - 0.65", "0.75 - 0.85"))
for (h in c(1:length(años))){
  
  año <- base[base$ANO == años[h],]
  año <- as.data.frame(año)
  for (y in c(1:length(años))){
    r1 =0
    r2=0
    r3=0
    for (p in c(1:length(año[,1]))){
      if (año$rango_Gini_general[p] == "0.25 - 0.45"){
        r1= r1+1
      }
      
      else if (año$rango_Gini_general[p] == "0.55 - 0.65"){
        r2=r2+1
      }
      else if(año$rango_Gini_general[p] == "0.75 - 0.85"){
        r3=r3+1
      }
      rangolm = c(rep(paste("(0.25 - 0.45) ",r1,sep="-"),r1),rep(paste("(0.55 -0.65) ", r2, sep="-"),r2), rep(paste("(0.75 - 0.85) ", r3, sep="-"),r3))
      l <- mean(acomp(año[, c(12,13,14)]))
      tosay = paste("Media composicional","  ","F", round((l[2]*100),1),"%","T", round((l[3]*100),1),"%", "C",round((l[1]*100),1),"%", sep=" ")
      
    }
  }
  gg <- ggtern(data=año ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour= rango_Gini_general)) + 
    geom_point(show.legend= TRUE, size=1.5, cex.axis =2.5, aes(shape=rango_Gini_general))+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    weight_percent() +
    theme_bw(base_size = 12) +
    
    xlab(("C")) + ylab(("F")) + zlab(("T"))+
    labs(title = paste(año$ANO[h]), fill =" Rango ginis", caption = paste( "n =", length(año$CodMun),"
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                                                                          
                       ", "Promedio gini=",round(mean(año$merge_ginis),3), sep=" "))+
    #stat_mean_ellipse(size= 1.3, color = "red", steps =22, r=0.1, alpha =1)+
    theme(plot.caption = element_text(size = rel(1), family = "FreeSerif", lineheight = .1, hjust = 0.5), plot.title = element_text(size = rel(1.5), hjust= 0.5))+
    scale_color_manual(values = c("0.25 - 0.45" = "darksalmon", "0.55 - 0.65" = "springgreen3", "0.75 - 0.85"= "slateblue3"))+
    scale_shape_manual(values =c( 15,  16, 17))
  
  
  
  png(filename = paste(año$ANO[h],"2.png", sep=""))
  plot(gg)
  dev.off()
  

}
