setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")
library(compositions)

library ("ggtern")

base <- read.csv("BASE1.csv", header=TRUE)
base$X <- NULL

base <- subset(base, !is.na(ANO))

años <- unique(base$ANO)

for (t in c(1:length(base$departamento))){
  base$anioypromedio[t] <- paste(base$ANO[t], " - ", round(base$merge_ginis[t],2), sep="")
}


lines <- data.frame(x = c(0.5, 0, 0.5), 
                    y = c(0.5, 0.5, 0), 
                    z = c(0, 0.5, 0.5), 
                    xend = c(1, 1, 1)/3, 
                    yend = c(1, 1, 1)/3, 
                    zend = c(1, 1, 1)/3)

##El ojetivo de este script es organizar un código que este en el orden de las figuras del paper,
##de forma tal que si es necesario hacer una modificación o revisar algo a futuro se pueda hacer 
##sencillamente limitandose a este código

###Figura 1
## Ternarios de Tibu y Maria La Baja con los añis 85, 2000 y 2015, los puntos son de diferentes formas
## y colores
setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")

library("readxl")
base <- read_excel("tibu_marialaba_general.xlsx")
municipios <- unique(base$CodMun )
g=1
años <- c(1985,2000,2015)

for (t in c(1:length(base$departamento))){
  base$anioypromedio[t] <- paste(base$ANO[t], " - ", round(base$merge_ginis[t],2), sep="")
}
setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/Paper final/Figura 1")

for (g in c(1:length(municipios))){
  municipio <- base[base$CodMun == municipios[g],]
  año <- municipio[(municipio$ANO == "1985") | (municipio$ANO == "2000") | (municipio$ANO == "2015"), ]
  
  municipio <- as.data.frame(municipio)
  compo <- año[, c(12,13,11)]
  if (municipio$ANO == "1985"){
    
    gg <- ggtern(data=año, aes(C_porcentaje,F_porcentaje,T_porcentaje, colour =as.factor((anioypromedio)))) + 
      geom_point(show.legend = TRUE,pch =17, size= 2.5) + 
      geom_segment(data = lines, 
                   aes(x, y, z, 
                       xend = xend, yend = yend, zend = zend), 
                   color = 'black', size = 0.5) +
      theme_bw(base_size = 15) +
      xlab("SF") + ylab("F") + zlab("LL") +
      weight_percent() +
      labs(color = "year - Gini",title = paste(municipio$municipio))+ 
      ggtitle(label =paste(municipio$municipio," (", municipio$departamento,")",sep=""))+
      theme(plot.title = element_text(hjust = 0.5))
    
    
    png(filename = paste(municipio$municipio[1],".png", sep=""))
    plot(gg) 
    dev.off()
  }
}


##### Figura 2 de ternario en carpeta, no necesita código

#### Figura 3 de ternario en carpeta, no necesita código

#### Figura 4
##Todos los muncipios con colores por sector, número de municipios en SF, F Y LL

setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/Paper final/Figura 4")

library(ggtern)
h=1
años <- c(1985,2000,2015)
base <- subset(base, !is.na(rango_Gini))
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
  gg <- ggtern(data=año ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour= sector)) + 
    geom_point(show.legend= FALSE, size=1.5, cex.axis =2.5)+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    weight_percent() +
    theme_bw(base_size = 15) +
    
    xlab(paste("SF","
                     (n=",C1, ")", sep ="")) + ylab(paste("F","
(n=",F1, ")", sep="")) + zlab(paste("LL","
(n=",T1, ")",sep=""))+
    labs(title = paste(año$ANO[h]), fill =" Rango ginis", caption = paste( "n=", length(año$CodMun),"
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
    ", "mean Gini=",round(mean(año$merge_ginis),2), sep=" "))+
    #stat_mean_ellipse(size= 1.3, color = "red", steps =22, r=0.1, alpha =1)+
    theme(plot.caption = element_text(size = rel(1), family = "FreeSerif", lineheight = .1, hjust = 0.5), plot.title = element_text(size = rel(1.2), hjust= 0.5))
  
  
  
  
  png(filename = paste(año$ANO[h],".png", sep=""))
  plot(gg)
  dev.off()
  
}

###### Figura 6
##### dos leyendas una con rango de gini (tres rangos 0.25-0.45, 0.55-0.65, 0.75 - 0.85) y 
## otra leyenda con número de municipios por rango, en las esquinas del ternario número de municipios
## por clase S, SF, LL
setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")
base <- read.csv("BASE1.csv", header=TRUE)
base$X <- NULL

base <- subset(base, !is.na(ANO))

años <- unique(base$ANO)

for (t in c(1:length(base$departamento))){
  base$anioypromedio[t] <- paste(base$ANO[t], " - ", round(base$merge_ginis[t],2), sep="")
}

setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/Paper final/Figura 6")

años <- (2015)
library(tidyverse)

base1 <- base %>%
  
  mutate(Gini = case_when(
    (merge_ginis >= 0.25 & merge_ginis <= 0.45) ~ "0.25 - 0.45",
    (merge_ginis >= 0.55 & merge_ginis <= 0.65) ~ "0.55 - 0.65",
    (merge_ginis >= 0.75 & merge_ginis <= 0.85) ~ "0.75 - 0.85"))

base1 <- subset(base1, !is.na(Gini))


for (h in c(1:length(años))){
  
  año <- base1[base1$ANO == años[h],]
  año <- as.data.frame(año)
  for (y in c(1:length(años))){
    r1 =0
    r2=0
    r3=0
    for (p in c(1:length(año[,1]))){
      if (año$Gini[p] == "0.25 - 0.45"){
        r1= r1+1
      }
      
      else if (año$Gini[p] == "0.55 - 0.65"){
        r2=r2+1
      }
      else if(año$Gini[p] == "0.75 - 0.85"){
        r3=r3+1
      }
      rangolm = c(rep(paste("(0.25 - 0.45) ",r1,sep="-"),r1),rep(paste("(0.55 -0.65) ", r2, sep="-"),r2), rep(paste("(0.75 - 0.85) ", r3, sep="-"),r3))
      l <- mean(acomp(año[, c(12,13,14)]))
      tosay = paste("Media composicional","  ","F", round((l[2]*100),1),"%","T", round((l[3]*100),1),"%", "C",round((l[1]*100),1),"%", sep=" ")
      
    }
  }
  for (y in c(1:length(años))){
    C1=0
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
  gg <- ggtern(data=año ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour= Gini)) + 
    geom_point(show.legend= TRUE, size=1.5, cex.axis =2.5, aes(shape=Gini))+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    weight_percent() +
    theme_bw(base_size = 12) +
    xlab(paste("SF","
                    (n=",C1, ")", sep ="")) + ylab(paste("F","
(n=",F1, ")", sep="")) + zlab(paste("LL","
(n=",T1, ")",sep=""))+
    labs(title = paste(año$ANO[h]), caption = paste( "n =", length(año$CodMun),"
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                          
    ", "Gini mean=",round(mean(año$merge_ginis),2), sep=" "))+
    #stat_mean_ellipse(size= 1.3, color = "red", steps =22, r=0.1, alpha =1)+
    theme(plot.caption = element_text(size = rel(1.2), family = "FreeSerif", lineheight = .1, hjust = 0.5), plot.title = element_text(size = rel(1.5), hjust= 0.5),
          legend.text=element_text(size=12), legend.title=element_text(size=13))+
    scale_color_manual(values = c("0.25 - 0.45" = "darksalmon", "0.55 - 0.65" = "springgreen3", "0.75 - 0.85"= "slateblue3"))+
    scale_shape_manual(values =c( 15,  16, 17))
  
  
  
  png(filename = paste(año$ANO[h],".png", sep=""))
  plot(gg)
  dev.off()
  
  
}

######################### Figura 7 #############################
###### tres gráficos todos del año 2015 el primero con los municipios cuyo gini
#### se encuentra en el rango de 0.6 - 0.7, el segundo municipios con rango 0.7 - 0.8 
###y el tercero con rango 0.8 - 0.9 - con numero de municipios por categoria SF, F Y LL 
### arreglos: pasar a ingles 

setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/Paper final/Figura 7")

library(tidyverse)

base1 <- base %>%
  
  mutate(Gini = case_when(
    (merge_ginis >= 0.6 & merge_ginis <= 0.7) ~ "Gini Range 0.6 - 0.7",
    (merge_ginis >= 0.7 & merge_ginis <= 0.8) ~ "Gini Range 0.7 - 0.8",
    (merge_ginis >= 0.8 & merge_ginis <= 0.9) ~ "Gini Range 0.8 - 0.9"))

base1 <- subset(base1, !is.na(Gini))
años <- (2015)
rangos <- unique(base1$Gini)

for (h in c(1:length(años))){
  
  año <- base1[base1$ANO == años[h],]
  año <- as.data.frame(año)
  for (y in c(1:length(rangos))){
    rango  <- año[año$Gini == rangos[y] ,]
    rango <- as.data.frame(rango)
    C1=0
    F1=0
    T1=0
      for (p in c(1:length(rango[,1]))){
        if (rango$sector[p] == "C"){
          C1= C1+1
        }
        
        else if (rango$sector[p] == "F"){
          F1=F1+1
        }
        else if(rango$sector[p] == "T"){
          T1=T1+1
        }
        rangolm = c(rep(paste("C",C1,sep="-"),C1),rep(paste("F", F1, sep="-"),F1), rep(paste("T", T1, sep="-"),T1))
        
      }
    
    
    gg <- ggtern(data=rango ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour= Gini)) + 
      geom_point(show.legend= FALSE, size=1.5, cex.axis =2.5, aes(fill = Gini))+
      geom_segment(data = lines, 
                   aes(x, y, z, 
                       xend = xend, yend = yend, zend = zend), 
                   color = 'black', size = 0.5) +
      weight_percent() +
      theme_bw(base_size = 12) +
      xlab(paste("SF","
                 (n=",C1, ")", sep ="")) + ylab(paste("F","
  (n=",F1, ")", sep="")) + zlab(paste("LL","
(n=",T1, ")",sep=""))+
      labs(title = paste(año$ANO[h], sep =""),
           subtitle = paste(rango$Gini[1]))+
      theme(text = element_text(size=13), plot.subtitle = element_text(size = rel(1.2), family = "FreeSerif", lineheight = .1, hjust = 0.5), plot.title = element_text(size = rel(1.5), hjust= 0.5),
            legend.text=element_text(size=12))
    
    
    
    png(filename = paste(rango$Gini[y],".png", sep=""))
    plot(gg)
    dev.off()
    
  }
}

library(data.table)
rangos <- setkey(setDT(base1), Gini)[list(AÑO = ANO), by= Gini]

#################### Figura 9 ###########################################
##La figura 8 corresponde a una ase de datos a la que llamamos panel balanceado, 
## son una serie de municipios que filtramos previamente para que sus caracteristicas los hagan comparables 
## entre si, las condiciones de filtrado son: donde 1895 <- catastral/cartografica > 80% & 
## No sean municipios madres de 1985 que se dividieron y formaron nuevos municipios en adelante
##(Después de este filtro pasamos de 1040 municipios a 969, de los cuales 781 tienen datos para 1985) 
##Tengan datos para los tres años de estudio 1985, 2000 y 2015
## (Después de este filtro pasamos de 969 a 772 municipios, de los cuales, todos tienen datos para 1985, obviamente)


setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/bases")

baseb <- read.csv(file= "BASE3.csv", header = TRUE)

setwd("D:/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/Paper final/Figura 9")

años <- c(1985,2000,2015)
baseb <- subset(baseb, !is.na(rango_Gini))
for (h in c(1:length(años))){
  
  año <- baseb[baseb$ANO == años[h],]
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
  gg <- ggtern(data=año ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour= sector)) + 
    geom_point(show.legend= FALSE, size=1.5, cex.axis =2.5)+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    weight_percent() +
    stat_mean_ellipse(size= 1, color = "red", steps =22, r=0.1, alpha =1) +
    theme_bw(base_size = 15) +
    
    xlab(paste("SF","
               (n=",C1, ")", sep ="")) + ylab(paste("F","
(n=",F1, ")", sep="")) + zlab(paste("LL","
(n=",T1, ")",sep=""))+
    labs(title = paste(año$ANO[h]), fill =" Rango ginis", caption = paste( "n=", length(año$CodMun),"
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
                                                                           
    ", "mean Gini=",round(mean(año$merge_ginis),2), sep=" "))+
    #stat_mean_ellipse(size= 1.3, color = "red", steps =22, r=0.1, alpha =1)+
    theme(plot.caption = element_text(size = rel(1), family = "FreeSerif", lineheight = .1, hjust = 0.5), plot.title = element_text(size = rel(1.2), hjust= 0.5))
  
  
  
  
  png(filename = paste(año$ANO[h],".png", sep=""))
  plot(gg)
  dev.off()
  
}
