setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")
library(compositions)

library ("ggtern")

base <- read.csv("BASE_FILTRADA85a2015.csv", header=TRUE)
base$X <- NULL

base <- subset(base, !is.na(ANO))

años <- unique(base$ANO)


lines <- data.frame(x = c(0.5, 0, 0.5), 
                    y = c(0.5, 0.5, 0), 
                    z = c(0, 0.5, 0.5), 
                    xend = c(1, 1, 1)/3, 
                    yend = c(1, 1, 1)/3, 
                    zend = c(1, 1, 1)/3)


### Cambiar carpeta d
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT/SALIDAS 3/tarea 2 - todos los años con colores por rango Gini y sector especifico")
h=0
años = (1985)
for (h in c(1:length(años))){
  
  año <- base[base$ANO == años[h],]
  año <- as.data.frame(año)
  
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
  gg <- ggtern(data=año ,aes(C_porcentaje,F_porcentaje,T_porcentaje, colour = rango_Gini)) + 
    geom_point(size=1.5, aes(fill=rangolm))+
    geom_segment(data = lines, 
                 aes(x, y, z, 
                     xend = xend, yend = yend, zend = zend), 
                 color = 'black', size = 0.5) +
    weight_percent() +
    scale_color_manual(values = c("0 - 0.1" = "#9999FF", "0.1 - 0.2" = "#CC0033", "0.2 - 0.3"= "#330033", "0.3 - 0.4" = "#FF9933", "0.4 - 0.5"= "#FF3399", 
                                  "0.5 - 0.6" = "#3333FF","0.6 - 0.7" = "#FFCC00", "0.7 - 0.8" = "#33C", "0.8 - 0.9" = "#FF3333", "0.9 - 1" = "#CC99CC"), aesthetics = "color")+
    guides(colour = guide_legend(order = 1))+
    theme_bw() +
    xlab("C") + ylab("F") + zlab("T")+
    labs(title = paste(año$ANO[h]), fill =" Rango ginis", caption= tosay)+
    theme(plot.caption = element_text(size = rel(1), family = "FreeSerif", lineheight = .1), legend.position = "right",legend.justification = "center")
  
  png(filename = paste(año$ANO[h],".png", sep=""))
  plot(gg) 
  dev.off()
  
}