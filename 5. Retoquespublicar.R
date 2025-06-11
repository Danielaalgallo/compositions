setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")
write.csv(full2, file ="BASE_marzo.csv")



municipiosfull2 <- unique(full2$CodDane)

tiemposfull2 <- unique(full2$ANO)
MFi <- matrix(ncol=13)
colnames(MFi) <- c("departamento","municipio","GINI_SIN_REPETICION","CodMun","ANO","C","F","T", "gini_empirico", "propietarios_total","propietarios_C","propietarios_F", "propietarios_T")

contadorsito=0
v = 0

for (q in c(1:length(municipiosfull2))){
  
  municipio <- full2[full2$CodDane == municipiosfull2[q],] 
  tiempos <- unique(municipio$AÑO)
  for (v in c(1:length(tiempos))){
    municipio <- full2[full2$CodDane == municipiosfull2[q],] 
    
    año <- municipio[municipio$AÑO == tiempos[v],]
    SF =0
    prop_sf = 0
    MF =0
    prop_mf = 0
    LL =0
    prop_ll = 0
    GINI = 0
    contadorsito = contadorsito + 1
    
    for (d in c(1:length(año[,1]))) {
      propietarios_total <- sum(año$NÚMERO.DE.PROPIETARIOS)
      if (año$clasecomposiciones[d] == "1") {
        SF = SF + año$HECTÁREAS[[d]]
        prop_sf= prop_sf + año$NÚMERO.DE.PROPIETARIOS[[d]]
        gini_empirico <- año$Gini_empirico[d]
        giniOficial <- as.numeric(as.character(año$PROPIETARIOS.SIN.REPETICION[d]))
        
      }
      else if ((año$clasecomposiciones[d] == "2")) {
        MF = MF + año$HECTÁREAS[[d]]
        prop_mf = prop_mf + año$NÚMERO.DE.PROPIETARIOS[[d]]
        gini_empirico <- año$Gini_empirico[d]
        giniOficial <- as.numeric(as.character(año$PROPIETARIOS.SIN.REPETICION[d]))
        
      }
      else if ( (año$clasecomposiciones[d] == "3")) {
        LL = LL + año$HECTÁREAS[[d]]
        prop_ll = prop_ll + año$NÚMERO.DE.PROPIETARIOS[[d]]
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
                          gini_empirico = gini_empirico, 
                          propietarios_total = propietarios_total,propietarios_C = prop_sf, propietarios_F =prop_mf,
                          propietarios_T =prop_ll )
    #### construir la base de datos
    MFi = rbind(MFi, matrixSD)
    
  }  
  
} 


### construir base con diferencias de propietarios
setwd("C:/Users/Ideapad320/Google Drive/Composiciones/Tratamiento base de datos/OUTPUT")

MFi <- subset(MFi, !is.na(CodMun))
base1 <- MFi

municipiosp <- unique(base1$CodMun)
MFp <- matrix(ncol=21)
colnames(MFp) <- c("departamento","municipio","CodMun", 
                   "prop_1985C","prop_2000C","prop_2015C", 
                   "prop_1985F","prop_2000F","prop_2015F",
                   "prop_1985C","prop_2000C","prop_2015C",
                   "dif_1985_2000_C", "dif_2000_2015_C", "dif_1985_2015_C", 
                   "dif_1985_2000_F", "dif_2000_2015_F", "dif_1985_2015_F", 
                   "dif_1985_2000_T", "dif_2000_2015_T", "dif_1985_2015_T")

l =0
for (to in c(1:length(municipiosp))){
  
  municipio <- base1[base1$CodMun == municipiosp[to] ,]
    if (nrow(municipio[1]) > 11){

      años <- subset(municipio, ANO == c(1985,2000,2015))
      años <- años[order(años$ANO),]
      prop_C_1985<- años$propietarios_C[1]
      prop_C_2000<- años$propietarios_C[2]
      prop_C_2015<- años$propietarios_C[3]
      
      prop_F_1985<- años$propietarios_F[1]
      prop_F_2000<- años$propietarios_F[2]
      prop_F_2015<- años$propietarios_F[3]
      
      prop_T_1985<- años$propietarios_T[1]
      prop_T_2000<- años$propietarios_T[2]
      prop_T_2015<- años$propietarios_T[3]

      prop1985 <- años$propietarios_total[1]
      prop2000 <- años$propietarios_total[2]
      prop2015 <- años$propietarios_total[3]
      
      dif19852000_C <-(prop_C_2000 - prop_C_1985) / prop_C_1985
      dif20002015_C <- (prop_C_2015 - prop_C_2000) / prop_C_2000
      dif19852015_C <- (prop_C_2015 - prop_C_1985) /prop_C_1985
      
      dif19852000_F <-(prop_F_2000 - prop_F_1985) / prop_F_1985
      dif20002015_F <- (prop_F_2015 - prop_F_2000) / prop_F_2000
      dif19852015_F <- (prop_F_2015 - prop_F_1985) /prop_F_1985
      
      dif19852000_T <-(prop_T_2000 - prop_T_1985) / prop_T_1985
      dif20002015_T <- (prop_T_2015 - prop_T_2000) / prop_T_2000
      dif19852015_T <- (prop_T_2015 - prop_T_1985) /prop_T_1985
      
      
      
    matrixS = data.frame(departamento = as.character(años$departamento),
                            municipio = as.character(años$municipio),
                            CodMun = (años$CodMun),
                            prop_1985C = prop_C_1985,
                            prop_2000C = prop_C_2000, 
                            prop_2015C = prop_C_2015, 
                         
                           prop_1985F = prop_F_1985,
                           prop_2000F = prop_F_2000, 
                           prop_2015F = prop_F_2015,
                         
                            prop_1985T = prop_T_1985,
                            prop_2000T = prop_T_2000, 
                            prop_2015T = prop_T_2015,
                  
                            dif_1985_2000_C = as.numeric(as.character(dif19852000_C)),
                            dif_2000_2015_C = as.numeric(as.character(dif20002015_C)), 
                            dif_1985_2015_C = as.numeric(as.character(dif19852015_C)),
                           
                           dif_1985_2000_F = as.numeric(as.character(dif19852000_F)),
                           dif_2000_2015_F = as.numeric(as.character(dif20002015_F)), 
                           dif_1985_2015_F = as.numeric(as.character(dif19852015_F)),
                           
                           dif_1985_2000_T = as.numeric(as.character(dif19852000_T)),
                           dif_2000_2015_T = as.numeric(as.character(dif20002015_T)), 
                           dif_1985_2015_T = as.numeric(as.character(dif19852015_T)) )
    matrixS <- unique(matrixS)
      
    colnames(MFp)<- colnames(matrixS)
    MFp <- rbind(MFp, matrixS)    
    }
  

}  

write.csv(MFp, "cambios_propietarios_clases.csv")
