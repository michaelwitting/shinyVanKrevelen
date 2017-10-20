input$z <- Settings$FormulaCharge
input$`mass (theoret.)` <- NA
input$`error (ppm)` <- NA

# Calculate chemical composition
# Calculate chemical composition
formulas <- as.data.frame(i2A(as.character(input[,3])))
formulas.raw <- formulas
formulas$H <- formulas$H - Settings$FormulaCharge
input <- cbind(input, formulas)

# check for element columns
tf <- colnames(formulas)
r <- c("C", "H", "O", "N", "S", "P")
tfr <- setdiff(r, tf)





if(length(tfr) > 0){
  for(i in 1:length(tfr)){
    tfr.i <- tfr[i]
    input[,paste(tfr.i)] <- rep(0, nrow(input))
  }
}

row.names(input) <- seq(1, nrow(input), by = 1)

# calculate sum of non-CHNOSP elements:
# calculate sum of non-CHNOSP elements:
nonCHNOSP <- input[,7:ncol(input)]
nonCHNOSP[,c("C", "H", "O", "N", "S", "P")] <- NULL
nonCHNOSP.sum <- rowSums(nonCHNOSP)
nonCHNOSP.sum[nonCHNOSP.sum > 0] <- NA
nonCHNOSP.i <- which(is.na(nonCHNOSP.sum))


# generate extended element list (only used to calculate molecular mass, not shown in input table)
element_list <- read.table("element_list.txt", sep = "\t", header = TRUE, check.names = FALSE)
r2 <- names(element_list)
tfr2 <- setdiff(r2, tf)
if(length(tfr2) > 0){
  for(i in 1:length(tfr2)){
    tfr2.i <- tfr2[i]
    formulas.raw[,paste(tfr2.i)] <- rep(0, nrow(formulas.raw))
  }
}

# Calculate monoisotopic mass (theoretical):
mass.theoret <- 
  formulas.raw[,"C"] * element_list[,"C"] + 
  formulas.raw[,"H"] * element_list[,"H"] + 
  formulas.raw[,"Cl"] * element_list[,"Cl"] + 
  formulas.raw[,"N"] * element_list[,"N"] + 
  formulas.raw[,"O"] * element_list[,"O"] + 
  formulas.raw[,"P"] * element_list[,"P"] + 
  formulas.raw[,"S"] * element_list[,"S"] + 
  formulas.raw[,"D"] * element_list[,"D"] + 
  formulas.raw[,"Li"] * element_list[,"Li"] + 
  formulas.raw[,"B"] * element_list[,"B"] + 
  formulas.raw[,"F"] * element_list[,"F"] + 
  formulas.raw[,"Na"] * element_list[,"Na"] + 
  formulas.raw[,"Mg"] * element_list[,"Mg"] + 
  formulas.raw[,"Si"] * element_list[,"Si"] + 
  formulas.raw[,"K"] * element_list[,"K"] + 
  formulas.raw[,"Ca"] * element_list[,"Ca"] + 
  formulas.raw[,"Mn"] * element_list[,"Mn"] + 
  formulas.raw[,"Fe"] * element_list[,"Fe"] + 
  formulas.raw[,"Br"] * element_list[,"Br"] + 
  formulas.raw[,"I"] * element_list[,"I"] 


mass.theoret.ion <- mass.theoret - (Settings$FormulaCharge * element_list[,"e"])
error <- round((input[,1] - mass.theoret.ion) / mass.theoret.ion * 10^6, 3)
input$`mass (theoret.)` <- round(mass.theoret, 6)
input$`error (ppm)` <- error 




# Calculate compositional space:
# Daniel: This could be changed to some string operations, then it would be possible to determine also other space such as CHOMg
temp <- input
temp$CHNOSP <- input$C * input$H * input$O * input$N * input$S * input$P
temp$CHNOS <- input$C * input$H * input$O * input$N * input$S
temp$CHNOP <- input$C * input$H * input$O * input$N * input$P
temp$CHOS <- input$C * input$H * input$O * input$S
temp$CHOP <- input$C * input$H * input$O * input$P
temp$CHNO <- input$C * input$H * input$O * input$N
temp$CHO <- input$C * input$H * input$O

temp$composition[temp$CHNOSP >= 1] <- "CHNOSP"
temp$composition[temp$CHNOSP == 0 & temp$CHNOS >= 1] <- "CHNOS"
temp$composition[temp$CHNOSP == 0 & temp$CHNOP >= 1] <- "CHNOP"
temp$composition[temp$CHNOSP == 0 & temp$CHNO == 0 & temp$CHOP == 0 & temp$CHOS >= 1] <- "CHOS"
temp$composition[temp$CHNOSP == 0 & temp$CHNO == 0 & temp$CHOP >= 1 & temp$CHOS == 0] <- "CHOP"
temp$composition[temp$CHNOSP == 0 & temp$CHNO >= 1 & temp$CHOP == 0 & temp$CHOS == 0] <- "CHNO"
temp$composition[temp$CHNOSP == 0 & temp$CHNO == 0 & temp$CHOP == 0 & temp$CHOS == 0 & temp$CHO >= 1] <- "CHO"
temp$composition[temp$CHNOS == 0 & temp$CHNO == 0 & temp$CHOS == 0 & temp$CHO == 0] <- "nd"
input$composition <- temp$composition


#calculate elemental ratios

# H/C:
input$HC <- round(input$H / input$C, 2)
# O/C:
input$OC <- round(input$O / input$C, 2)
# N/C:
input$NC <- round(input$N / input$C, 2)
# P/C:
input$PC <- round(input$P / input$C, 2)
# S/C:
input$SC <- round(input$S / input$C, 2)



# DBE (only defined for CHNOSP):
# Other space = NA
input$DBE <- ((input$H * Settings$valenceH + 
                 input$C * Settings$valenceC + 
                 input$O * Settings$valenceO + 
                 input$N * Settings$valenceN +
                 input$S * Settings$valenceS +
                 input$P * Settings$valenceP)/ 2) -
                (input$H + input$C + input$O + input$N + input$S + input$P) + 1

if(length(nonCHNOSP.i) > 0){
  for(i in 1:length(nonCHNOSP.i)){
    row <- nonCHNOSP.i[i]
    input$DBE[i] <- "n.d."
  }
}



# Aromaticity Index (AI):
input$AI <- round((1 + input$C - input$O - input$S - 0.5 * input$H) / 
  (input$C - input$O - input$S - input$N - input$P), 2)

if(length(nonCHNOSP.i) > 0){
  for(i in 1:length(nonCHNOSP.i)){
    row <- nonCHNOSP.i[i]
    input$AI[i] <- "n.d."
  }
}

# Aromaticity Equivalent (Xc):
input$Xc <- round((2 * input$C + input$N + input$P - input$H - 2 * Settings$Xc_m * input$O - 2 * Settings$Xc_n * input$S) /
  (input$DBE - Settings$Xc_m * input$O - Settings$Xc_n * input$S) + 1, 2)

if(length(nonCHNOSP.i) > 0){
  for(i in 1:length(nonCHNOSP.i)){
    row <- nonCHNOSP.i[i]
    input$Xc[i] <- "n.d."
  }
}

# KMD:
input$KMD <- round((round(input$`mass (theoret.)`, 0) - input$`mass (theoret.)` * round(Settings$KMD_IUPAC, 0) / Settings$KMD_IUPAC), 6)




# Absolute mass defect (AMD):
input$AMD <- round(input[,1] - floor(input[,1]), 5)
