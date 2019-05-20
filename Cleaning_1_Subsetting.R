library(tidyr)
library(tidyverse)
install.packages(splitstack)
library(splitstackshape) 
install.packages("naniar")
library(naniar)


data = read.csv("Listeria data March 2017 raw.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)

names(data)[1] <- "ID"     # heading was weird so changed it 

#################### SUBSETTING #######################################################################

# ADRIA-N 
ADRIA_N <- subset(data, grepl("ADRIA", source))
View(ADRIA_N)
# source has no condition variables. have to come back to tackle time and count 

# subset Aguirre_09
Aguirre_09<- subset(data, grepl("Aguirre_09", source))
View(Aguirre_09)
# no condition obvs come back for count and time 

# subset Ahammad_89
Ahammad_89 <- subset(data, grepl("Ahammad_89", source))
#write.csv(Ahammad_89, "Ahammad_89.csv")  # cant separate so write csv and utilise excel
Ahammad_89 <- read.csv("Ahammad_89.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)

#subset Ait-Ouazzou_11 
Ait_Ouazzou_11 <- subset(data, grepl("Ait-Ouazzou_11", source))
View(Ait_Ouazzou_11)
# no condition variables 

# subset Alpas_99 
Alpas_99 <- subset(data, grepl("Alpas_99", source))
View(Alpas_99)
# no condition variable  

Alvarez_Ordonez_09 <- subset(data, grepl("Alvarez-Ordonez_09", source))
View(Alvarez_Ordonez_09) 
# no condition variable 

Amezquita_02 <- subset(data, grepl("Amezquita_02", source))
View(Amezquita_02)  
Amezquita_02 <- cSplit(Amezquita_02, "condition", sep="," )  # separate 
names(Amezquita_02)[11] <- "vacuum_packed" # rename 
names(Amezquita_02)[12] <- "co_culture" # rename 


Anderson_91 <- subset(data, grepl("Anderson_91", source))
View(Anderson_91) 
names(Anderson_91)[6] <- "NaCl" # rename 


Augustin_00 <- subset(data, grepl("Augustin_00", source))
View(Augustin_00) 
names(Augustin_00)[6] <- "NaCl" # rename 


Bajard_96 <- subset(data, grepl("Bajard_96", source))
View(Bajard_96) 
# no condition obvs 



Belda_Galbis_14a <- subset(data, grepl("Belda-Galbis_14a", source))
View(Belda_Galbis_14a) 
Belda_Galbis_14a <- cSplit(Belda_Galbis_14a, "condition", sep="," ) # separate and rename 
names(Belda_Galbis_14a)[11] <- "shaken" 
names(Belda_Galbis_14a)[12] <- "sterile"

Birrell_05 <- subset(data, grepl("Birrell_05", source))
View(Birrell_05)  
names(Birrell_05)[6] <- "NaCl"

Bouttefroy_00 <- subset(data, grepl(c("Bouttefroy_00"), source))
View(Bouttefroy_00)
names(Bouttefroy_00)[6] <- "NaCl"


Bovill_00 <- subset(data, grepl("Bovill_00", source))
View(Bovill_00) 
# no condition obvs 

Bowman_10 <- subset(data, grepl("Bowman_10", source))
View(Bowman_10) 
# no condition obvs 

Buchanan_90 <- subset(data, grepl("Buchanan_90", source))
View(Buchanan_90)
Buchanan_90 <- cSplit(Buchanan_90, "condition", sep="," ) 
names(Buchanan_90)[11] <- "NaCl" 
#write.csv(Buchanan_90, "Buchanan_90.csv") 
Buchanan_90 <- read.csv("Buchanan_90.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
# utilise excel for separation 


Buchanan_92b <- subset(data, grepl("Buchanan_92b", source))
View(Buchanan_92b) 
Buchanan_92b <- cSplit(Buchanan_92b, "condition", sep="," ) 
names(Buchanan_92b)[14] <- "co_culture"  
#write.csv(Buchanan_92b, "Buchanan_92b.csv") 
Buchanan_92b <- read.csv("Buchanan_92b.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)



Buchanan_94a <- subset(data, grepl("Buchanan_94a", source))
View(Buchanan_94a) 
Buchanan_94a <- cSplit(Buchanan_94a, "condition", sep="," ) 
names(Buchanan_94a)[11] <- "NaCl" 
#write.csv(Buchanan_94a, "Buchanan_94a.csv") 
Buchanan_94a <- read.csv("Buchanan_94a.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)




Buchanan_95 <- subset(data, grepl("Buchanan_95", source))
View(Buchanan_95) 
Buchanan_95 <- cSplit(Buchanan_95, "condition", sep="," ) 
names(Buchanan_95)[11] <- "anaerobic"  
names(Buchanan_95)[12] <- "N2"  
names(Buchanan_95)[13] <- "shaken"  
names(Buchanan_95)[14] <- "NaCl" 
#write.csv(Buchanan_95, "Buchanan_95.csv") 
Buchanan_95 <- read.csv("Buchanan_95.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)




Buchanan_97a <- subset(data, grepl("Buchanan_97a", source))
View(Buchanan_97a) 
Buchanan_97a <- cSplit(Buchanan_97a, "condition", sep="," ) 
write.csv(Buchanan_97a, "Buchanan_97a.csv") 
Buchanan_97a <- read.csv("Buchanan_97a.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)




Buncic_01 <- subset(data, grepl("Buncic_01", source))
View(Buncic_01) 
# no condition

Buzrul_07 <- subset(data, grepl("Buzrul_07", source))
View(Buzrul_07)  
# no condition

Casadei_98<- subset(data, grepl("Casadei_98", source))
View(Casadei_98) 
# no condition


Cheroutre_00 <- subset(data, grepl("Cheroutre_00", source))
View(Cheroutre_00) 
names(Cheroutre_00)[6] <- "NaCl"

Chiruta_97 <- subset(data, grepl("Chiruta_97", source))
View(Chiruta_97) 
# no condition

Chi_Zhang_03 <- subset(data, grepl("Chi-Zhang_03", source))
View(Chi_Zhang_03)
names(Chi_Zhang_03)[6] <- "shaken"

Cox_90 <- subset(data, grepl("Cox_90", source))
View(Cox_90) 
# no condition

CSIC_Frio <- subset(data, grepl("CSIC-Frio", source))
View(CSIC_Frio) 
names(CSIC_Frio)[6] <- "co_culture"

CTSCCV <- subset(data, grepl("CTSCCV", source))
View(CTSCCV)
names(CTSCCV)[6] <- "modified_atmosphere"

delCampo_01 <- subset(data, grepl("delCampo_01", source))
View(delCampo_01) 
names(delCampo_01)[6] <- "co_culture"

Denis_89 <- subset(data, grepl("Denis_89", source))
View(Denis_89) 
# no condition

Duh_93 <- subset(data, grepl("Duh_93", source))
View(Duh_93) 
# no condition

Dykes_02 <- subset(data, grepl("Dykes_02", source))
View(Dykes_02) 
names(Dykes_02)[6] <- "vacuum_packed"

Ells_09 <- subset(data, grepl("Ells_09", source))
View(Ells_09) 
# no condition

ElShenawy_88 <- subset(data, grepl("ElShenawy_88", source))
View(ElShenawy_88)
names(ElShenawy_88)[6] <- "scorbic_acid"


ElShenawy_89 <- subset(data, grepl("ElShenawy_89", source))
View(ElShenawy_89) 
names(ElShenawy_89)[6] <- "propionic_acid"

ElShenawy_90a <- subset(data, grepl("ElShenawy_90a", source))
View(ElShenawy_90a) 
# no condition 

ElShenawy_91 <- subset(data, grepl("ElShenawy_91", source))
View(ElShenawy_91) 
names(ElShenawy_91)[6] <- "scorbic_acid"

ElShenawy_92 <- subset(data, grepl("ElShenawy_92", source))
View(ElShenawy_92) 
names(ElShenawy_92)[6] <- "propionic_acid"

ENVA <- subset(data, grepl("ENVA", source))
View(ENVA) 
# no condition

Fernandez_07 <- subset(data, grepl("Fernandez_07", source))
View(Fernandez_07) 
names(Fernandez_07)[6] <- "sugar"

Fernandez_97 <- subset(data, grepl("Fernandez_97", source))
View(Fernandez_97) 
Fernandez_97 <- cSplit(Fernandez_97, "condition", sep="," ) 
names(Fernandez_97)[11] <- "NaCl"  
names(Fernandez_97)[12] <- "CO2"



Francis_01 <- subset(data, grepl("Francis_01", source))
View(Francis_01) 
Francis_01 <- cSplit(Francis_01, "condition", sep="," ) 
names(Francis_01)[11] <- "CO2"  
names(Francis_01)[12] <- "O2"

Francis_05 <- subset(data, grepl("Francis_05", source))
View(Francis_05) 
# no condition

FSA_CCFRA <- subset(data, grepl("FSA-CCFRA", source))
View(FSA_CCFRA)  
names(FSA_CCFRA)[6] <- "NaCl"

FSA_IFR <- subset(data, grepl("FSA-IFR", source))
View(FSA_IFR) 
FSA_IFR  <- cSplit(FSA_IFR, "condition", sep="," ) 
#write.csv(FSA_IFR, "FSA_IFR.csv") 
FSA_IFR  <- read.csv("FSA_IFR.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)


Gabriel_09 <- subset(data, grepl("Gabriel_09", source))
View(Gabriel_09) 
# no condition

George_92 <- subset(data, grepl("George_92", source))
View(George_92) 
George_92  <- cSplit(George_92, "condition", sep="," )  
names(George_92)[11] <- "shaken"
names(George_92)[12] <- "N2"


George_98 <- subset(data, grepl("George_98", source))
View(George_98) 
names(George_98)[6] <- "anaerobic" 
George_98$anaerobic <- tolower(George_98$anaerobic) 
# anaerobic in this subset was capital A all the ones so far have been lower case so have to match them

Giefing_01 <- subset(data, grepl("Giefing_01", source))
View(Giefing_01) 
write.csv(Giefing_01, "Giefing_01.csv") 
Giefing_01 <- read.csv("Giefing_01.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)

Hassani_05 <- subset(data, grepl("Hassani_05", source))
View(Hassani_05) 
# no conditions 

Hefnawy_93a <- subset(data, grepl("Hefnawy_93a", source))
View(Hefnawy_93a) 
names(Hefnawy_93a)[6] <- "NaCl"

Hudson_92b <- subset(data, grepl("Hudson_92b", source))
View(Hudson_92b)


IFR <- data[4072:4094, ] 
View(IFR)
# could not get grel or reg expression working so subsetted like this

IZS_BS<- subset(data, grepl("IZS-BS", data$source))
View(IZS_BS) 
#write.csv(IZS_BS, "IZS-BS.csv")
IZS_BS <- read.csv('IZS-BS.csv', na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
names(IZS_BS)[8] <- "co_culture" 
names(IZS_BS)[9] <- "NaCl"

Johnston_03 <- subset(data, grepl("Johnston_03", source))
View(Johnston_03) 
# no conditions 

Kim_01 <- subset(data, grepl("Kim_01", source))
View(Kim_01) 
# no conditiosn

Le_Marc_01 <- subset(data, grepl("Le_Marc_01", source))
View(Le_Marc_01) 
# write.csv(Le_Marc_01, "Le_Marc_01.csv") 
Le_Marc_01 <- read.csv("Le_Marc_01.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)


Lee_91 <- subset(data, grepl("Lee_91", source))
View(Lee_91) 
# no conditions 

Lin_04 <- subset(data, grepl("Lin_04", source))
View(Lin_04) 
#  no conditions

Linton_95a <- subset(data, grepl("Linton_95a", source))
View(Linton_95a)
Linton_95a  <- cSplit(Linton_95a, "condition", sep="," ) 
names(Linton_95a)[11] <- "sterile"
names(Linton_95a)[12] <- "shaken" 
names(Linton_95a)[13] <- "NaCl"

Lorentzen_10 <- subset(data, grepl("Lorentzen_10", source))
View(Lorentzen_10) 
# no conditions



Lou_97 <- subset(data, grepl("Lou_97", source))
View(Lou_97) 
# write.csv(Lou_97, "Lou_97.csv") 
Lou_97 <- read.csv("Lou_97.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)



Mathieu_94 <- subset(data, grepl("Mathieu_94", source))
View(Mathieu_94)  
# no conditions

Mellefont_03a <- subset(data, grepl("Mellefont_03a", source))
View(Mellefont_03a)  
names(Mellefont_03a)[6] <- "NaCl"


Miller_00 <- subset(data, grepl("Miller_00", source))
View(Miller_00) 
# no conditions

Miller_09 <- subset(data, grepl("Miller_09", source))
View(Miller_09)  
# no condition

Modi_00 <- subset(data, grepl("Modi_00", source))
View(Modi_00) 
# no condituion

Nolan_92 <- subset(data, grepl("Nolan_92", source))
View(Nolan_92)  
names(Nolan_92)[6] <- "NaCl"

Nyati_00 <- subset(data, grepl("Nyati_00", source))
View(Nyati_00)  
names(Nyati_00)[6] <- "anaerobic"

Oh_93a <- subset(data, grepl("Oh_93a", source))
View(Oh_93a) 
# no condition

Oh_93b <- subset(data, grepl("Oh_93b", source))
View(Oh_93b) 
names(Oh_93b)[6] <- "ethanol"



Parish_89 <- subset(data, grepl("Parish_89", source))
View(Parish_89) 
# no condition

Pascual_01 <- subset(data, grepl("Pascual_01", source))
View(Pascual_01) 
names(Pascual_01)[6] <- "NaCl"

Pearson_90a <- subset(data, grepl("Pearson_90a", source))
View(Pearson_90a) 
names(Pearson_90a)[6] <- "shaken"

Pearson_90c <- subset(data, grepl("Pearson_90c", source))
View(Pearson_90c) 
# no condition

Petran_88 <- subset(data, grepl("Petran_88", source))
View(Petran_88) 
# no condition

Petran_89 <- subset(data, grepl("Petran_89", source))
View(Petran_89) 
# no condition

Pin_01 <- subset(data, grepl("Pin_01", source))
View(Pin_01) 
Pin_01  <- cSplit(Pin_01, "condition", sep="," ) 
names(Pin_01)[11] <- "CO2"
names(Pin_01)[12] <- "O2" 
names(Pin_01)[13] <- "N2"

Porteus_08 <- subset(data, grepl("Porteus_08", source))
View(Porteus_08)
names(Porteus_08)[6] <- "NaCl"


Ryser_89a <- subset(data, grepl("Ryser_89a", source))
View(Ryser_89a) 
# no condition

Schillinger_01 <- subset(data, grepl("Schillinger_01", source))
View(Schillinger_01) 
names(Schillinger_01)[6] <- "co_culture"

Sharma_08 <- subset(data, grepl("Sharma_08", source))
View(Sharma_08) 
names(Sharma_08)[6] <- "CIO2"

Sivarooban_07 <- subset(data, grepl("Sivarooban_07", source))
View(Sivarooban_07) 
# no condition

Solomakos_08 <- subset(data, grepl("Solomakos_08", source))
View(Solomakos_08) 
# no condition

Sunen_01 <- subset(data, grepl("Sunen_01", source))
View(Sunen_01) 
names(Sunen_01)[6] <- "smoked"

TIA_UTAS <- subset(data, grepl("TIA-UTAS", source))
View(TIA_UTAS) 
# write.csv(TIA_UTAS, "TIA-UTAS.csv")
TIA_UTAS <- read.csv("TIA-UTAS.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
names(TIA_UTAS)[14] <- "co_culture"


UCM_NBTA <- subset(data, grepl("UCM-NBTA", source))
View(UCM_NBTA) 
UCM_NBTA  <- cSplit(UCM_NBTA, "condition", sep="," ) 
names(UCM_NBTA)[11] <- "CO2"
names(UCM_NBTA)[12] <- "O2" 
names(UCM_NBTA)[13] <- "N2"

UCO_DBTA <- subset(data, grepl("UCO-DBTA", source))
View(UCO_DBTA) 
UCO_DBTA <- cSplit(UCO_DBTA, "condition", sep="," ) 
names(UCO_DBTA)[11] <- "citric_acid"
names(UCO_DBTA)[12] <- "ascorbic_acid" 


UPCT <- subset(data, grepl("UPCT", source))
View(UPCT) 
names(UPCT)[6] <- "NaCl"

Uyttendaele_04 <- subset(data, grepl("Uyttendaele_04", source))
View(Uyttendaele_04) 
names(Uyttendaele_04)[6] <- "NaCl"

Vialette_03 <- subset(data, grepl("Vialette_03", source))
View(Vialette_03) 
names(Vialette_03)[6] <- "NaCl"

Walker_90 <- subset(data, grepl("Walker_90", source))
View(Walker_90) 
# no condition

WanNorhana_10 <- subset(data, grepl("WanNorhana_10", source))
View(WanNorhana_10) 
# no condition

Wenzel_90 <- subset(data, grepl("Wenzel_90", source))
View(Wenzel_90) 
# no condition

Whiting_92 <- subset(data, grepl("Whiting_92", source))
View(Whiting_92) 
# no condition

Yamazaki_03a <- subset(data, grepl("Yamazaki_03a", source))
View(Yamazaki_03a) 
names(Yamazaki_03a)[6] <- "co_culture"

Zhang_08 <- subset(data, grepl("Zhang_08", source))
View(Zhang_08) 
# no condition

