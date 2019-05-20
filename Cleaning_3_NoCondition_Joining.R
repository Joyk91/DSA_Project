# no condition joining all the subsets with no condition data back together


library(tidyr)
library(tidyverse)
install.packages(splitstack)
library(splitstackshape) 
install.packages("naniar")
library(naniar)

data = read.csv("Listeria data March 2017 raw.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)

names(data)[1] <- "ID"     # heading was weird so changed it 


#subset Ait-Ouazzou_11 
Ait_Ouazzou_11 <- subset(data, grepl("Ait-Ouazzou_11", source))

# subset Alpas_99 
Alpas_99 <- subset(data, grepl("Alpas_99", source))
df_noCond <- rbind(Ait_Ouazzou_11, Alpas_99)
# no condition variable   

Alvarez_Ordonez_09 <- subset(data, grepl("Alvarez-Ordonez_09", source))


df_noCond <- rbind(df_noCond, Alvarez_Ordonez_09)

Bajard_96 <- subset(data, grepl("Bajard_96", source))
 
df_noCond <- rbind(df_noCond, Bajard_96) 

Bovill_00 <- subset(data, grepl("Bovill_00", source))
  
df_noCond <- rbind(df_noCond, Bovill_00) 

Bowman_10 <- subset(data, grepl("Bowman_10", source))

df_noCond <- rbind(df_noCond, Bowman_10)

Buncic_01 <- subset(data, grepl("Buncic_01", source))

df_noCond <- rbind(df_noCond,Buncic_01)

Buzrul_07 <- subset(data, grepl("Buzrul_07", source))

df_noCond <- rbind(df_noCond,Buzrul_07) 

Casadei_98<- subset(data, grepl("Casadei_98", source))

df_noCond <- rbind(df_noCond,Casadei_98) 

Chiruta_97 <- subset(data, grepl("Chiruta_97", source))
 
df_noCond <- rbind(df_noCond,Chiruta_97) 

Cox_90 <- subset(data, grepl("Cox_90", source))
 
df_noCond <- rbind(df_noCond,Cox_90) 

Denis_89 <- subset(data, grepl("Denis_89", source))

df_noCond <- rbind(df_noCond,Denis_89) 

Duh_93 <- subset(data, grepl("Duh_93", source))

df_noCond <- rbind(df_noCond,Duh_93)  

Ells_09 <- subset(data, grepl("Ells_09", source))
 
df_noCond <- rbind(df_noCond,Ells_09)  

ElShenawy_90a <- subset(data, grepl("ElShenawy_90a", source))
 
df_noCond <- rbind(df_noCond,ElShenawy_90a)  

ENVA <- subset(data, grepl("ENVA", source))
 
df_noCond <- rbind(df_noCond,ENVA)  

Francis_05 <- subset(data, grepl("Francis_05", source))

df_noCond <- rbind(df_noCond,Francis_05)  

Gabriel_09 <- subset(data, grepl("Gabriel_09", source))
 
df_noCond <- rbind(df_noCond,Gabriel_09)  

Hassani_05 <- subset(data, grepl("Hassani_05", source))
 
df_noCond <- rbind(df_noCond,Hassani_05)  

Hudson_92b <- subset(data, grepl("Hudson_92b", source))

df_noCond <- rbind(df_noCond,Hudson_92b) 

IFR <- data[4072:4094, ] 

# could not get grel or reg expression working so subsetted like this
df_noCond <- rbind(df_noCond,IFR)  

Johnston_03 <- subset(data, grepl("Johnston_03", source))
 
# no conditions 
df_noCond <- rbind(df_noCond,Johnston_03 )  

Kim_01 <- subset(data, grepl("Kim_01", source))
 
# no conditiosn 
df_noCond <- rbind(df_noCond,Kim_01)  


Lorentzen_10 <- subset(data, grepl("Lorentzen_10", source))
 
df_noCond <- rbind(df_noCond,Lorentzen_10)   

Lee_91 <- subset(data, grepl("Lee_91", source))

df_noCond <- rbind(df_noCond,Lee_91)   

Lin_04 <- subset(data, grepl("Lin_04", source))

df_noCond <- rbind(df_noCond,Lin_04)  

Mathieu_94 <- subset(data, grepl("Mathieu_94", source))

df_noCond <- rbind(df_noCond,Mathieu_94)  

Miller_00 <- subset(data, grepl("Miller_00", source))

df_noCond <- rbind(df_noCond,Miller_00)  

Modi_00 <- subset(data, grepl("Modi_00", source))
 
df_noCond <- rbind(df_noCond,Modi_00) 

Oh_93a <- subset(data, grepl("Oh_93a", source))
 
df_noCond <- rbind(df_noCond,Oh_93a) 

Parish_89 <- subset(data, grepl("Parish_89", source))

df_noCond <- rbind(df_noCond,Parish_89) 

Pearson_90c <- subset(data, grepl("Pearson_90c", source))

df_noCond <- rbind(df_noCond,Pearson_90c) 

Petran_88 <- subset(data, grepl("Petran_88", source))

df_noCond <- rbind(df_noCond,Petran_88) 

Petran_89 <- subset(data, grepl("Petran_89", source))
 
df_noCond <- rbind(df_noCond,Petran_89) 

Ryser_89a <- subset(data, grepl("Ryser_89a", source))

df_noCond <- rbind(df_noCond,Ryser_89a) 

Sivarooban_07 <- subset(data, grepl("Sivarooban_07", source))

df_noCond <- rbind(df_noCond,Sivarooban_07) 

Solomakos_08 <- subset(data, grepl("Solomakos_08", source))
 
df_noCond <- rbind(df_noCond,Solomakos_08) 

Walker_90 <- subset(data, grepl("Walker_90", source))

df_noCond <- rbind(df_noCond,Walker_90) 

WanNorhana_10 <- subset(data, grepl("WanNorhana_10", source))

df_noCond <- rbind(df_noCond,WanNorhana_10)  

Wenzel_90 <- subset(data, grepl("Wenzel_90", source))
 
df_noCond <- rbind(df_noCond,Wenzel_90)  

Whiting_92 <- subset(data, grepl("Whiting_92", source))
 
df_noCond <- rbind(df_noCond,Whiting_92)  

Zhang_08 <- subset(data, grepl("Zhang_08", source))

df_noCond <- rbind(df_noCond,Zhang_08)  

# ADRIA-N 
ADRIA_N <- subset(data, grepl("ADRIA", source))
 
df_noCond <- rbind(df_noCond,ADRIA_N)   

# subset Aguirre_09
Aguirre_09<- subset(data, grepl("Aguirre_09", source))

# no condition obvs come back for count and time 
df_noCond <- rbind(df_noCond,Aguirre_09)   

 

df_noCond <- cSplit(df_noCond, "logc", sep=";" ) 

#write.csv(df_noCond, "df_noCond_add1.csv")
