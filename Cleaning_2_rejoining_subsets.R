library(tidyr)
library(tidyverse)
install.packages(splitstack)
library(splitstackshape) 
install.packages("naniar")
library(naniar)


data = read.csv("Listeria data March 2017 raw.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)


names(data)[1] <- "ID"     # heading was weird so changed it 
# subsets containing condition variables will be dealt with in this script

Ahammad_89 <- read.csv("Ahammad_89.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Ahammad_89[1] <- NULL  # delete forst column 

# 24 condition variables in  the dataset so need columns with these in each subset for rejoining later 
# leave out the condition variables that this subset already has. i.e lactic_acid, acetic_acid and citric_acid 
# but need columns with na's for all other condition variables 
Ahammad_89$NaCl <- NA  
Ahammad_89$anaerobic <- NA 
Ahammad_89$vacuum_packed <- NA 
Ahammad_89$co_culture <- NA 
Ahammad_89$shaken <- NA
Ahammad_89$sterile <- NA
Ahammad_89$nitrite <- NA 
Ahammad_89$N2 <- NA
Ahammad_89$CO2 <- NA
Ahammad_89$O2 <- NA
Ahammad_89$scorbic_acid <- NA
Ahammad_89$propionoc_acid <- NA
Ahammad_89$modified_atmosphere <- NA
Ahammad_89$sugar <- NA
Ahammad_89$pressure <- NA
Ahammad_89$dried <- NA
Ahammad_89$heated <- NA
Ahammad_89$ethanol <- NA
Ahammad_89$CIO2 <- NA
Ahammad_89$smoked <- NA
Ahammad_89$ascorbic_acid <- NA

# organise the data the way i want it for rejoining  
# repeat for all other subsets containing condition variables
Ahammad_89 <- Ahammad_89[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid", "logc",
                            "spec_rate")]



Amezquita_02 <- subset(data, grepl("Amezquita_02", source))
Amezquita_02 <- cSplit(Amezquita_02, "condition", sep="," ) 
names(Amezquita_02)[11] <- "vacuum_packed" 
names(Amezquita_02)[12] <- "co_culture" 

Amezquita_02$NaCl <- NA  
Amezquita_02$lactic_acid <- NA 
Amezquita_02$acetic_acid <- NA 
Amezquita_02$anaerobic <- NA 
Amezquita_02$shaken <- NA
Amezquita_02$sterile <- NA
Amezquita_02$nitrite <- NA 
Amezquita_02$N2 <- NA
Amezquita_02$CO2 <- NA
Amezquita_02$O2 <- NA
Amezquita_02$scorbic_acid <- NA
Amezquita_02$propionoc_acid <- NA
Amezquita_02$modified_atmosphere <- NA
Amezquita_02$sugar <- NA
Amezquita_02$pressure <- NA
Amezquita_02$dried <- NA
Amezquita_02$heated <- NA
Amezquita_02$ethanol <- NA
Amezquita_02$CIO2 <- NA
Amezquita_02$smoked <- NA
Amezquita_02$citric_acid <- NA
Amezquita_02$ascorbic_acid <- NA

Amezquita_02 <- Amezquita_02[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc", 
                                "spec_rate")]


# use rbind to rejoin the datasets 
df_clean <- rbind(Ahammad_89, Amezquita_02)


Anderson_91 <- subset(data, grepl("Anderson_91", source))
names(Anderson_91)[6] <- "NaCl"  

Anderson_91$lactic_acid <- NA 
Anderson_91$acetic_acid <- NA 
Anderson_91$anaerobic <- NA  
Anderson_91$vacuum_packed <- NA 
Anderson_91$co_culture <- NA 
Anderson_91$shaken <- NA
Anderson_91$sterile <- NA
Anderson_91$nitrite <- NA 
Anderson_91$N2 <- NA
Anderson_91$CO2 <- NA
Anderson_91$O2 <- NA
Anderson_91$scorbic_acid <- NA
Anderson_91$propionoc_acid <- NA
Anderson_91$modified_atmosphere <- NA
Anderson_91$sugar <- NA
Anderson_91$pressure <- NA
Anderson_91$dried <- NA
Anderson_91$heated <- NA
Anderson_91$ethanol <- NA
Anderson_91$CIO2 <- NA
Anderson_91$smoked <- NA
Anderson_91$citric_acid <- NA
Anderson_91$ascorbic_acid <- NA

Anderson_91 <- Anderson_91[,c("ID", "organism", "source", "b_f", "meas_method", 
                              "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                              "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                              "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                              "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                              "ascorbic_acid", "logc", 
                              "spec_rate")]



df_clean <- rbind(df_clean, Anderson_91) 


Augustin_00 <- subset(data, grepl("Augustin_00", source))
names(Augustin_00)[6] <- "NaCl"

Augustin_00$lactic_acid <- NA 
Augustin_00$acetic_acid <- NA 
Augustin_00$anaerobic <- NA 
Augustin_00$vacuum_packed <- NA 
Augustin_00$co_culture <- NA 
Augustin_00$shaken <- NA
Augustin_00$sterile <- NA
Augustin_00$nitrite <- NA 
Augustin_00$N2 <- NA
Augustin_00$CO2 <- NA
Augustin_00$O2 <- NA
Augustin_00$scorbic_acid <- NA
Augustin_00$propionoc_acid <- NA
Augustin_00$modified_atmosphere <- NA
Augustin_00$sugar <- NA
Augustin_00$pressure <- NA
Augustin_00$dried <- NA
Augustin_00$heated <- NA
Augustin_00$ethanol <- NA
Augustin_00$CIO2 <- NA
Augustin_00$smoked <- NA
Augustin_00$citric_acid <- NA
Augustin_00$ascorbic_acid <- NA 

Augustin_00 <- Augustin_00[,c("ID", "organism", "source", "b_f", "meas_method", 
                              "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                              "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                              "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                              "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                              "ascorbic_acid", "logc", 
                              "spec_rate")]

df_clean <- rbind(df_clean, Augustin_00)




Belda_Galbis_14a <- subset(data, grepl("Belda-Galbis_14a", source))
Belda_Galbis_14a <- cSplit(Belda_Galbis_14a, "condition", sep="," ) 
names(Belda_Galbis_14a)[11] <- "shaken" 
names(Belda_Galbis_14a)[12] <- "sterile"  

Belda_Galbis_14a$NaCl <- NA  
Belda_Galbis_14a$lactic_acid <- NA 
Belda_Galbis_14a$acetic_acid <- NA 
Belda_Galbis_14a$anaerobic <- NA 
Belda_Galbis_14a$vacuum_packed <- NA 
Belda_Galbis_14a$co_culture <- NA 
Belda_Galbis_14a$N2 <- NA
Belda_Galbis_14a$CO2 <- NA
Belda_Galbis_14a$O2 <- NA 
Belda_Galbis_14a$nitrite <- NA
Belda_Galbis_14a$scorbic_acid <- NA
Belda_Galbis_14a$propionoc_acid <- NA
Belda_Galbis_14a$modified_atmosphere <- NA
Belda_Galbis_14a$sugar <- NA
Belda_Galbis_14a$pressure <- NA
Belda_Galbis_14a$dried <- NA
Belda_Galbis_14a$heated <- NA
Belda_Galbis_14a$ethanol <- NA
Belda_Galbis_14a$CIO2 <- NA
Belda_Galbis_14a$smoked <- NA
Belda_Galbis_14a$citric_acid <- NA
Belda_Galbis_14a$ascorbic_acid <- NA


Belda_Galbis_14a <- Belda_Galbis_14a[,c("ID", "organism", "source", "b_f", "meas_method", 
                                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                        "ascorbic_acid", "logc", 
                                        "spec_rate")]


df_clean <- rbind(df_clean, Belda_Galbis_14a)




Birrell_05 <- subset(data, grepl("Birrell_05", source))
names(Birrell_05)[6] <- "NaCl" 

Birrell_05$lactic_acid <- NA 
Birrell_05$acetic_acid <- NA 
Birrell_05$anaerobic <- NA 
Birrell_05$vacuum_packed <- NA 
Birrell_05$co_culture <- NA 
Birrell_05$shaken <- NA
Birrell_05$sterile <- NA
Birrell_05$nitrite <- NA 
Birrell_05$N2 <- NA
Birrell_05$CO2 <- NA
Birrell_05$O2 <- NA
Birrell_05$scorbic_acid <- NA
Birrell_05$propionoc_acid <- NA
Birrell_05$modified_atmosphere <- NA
Birrell_05$sugar <- NA
Birrell_05$pressure <- NA
Birrell_05$dried <- NA
Birrell_05$heated <- NA
Birrell_05$ethanol <- NA
Birrell_05$CIO2 <- NA
Birrell_05$smoked <- NA
Birrell_05$citric_acid <- NA
Birrell_05$ascorbic_acid <- NA 


Birrell_05 <- Birrell_05[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid",  "logc", 
                            "spec_rate")]
df_clean <- rbind(df_clean, Birrell_05)



Bouttefroy_00 <- subset(data, grepl(c("Bouttefroy_00"), source))
names(Bouttefroy_00)[6] <- "NaCl"


Bouttefroy_00$lactic_acid <- NA 
Bouttefroy_00$acetic_acid <- NA 
Bouttefroy_00$anaerobic <- NA 
Bouttefroy_00$vacuum_packed <- NA 
Bouttefroy_00$co_culture <- NA 
Bouttefroy_00$shaken <- NA
Bouttefroy_00$sterile <- NA
Bouttefroy_00$nitrite <- NA 
Bouttefroy_00$N2 <- NA
Bouttefroy_00$CO2 <- NA
Bouttefroy_00$O2 <- NA
Bouttefroy_00$scorbic_acid <- NA
Bouttefroy_00$propionoc_acid <- NA
Bouttefroy_00$modified_atmosphere <- NA
Bouttefroy_00$sugar <- NA
Bouttefroy_00$pressure <- NA
Bouttefroy_00$dried <- NA
Bouttefroy_00$heated <- NA
Bouttefroy_00$ethanol <- NA
Bouttefroy_00$CIO2 <- NA
Bouttefroy_00$smoked <- NA
Bouttefroy_00$citric_acid <- NA
Bouttefroy_00$ascorbic_acid <- NA 


Bouttefroy_00<- Bouttefroy_00[,c("ID", "organism", "source", "b_f", "meas_method", 
                                 "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                 "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                 "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                 "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                 "ascorbic_acid", "logc", 
                                 "spec_rate")]
df_clean <- rbind(df_clean, Bouttefroy_00)



Buchanan_90 <- read.csv("Buchanan_90.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Buchanan_90$X <- NULL

Buchanan_90$lactic_acid <- NA 
Buchanan_90$acetic_acid <- NA 
Buchanan_90$vacuum_packed <- NA 
Buchanan_90$co_culture <- NA 
Buchanan_90$shaken <- NA
Buchanan_90$sterile <- NA
Buchanan_90$N2 <- NA
Buchanan_90$CO2 <- NA
Buchanan_90$O2 <- NA
Buchanan_90$scorbic_acid <- NA
Buchanan_90$propionoc_acid <- NA
Buchanan_90$modified_atmosphere <- NA
Buchanan_90$sugar <- NA
Buchanan_90$pressure <- NA
Buchanan_90$dried <- NA
Buchanan_90$heated <- NA
Buchanan_90$ethanol <- NA
Buchanan_90$CIO2 <- NA
Buchanan_90$smoked <- NA
Buchanan_90$citric_acid <- NA
Buchanan_90$ascorbic_acid <- NA


Buchanan_90<- Buchanan_90[,c("ID", "organism", "source", "b_f", "meas_method", 
                             "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                             "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                             "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                             "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                             "ascorbic_acid", "logc", 
                             "spec_rate")]
df_clean <- rbind(df_clean, Buchanan_90)



Buchanan_92b <- read.csv("Buchanan_92b.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Buchanan_92b$X <- NULL  
names(Buchanan_92b)[13] <- "co_culture"

Buchanan_92b$lactic_acid <- NA 
Buchanan_92b$acetic_acid <- NA 
Buchanan_92b$vacuum_packed <- NA 
Buchanan_92b$shaken <- NA
Buchanan_92b$sterile <- NA
Buchanan_92b$nitrite <- NA 
Buchanan_92b$N2 <- NA
Buchanan_92b$CO2 <- NA
Buchanan_92b$O2 <- NA
Buchanan_92b$scorbic_acid <- NA
Buchanan_92b$propionoc_acid <- NA
Buchanan_92b$modified_atmosphere <- NA
Buchanan_92b$sugar <- NA
Buchanan_92b$pressure <- NA
Buchanan_92b$dried <- NA
Buchanan_92b$heated <- NA
Buchanan_92b$ethanol <- NA
Buchanan_92b$CIO2 <- NA
Buchanan_92b$smoked <- NA
Buchanan_92b$citric_acid <- NA
Buchanan_92b$ascorbic_acid <- NA 

Buchanan_92b <- Buchanan_92b[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")] 

df_clean <- rbind(df_clean, Buchanan_92b)


Buchanan_94a <- read.csv("Buchanan_94a.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Buchanan_94a$X <- NULL

Buchanan_94a$acetic_acid <- NA 
Buchanan_94a$anaerobic <- NA 
Buchanan_94a$vacuum_packed <- NA 
Buchanan_94a$co_culture <- NA 
Buchanan_94a$shaken <- NA
Buchanan_94a$sterile <- NA
Buchanan_94a$N2 <- NA
Buchanan_94a$CO2 <- NA
Buchanan_94a$O2 <- NA
Buchanan_94a$scorbic_acid <- NA
Buchanan_94a$propionoc_acid <- NA
Buchanan_94a$modified_atmosphere <- NA
Buchanan_94a$sugar <- NA
Buchanan_94a$pressure <- NA
Buchanan_94a$dried <- NA
Buchanan_94a$heated <- NA
Buchanan_94a$ethanol <- NA
Buchanan_94a$CIO2 <- NA
Buchanan_94a$smoked <- NA
Buchanan_94a$citric_acid <- NA
Buchanan_94a$ascorbic_acid <- NA

Buchanan_94a <- Buchanan_94a[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]


df_clean <- rbind(df_clean, Buchanan_94a)

Buchanan_95 <- read.csv("Buchanan_95.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Buchanan_95$X <- NULL

Buchanan_95$acetic_acid <- NA 
Buchanan_95$vacuum_packed <- NA 
Buchanan_95$co_culture <- NA 
Buchanan_95$sterile <- NA
Buchanan_95$CO2 <- NA
Buchanan_95$O2 <- NA
Buchanan_95$scorbic_acid <- NA
Buchanan_95$propionoc_acid <- NA
Buchanan_95$modified_atmosphere <- NA
Buchanan_95$sugar <- NA
Buchanan_95$pressure <- NA
Buchanan_95$dried <- NA
Buchanan_95$heated <- NA
Buchanan_95$ethanol <- NA
Buchanan_95$CIO2 <- NA
Buchanan_95$smoked <- NA
Buchanan_95$citric_acid <- NA
Buchanan_95$ascorbic_acid <- NA

Buchanan_95 <- Buchanan_95[,c("ID", "organism", "source", "b_f", "meas_method", 
                              "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                              "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                              "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                              "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                              "ascorbic_acid", "logc",
                              "spec_rate")]

df_clean <- rbind(df_clean, Buchanan_95)

Buchanan_97a <- read.csv("Buchanan_97a.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Buchanan_97a$X <- NULL
names(Buchanan_97a)[1] <- "ID"


Buchanan_97a$acetic_acid <- NA 
Buchanan_97a$vacuum_packed <- NA 
Buchanan_97a$co_culture <- NA 
Buchanan_97a$shaken <- NA
Buchanan_97a$sterile <- NA
Buchanan_97a$CO2 <- NA
Buchanan_97a$O2 <- NA
Buchanan_97a$scorbic_acid <- NA
Buchanan_97a$propionoc_acid <- NA
Buchanan_97a$modified_atmosphere <- NA
Buchanan_97a$sugar <- NA
Buchanan_97a$pressure <- NA
Buchanan_97a$dried <- NA
Buchanan_97a$heated <- NA
Buchanan_97a$ethanol <- NA
Buchanan_97a$CIO2 <- NA
Buchanan_97a$smoked <- NA
Buchanan_97a$citric_acid <- NA
Buchanan_97a$ascorbic_acid <- NA

Buchanan_97a <- Buchanan_97a[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]
df_clean <- rbind(df_clean, Buchanan_97a)


Cheroutre_00 <- subset(data, grepl("Cheroutre_00", source))
names(Cheroutre_00)[6] <- "NaCl"

Cheroutre_00$lactic_acid <- NA 
Cheroutre_00$acetic_acid <- NA 
Cheroutre_00$anaerobic <- NA 
Cheroutre_00$vacuum_packed <- NA 
Cheroutre_00$co_culture <- NA 
Cheroutre_00$shaken <- NA
Cheroutre_00$sterile <- NA
Cheroutre_00$nitrite <- NA 
Cheroutre_00$N2 <- NA
Cheroutre_00$CO2 <- NA
Cheroutre_00$O2 <- NA
Cheroutre_00$scorbic_acid <- NA
Cheroutre_00$propionoc_acid <- NA
Cheroutre_00$modified_atmosphere <- NA
Cheroutre_00$sugar <- NA
Cheroutre_00$pressure <- NA
Cheroutre_00$dried <- NA
Cheroutre_00$heated <- NA
Cheroutre_00$ethanol <- NA
Cheroutre_00$CIO2 <- NA
Cheroutre_00$smoked <- NA
Cheroutre_00$citric_acid <- NA
Cheroutre_00$ascorbic_acid <- NA

Cheroutre_00 <- Cheroutre_00[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]
df_clean <- rbind(df_clean, Cheroutre_00)


Chi_Zhang_03 <- subset(data, grepl("Chi-Zhang_03", source))
names(Chi_Zhang_03)[6] <- "shaken"

Chi_Zhang_03$NaCl <- NA  
Chi_Zhang_03$lactic_acid <- NA 
Chi_Zhang_03$acetic_acid <- NA 
Chi_Zhang_03$anaerobic <- NA 
Chi_Zhang_03$vacuum_packed <- NA 
Chi_Zhang_03$co_culture <- NA 
Chi_Zhang_03$sterile <- NA
Chi_Zhang_03$nitrite <- NA 
Chi_Zhang_03$N2 <- NA
Chi_Zhang_03$CO2 <- NA
Chi_Zhang_03$O2 <- NA
Chi_Zhang_03$scorbic_acid <- NA
Chi_Zhang_03$propionoc_acid <- NA
Chi_Zhang_03$modified_atmosphere <- NA
Chi_Zhang_03$sugar <- NA
Chi_Zhang_03$pressure <- NA
Chi_Zhang_03$dried <- NA
Chi_Zhang_03$heated <- NA
Chi_Zhang_03$ethanol <- NA
Chi_Zhang_03$CIO2 <- NA
Chi_Zhang_03$smoked <- NA
Chi_Zhang_03$citric_acid <- NA
Chi_Zhang_03$ascorbic_acid <- NA
Chi_Zhang_03 <- Chi_Zhang_03[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]
df_clean <- rbind(df_clean, Chi_Zhang_03)



CSIC_Frio <- subset(data, grepl("CSIC-Frio", source))
names(CSIC_Frio)[6] <- "co_culture"

CSIC_Frio$NaCl <- NA  
CSIC_Frio$lactic_acid <- NA 
CSIC_Frio$acetic_acid <- NA 
CSIC_Frio$anaerobic <- NA 
CSIC_Frio$vacuum_packed <- NA 
CSIC_Frio$shaken <- NA
CSIC_Frio$sterile <- NA
CSIC_Frio$nitrite <- NA 
CSIC_Frio$N2 <- NA
CSIC_Frio$CO2 <- NA
CSIC_Frio$O2 <- NA
CSIC_Frio$scorbic_acid <- NA
CSIC_Frio$propionoc_acid <- NA
CSIC_Frio$modified_atmosphere <- NA
CSIC_Frio$sugar <- NA
CSIC_Frio$pressure <- NA
CSIC_Frio$dried <- NA
CSIC_Frio$heated <- NA
CSIC_Frio$ethanol <- NA
CSIC_Frio$CIO2 <- NA
CSIC_Frio$smoked <- NA
CSIC_Frio$citric_acid <- NA
CSIC_Frio$ascorbic_acid <- NA

CSIC_Frio <- CSIC_Frio[,c("ID", "organism", "source", "b_f", "meas_method", 
                          "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                          "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                          "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                          "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                          "ascorbic_acid", "logc",
                          "spec_rate")]
df_clean <- rbind(df_clean, CSIC_Frio)


CTSCCV <- subset(data, grepl("CTSCCV", source))
names(CTSCCV)[6] <- "modified_atmosphere" 

CTSCCV$NaCl <- NA  
CTSCCV$lactic_acid <- NA 
CTSCCV$acetic_acid <- NA 
CTSCCV$anaerobic <- NA 
CTSCCV$vacuum_packed <- NA 
CTSCCV$shaken <- NA
CTSCCV$sterile <- NA
CTSCCV$nitrite <- NA 
CTSCCV$N2 <- NA
CTSCCV$CO2 <- NA
CTSCCV$O2 <- NA
CTSCCV$scorbic_acid <- NA
CTSCCV$propionoc_acid <- NA
CTSCCV$co_culture <- NA
CTSCCV$sugar <- NA
CTSCCV$pressure <- NA
CTSCCV$dried <- NA
CTSCCV$heated <- NA
CTSCCV$ethanol <- NA
CTSCCV$CIO2 <- NA
CTSCCV$smoked <- NA
CTSCCV$citric_acid <- NA
CTSCCV$ascorbic_acid <- NA

CTSCCV <- CTSCCV[,c("ID", "organism", "source", "b_f", "meas_method", 
                    "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                    "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                    "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                    "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                    "ascorbic_acid", "logc",
                    "spec_rate")]
df_clean <- rbind(df_clean, CTSCCV)



delCampo_01 <- subset(data, grepl("delCampo_01", source))
names(delCampo_01)[6] <- "co_culture"

delCampo_01$NaCl <- NA  
delCampo_01$lactic_acid <- NA 
delCampo_01$acetic_acid <- NA 
delCampo_01$anaerobic <- NA 
delCampo_01$vacuum_packed <- NA 
delCampo_01$shaken <- NA
delCampo_01$sterile <- NA
delCampo_01$nitrite <- NA 
delCampo_01$N2 <- NA
delCampo_01$CO2 <- NA
delCampo_01$O2 <- NA
delCampo_01$scorbic_acid <- NA
delCampo_01$propionoc_acid <- NA
delCampo_01$modified_atmosphere <- NA
delCampo_01$sugar <- NA
delCampo_01$pressure <- NA
delCampo_01$dried <- NA
delCampo_01$heated <- NA
delCampo_01$ethanol <- NA
delCampo_01$CIO2 <- NA
delCampo_01$smoked <- NA
delCampo_01$citric_acid <- NA
delCampo_01$ascorbic_acid <- NA

delCampo_01 <- delCampo_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                              "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                              "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                              "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                              "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                              "ascorbic_acid", "logc",
                              "spec_rate")]
df_clean <- rbind(df_clean, delCampo_01)

Dykes_02 <- subset(data, grepl("Dykes_02", source))
names(Dykes_02)[6] <- "vacuum_packed"

Dykes_02$NaCl <- NA  
Dykes_02$lactic_acid <- NA 
Dykes_02$acetic_acid <- NA 
Dykes_02$anaerobic <- NA 
Dykes_02$co_culture <- NA 
Dykes_02$shaken <- NA
Dykes_02$sterile <- NA
Dykes_02$nitrite <- NA 
Dykes_02$N2 <- NA
Dykes_02$CO2 <- NA
Dykes_02$O2 <- NA
Dykes_02$scorbic_acid <- NA
Dykes_02$propionoc_acid <- NA
Dykes_02$modified_atmosphere <- NA
Dykes_02$sugar <- NA
Dykes_02$pressure <- NA
Dykes_02$dried <- NA
Dykes_02$heated <- NA
Dykes_02$ethanol <- NA
Dykes_02$CIO2 <- NA
Dykes_02$smoked <- NA
Dykes_02$citric_acid <- NA
Dykes_02$ascorbic_acid <- NA

Dykes_02 <- Dykes_02[,c("ID", "organism", "source", "b_f", "meas_method", 
                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                        "ascorbic_acid", "logc",
                        "spec_rate")]
df_clean <- rbind(df_clean, Dykes_02)


ElShenawy_88 <- subset(data, grepl("ElShenawy_88", source))
names(ElShenawy_88)[6] <- "scorbic_acid"

ElShenawy_88$NaCl <- NA  
ElShenawy_88$lactic_acid <- NA 
ElShenawy_88$acetic_acid <- NA 
ElShenawy_88$anaerobic <- NA 
ElShenawy_88$vacuum_packed <- NA 
ElShenawy_88$co_culture <- NA 
ElShenawy_88$shaken <- NA
ElShenawy_88$sterile <- NA
ElShenawy_88$nitrite <- NA 
ElShenawy_88$N2 <- NA
ElShenawy_88$CO2 <- NA
ElShenawy_88$O2 <- NA
ElShenawy_88$propionoc_acid <- NA
ElShenawy_88$modified_atmosphere <- NA
ElShenawy_88$sugar <- NA
ElShenawy_88$pressure <- NA
ElShenawy_88$dried <- NA
ElShenawy_88$heated <- NA
ElShenawy_88$ethanol <- NA
ElShenawy_88$CIO2 <- NA
ElShenawy_88$smoked <- NA
ElShenawy_88$citric_acid <- NA
ElShenawy_88$ascorbic_acid <- NA

ElShenawy_88<- ElShenawy_88[,c("ID", "organism", "source", "b_f", "meas_method", 
                               "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                               "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                               "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                               "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                               "ascorbic_acid", "logc",
                               "spec_rate")]
df_clean <- rbind(df_clean, ElShenawy_88)


ElShenawy_89 <- subset(data, grepl("ElShenawy_89", source))
names(ElShenawy_89)[6] <- "propionoc_acid"


ElShenawy_89$NaCl <- NA  
ElShenawy_89$lactic_acid <- NA 
ElShenawy_89$acetic_acid <- NA 
ElShenawy_89$anaerobic <- NA 
ElShenawy_89$vacuum_packed <- NA 
ElShenawy_89$co_culture <- NA 
ElShenawy_89$shaken <- NA
ElShenawy_89$sterile <- NA
ElShenawy_89$nitrite <- NA 
ElShenawy_89$N2 <- NA
ElShenawy_89$CO2 <- NA
ElShenawy_89$O2 <- NA
ElShenawy_89$scorbic_acid <- NA
ElShenawy_89$modified_atmosphere <- NA
ElShenawy_89$sugar <- NA
ElShenawy_89$pressure <- NA
ElShenawy_89$dried <- NA
ElShenawy_89$heated <- NA
ElShenawy_89$ethanol <- NA
ElShenawy_89$CIO2 <- NA
ElShenawy_89$smoked <- NA
ElShenawy_89$citric_acid <- NA
ElShenawy_89$ascorbic_acid <- NA

ElShenawy_89<- ElShenawy_89[,c("ID", "organism", "source", "b_f", "meas_method", 
                               "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                               "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                               "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                               "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                               "ascorbic_acid", "logc",
                               "spec_rate")]
df_clean <- rbind(df_clean, ElShenawy_89) 



ElShenawy_91 <- subset(data, grepl("ElShenawy_91", source))
names(ElShenawy_91)[6] <- "scorbic_acid" 


ElShenawy_91$NaCl <- NA  
ElShenawy_91$lactic_acid <- NA 
ElShenawy_91$acetic_acid <- NA 
ElShenawy_91$anaerobic <- NA 
ElShenawy_91$vacuum_packed <- NA 
ElShenawy_91$co_culture <- NA 
ElShenawy_91$shaken <- NA
ElShenawy_91$sterile <- NA
ElShenawy_91$nitrite <- NA 
ElShenawy_91$N2 <- NA
ElShenawy_91$CO2 <- NA
ElShenawy_91$O2 <- NA
ElShenawy_91$propionoc_acid <- NA
ElShenawy_91$modified_atmosphere <- NA
ElShenawy_91$sugar <- NA
ElShenawy_91$pressure <- NA
ElShenawy_91$dried <- NA
ElShenawy_91$heated <- NA
ElShenawy_91$ethanol <- NA
ElShenawy_91$CIO2 <- NA
ElShenawy_91$smoked <- NA
ElShenawy_91$citric_acid <- NA
ElShenawy_91$ascorbic_acid <- NA

ElShenawy_91 <- ElShenawy_91[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]
df_clean <- rbind(df_clean, ElShenawy_91)



Fernandez_07 <- subset(data, grepl("Fernandez_07", source))
names(Fernandez_07)[6] <- "sugar"

Fernandez_07$NaCl <- NA  
Fernandez_07$lactic_acid <- NA 
Fernandez_07$acetic_acid <- NA 
Fernandez_07$anaerobic <- NA 
Fernandez_07$vacuum_packed <- NA 
Fernandez_07$co_culture <- NA 
Fernandez_07$shaken <- NA
Fernandez_07$sterile <- NA
Fernandez_07$nitrite <- NA 
Fernandez_07$N2 <- NA
Fernandez_07$CO2 <- NA
Fernandez_07$O2 <- NA
Fernandez_07$scorbic_acid <- NA
Fernandez_07$propionoc_acid <- NA
Fernandez_07$modified_atmosphere <- NA
Fernandez_07$pressure <- NA
Fernandez_07$dried <- NA
Fernandez_07$heated <- NA
Fernandez_07$ethanol <- NA
Fernandez_07$CIO2 <- NA
Fernandez_07$smoked <- NA
Fernandez_07$citric_acid <- NA
Fernandez_07$ascorbic_acid <- NA

Fernandez_07 <-Fernandez_07[,c("ID", "organism", "source", "b_f", "meas_method", 
                               "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                               "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                               "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                               "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                               "ascorbic_acid", "logc",
                               "spec_rate")]
df_clean <- rbind(df_clean, Fernandez_07)



Fernandez_97 <- subset(data, grepl("Fernandez_97", source))
Fernandez_97 <- cSplit(Fernandez_97, "condition", sep="," ) 
names(Fernandez_97)[11] <- "NaCl"  
names(Fernandez_97)[12] <- "CO2"


Fernandez_97$lactic_acid <- NA 
Fernandez_97$acetic_acid <- NA 
Fernandez_97$anaerobic <- NA 
Fernandez_97$vacuum_packed <- NA 
Fernandez_97$co_culture <- NA 
Fernandez_97$shaken <- NA
Fernandez_97$sterile <- NA
Fernandez_97$nitrite <- NA 
Fernandez_97$N2 <- NA
Fernandez_97$O2 <- NA
Fernandez_97$scorbic_acid <- NA
Fernandez_97$propionoc_acid <- NA
Fernandez_97$modified_atmosphere <- NA
Fernandez_97$sugar <- NA
Fernandez_97$pressure <- NA
Fernandez_97$dried <- NA
Fernandez_97$heated <- NA
Fernandez_97$ethanol <- NA
Fernandez_97$CIO2 <- NA
Fernandez_97$smoked <- NA
Fernandez_97$citric_acid <- NA
Fernandez_97$ascorbic_acid <- NA

Fernandez_97 <- Fernandez_97[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]
df_clean <- rbind(df_clean, Fernandez_97)


Francis_01 <- subset(data, grepl("Francis_01", source))
Francis_01 <- cSplit(Francis_01, "condition", sep="," ) 
names(Francis_01)[11] <- "CO2"  
names(Francis_01)[12] <- "O2"

Francis_01$NaCl <- NA  
Francis_01$lactic_acid <- NA 
Francis_01$acetic_acid <- NA 
Francis_01$anaerobic <- NA 
Francis_01$vacuum_packed <- NA 
Francis_01$co_culture <- NA 
Francis_01$shaken <- NA
Francis_01$sterile <- NA
Francis_01$nitrite <- NA 
Francis_01$N2 <- NA
Francis_01$scorbic_acid <- NA
Francis_01$propionoc_acid <- NA
Francis_01$modified_atmosphere <- NA
Francis_01$sugar <- NA
Francis_01$pressure <- NA
Francis_01$dried <- NA
Francis_01$heated <- NA
Francis_01$ethanol <- NA
Francis_01$CIO2 <- NA
Francis_01$smoked <- NA
Francis_01$citric_acid <- NA
Francis_01$ascorbic_acid <- NA

Francis_01 <- Francis_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid", "logc",
                            "spec_rate")]
df_clean <- rbind(df_clean, Francis_01)

FSA_CCFRA <- subset(data, grepl("FSA-CCFRA", source))
names(FSA_CCFRA)[6] <- "NaCl"

FSA_CCFRA$lactic_acid <- NA 
FSA_CCFRA$acetic_acid <- NA 
FSA_CCFRA$anaerobic <- NA 
FSA_CCFRA$vacuum_packed <- NA 
FSA_CCFRA$co_culture <- NA 
FSA_CCFRA$shaken <- NA
FSA_CCFRA$sterile <- NA
FSA_CCFRA$nitrite <- NA 
FSA_CCFRA$N2 <- NA
FSA_CCFRA$CO2 <- NA
FSA_CCFRA$O2 <- NA
FSA_CCFRA$scorbic_acid <- NA
FSA_CCFRA$propionoc_acid <- NA
FSA_CCFRA$modified_atmosphere <- NA
FSA_CCFRA$sugar <- NA
FSA_CCFRA$pressure <- NA
FSA_CCFRA$dried <- NA
FSA_CCFRA$heated <- NA
FSA_CCFRA$ethanol <- NA
FSA_CCFRA$CIO2 <- NA
FSA_CCFRA$smoked <- NA
FSA_CCFRA$citric_acid <- NA
FSA_CCFRA$ascorbic_acid <- NA

FSA_CCFRA <- FSA_CCFRA[,c("ID", "organism", "source", "b_f", "meas_method", 
                          "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                          "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                          "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                          "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                          "ascorbic_acid", "logc",
                          "spec_rate")]
df_clean <- rbind(df_clean, FSA_CCFRA)



FSA_IFR  <- read.csv("FSA_IFR.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
FSA_IFR$X <- NULL
names(FSA_IFR)[1] <- "ID"  

FSA_IFR$anaerobic <- NA 
FSA_IFR$vacuum_packed <- NA 
FSA_IFR$co_culture <- NA 
FSA_IFR$shaken <- NA
FSA_IFR$sterile <- NA
FSA_IFR$N2 <- NA
FSA_IFR$O2 <- NA
FSA_IFR$propionoc_acid <- NA
FSA_IFR$modified_atmosphere <- NA
FSA_IFR$sugar <- NA
FSA_IFR$pressure <- NA
FSA_IFR$dried <- NA
FSA_IFR$heated <- NA
FSA_IFR$ethanol <- NA
FSA_IFR$CIO2 <- NA
FSA_IFR$smoked <- NA
FSA_IFR$citric_acid <- NA
FSA_IFR$ascorbic_acid <- NA

FSA_IFR <- FSA_IFR[,c("ID", "organism", "source", "b_f", "meas_method", 
                      "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                      "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                      "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                      "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                      "ascorbic_acid", "logc",
                      "spec_rate")]
df_clean <- rbind(df_clean, FSA_IFR)



George_92 <- subset(data, grepl("George_92", source))
George_92  <- cSplit(George_92, "condition", sep="," )  
names(George_92)[11] <- "shaken"
names(George_92)[12] <- "N2"

George_92$NaCl <- NA  
George_92$lactic_acid <- NA 
George_92$acetic_acid <- NA 
George_92$anaerobic <- NA 
George_92$vacuum_packed <- NA 
George_92$co_culture <- NA 
George_92$sterile <- NA
George_92$nitrite <- NA 
George_92$CO2 <- NA
George_92$O2 <- NA 
George_92$modified_atmosphere <- NA
George_92$scorbic_acid <- NA
George_92$propionoc_acid <- NA
George_92$sugar <- NA
George_92$pressure <- NA
George_92$dried <- NA
George_92$heated <- NA
George_92$ethanol <- NA
George_92$CIO2 <- NA
George_92$smoked <- NA
George_92$citric_acid <- NA
George_92$ascorbic_acid <- NA

George_92 <- George_92[,c("ID", "organism", "source", "b_f", "meas_method", 
                          "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                          "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                          "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                          "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                          "ascorbic_acid", "logc",
                          "spec_rate")]
df_clean <- rbind(df_clean, George_92) 



George_98 <- subset(data, grepl("George_98", source))
names(George_98)[6] <- "anaerobic" 
George_98$anaerobic <- tolower(George_98$anaerobic) 
# anaerobic in this subset was capital A all the ones so far have been lower case so have to match them

George_98$NaCl <- NA  
George_98$lactic_acid <- NA 
George_98$acetic_acid <- NA 
George_98$vacuum_packed <- NA 
George_98$co_culture <- NA 
George_98$shaken <- NA
George_98$sterile <- NA
George_98$nitrite <- NA 
George_98$N2 <- NA
George_98$CO2 <- NA
George_98$O2 <- NA
George_98$scorbic_acid <- NA
George_98$propionoc_acid <- NA
George_98$modified_atmosphere <- NA
George_98$sugar <- NA
George_98$pressure <- NA
George_98$dried <- NA
George_98$heated <- NA
George_98$ethanol <- NA
George_98$CIO2 <- NA
George_98$smoked <- NA
George_98$citric_acid <- NA
George_98$ascorbic_acid <- NA

George_98 <- George_98[,c("ID", "organism", "source", "b_f", "meas_method", 
                          "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                          "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                          "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                          "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                          "ascorbic_acid", "logc",
                          "spec_rate")]
df_clean <- rbind(df_clean, George_98)




Giefing_01 <- subset(data, grepl("Giefing_01", source))
Giefing_01 <- read.csv("Giefing_01.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Giefing_01$X <- NULL
names(Giefing_01)[1] <- "ID"
names(Giefing_01)[6] <- "co_culture"

Giefing_01$NaCl <- NA  
Giefing_01$lactic_acid <- NA 
Giefing_01$acetic_acid <- NA 
Giefing_01$anaerobic <- NA 
Giefing_01$vacuum_packed <- NA 
Giefing_01$shaken <- NA
Giefing_01$sterile <- NA
Giefing_01$nitrite <- NA 
Giefing_01$O2 <- NA
Giefing_01$scorbic_acid <- NA
Giefing_01$propionoc_acid <- NA
Giefing_01$modified_atmosphere <- NA
Giefing_01$sugar <- NA
Giefing_01$dried <- NA
Giefing_01$heated <- NA
Giefing_01$ethanol <- NA
Giefing_01$CIO2 <- NA
Giefing_01$smoked <- NA
Giefing_01$citric_acid <- NA
Giefing_01$ascorbic_acid <- NA

Giefing_01 <- Giefing_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid", "logc",
                            "spec_rate")]
df_clean <- rbind(df_clean, Giefing_01)



Hefnawy_93a <- subset(data, grepl("Hefnawy_93a", source))
names(Hefnawy_93a)[6] <- "NaCl" 

Hefnawy_93a$lactic_acid <- NA 
Hefnawy_93a$acetic_acid <- NA 
Hefnawy_93a$anaerobic <- NA 
Hefnawy_93a$vacuum_packed <- NA 
Hefnawy_93a$co_culture <- NA 
Hefnawy_93a$shaken <- NA
Hefnawy_93a$sterile <- NA
Hefnawy_93a$nitrite <- NA 
Hefnawy_93a$N2 <- NA
Hefnawy_93a$CO2 <- NA
Hefnawy_93a$O2 <- NA
Hefnawy_93a$scorbic_acid <- NA
Hefnawy_93a$propionoc_acid <- NA
Hefnawy_93a$modified_atmosphere <- NA
Hefnawy_93a$sugar <- NA
Hefnawy_93a$pressure <- NA
Hefnawy_93a$dried <- NA
Hefnawy_93a$heated <- NA
Hefnawy_93a$ethanol <- NA
Hefnawy_93a$CIO2 <- NA
Hefnawy_93a$smoked <- NA
Hefnawy_93a$citric_acid <- NA
Hefnawy_93a$ascorbic_acid <- NA

Hefnawy_93a <- Hefnawy_93a[,c("ID", "organism", "source", "b_f", "meas_method", 
                              "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                              "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                              "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                              "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                              "ascorbic_acid", "logc",
                              "spec_rate")]
df_clean <- rbind(df_clean, Hefnawy_93a)




IZS_BS <- read.csv('IZS-BS.csv', na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
names(IZS_BS)[8] <- "co_culture" 
names(IZS_BS)[9] <- "NaCl" 
IZS_BS$X<- NULL 
names(IZS_BS)[1] <- "ID" 

IZS_BS$lactic_acid <- NA 
IZS_BS$acetic_acid <- NA 
IZS_BS$anaerobic <- NA 
IZS_BS$vacuum_packed <- NA 
IZS_BS$shaken <- NA
IZS_BS$sterile <- NA
IZS_BS$nitrite <- NA 
IZS_BS$N2 <- NA
IZS_BS$CO2 <- NA
IZS_BS$O2 <- NA
IZS_BS$scorbic_acid <- NA
IZS_BS$propionoc_acid <- NA
IZS_BS$modified_atmosphere <- NA
IZS_BS$sugar <- NA
IZS_BS$pressure <- NA
IZS_BS$ethanol <- NA
IZS_BS$CIO2 <- NA
IZS_BS$smoked <- NA
IZS_BS$citric_acid <- NA
IZS_BS$ascorbic_acid <- NA

IZS_BS <- IZS_BS[,c("ID", "organism", "source", "b_f", "meas_method", 
                    "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                    "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                    "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                    "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                    "ascorbic_acid", "logc",
                    "spec_rate")]
df_clean <- rbind(df_clean, IZS_BS)



Le_Marc_01 <- read.csv("Le_Marc_01.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Le_Marc_01$X <- NULL 
names(Le_Marc_01)[1] <- "ID"

Le_Marc_01$acetic_acid <- NA 
Le_Marc_01$anaerobic <- NA 
Le_Marc_01$vacuum_packed <- NA 
Le_Marc_01$co_culture <- NA 
Le_Marc_01$shaken <- NA
Le_Marc_01$sterile <- NA
Le_Marc_01$nitrite <- NA 
Le_Marc_01$N2 <- NA
Le_Marc_01$CO2 <- NA
Le_Marc_01$O2 <- NA
Le_Marc_01$scorbic_acid <- NA
Le_Marc_01$propionoc_acid <- NA
Le_Marc_01$modified_atmosphere <- NA
Le_Marc_01$sugar <- NA
Le_Marc_01$pressure <- NA
Le_Marc_01$dried <- NA
Le_Marc_01$heated <- NA
Le_Marc_01$ethanol <- NA
Le_Marc_01$CIO2 <- NA
Le_Marc_01$smoked <- NA
Le_Marc_01$citric_acid <- NA
Le_Marc_01$ascorbic_acid <- NA

Le_Marc_01 <- Le_Marc_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid", "logc",
                            "spec_rate")]
df_clean <- rbind(df_clean, Le_Marc_01) 


Linton_95a <- subset(data, grepl("Linton_95a", source))
Linton_95a  <- cSplit(Linton_95a, "condition", sep="," ) 
names(Linton_95a)[11] <- "sterile"
names(Linton_95a)[12] <- "shaken" 
names(Linton_95a)[13] <- "NaCl"

Linton_95a$NaCl <- NA  
Linton_95a$lactic_acid <- NA 
Linton_95a$acetic_acid <- NA 
Linton_95a$anaerobic <- NA 
Linton_95a$vacuum_packed <- NA 
Linton_95a$co_culture <- NA 
Linton_95a$shaken <- NA
Linton_95a$sterile <- NA
Linton_95a$nitrite <- NA 
Linton_95a$N2 <- NA
Linton_95a$CO2 <- NA
Linton_95a$O2 <- NA
Linton_95a$scorbic_acid <- NA
Linton_95a$propionoc_acid <- NA
Linton_95a$modified_atmosphere <- NA
Linton_95a$sugar <- NA
Linton_95a$pressure <- NA
Linton_95a$dried <- NA
Linton_95a$heated <- NA
Linton_95a$ethanol <- NA
Linton_95a$CIO2 <- NA
Linton_95a$smoked <- NA
Linton_95a$citric_acid <- NA
Linton_95a$ascorbic_acid <- NA

Linton_95a <- Linton_95a[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid", "logc",
                            "spec_rate")]
df_clean <- rbind(df_clean, Linton_95a)




Lou_97 <- read.csv("Lou_97.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
Lou_97$X <- NULL
names(Lou_97)[1] <- "ID"

Lou_97$lactic_acid <- NA 
Lou_97$acetic_acid <- NA 
Lou_97$anaerobic <- NA 
Lou_97$vacuum_packed <- NA 
Lou_97$co_culture <- NA 
Lou_97$shaken <- NA
Lou_97$sterile <- NA
Lou_97$nitrite <- NA 
Lou_97$N2 <- NA
Lou_97$CO2 <- NA
Lou_97$O2 <- NA
Lou_97$scorbic_acid <- NA
Lou_97$propionoc_acid <- NA
Lou_97$modified_atmosphere <- NA
Lou_97$sugar <- NA
Lou_97$pressure <- NA
Lou_97$dried <- NA
Lou_97$heated <- NA
Lou_97$CIO2 <- NA
Lou_97$smoked <- NA
Lou_97$citric_acid <- NA
Lou_97$ascorbic_acid <- NA

Lou_97 <- Lou_97[,c("ID", "organism", "source", "b_f", "meas_method", 
                    "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                    "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                    "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                    "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                    "ascorbic_acid", "logc",
                    "spec_rate")]
df_clean <- rbind(df_clean, Lou_97)



Mellefont_03a <- subset(data, grepl("Mellefont_03a", source))
names(Mellefont_03a)[6] <- "NaCl"

Mellefont_03a$lactic_acid <- NA 
Mellefont_03a$acetic_acid <- NA 
Mellefont_03a$anaerobic <- NA 
Mellefont_03a$vacuum_packed <- NA 
Mellefont_03a$co_culture <- NA 
Mellefont_03a$shaken <- NA
Mellefont_03a$sterile <- NA
Mellefont_03a$nitrite <- NA 
Mellefont_03a$N2 <- NA
Mellefont_03a$CO2 <- NA
Mellefont_03a$O2 <- NA
Mellefont_03a$scorbic_acid <- NA
Mellefont_03a$propionoc_acid <- NA
Mellefont_03a$modified_atmosphere <- NA
Mellefont_03a$sugar <- NA
Mellefont_03a$pressure <- NA
Mellefont_03a$dried <- NA
Mellefont_03a$heated <- NA
Mellefont_03a$ethanol <- NA
Mellefont_03a$CIO2 <- NA
Mellefont_03a$smoked <- NA
Mellefont_03a$citric_acid <- NA
Mellefont_03a$ascorbic_acid <- NA

Mellefont_03a <- Mellefont_03a[,c("ID", "organism", "source", "b_f", "meas_method", 
                                  "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                  "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                  "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                  "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                  "ascorbic_acid", "logc",
                                  "spec_rate")]
df_clean <- rbind(df_clean, Mellefont_03a)



Nolan_92 <- subset(data, grepl("Nolan_92", source))
names(Nolan_92)[6] <- "NaCl" 

Nolan_92$lactic_acid <- NA 
Nolan_92$acetic_acid <- NA 
Nolan_92$anaerobic <- NA 
Nolan_92$vacuum_packed <- NA 
Nolan_92$co_culture <- NA 
Nolan_92$shaken <- NA
Nolan_92$sterile <- NA
Nolan_92$nitrite <- NA 
Nolan_92$N2 <- NA
Nolan_92$CO2 <- NA
Nolan_92$O2 <- NA
Nolan_92$scorbic_acid <- NA
Nolan_92$propionoc_acid <- NA
Nolan_92$modified_atmosphere <- NA
Nolan_92$sugar <- NA
Nolan_92$pressure <- NA
Nolan_92$dried <- NA
Nolan_92$heated <- NA
Nolan_92$ethanol <- NA
Nolan_92$CIO2 <- NA
Nolan_92$smoked <- NA
Nolan_92$citric_acid <- NA
Nolan_92$ascorbic_acid <- NA

Nolan_92 <- Nolan_92[,c("ID", "organism", "source", "b_f", "meas_method", 
                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                        "ascorbic_acid", "logc",
                        "spec_rate")]
df_clean <- rbind(df_clean, Nolan_92)



Nyati_00 <- subset(data, grepl("Nyati_00", source))
names(Nyati_00)[6] <- "anaerobic"

Nyati_00$NaCl <- NA  
Nyati_00$lactic_acid <- NA 
Nyati_00$acetic_acid <- NA 
Nyati_00$vacuum_packed <- NA 
Nyati_00$co_culture <- NA 
Nyati_00$shaken <- NA
Nyati_00$sterile <- NA
Nyati_00$nitrite <- NA 
Nyati_00$N2 <- NA
Nyati_00$CO2 <- NA
Nyati_00$O2 <- NA
Nyati_00$scorbic_acid <- NA
Nyati_00$propionoc_acid <- NA
Nyati_00$modified_atmosphere <- NA
Nyati_00$sugar <- NA
Nyati_00$pressure <- NA
Nyati_00$dried <- NA
Nyati_00$heated <- NA
Nyati_00$ethanol <- NA
Nyati_00$CIO2 <- NA
Nyati_00$smoked <- NA
Nyati_00$citric_acid <- NA
Nyati_00$ascorbic_acid <- NA

Nyati_00 <- Nyati_00[,c("ID", "organism", "source", "b_f", "meas_method", 
                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                        "ascorbic_acid", "logc",
                        "spec_rate")]
df_clean <- rbind(df_clean, Nyati_00)


Oh_93b <- subset(data, grepl("Oh_93b", source))
names(Oh_93b)[6] <- "ethanol"

Oh_93b$NaCl <- NA  
Oh_93b$lactic_acid <- NA 
Oh_93b$acetic_acid <- NA 
Oh_93b$anaerobic <- NA 
Oh_93b$vacuum_packed <- NA 
Oh_93b$co_culture <- NA 
Oh_93b$shaken <- NA
Oh_93b$sterile <- NA
Oh_93b$nitrite <- NA 
Oh_93b$N2 <- NA
Oh_93b$CO2 <- NA
Oh_93b$O2 <- NA
Oh_93b$scorbic_acid <- NA
Oh_93b$propionoc_acid <- NA
Oh_93b$modified_atmosphere <- NA
Oh_93b$sugar <- NA
Oh_93b$pressure <- NA
Oh_93b$dried <- NA
Oh_93b$heated <- NA
Oh_93b$ethanol <- NA
Oh_93b$CIO2 <- NA
Oh_93b$smoked <- NA
Oh_93b$citric_acid <- NA
Oh_93b$ascorbic_acid <- NA

Oh_93b <- Oh_93b[,c("ID", "organism", "source", "b_f", "meas_method", 
                    "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                    "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                    "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                    "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                    "ascorbic_acid", "logc",
                    "spec_rate")]
df_clean <- rbind(df_clean, Oh_93b)



Pascual_01 <- subset(data, grepl("Pascual_01", source))
names(Pascual_01)[6] <- "NaCl"

Pascual_01$lactic_acid <- NA 
Pascual_01$acetic_acid <- NA 
Pascual_01$anaerobic <- NA 
Pascual_01$vacuum_packed <- NA 
Pascual_01$co_culture <- NA 
Pascual_01$shaken <- NA
Pascual_01$sterile <- NA
Pascual_01$nitrite <- NA 
Pascual_01$N2 <- NA
Pascual_01$CO2 <- NA
Pascual_01$O2 <- NA
Pascual_01$scorbic_acid <- NA
Pascual_01$propionoc_acid <- NA
Pascual_01$modified_atmosphere <- NA
Pascual_01$sugar <- NA
Pascual_01$pressure <- NA
Pascual_01$dried <- NA
Pascual_01$heated <- NA
Pascual_01$ethanol <- NA
Pascual_01$CIO2 <- NA
Pascual_01$smoked <- NA
Pascual_01$citric_acid <- NA
Pascual_01$ascorbic_acid <- NA

Pascual_01 <- Pascual_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid", "logc",
                            "spec_rate")]
df_clean <- rbind(df_clean, Pascual_01)




Pearson_90a <- subset(data, grepl("Pearson_90a", source))
names(Pearson_90a)[6] <- "shaken"

Pearson_90a$NaCl <- NA  
Pearson_90a$lactic_acid <- NA 
Pearson_90a$acetic_acid <- NA 
Pearson_90a$anaerobic <- NA 
Pearson_90a$vacuum_packed <- NA 
Pearson_90a$co_culture <- NA 
Pearson_90a$sterile <- NA
Pearson_90a$nitrite <- NA 
Pearson_90a$N2 <- NA
Pearson_90a$CO2 <- NA
Pearson_90a$O2 <- NA
Pearson_90a$scorbic_acid <- NA
Pearson_90a$propionoc_acid <- NA
Pearson_90a$modified_atmosphere <- NA
Pearson_90a$sugar <- NA
Pearson_90a$pressure <- NA
Pearson_90a$dried <- NA
Pearson_90a$heated <- NA
Pearson_90a$ethanol <- NA
Pearson_90a$CIO2 <- NA
Pearson_90a$smoked <- NA
Pearson_90a$citric_acid <- NA
Pearson_90a$ascorbic_acid <- NA

Pearson_90a <- Pearson_90a[,c("ID", "organism", "source", "b_f", "meas_method", 
                              "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                              "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                              "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                              "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                              "ascorbic_acid", "logc",
                              "spec_rate")]
df_clean <- rbind(df_clean, Pearson_90a)


Pin_01 <- subset(data, grepl("Pin_01", source))
Pin_01  <- cSplit(Pin_01, "condition", sep="," ) 
names(Pin_01)[11] <- "CO2"
names(Pin_01)[12] <- "O2" 
names(Pin_01)[13] <- "N2" 


Pin_01$NaCl <- NA  
Pin_01$lactic_acid <- NA 
Pin_01$acetic_acid <- NA 
Pin_01$anaerobic <- NA 
Pin_01$vacuum_packed <- NA 
Pin_01$co_culture <- NA 
Pin_01$shaken <- NA
Pin_01$sterile <- NA
Pin_01$nitrite <- NA 
Pin_01$scorbic_acid <- NA
Pin_01$propionoc_acid <- NA
Pin_01$modified_atmosphere <- NA
Pin_01$sugar <- NA
Pin_01$pressure <- NA
Pin_01$dried <- NA
Pin_01$heated <- NA
Pin_01$ethanol <- NA
Pin_01$CIO2 <- NA
Pin_01$smoked <- NA
Pin_01$citric_acid <- NA
Pin_01$ascorbic_acid <- NA

Pin_01 <- Pin_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                    "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                    "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                    "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                    "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                    "ascorbic_acid", "logc",
                    "spec_rate")]
df_clean <- rbind(df_clean, Pin_01)


Porteus_08 <- subset(data, grepl("Porteus_08", source))
names(Porteus_08)[6] <- "NaCl"

Porteus_08$lactic_acid <- NA 
Porteus_08$acetic_acid <- NA 
Porteus_08$anaerobic <- NA 
Porteus_08$vacuum_packed <- NA 
Porteus_08$co_culture <- NA 
Porteus_08$shaken <- NA
Porteus_08$sterile <- NA
Porteus_08$nitrite <- NA 
Porteus_08$N2 <- NA
Porteus_08$CO2 <- NA
Porteus_08$O2 <- NA
Porteus_08$scorbic_acid <- NA
Porteus_08$propionoc_acid <- NA
Porteus_08$modified_atmosphere <- NA
Porteus_08$sugar <- NA
Porteus_08$pressure <- NA
Porteus_08$dried <- NA
Porteus_08$heated <- NA
Porteus_08$ethanol <- NA
Porteus_08$CIO2 <- NA
Porteus_08$smoked <- NA
Porteus_08$citric_acid <- NA
Porteus_08$ascorbic_acid <- NA

Porteus_08 <- Porteus_08[,c("ID", "organism", "source", "b_f", "meas_method", 
                            "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                            "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                            "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                            "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                            "ascorbic_acid", "logc",
                            "spec_rate")]
df_clean <- rbind(df_clean, Porteus_08)


Schillinger_01 <- subset(data, grepl("Schillinger_01", source))
names(Schillinger_01)[6] <- "co_culture" 

Schillinger_01$NaCl <- NA  
Schillinger_01$lactic_acid <- NA 
Schillinger_01$acetic_acid <- NA 
Schillinger_01$anaerobic <- NA 
Schillinger_01$vacuum_packed <- NA 
Schillinger_01$shaken <- NA
Schillinger_01$sterile <- NA
Schillinger_01$nitrite <- NA 
Schillinger_01$N2 <- NA
Schillinger_01$CO2 <- NA
Schillinger_01$O2 <- NA
Schillinger_01$scorbic_acid <- NA
Schillinger_01$propionoc_acid <- NA
Schillinger_01$modified_atmosphere <- NA
Schillinger_01$sugar <- NA
Schillinger_01$pressure <- NA
Schillinger_01$dried <- NA
Schillinger_01$heated <- NA
Schillinger_01$ethanol <- NA
Schillinger_01$CIO2 <- NA
Schillinger_01$smoked <- NA
Schillinger_01$citric_acid <- NA
Schillinger_01$ascorbic_acid <- NA

Schillinger_01 <- Schillinger_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                                    "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                    "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                    "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                    "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                    "ascorbic_acid", "logc",
                                    "spec_rate")]
df_clean <- rbind(df_clean, Schillinger_01)



Sharma_08 <- subset(data, grepl("Sharma_08", source))
names(Sharma_08)[6] <- "CIO2"

Sharma_08$NaCl <- NA  
Sharma_08$lactic_acid <- NA 
Sharma_08$acetic_acid <- NA 
Sharma_08$anaerobic <- NA 
Sharma_08$vacuum_packed <- NA 
Sharma_08$co_culture <- NA 
Sharma_08$shaken <- NA
Sharma_08$sterile <- NA
Sharma_08$nitrite <- NA 
Sharma_08$N2 <- NA
Sharma_08$CO2 <- NA
Sharma_08$O2 <- NA
Sharma_08$scorbic_acid <- NA
Sharma_08$propionoc_acid <- NA
Sharma_08$modified_atmosphere <- NA
Sharma_08$sugar <- NA
Sharma_08$pressure <- NA
Sharma_08$dried <- NA
Sharma_08$heated <- NA
Sharma_08$ethanol <- NA
Sharma_08$smoked <- NA
Sharma_08$citric_acid <- NA
Sharma_08$ascorbic_acid <- NA

Sharma_08 <- Sharma_08[,c("ID", "organism", "source", "b_f", "meas_method", 
                          "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                          "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                          "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                          "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                          "ascorbic_acid", "logc",
                          "spec_rate")]
df_clean <- rbind(df_clean, Sharma_08)



Sunen_01 <- subset(data, grepl("Sunen_01", source))
names(Sunen_01)[6] <- "smoked"

Sunen_01$NaCl <- NA  
Sunen_01$lactic_acid <- NA 
Sunen_01$acetic_acid <- NA 
Sunen_01$anaerobic <- NA 
Sunen_01$vacuum_packed <- NA 
Sunen_01$co_culture <- NA 
Sunen_01$shaken <- NA
Sunen_01$sterile <- NA
Sunen_01$nitrite <- NA 
Sunen_01$N2 <- NA
Sunen_01$CO2 <- NA
Sunen_01$O2 <- NA
Sunen_01$scorbic_acid <- NA
Sunen_01$propionoc_acid <- NA
Sunen_01$modified_atmosphere <- NA
Sunen_01$sugar <- NA
Sunen_01$pressure <- NA
Sunen_01$dried <- NA
Sunen_01$heated <- NA
Sunen_01$ethanol <- NA
Sunen_01$CIO2 <- NA
Sunen_01$citric_acid <- NA
Sunen_01$ascorbic_acid <- NA

Sunen_01 <- Sunen_01[,c("ID", "organism", "source", "b_f", "meas_method", 
                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                        "ascorbic_acid", "logc",
                        "spec_rate")]
df_clean <- rbind(df_clean, Sunen_01)




TIA_UTAS <- read.csv("TIA-UTAS.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
names(TIA_UTAS)[14] <- "co_culture" 
TIA_UTAS$X <- NULL 
names(TIA_UTAS)[1] <- "ID"

TIA_UTAS$acetic_acid <- NA 
TIA_UTAS$anaerobic <- NA 
TIA_UTAS$vacuum_packed <- NA 
TIA_UTAS$shaken <- NA
TIA_UTAS$sterile <- NA
TIA_UTAS$nitrite <- NA 
TIA_UTAS$N2 <- NA
TIA_UTAS$CO2 <- NA
TIA_UTAS$O2 <- NA
TIA_UTAS$scorbic_acid <- NA
TIA_UTAS$propionoc_acid <- NA
TIA_UTAS$modified_atmosphere <- NA
TIA_UTAS$sugar <- NA
TIA_UTAS$pressure <- NA
TIA_UTAS$dried <- NA
TIA_UTAS$heated <- NA
TIA_UTAS$ethanol <- NA
TIA_UTAS$CIO2 <- NA
TIA_UTAS$smoked <- NA
TIA_UTAS$citric_acid <- NA
TIA_UTAS$ascorbic_acid <- NA

TIA_UTAS <- TIA_UTAS[,c("ID", "organism", "source", "b_f", "meas_method", 
                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                        "ascorbic_acid", "logc",
                        "spec_rate")]
df_clean <- rbind(df_clean, TIA_UTAS)




UCM_NBTA <- subset(data, grepl("UCM-NBTA", source))
UCM_NBTA  <- cSplit(UCM_NBTA, "condition", sep="," ) 
names(UCM_NBTA)[11] <- "CO2"
names(UCM_NBTA)[12] <- "O2" 
names(UCM_NBTA)[13] <- "N2" 

UCM_NBTA$NaCl <- NA  
UCM_NBTA$lactic_acid <- NA 
UCM_NBTA$acetic_acid <- NA 
UCM_NBTA$anaerobic <- NA 
UCM_NBTA$vacuum_packed <- NA 
UCM_NBTA$co_culture <- NA 
UCM_NBTA$shaken <- NA
UCM_NBTA$sterile <- NA
UCM_NBTA$nitrite <- NA 
UCM_NBTA$scorbic_acid <- NA
UCM_NBTA$propionoc_acid <- NA
UCM_NBTA$modified_atmosphere <- NA
UCM_NBTA$sugar <- NA
UCM_NBTA$pressure <- NA
UCM_NBTA$dried <- NA
UCM_NBTA$heated <- NA
UCM_NBTA$ethanol <- NA
UCM_NBTA$CIO2 <- NA
UCM_NBTA$smoked <- NA
UCM_NBTA$citric_acid <- NA
UCM_NBTA$ascorbic_acid <- NA

UCM_NBTA <- UCM_NBTA[,c("ID", "organism", "source", "b_f", "meas_method", 
                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                        "ascorbic_acid", "logc",
                        "spec_rate")]
df_clean <- rbind(df_clean, UCM_NBTA)




UCO_DBTA <- subset(data, grepl("UCO-DBTA", source))
UCO_DBTA <- cSplit(UCO_DBTA, "condition", sep="," ) 
names(UCO_DBTA)[11] <- "citric_acid"
names(UCO_DBTA)[12] <- "ascorbic_acid" 

UCO_DBTA$NaCl <- NA  
UCO_DBTA$lactic_acid <- NA 
UCO_DBTA$acetic_acid <- NA 
UCO_DBTA$anaerobic <- NA 
UCO_DBTA$vacuum_packed <- NA 
UCO_DBTA$co_culture <- NA 
UCO_DBTA$shaken <- NA
UCO_DBTA$sterile <- NA
UCO_DBTA$nitrite <- NA 
UCO_DBTA$N2 <- NA
UCO_DBTA$CO2 <- NA
UCO_DBTA$O2 <- NA
UCO_DBTA$scorbic_acid <- NA
UCO_DBTA$propionoc_acid <- NA
UCO_DBTA$modified_atmosphere <- NA
UCO_DBTA$sugar <- NA
UCO_DBTA$pressure <- NA
UCO_DBTA$dried <- NA
UCO_DBTA$heated <- NA
UCO_DBTA$ethanol <- NA
UCO_DBTA$CIO2 <- NA
UCO_DBTA$smoked <- NA

UCO_DBTA <- UCO_DBTA[,c("ID", "organism", "source", "b_f", "meas_method", 
                        "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                        "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                        "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                        "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                        "ascorbic_acid", "logc",
                        "spec_rate")]
df_clean <- rbind(df_clean, UCO_DBTA)



UPCT <- subset(data, grepl("UPCT", source))
names(UPCT)[6] <- "NaCl" 


UPCT$lactic_acid <- NA 
UPCT$acetic_acid <- NA 
UPCT$anaerobic <- NA 
UPCT$vacuum_packed <- NA 
UPCT$co_culture <- NA 
UPCT$shaken <- NA
UPCT$sterile <- NA
UPCT$nitrite <- NA 
UPCT$N2 <- NA
UPCT$CO2 <- NA
UPCT$O2 <- NA
UPCT$scorbic_acid <- NA
UPCT$propionoc_acid <- NA
UPCT$modified_atmosphere <- NA
UPCT$sugar <- NA
UPCT$pressure <- NA
UPCT$dried <- NA
UPCT$heated <- NA
UPCT$ethanol <- NA
UPCT$CIO2 <- NA
UPCT$smoked <- NA
UPCT$citric_acid <- NA
UPCT$ascorbic_acid <- NA

UPCT <- UPCT[,c("ID", "organism", "source", "b_f", "meas_method", 
                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                "ascorbic_acid", "logc",
                "spec_rate")]
df_clean <- rbind(df_clean, UPCT)


Uyttendaele_04 <- subset(data, grepl("Uyttendaele_04", source))
names(Uyttendaele_04)[6] <- "NaCl" 

Uyttendaele_04$lactic_acid <- NA 
Uyttendaele_04$acetic_acid <- NA 
Uyttendaele_04$anaerobic <- NA 
Uyttendaele_04$vacuum_packed <- NA 
Uyttendaele_04$co_culture <- NA 
Uyttendaele_04$shaken <- NA
Uyttendaele_04$sterile <- NA
Uyttendaele_04$nitrite <- NA 
Uyttendaele_04$N2 <- NA
Uyttendaele_04$CO2 <- NA
Uyttendaele_04$O2 <- NA
Uyttendaele_04$scorbic_acid <- NA
Uyttendaele_04$propionoc_acid <- NA
Uyttendaele_04$modified_atmosphere <- NA
Uyttendaele_04$sugar <- NA
Uyttendaele_04$pressure <- NA
Uyttendaele_04$dried <- NA
Uyttendaele_04$heated <- NA
Uyttendaele_04$ethanol <- NA
Uyttendaele_04$CIO2 <- NA
Uyttendaele_04$smoked <- NA
Uyttendaele_04$citric_acid <- NA
Uyttendaele_04$ascorbic_acid <- NA

Uyttendaele_04 <- Uyttendaele_04[,c("ID", "organism", "source", "b_f", "meas_method", 
                                    "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                    "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                    "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                    "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                    "ascorbic_acid", "logc",
                                    "spec_rate")]
df_clean <- rbind(df_clean, Uyttendaele_04)


Vialette_03 <- subset(data, grepl("Vialette_03", source))
names(Vialette_03)[6] <- "NaCl"


Vialette_03$lactic_acid <- NA 
Vialette_03$acetic_acid <- NA 
Vialette_03$anaerobic <- NA 
Vialette_03$vacuum_packed <- NA 
Vialette_03$co_culture <- NA 
Vialette_03$shaken <- NA
Vialette_03$sterile <- NA
Vialette_03$nitrite <- NA 
Vialette_03$N2 <- NA
Vialette_03$CO2 <- NA
Vialette_03$O2 <- NA
Vialette_03$scorbic_acid <- NA
Vialette_03$propionoc_acid <- NA
Vialette_03$modified_atmosphere <- NA
Vialette_03$sugar <- NA
Vialette_03$pressure <- NA
Vialette_03$dried <- NA
Vialette_03$heated <- NA
Vialette_03$ethanol <- NA
Vialette_03$CIO2 <- NA
Vialette_03$smoked <- NA
Vialette_03$citric_acid <- NA
Vialette_03$ascorbic_acid <- NA

Vialette_03 <- Vialette_03[,c("ID", "organism", "source", "b_f", "meas_method", 
                              "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                              "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                              "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                              "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                              "ascorbic_acid", "logc",
                              "spec_rate")]
df_clean <- rbind(df_clean, Vialette_03)



Yamazaki_03a <- subset(data, grepl("Yamazaki_03a", source))
names(Yamazaki_03a)[6] <- "co_culture"

Yamazaki_03a$NaCl <- NA  
Yamazaki_03a$lactic_acid <- NA 
Yamazaki_03a$acetic_acid <- NA 
Yamazaki_03a$anaerobic <- NA 
Yamazaki_03a$vacuum_packed <- NA 
Yamazaki_03a$shaken <- NA
Yamazaki_03a$sterile <- NA
Yamazaki_03a$nitrite <- NA 
Yamazaki_03a$N2 <- NA
Yamazaki_03a$CO2 <- NA
Yamazaki_03a$O2 <- NA
Yamazaki_03a$scorbic_acid <- NA
Yamazaki_03a$propionoc_acid <- NA
Yamazaki_03a$modified_atmosphere <- NA
Yamazaki_03a$sugar <- NA
Yamazaki_03a$pressure <- NA
Yamazaki_03a$dried <- NA
Yamazaki_03a$heated <- NA
Yamazaki_03a$ethanol <- NA
Yamazaki_03a$CIO2 <- NA
Yamazaki_03a$smoked <- NA
Yamazaki_03a$citric_acid <- NA
Yamazaki_03a$ascorbic_acid <- NA

Yamazaki_03a <- Yamazaki_03a[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]
df_clean <- rbind(df_clean, Yamazaki_03a) 
#df_clean <- cSplit(df_clean, "logc", sep = ";")
#write.csv(df_clean, "df_clean_final_add1.csv") 
df <- read.csv("df_clean_final.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
View(df)


# now need to deal with the logc variable. This has to be separated and all renamed 
df <- cSplit(df, "logc", sep=";" ) 


names(df)[35] <- "time_1" 
names(df)[36] <- "count_1" 
names(df)[37] <- "time_2" 
names(df)[38] <- "count_2" 
names(df)[39] <- "time_3" 
names(df)[40] <- "count_3" 
names(df)[41] <- "time_4" 
names(df)[42] <- "count_4" 
names(df)[43] <- "time_5" 
names(df)[44] <- "count_5" 
names(df)[45] <- "time_6" 
names(df)[46] <- "count_6" 
names(df)[47] <- "time_7" 
names(df)[48] <- "count_7" 
names(df)[49] <- "time_8" 
names(df)[50] <- "count_8"
names(df)[51] <- "time_9" 
names(df)[52] <- "count_9" 
names(df)[53] <- "time_10" 
names(df)[54] <- "count_10" 
names(df)[55] <- "time_11" 
names(df)[56] <- "count_11" 
names(df)[57] <- "time_12" 
names(df)[58] <- "count_12"
names(df)[59] <- "time_13" 
names(df)[60] <- "count_13" 
names(df)[61] <- "time_14" 
names(df)[62] <- "count_14" 
names(df)[63] <- "time_15" 
names(df)[64] <- "count_15" 
names(df)[65] <- "time_16" 
names(df)[66] <- "count_16" 
names(df)[67] <- "time_17" 
names(df)[68] <- "count_17" 
names(df)[69] <- "time_18" 
names(df)[70] <- "count_18" 
names(df)[71] <- "time_19" 
names(df)[72] <- "count_19" 
names(df)[73] <- "time_20" 
names(df)[74] <- "count_20" 
names(df)[75] <- "time_21" 
names(df)[76] <- "count_21" 
names(df)[77] <- "time_22" 
names(df)[78] <- "count_22"
names(df)[79] <- "time_23" 
names(df)[80] <- "count_23" 
names(df)[81] <- "time_24" 
names(df)[82] <- "count_24" 
names(df)[83] <- "time_25" 
names(df)[84] <- "count_25" 
names(df)[85] <- "time_26" 
names(df)[86] <- "count_26"
names(df)[87] <- "time_27" 
names(df)[88] <- "count_27" 
names(df)[89] <- "time_28" 
names(df)[90] <- "count_28" 
names(df)[91] <- "time_29" 
names(df)[92] <- "count_29" 
names(df)[93] <- "time_30" 
names(df)[94] <- "count_30" 
names(df)[95] <- "time_31" 
names(df)[96] <- "count_31" 
names(df)[97] <- "time_32" 
names(df)[98] <- "count_32" 
names(df)[99] <- "time_33" 
names(df)[100] <- "count_33" 
names(df)[101] <- "time_34" 
names(df)[102] <- "count_34" 
names(df)[103] <- "time_35" 
names(df)[104] <- "count_35" 
names(df)[105] <- "time_36" 
names(df)[106] <- "count_36"
names(df)[107] <- "time_37" 
names(df)[108] <- "count_37" 
names(df)[109] <- "time_38" 
names(df)[110] <- "count_38" 
names(df)[111] <- "time_39" 
names(df)[112] <- "count_39" 
names(df)[113] <- "time_40" 
names(df)[114] <- "count_40"
names(df)[115] <- "time_41" 
names(df)[116] <- "count_41" 
names(df)[117] <- "time_42" 
names(df)[118] <- "count_42" 
names(df)[119] <- "time_43" 
names(df)[120] <- "count_43" 
names(df)[121] <- "time_44" 
names(df)[122] <- "count_44" 
names(df)[123] <- "time_45" 
names(df)[124] <- "count_45" 
names(df)[125] <- "time_46" 
names(df)[126] <- "count_46" 

#write.csv(df, "df_final.csv")
