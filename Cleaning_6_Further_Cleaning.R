#################### FURTHER CLEANING FOR MODELLING #####################################
library(stringr) 
library(plyr) 



#### DATA IS CLEAN NOW NEED TO FURTHER CLEAN BY GETTIN RID OF NA'S IN SPEC_RATE AND lOG COUNTS 

df_1 <- read.csv("FINAL_MAIN_df.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
df_1$X <- NULL
# need to get rid of NA'S in the spec rate as this is the fitted growth values for models  '

sum(is.na(df_1$spec_rate))
# 1582 NA'S in spec_rate these will all have to be deleted 
# remove all NA'S in  
sum(is.na(df_1$initial_time)) #1493 
sum(is.na(df_1$count_1)) # 1493 
sum(is.na(df_1$final_time)) #1494 
sum(is.na(df_1$final_logc)) #1494 
sum(is.na(df_1$no_count_exp)) # 1494 seem the cases with na's in initial time match those in count_1, final_time, finla_logc and np_count_exp

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

df_1 <- completeFun(df_1, c("spec_rate", "initial_time"))
# 2936 of obvs have complete cases 

#write.csv(df_1, "Complete_dataset.csv")




df <- read.csv("rf_df_model_data.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
df$X <- NULL 

df <- Filter(function(x) !all(is.na(x)), df) # function to get rid on all cols that only contain NA'S

# impute all nas to zero for random forest 
df[is.na(df)] <- 0

# we have imputed a tartet variable from sprc_rate for growth no growth respose variable therfroe we need to drop spec tae variable 
df$target <- factor ( with ( df, ifelse ( ( spec_rate <= 0 ), "No Growth" , "Growth" ) ) ) 
summary(df$target) 
# 1989 growth and 947 no growth
df$spec_rate <- NULL

# need to clean conditions 
# numeric conditions like nacl will have to be cleaned 
#  catergorical variabls  we want yes or no 


df$anaerobic <- revalue(df$anaerobic, c( "0" = "No"))
df$anaerobic <- revalue(df$anaerobic, c("anaerobic"="Yes"))
head(df$anaerobic)


df$co_culture <- revalue(df$co_culture, c("0" = "No")) 
df$co_culture <- revalue(df$co_culture, c("co-culture" = "Yes"))

summary(df$N2)
df$modified_atmosphere <- revalue(df$modified_atmosphere, c("0" = "No")) 
df$modified_atmosphere <- revalue(df$modified_atmosphere, c("Modified_Atmosphere" = "Yes"))



# now need to deal with numerica variable reg expression going to use stringr

df$NaCl <- str_remove_all(df$NaCl, "[NaCl(%):]") 
df$lactic_acid <- str_remove_all(df$lactic_acid, "[NaCl(%):lactic_acid(ppm):]")
df$acetic_acid <- str_remove_all(df$acetic_acid, "[acetic_acid(ppm):]") 
df$nitrite <- str_remove_all(df$nitrite, "[nNitrite(ppm):]") 
df$N2 <- str_remove_all(df$N2, "[N2(%):]")  
df$CO2 <- str_remove_all(df$CO2, "[CO2(%):]") 
df$O2 <- str_remove_all(df$O2, "[O2(%):]") 
df$scorbic_acid <- str_remove_all(df$scorbic_acid, "[sorbic_acid(ppm):]") 
df$pressure <- str_remove_all(df$pressure, "[pressure(Mpa):]") 
df$ethanol <- str_remove_all(df$ethanol, "[ethanol(%):]") 
df$citric_acid <- str_remove_all(df$citric_acid, "[citric_acid(ppm):]") 
df$O2 <- str_remove_all(df$O2, "[.]")  
sum(is.na(df$N2))
# change all character variables to numeric  
df$NaCl <- as.numeric(df$NaCl)
df$lactic_acid <- as.numeric(df$lactic_acid) 
df$acetic_acid <- as.numeric(df$acetic_acid) 
df$nitrite <- as.numeric(df$nitrite) 
df$N2 <- as.numeric(df$N2)  
df$CO2 <- as.numeric(df$CO2)
df$O2 <- as.numeric(df$O2)
df$scorbic_acid <- as.numeric(df$scorbic_acid)
df$pressure <- as.numeric(df$pressure) 
df$ethanol <- as.numeric(df$ethanol) 
df$citric_acid <- as.numeric(df$citric_acid)

 
# now goimg to subset data fro temp between 0-20 and get rid of high initiaml counts 

df <- subset(df, temp >= 0 & temp <= 20)  
df <- subset(df, count_1 <= 6)  # left with 1753 obs
#write.csv(df, "clean_model_data.csv", row.names = FALSE)
sum(is.na(df$N2))
df[is.na(df)] <- 0
