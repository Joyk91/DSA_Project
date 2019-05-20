# rejoin both the datasset that ontained condition data with the dataset that has subsets with no condition variabel 

data_cond <- read.csv("df1count clean.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
data_noCond <- read.csv("df_noCond.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)

# my two dataframes 
# have to rejoin them 

# get rid of unwanted cols in both  
data_cond$X <- NULL 
data_cond$X.1 <- NULL  
data_cond$time_2 <- NULL
data_cond$count_2 <- NULL
data_cond$time_3 <- NULL
data_cond$count_3 <- NULL
data_cond$time_4 <-NULL 
data_cond$count_4 <- NULL
data_cond$time_5 <-NULL
data_cond$count_5 <-NULL
data_cond$time_6 <- NULL
data_cond$count_6 <- NULL
data_cond$time_7 <- NULL
data_cond$count_7 <- NULL
data_cond$time_8 <-NULL
data_cond$count_8 <- NULL
data_cond$time_9<- NULL
data_cond$count_9 <- NULL
data_cond$time_10 <- NULL
data_cond$count_10 <- NULL
data_cond$time_11 <-NULL
data_cond$count_11 <-NULL 
data_cond$time_12 <- NULL
data_cond$count_12 <-NULL
data_cond$time_13 <-NULL 
data_cond$count_13 <-NULL
data_cond$time_14 <-NULL
data_cond$count_14 <-NULL 
data_cond$time_15 <- NULL
data_cond$count_15 <- NULL
data_cond$time_16 <- NULL
data_cond$count_16 <- NULL
data_cond$time_17<- NULL
data_cond$count_17 <- NULL
data_cond$time_18 <-NULL
data_cond$count_18 <- NULL
data_cond$time_19 <- NULL
data_cond$count_19<-NULL
data_cond$time_20 <- NULL
data_cond$count_20 <- NULL
data_cond$time_21 <-NULL
data_cond$count_21 <- NULL
data_cond$time_22 <- NULL 
data_cond$count_22 <-NULL
data_cond$time_23 <- NULL 
data_cond$count_23 <- NULL 
data_cond$time_24 <- NULL
data_cond$count_24 <-NULL
data_cond$time_25 <-NULL
data_cond$count_25 <-NULL
data_cond$time_26 <-NULL
data_cond$count_26 <- NULL
data_cond$time_27<- NULL
data_cond$count_27<- NULL
data_cond$time_28 <- NULL
data_cond$count_28 <-NULL
data_cond$time_29 <-NULL
data_cond$count_29 <-NULL
data_cond$time_30 <-NULL
data_cond$count_30 <-NULL
data_cond$time_31 <- NULL
data_cond$count_31 <- NULL
data_cond$time_32 <-NULL
data_cond$count_32 <- NULL  
data_cond$time_33 <-NULL
data_cond$count_33<-NULL
data_cond$time_34 <- NULL
data_cond$count_34 <- NULL
data_cond$time_35 <- NULL
data_cond$count_35<-NULL
data_cond$time_36 <-NULL
data_cond$count_36 <- NULL
data_cond$time_37 <- NULL
data_cond$count_37 <-NULL
data_cond$time_38 <- NULL
data_cond$count_38 <-NULL
data_cond$time_39 <- NULL
data_cond$count_39 <- NULL
data_cond$time_40 <- NULL
data_cond$count_40 <- NULL
data_cond$time_41 <- NULL
data_cond$count_41 <-NULL
data_cond$time_42<- NULL
data_cond$count_42 <-NULL
data_cond$time_43<- NULL
data_cond$count_43 <-NULL
data_cond$time_44<- NULL
data_cond$count_44 <- NULL
data_cond$time_45<- NULL
data_cond$count_45 <- NULL
data_cond$time_46<-NULL
data_cond$count_46 <- NULL
names(data_cond)[34] <- "initial_time"





data_noCond$X <- NULL 
data_noCond$X.1 <- NULL
data_noCond$logc_03 <-NULL
data_noCond$logc_04 <- NULL
data_noCond$logc_05 <- NULL
data_noCond$logc_06 <-NULL 
data_noCond$logc_07<- NULL
data_noCond$logc_08 <- NULL
data_noCond$logc_09 <- NULL
data_noCond$logc_10 <- NULL
data_noCond$logc_11 <- NULL
data_noCond$logc_12 <-NULL
data_noCond$logc_13 <-NULL
data_noCond$logc_14 <- NULL
data_noCond$logc_15 <- NULL
data_noCond$logc_16<- NULL
data_noCond$logc_17 <- NULL
data_noCond$logc_18 <-NULL
data_noCond$logc_19 <- NULL
data_noCond$logc_20 <-NULL 
data_noCond$logc_21 <-NULL 
data_noCond$logc_22 <- NULL 
data_noCond$logc_23 <-NULL
data_noCond$logc_24 <-NULL 
data_noCond$logc_25 <- NULL
data_noCond$logc_26 <-NULL
data_noCond$logc_27 <- NULL 
data_noCond$logc_28 <- NULL
data_noCond$logc_29 <- NULL
data_noCond$logc_30 <-NULL
data_noCond$logc_31 <- NULL
data_noCond$logc_32<- NULL
data_noCond$logc_33 <- NULL
data_noCond$logc_34 <-NULL
data_noCond$logc_35 <- NULL
data_noCond$logc_36 <-NULL
data_noCond$logc_37<- NULL
data_noCond$logc_38 <- NULL
data_noCond$logc_39 <- NULL
data_noCond$logc_40 <-NULL
data_noCond$logc_41 <- NULL
data_noCond$logc_42 <-NULL 
data_noCond$logc_43 <- NULL
data_noCond$logc_44 <- NULL
data_noCond$logc_45 <- NULL 
data_noCond$logc_46 <-NULL
data_noCond$logc_47 <- NULL
data_noCond$logc_48 <- NULL
data_noCond$logc_49 <- NULL
data_noCond$logc_50 <- NULL
data_noCond$logc_51 <- NULL
data_noCond$logc_52<- NULL
data_noCond$logc_53<-NULL
data_noCond$logc_54 <- NULL
data_noCond$logc_55 <- NULL
data_noCond$logc_56 <- NULL
names(data_noCond)[11] <- "initial_time" 
data_noCond$condition <- NULL

# add cols for conditions to no condition data 
data_noCond$NaCl <- NA  
data_noCond$lactic_acid <- NA 
data_noCond$acetic_acid <- NA 
data_noCond$anaerobic <- NA 
data_noCond$vacuum_packed <- NA 
data_noCond$co_culture <- NA 
data_noCond$shaken <- NA
data_noCond$sterile <- NA
data_noCond$nitrite <- NA 
data_noCond$N2 <- NA
data_noCond$CO2 <- NA
data_noCond$O2 <- NA
data_noCond$scorbic_acid <- NA
data_noCond$propionoc_acid <- NA
data_noCond$modified_atmosphere <- NA
data_noCond$sugar <- NA
data_noCond$pressure <- NA
data_noCond$dried <- NA
data_noCond$heated <- NA
data_noCond$ethanol <- NA
data_noCond$CIO2 <- NA
data_noCond$smoked <- NA
data_noCond$citric_acid <- NA
data_noCond$ascorbic_acid <- NA  
names(data_noCond)[11] <- "count_1"

df <- rbind(data_cond, data_noCond)

#write.csv(df, "Final_df.csv") 

# notice that the the final dataframe after cleaning contains 5966 observations and the original dataset 
# contained 6011 observations. Indicating that after cleaning and rejoining a total of 45 missing 
# observations have been lost. Need to find a way to uncover these 45 missing obs as do not want a biased   dataset  

# going to use compare package. Need unique variable in each dataset to compare so going to use ID  in each  

install.packages("compare") 
install.packages("compareDF")
library(compareDF)
library(compare)

compare_original <-data.frame(data$ID) 
names(compare_original)[1] <- "ID"
compare_cleaned <- data.frame(df$ID) 
names(compare_cleaned)[1] <- "ID"

comparison <- compare(compare_original,compare_cleaned,allowAll=TRUE)
comparison$tM 

install.packages("daff")
library(daff)

render_diff(diff_data(data_ref = compare_original,
          data = compare_cleaned))
find <- subset(data, grepl("ElShenawy_92", source)) # missing 42
find_i <- subset(df, grepl("ElShenawy_92", source)) 
a <- subset(data, grepl("SCS_1606", ID)) # looking for source miller_09 
a <- subset(data, grepl("Miller_09", source)) # missing from dataset 
b <- subset(df, grepl("Miller_09", source)) 


c <- subset(data, grepl("Utd_025", ID)) # looking for source miller_09 
d <- subset(data, grepl("Johnston_03", source)) # missing from dataset 
e <- subset(df, grepl("Johnston_03", source))
# so it seems I am missing 42 values from source Elshenawy_92 and 27 values from miller_09 
# and have double of Johnston_03, should only have 24 obs but have 48 
# with these fixed will have my missing 45 obvs  
# since the logc times and counts were sorted in excel will ovep this fibnal dataframe delete repeated Johnston_03 
# and then add two missing subsets to the end of the dataframe so as not to lose all the counts  


ElShenawy_92 <- subset(data, grepl("ElShenawy_92", source))
names(ElShenawy_92)[6] <- "propionoc_acid"

ElShenawy_92$NaCl <- NA  
ElShenawy_92$lactic_acid <- NA 
ElShenawy_92$acetic_acid <- NA 
ElShenawy_92$anaerobic <- NA 
ElShenawy_92$vacuum_packed <- NA 
ElShenawy_92$co_culture <- NA 
ElShenawy_92$shaken <- NA
ElShenawy_92$sterile <- NA
ElShenawy_92$nitrite <- NA 
ElShenawy_92$N2 <- NA
ElShenawy_92$CO2 <- NA
ElShenawy_92$O2 <- NA
ElShenawy_92$scorbic_acid <- NA
ElShenawy_92$modified_atmosphere <- NA
ElShenawy_92$sugar <- NA
ElShenawy_92$pressure <- NA
ElShenawy_92$dried <- NA
ElShenawy_92$heated <- NA
ElShenawy_92$ethanol <- NA
ElShenawy_92$CIO2 <- NA
ElShenawy_92$smoked <- NA
ElShenawy_92$citric_acid <- NA
ElShenawy_92$ascorbic_acid <- NA

ElShenawy_92 <- ElShenawy_92[,c("ID", "organism", "source", "b_f", "meas_method", 
                                "temp", "pH", "aw", "NaCl", "lactic_acid", "acetic_acid", "anaerobic", "vacuum_packed", "co_culture", 
                                "shaken", "sterile", "nitrite", "N2", "CO2", "O2", "scorbic_acid", 
                                "propionoc_acid", "modified_atmosphere", "sugar", "pressure", 
                                "dried", "heated", "ethanol", "CIO2", "smoked", "citric_acid", 
                                "ascorbic_acid", "logc",
                                "spec_rate")]
#ElShenawy_92 <- cSplit(ElShenawy_92, "logc", sep = ";") no log counts 
write.csv(ElShenawy_92, "ElShenawy_92.csv")
# this one can just be added on as there are no log counts  
# read in cleaned dataframe 
df <- read.csv("Main_add_miss.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE) 
# read on fixed data for ElShenawy_92
ElShenawy_92 <- read.csv("ElShenawy_92.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
ElShenawy_92$X <- NULL # get rid of unwanted cols 
ElShenawy_92$initial_time <- NA
ElShenawy_92$count_1 <- NA
df <- rbind(df, ElShenawy_92)


Miller_09 <- subset(data, grepl("Miller_09", source)) 
Miller_09 <- cSplit(Miller_09, "logc", sep = ";")
#write.csv(Miller_09, "Miller_09.csv")
Miller_09 <- read.csv("Miller_09.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
# read in fixed data for Miller_09
#delete unwamted cols 
Miller_09$X <- NULL 
Miller_09$X.1 <- NULL
Miller_09$logc_03 <-NULL
Miller_09$logc_04 <- NULL
Miller_09$logc_05 <- NULL
Miller_09$logc_06 <-NULL 
Miller_09$logc_07<- NULL
Miller_09$logc_08 <- NULL
Miller_09$logc_09 <- NULL
Miller_09$logc_10 <- NULL
Miller_09$logc_11 <- NULL
Miller_09$logc_12 <-NULL
Miller_09$logc_13 <-NULL
Miller_09$logc_14 <- NULL
Miller_09$logc_15 <- NULL
Miller_09$logc_16<- NULL
Miller_09$logc_17 <- NULL
Miller_09$logc_18 <-NULL
Miller_09$logc_19 <- NULL
Miller_09$logc_20 <-NULL 
Miller_09$logc_21 <-NULL 
Miller_09$logc_22 <- NULL 
Miller_09$logc_23 <-NULL
Miller_09$logc_24 <-NULL 
Miller_09$logc_25 <- NULL
Miller_09$logc_26 <-NULL
Miller_09$logc_27 <- NULL 
Miller_09$logc_28 <- NULL
Miller_09$logc_29 <- NULL
Miller_09$logc_30 <-NULL
Miller_09$logc_31 <- NULL
Miller_09$logc_32<- NULL
Miller_09$logc_33 <- NULL
Miller_09$logc_34 <-NULL
Miller_09$logc_35 <- NULL
Miller_09$logc_36 <-NULL
Miller_09$logc_37<- NULL
Miller_09$logc_38 <- NULL
Miller_09$logc_39 <- NULL
Miller_09$logc_40 <-NULL
Miller_09$logc_41 <- NULL
Miller_09$logc_42 <-NULL 
Miller_09$logc_43 <- NULL
Miller_09$logc_44 <- NULL
Miller_09$logc_45 <- NULL 
Miller_09$logc_46 <-NULL
Miller_09$logc_47 <- NULL
Miller_09$logc_48 <- NULL
Miller_09$logc_49 <- NULL
Miller_09$logc_50 <- NULL
Miller_09$logc_51 <- NULL
Miller_09$logc_52<- NULL
Miller_09$logc_53<-NULL
Miller_09$logc_54 <- NULL
Miller_09$logc_55 <- NULL
Miller_09$logc_56 <- NULL 
Miller_09$logc_57 <-NULL
Miller_09$logc_58 <- NULL
Miller_09$logc_59 <- NULL
Miller_09$logc_60 <- NULL
Miller_09$logc_61 <- NULL
Miller_09$logc_62 <- NULL
Miller_09$logc_63<- NULL
Miller_09$logc_64<-NULL

names(Miller_09)[11] <- "initial_time"  # rename cols 
names(Miller_09)[12] <- "count_1"   
Miller_09$condition <- NULL

# add in cols to match dataframe for merging
Miller_09$NaCl <- NA  
Miller_09$lactic_acid <- NA 
Miller_09$acetic_acid <- NA 
Miller_09$anaerobic <- NA 
Miller_09$vacuum_packed <- NA 
Miller_09$co_culture <- NA 
Miller_09$shaken <- NA
Miller_09$sterile <- NA
Miller_09$nitrite <- NA 
Miller_09$N2 <- NA
Miller_09$CO2 <- NA
Miller_09$O2 <- NA
Miller_09$scorbic_acid <- NA
Miller_09$propionoc_acid <- NA
Miller_09$modified_atmosphere <- NA
Miller_09$sugar <- NA
Miller_09$pressure <- NA
Miller_09$dried <- NA
Miller_09$heated <- NA
Miller_09$ethanol <- NA
Miller_09$CIO2 <- NA
Miller_09$smoked <- NA
Miller_09$citric_acid <- NA
Miller_09$ascorbic_acid <- NA


df <- rbind(df, Miller_09)
# now have all original 6011 obvs from first dataframe  
#write.csv(df, "FINAL_MAIN_df.csv")
