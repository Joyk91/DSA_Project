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
