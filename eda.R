library(tidyr)
library(tidyverse)
install.packages(splitstack)
library(splitstackshape)
library(dplyr)
library(reshape2)
library(ggplot2)
install.packages("growthcurver")
library(growthcurver)
library(purrr)
library(DataExplorer) 
library(naniar) 
install.packages("UpSetR")
library(UpSetR)


data = read.csv("FINAL_MAIN_df.csv", na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)
View(data)
summary(data) 
names(data)[1] <- "ID"


# ------------------------------------------------------------# 
## Instect Th data 
# -----------------------------------------------------------#

# Inspect the data 
names(data)
head(data)
dim(data) 
# Number of NAs 
sum(is.na(data))  
summary(traindata) 



#####################Exploratory Data Analysis #########################`
# just a quick look
plot_missing(data) 
plot_bar(data$meas_method)# as we can see we have reg expression problems will have to be fixed od and OD
create_report(data)
# credit standing very important  

########################################################################## 
# univar plots 
hist(data$temp)
ggplot(data, aes(x = pH))+ geom_histogram()
counts <- table(data$temp)
barplot(counts)

df <- data %>%
  group_by(temp) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = temp, y = counts)) +
  geom_bar(fill = "#0073C2FF") +
  geom_text(aes(label = counts), vjust = -0.3) 

boxplot(temp~pH,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon") 
boxplot(data$temp~data$pH, data = data)


### Initial Data Analysis  
# summary statistics 
summary(data$temp)

# univariate plots 

hist(data$temp, xlab = 'temperature', main = "Temperature Distribution")  

library(ggplot2)

# Change colors
temp<-ggplot(data, aes(x=temp)) + 
  geom_histogram(color="pink", fill="blue") +ggtitle("Temperature Distribution")

temp+ geom_vline(aes(xintercept=mean(temp)),
                 color="green", linetype="dashed", size=2)

sum(is.na(data$temp))



# summary statistics 
summary(data$pH)

# univariate plots 

hist(data$temp, xlab = 'temperature', main = "Temperature Distribution") 
library(ggplot2)
hist(data$pH)
boxplot(data$pH)
# Change colors
ph<-ggplot(data, aes(x=pH)) + 
  geom_histogram(color="blue", fill="pink") +ggtitle("pH Distribution")
ph
ph+ geom_vline(aes(xintercept=mean(pH)),
               color="green", linetype="dashed", size=2)

sum(is.na(data$pH))

neutral <- filter(data, pH==7)
View(neutral)
summary(neutral)

# univariate plots 

summary(data$aw)

# Change colors
aw<-ggplot(data, aes(x=aw)) + 
  geom_histogram(color="pink", fill="blue") +ggtitle("Water Activity (aw) Distribution")
ph
aw+ geom_vline(aes(xintercept=mean(aw)),
               color="green", linetype="dashed", size=2)

sum(is.na(data$aw))


######### Bivariate plotting 
boxplot(temp~pH*aw, data = data, col=(c("gold", "darkgreen", "pink")), main = "Boxplot of Temperature, AW & pH", xlab = "aw and pH")
boxplot(data$temp,col="gold", main= 'Temperature Boxplot')
boxplot(data$pH,col="darkgreen", main= 'pH Boxplot')
boxplot(data$aw,col="blue", main= 'aw Boxplot') 
boxplot(data$count_1, col= "pink", main = 'Initial Count')


par(mfrow = c(2,2))
plot(data$pH, data$temp) 



###################### MISSING DATA ##################################### 

vis_miss(data)
gg_miss_upset(data)
gg_miss_upset(riskfactors)
ggplot(data, aes(x= spec_rate, y = logc))+ geom_miss_point()
gg_miss_var(data, facet = source) 
gg_miss_var(data, facet = meas_method)

########################## Bivariate plotting 


ggplot(data, aes(x = initial_time, y = count_1)) + geom_point(color = "blue")+ labs(title="Scatterplot Initial_time vs Initial_count", x="Initial Time", y="count")
summary(data$spec_rate) 
hist(data$count_1, xlab = "Initial Count", main = "Histogram Initial Count") 

ggplot(data, aes(x = spec_rate, y = temp)) + geom_point(color = "red")+ labs(title="Scatterplot Temperature vs Spec_rate", x="Spec_rate", y="Temperature")
summary(data$spec_rate)  

ggplot(data, aes(x = spec_rate, y = final_logc)) + geom_point(color = "green")+ labs(title="Scatterplot Final Log Count vs Spec_rate", x="Spec_rate", y="final_logc")
summary(data$spec_rate) 

ggplot(data, aes(x = spec_rate, y = count_1)) + geom_point(color = "red")+ labs(title="Scatterplot Initial Log Count vs Spec_rate", x="Spec_rate", y="count_1")
summary(data$spec_rate)
 

# -------------------------------------------------------- # 
## Modelling Dataset 
# --------------------------------------------------------#

ggplot(data = df) +
  aes(x = temp, fill = target) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  facet_wrap(vars(target))




ggplot(data = df) +
  aes(x = pH, y = temp, fill = target, color = target) +
  geom_point() +
  labs(title = "Temperature vs Ph") +
  theme_minimal() +
  facet_wrap(vars(target)) 

ggplot(data = df) +
  aes(x = pH, y = temp, fill = target, color = target) +
  geom_point() +
  theme_minimal() 




ggplot(data = df) +
  aes(x = pH, y = NaCl, fill = target, color = target) +
  geom_point() +
  theme_minimal() +
  facet_wrap(vars(target)) 


ggplot(data = df) +
  aes(x = pH, y = NaCl, fill = target, color = target) +
  geom_point() + 
  labs(title = "pH vs NaCl") +
  theme_minimal() 

ggplot(data = df) +
  aes(x = temp, y = aw, color = target) +
  geom_point() +
  theme_minimal() 


ggplot(data = df) +
  aes(x = nitrite, y = temp, fill = target, color = target) +
  geom_point() +
  theme_minimal()


# ------------------------------------------------------# 
## Correlations 
# -----------------------------------------------------#

install.packages('corrplot')
library(corrplot)
corrplot(df)



cor_data <- df[, sapply(df, is.numeric)]
cor_df <- cor(cor_data)
cor_df
corrplot(cor_df)
