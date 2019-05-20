# -------------------------------------------------------------------------------------------------------# 
##  Modelling - Random Forest
## PACKAGE'S Required 
# -------------------------------------------------------------------------------------------------------# 
library(randomForest)
library(caret)
library(e1071) 
library(tidyr) 
library(dplyr)  
library(pROC) 
library(DMwR) 
library(ROSE)  
library(ROCR) 
library(purrr) 
library(ggplot2) 
library(plotly)


# ------------------------------------------------------------------------------------------------------# 
## Read in the Data  
# ------------------------------------------------------------------------------------------------------#

df <- read.csv('clean_model_data.csv', na.strings=c(""," ","NA"),header=TRUE, stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------------------------------# 
## Data Transformation for Modelling 
# -----------------------------------------------------------------------------------------------------# 

df <- df %>%  mutate_if(is.character, as.factor) # any character vector in our dataset we want to transform it to a factor for the random forest algorithim
df$target <- as.factor(df$target) # transform target to a factor 
summary(df$target) # imbalanced data class: Grwoth = 1508 observations, class: No Growth = 245 observation


df$no_count_exp <- NULL
df$final_logc <- NULL 
df$final_time <- NULL 
df$count_1 <- NULL  
df$initial_time <- NULL


# ----------------------------------------------------------------------------------------------------# 
## Train and Test Split 
# -----------------------------------------------------------------------------------------------------#

set.seed(161) # For reproducability set.seed will be used in all modelling tasks  
# split into train and test set 
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ] 

## Baseline rate 
table(df$target)/nrow(df) # baseline rate  Growth: 0.8602396, No Growth : 0.1397604 (note imbalanced dataset) 

# now we have train and test set and all our variabales are transformed to be compatible with random forrest we will 
# run a default random forrest. 
# this will include no cross-validation 


# PROBLEM WITH FACTOR LEVELS FOR PREBABILITIES THERFORE HAD TO CHANGE THEM 
levels(train$target) <- c("Growth", "NoGrowth")  
levels(test$target) <- c("Growth", "NoGrowth")  
table(train$target)  

# ------------------------------------------------------- # 
## Feature Selection RFE
# ----------------------------------------------------- #
levels(df$target) <- c("Growth", "NoGrowth")  
table(train$target) 
df$NaCl <- NULL

set.seed(161)
trControl <- trainControl(method="repeatedcv",
                          number = 10, 
                          repeats = 5,
                          search = "grid", 
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE) 

set.seed(161)
rf_varIm <- train(target~.,
                  data = df,
                  method = "ranger", 
                  metric = 'ROC', 
                  trControl = trControl,
                  importance = "impurity")

var_importance <- varImp(rf_varIm, scale = FALSE)
plot(var_importance, main = "Variable Importance Plot")

print(rf_orig)


# ----------------------------------------------------------------------------------------------------------# 
## Random Forest Modeling 
# ---------------------------------------------------------------------------------------------------------#

# ************************************************ # 
## Origional Model Utilising CV Strategy 
# *********************************************** # 

# basic cross-validation strategy 
set.seed(161)
trControl <- trainControl(method="repeatedcv",
                          number = 10, 
                          repeats = 5,
                          search = "grid", 
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE) 


set.seed(161)
rf_orig <- train(target~.,
               data = train,
               method = "ranger", 
               metric = 'ROC',
               trControl = trControl, 
               importance = "impurity")

var_importance <- varImp(rf_orig, scale = FALSE)
plot(var_importance, main = "Variable Importance Plot")

print(rf_orig)

plot(rf_orig, plotType = "scatter", auto.key = TRUE, main = "Original Fit Classifier Performance")
pred_orig_train <- predict(rf_orig, train)
pred_orig <- predict(rf_orig, test)

confusionMatrix(pred_orig, test$target) 


roc_orig <- roc(test$target,
           predict(rf_orig, test, type = "prob")[,1],
           levels = rev(levels(test$target)))

roc_orig$
## Now plot ROC at different thersholds 
plot(roc_orig, print.thres = c(.5), type = "S", 
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
     print.thres.cex = .8, col = 'gold',
     legacy.axes = TRUE, main = "ROC Curve Model 1 Test Set")


 
# ********************************************* #
## Model 2 Tunning 
# ********************************************* # 
set.seed(161)
tgrid <- expand.grid(
  .mtry = 2:15,
  .splitrule = "gini",
  .min.node.size = c(5,10,15, 20))

set.seed(161)
model_tune <- train(target  ~ .,
                     data = train,
                     method = "ranger",
                     trControl = trControl, 
                     metric = "ROC",
                     tuneGrid = tgrid,
                     num.trees = 100, 
                     importance = "impurity")

print(model_tune) 
plot(model_tune, main = "Model 2 Classifier Performance") 

pred_tune <-predict(model_tune, test) 

#You can use the prediction to compute the confusion matrix and see the accuracy score  
confusionMatrix(pred_tune, test$target) 

results <- resamples(list(Model_1=rf_orig, Model_2=model_tune))



# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales, main = "Model Comparison", col = 'red') 

roc_tune <- roc(test$target,
           predict(model_tune, test, type = "prob")[,1],
           levels = rev(levels(test$target)))



## Now plot ROC at different thersholds 
plot(roc_tune, print.thres = c(.5), type = "S", 
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
     print.thres.cex = .8, col = 'red',
     legacy.axes = TRUE, main = "ROC Curve Model 2 Test Set")



# *********************************************** # 
## Model 3 RFE Subset 
# ********************************************** # 
rfe_train <- as.data.frame(train[,c('pH', 'temp', 'nitrite', 'acetic_acid', 'lactic_acid', 'NaCl', 'N2', 'aw', 'anaerobic', 'pressure', 'target')])
rfe_test <- as.data.frame(test[,c('pH', 'temp', 'nitrite', 'acetic_acid', 'lactic_acid', 'NaCl', 'N2', 'aw', 'anaerobic', 'pressure', 'target')])

set.seed(161)
tgrid_rfe <- expand.grid(
  .mtry = 1:8,
  .splitrule = "gini",
  .min.node.size = c(0.1, 1, 5,10,15, 20))


set.seed(161)
model_rfe <- train(target  ~ .,
                    data = rfe_train,
                    method = "ranger",
                    trControl = trControl, 
                    metric = "ROC",
                    tuneGrid = tgrid_rfe,
                    num.trees = 100, 
                    importance = "impurity")

print(model_rfe) 
plot(model_rfe, main = "Model 3 Classifier Performance") 

pred_rfe <-predict(model_rfe, rfe_test) 

#You can use the prediction to compute the confusion matrix and see the accuracy score  
confusionMatrix(pred_rfe, rfe_test$target) 

results <- resamples(list(Model_1=rf_orig, Model_2=model_tune, Model_3=model_rfe))
boxplot(results, scales=scales)


# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales, main = "Model Comparison", col = 'red') 


roc_rfe <- roc(test$target,
                predict(model_rfe, test, type = "prob")[,1],
                levels = rev(levels(test$target)))


## Now plot ROC at different thersholds 
plot(roc_rfe, print.thres = c(.5), type = "S", 
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
     print.thres.cex = .8, col = 'green',
     legacy.axes = TRUE, main = "ROC Curve Model 3 Test Set") 




model_list_test <- list(original = a,
                        weighted = b,
                        down =c,
                        up = d,
                        SMOTE = e) 

model_list1 <- list(original = rf_orig,
                   Model_2=model_tune,
                   Model_3=model_rfe)


results <- resamples(list(Model_1=rf_orig, Model_2=model_tune, Model_3=model_rfe))

test_roc <- function(model, data) {
  
  roc(data$target,
      predict(model, data, type = "prob")[, "NoGrowth"])
  
}

rf_orig %>%
  test_roc(data = test) %>%
  auc()


model_list_roc1 <- model_list1 %>%
  map(test_roc, data = test)

model_list_roc1 %>%
  map(auc)


results_list_roc1 <- list(NA)
num_mod1 <- 1

for(the_roc in model_list_roc1){
  
  results_list_roc1[[num_mod1]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list1)[num_mod1])
  
  num_mod1 <- num_mod1 + 1
  
}

results_df_roc1 <- bind_rows(results_list_roc1)

# Plot ROC curve for all 5 models

custom_col1 <- c("#000000", "#009E73", "#0072B2")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc1) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col1) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)+ggtitle("Chart of Performance Metrics Test Data") 









# ************************************************* # 
## Model 4 Cost Sensitive Learning 
# ************************************************* # 

set.seed(161)
model_weights <- ifelse(train$target == "Growth",
                        (1/table(train$target)[1]) * 0.87,
                        (1/table(train$target)[2]) * 0.13) 

model_weights <- ifelse(train$target == "NoGrowth",6, 1)

1/table(train$target)[1] * 0.87
1/table(train$target)[2] * 0.13
model_weights

set.seed(161)
model_weighted <- train(target~.,
                 data = train,
                 method = "ranger", 
                 metric = 'ROC',
                 trControl = trControl, 
                 class.weights = c('Growth' = 1, 'NoGrowth' = 500),  
                 importance = "impurity") 
class.weights
c(1,10,10) * 50

print(model_weighted)

plot(model_weighted, main = "Model 4 Classifier Performance") 

pred_weighted <-predict(model_weighted, test) 
#You can use the prediction to compute the confusion matrix and see the accuracy score  
confusionMatrix(pred_weighted, test$target) 

results <- resamples(list(Model_1=rf_orig, Model_2=model_tune, Model_3=model_rfe, Model_4=model_weighted))
boxplot(results, scales=scales)


# box and whisker plots to compare models
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales, main = "Model Comparison", col = 'red') 


roc_weighted <- roc(test$target,
                predict(model_weighted, test, type = "prob")[,1],
                levels = rev(levels(test$target)))


## Now plot ROC at different thersholds 
plot(roc_weighted, print.thres = c(.5), type = "S", 
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
     print.thres.cex = .8, col = 'blue',
     legacy.axes = TRUE, main = "ROC Curve Model 4 Test Set")





# ----------------------------------------------------------------------------------------------------------# 
## Class Imbalance 
# ---------------------------------------------------------------------------------------------------------# 
# now we will investigate the class imbalance, even though the model already distinguises between each class 
# extremley well (highly sprcific and sensitive). we want the model to be highly adaptable for industry porofessionals 
# to be able to adjust for sprcific reasons. 

# we will use the best model 
set.seed(161)
rf_orig <- train(target~.,
                 data = train,
                 method = "ranger", 
                 metric = 'ROC',
                 trControl = trControl, 
                 importance = "impurity")

print(orig_fit) # ROC was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 11. 
##   mtry  ROC        Sens       Spec 
##    11  0.9989712  0.9912048  0.9722222
orig_pred <- predict(rf_orig, test)
#plsProbs <- predict(plsFit, newdata = testing, type = "prob")
#head(orig_pred)
confusionMatrix(orig_pred, test$target)
# Create model weights (they sum to one)  
orig <- confusionMatrix(orig_pred, test$target)  





roc_orig <- roc(test$target,
                predict(rf_orig, test, type = "prob")[,1],
                levels = rev(levels(test$target)))


## Now plot ROC at different thersholds 
plot(roc_orig, print.thres = c(.5), type = "S", 
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
     print.thres.cex = .8, col = 'gold',
     legacy.axes = TRUE, main = "ROC Curve Model 1 Test Set")




down_train <- downSample(x = train[, -ncol(train)],
                         y = train$target)

set.seed(161)
trControl$sampling <- "down"


set.seed(161)
down_fit <- train(target~.,
                  data = train,
                  method = "ranger", 
                  metric = 'ROC',
                  trControl = trControl, 
                  importance = "impurity")
print(down_fit) 


pred_down <-predict(down_fit, test)
#You can use the prediction to compute the confusion matrix and see the accuracy score  
confusionMatrix(pred_down, test$target)  # there is a differb=nce between original fit and down sampling 
# the problem we have is we dont want growth labelled as no growth. This has insured that zero are labelled as no
# growth when there is in fact growth. However that mean that more type 2 errors of no growth labelled as growth 
# this mean uneseary food disposal
down <- confusionMatrix(pred_down, test$target)

set.seed(161)
trControl$sampling <- "up" 

set.seed(161)
up_fit <- train(target~.,
                data = train,
                method = "ranger", 
                metric = 'ROC',
                trControl = trControl, 
                importance = "impurity")

print(up_fit) 


pred_up <-predict(up_fit, test)
#You can use the prediction to compute the confusion matrix and see the accuracy score  
confusionMatrix(pred_up, test$target) 
# this method up sampling has again addressed the problem of growth being labeled as no growth and there is less 
# type 2 errors of no growth being labeled as growth and therfore less food disposal.  
# seems to be a better stategy than down sampling 
up <- confusionMatrix(pred_up, test$target)  



set.seed(161)
smote_train <- SMOTE(target ~ ., data  = train)                         
table(smote_train$target)

trControl$sampling <- "smote"
set.seed(161)
smote_fit <- train(target ~ .,
                   data = train,
                   method = "rf", 
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = trControl)

print(smote_fit) 


pred_smote <-predict(smote_fit, test)
#You can use the prediction to compute the confusion matrix and see the accuracy score  
confusionMatrix(pred_smote, test$target)  
# same confusion matrix as up sampling 
smote <- confusionMatrix(pred_smote, test$target)
# Examine results for test set

model_list <- list(original = rf_orig,
                   #weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

print(model_list)
model_list_test <- list(original = a,
                        weighted = b,
                        down =c,
                        up = d,
                        SMOTE = e)
print(model_list_test) 



# Build custom AUC function to extract AUC
# from the caret model object


test_roc <- function(model, data) {
  
  roc(data$target,
      predict(model, data, type = "prob")[, "NoGrowth"])
  
}

rf_orig %>%
  test_roc(data = test) %>%
  auc()


model_list_roc <- model_list %>%
  map(test_roc, data = test)

model_list_roc %>%
  map(auc)


results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00") #, "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)+ggtitle("Chart of Performance Metrics (Re-Sampling Methods)") 


roc_orig <- roc(test$target,
                predict(rf_orig, test, type = "prob")[,1],
                levels = rev(levels(test$target)))

roc_orig$
  ## Now plot ROC at different thersholds 
  plot(roc_orig, print.thres = c(.5), type = "S", 
       print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
       print.thres.cex = .8, col = 'gold',
       legacy.axes = TRUE, main = "ROC Curve Model 1 Test Set")



matplot(data.frame(roc_orig$sensitivities, roc_orig$specificities), x = roc_orig$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR', main = "Exploring Threshold Value",)
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

################################################################### 
## change with ROC 

plot(roc_orig, print.thres = c(.25), type = "S", 
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
     print.thres.cex = .8, col = 'red',
     legacy.axes = TRUE, main = "ROC Curve Adjusted Threshold ")


plot(roc_orig, print.thres = c(.50), type = "S", 
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)", 
     print.thres.cex = .8, col = 'green', 
     legacy.axes = TRUE, main = "ROC Curve Default Threshold")

#----------------------------------------------------------# 
### prob thresholds train and test   
# ---------------------------------------------------------#

pred_default <- predict(rf_orig, train)
confusionMatrix(pred_default, train$target) 


probs_t <- predict(rf_orig, train, type = "prob")
threshold_t <- 0.25
pred_t      <- factor( ifelse(probs_t[, "Growth"] > threshold_t, "Growth", "NoGrowth") )
pred_t      <- relevel(pred_t, "Growth")   # you may or may not need this; I did
confusionMatrix(pred_t, train$target)



confusionMatrix(table(predict(rf_orig, type="prob")[ ,"Growth"] >= 0.50, train$target == "Growth"))


pred_dtest <- predict(rf_orig, test) 
confusionMatrix(pred_dtest, test$target) 

probsTest <- predict(rf_orig, test, type = "prob")
threshold <- 0.25
pred      <- factor( ifelse(probsTest[, "Growth"] > threshold, "Growth", "NoGrowth") )
pred      <- relevel(pred, "Growth")   # you may or may not need this; I did
confusionMatrix(pred, test$target)


probsTrain <- predict(rf_orig, test, type = "prob")
rocCurve   <- roc(response = test$target, 
                  print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
                  predictor = probsTrain[, "Growth"],
                  levels = rev(levels(test$target)))
plot(rocCurve, print.thres = "best", main = "Best Probability Threshold Test Data")

# extract predictions 
boundary <- cbind(test, probsTest) 

boundary$Growth <- factor ( with ( boundary, ifelse ( ( Growth <= 0.5), "NoGrowth" , "Growth" ) ) )  


# EDA on Boundary Models 
ggplot(data = df_pred) +
  aes(x = temp, y = aw, color = Growth) +
  geom_point(size = 3) +
  labs(title = "Temperature vs aw") +
  theme_minimal()


ggplot(data = df_pred) +
  aes(x = temp, y = NaCl, color = Growth) +
  geom_point(size = 3) +
  labs(title = "Temperature vs NaCl") +
  theme_minimal()



ggplot(data = df_pred) +
  aes(x = temp, y = pH, fill = Growth, color = Growth) +
  geom_point(size = 3) + 
  labs(title = "Temperature vs pH") +
  theme_minimal() +
  theme()

ggplot(data = df_pred) +
  aes(x = temp, y = pH, color = Growth) +
  geom_point(size = 3) +
  theme_minimal() + 
  labs(title = "Temperature vs Ph") +
  facet_wrap(vars(Growth))


ggplot(data = df_pred) +
  aes(x = temp, y = NaCl, color = Growth) +
  geom_point(size = 3) + 
  labs(title = "Temperature vs NaCl") +
  theme_minimal() +
  facet_wrap(vars(Growth))


ggplot(data = df_pred) +
  aes(x = temp, y = nitrite, color = Growth) +
  geom_point(size = 3) + 
  labs(title = "Temperature vs Nitrite") +
  theme_minimal() +
  facet_wrap(vars(Growth))


## 3D Boundary Plot 
df_pred <- as.data.frame(boundary[,c('pH','temp', 'NaCl', 'nitrite', 'target', 'Growth', 'aw' )]) 



p <- plot_ly(df_pred, x = ~temp, y = ~pH, z = ~NaCl, color = ~Growth, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Temperature'),
                      yaxis = list(title = 'pH'),
                      zaxis = list(title = 'NaCl'))) %>%
  layout(title = 'Boundary of L. monocytogenes as a 
         Function of Temperature, pH and NaCl')

p



