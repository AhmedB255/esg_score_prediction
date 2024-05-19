library(readxl)
library(randomForest)
library(caret)
library(iml)


setwd("C:/Users/Michael/Desktop/Msc FINTECH STRATHCLYDE/SEMESTER 2/AG 947 SUSTAINABLE FINANCE/WORKSHOP/ASSIGNMENT SUSTAINABLE")

training_data_orig <- read_excel("Corporate ESG data - 2012-2023 - filtered - E-S-G.xlsx", sheet = "ESG Sample - Training")
testing_data_orig <- read_excel("Corporate ESG data - 2012-2023 - filtered - E-S-G.xlsx", sheet = "ESG Sample - Testing")

#specific columns for analysis
training_data <- subset(training_data_orig, select = c('HighE75perc','TOTALASSETS','TOTALDEBTCOMMONEQUITY','EARNINGSPERSHARE','RETURNONEQUITYTOTAL','RETURNONASSETS','NETSALESORREVENUES'))
testing_data <- subset(testing_data_orig, select = c('HighE75perc','TOTALASSETS','TOTALDEBTCOMMONEQUITY','EARNINGSPERSHARE','RETURNONEQUITYTOTAL','RETURNONASSETS','NETSALESORREVENUES'))

# Encoding the target features from the training and testing data
training_data$HighE75perc <- factor(training_data$HighE75perc)
testing_data$HighE75perc <- factor(testing_data$HighE75perc)

# Dealing with missing values
testing_data[,-1] <- lapply(testing_data[,-1], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Training the random forest model
rf_model_train <- randomForest(HighE75perc ~ TOTALASSETS + TOTALDEBTCOMMONEQUITY + EARNINGSPERSHARE + RETURNONEQUITYTOTAL + RETURNONASSETS + NETSALESORREVENUES, data = training_data)

predicted_train <- predict(rf_model_train, training_data) 

cm_train <- confusionMatrix(predicted_train, training_data$HighE75perc)

predicted_test <- predict(rf_model_train, newdata = testing_data, type = "response")
cm_test <- confusionMatrix(predicted_test, testing_data$HighE75perc)
print(cm_train)
print(cm_test)


mod <- Predictor$new(rf_model_train, data = training_data, y = training_data$HighE75perc)
# creating predictor object

#1. Feature importance - model level
imp <- FeatureImp$new(mod, loss = "ce", compare = "difference")
plot(imp)

#2. Feature effects - model level
eff_1 <- FeatureEffect$new(mod, feature = "TOTALASSETS", method = 'ale')
plot(eff_1)

eff_2 <- FeatureEffect$new(mod, feature = "TOTALDEBTCOMMONEQUITY", method = 'ale')
plot(eff_2)

eff_3 <- FeatureEffect$new(mod, feature = "EARNINGSPERSHARE", method = 'ale')
plot(eff_3)

eff_4 <- FeatureEffect$new(mod, feature = "RETURNONEQUITYTOTAL", method = 'ale')
plot(eff_4)

eff_5 <- FeatureEffect$new(mod, feature = "RETURNONASSETS", method = 'ale')
plot(eff_5)

eff_6 <- FeatureEffect$new(mod, feature = "NETSALESORREVENUES", method = 'ale')
plot(eff_6)

# 3. Shapley values - local predictions
shapley <- Shapley$new(mod, x.interest = testing_data[8,])
plot(shapley)


# Analysis of feature importance based on four scenarios
results <- data.frame(Row = 1:nrow(testing_data), Actual = testing_data$HighE75perc, Predicted = predicted_test, row.names = NULL)


results$Outcome <- with(results, ifelse(Actual == Predicted & Actual == "1", "TP",
                                        ifelse(Actual == Predicted & Actual == "0", "TN",
                                               ifelse(Actual != Predicted & Actual == "1", "FN", "FP"))))

# choosing one row in each scenario
TP_row <- subset(results, Outcome == "TP")$Row[1]
TN_row <- subset(results, Outcome == "TN")$Row[1]
FP_row <- subset(results, Outcome == "FP")$Row[1]
FN_row <- subset(results, Outcome == "FN")$Row[1]

predictor <- Predictor$new(rf_model_train, data = testing_data, y = testing_data$HighE75perc)

# Function to plot Shapley values for a given instance index
plot_shapley_for_instance <- function(instance_index) {
  shapley <- Shapley$new(predictor, x.interest = testing_data[instance_index,])
  plot(shapley)
}

plot_shapley_for_instance(TP_row)
plot_shapley_for_instance(TN_row)
plot_shapley_for_instance(FP_row)
plot_shapley_for_instance(FN_row)





# S SCORE
setwd("C:/Users/Michael/Desktop/Msc FINTECH STRATHCLYDE/SEMESTER 2/AG 947 SUSTAINABLE FINANCE/WORKSHOP/ASSIGNMENT SUSTAINABLE")

training_data_orig <- read_excel("Corporate ESG data - 2012-2023 - filtered - E-S-G.xlsx", sheet = "ESG Sample - Training")
testing_data_orig <- read_excel("Corporate ESG data - 2012-2023 - filtered - E-S-G.xlsx", sheet = "ESG Sample - Testing")

training_data <- subset(training_data_orig, select = c('HighS75perc','TOTALASSETS','TOTALDEBTCOMMONEQUITY','EARNINGSPERSHARE','RETURNONEQUITYTOTAL','RETURNONASSETS','NETSALESORREVENUES'))
testing_data <- subset(testing_data_orig, select = c('HighS75perc','TOTALASSETS','TOTALDEBTCOMMONEQUITY','EARNINGSPERSHARE','RETURNONEQUITYTOTAL','RETURNONASSETS','NETSALESORREVENUES'))

training_data$HighS75perc <- factor(training_data$HighS75perc)
testing_data$HighS75perc <- factor(testing_data$HighS75perc)

# Filling missing values
testing_data[,-1] <- lapply(testing_data[,-1], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

#training random forest model
rf_model_train <- randomForest(HighS75perc ~ TOTALASSETS + TOTALDEBTCOMMONEQUITY + EARNINGSPERSHARE + RETURNONEQUITYTOTAL + RETURNONASSETS + NETSALESORREVENUES, data = training_data)

# predictions on the training data
predicted_train <- predict(rf_model_train, training_data) 

# Creating confusion matrix
cm_train <- confusionMatrix(predicted_train, training_data$HighS75perc)

predicted_test <- predict(rf_model_train, newdata = testing_data, type = "response")
cm_test <- confusionMatrix(predicted_test, testing_data$HighS75perc)
print(cm_test)
print(cm_train)



# EXPLAINABILITY VIA IML (INTERPRETABLE MACHINE LEARNING) PACKAGE
mod <- Predictor$new(rf_model_train, data = training_data, y = training_data$HighS75perc)
#creating predictor object

#1. Feature importance - model level
imp <- FeatureImp$new(mod, loss = "ce", compare = "difference")
plot(imp)

#2. Feature effects - model level
eff_1 <- FeatureEffect$new(mod, feature = "TOTALASSETS", method = 'ale')
plot(eff_1)

eff_2 <- FeatureEffect$new(mod, feature = "TOTALDEBTCOMMONEQUITY", method = 'ale')
plot(eff_2)

eff_3 <- FeatureEffect$new(mod, feature = "EARNINGSPERSHARE", method = 'ale')
plot(eff_3)

eff_4 <- FeatureEffect$new(mod, feature = "RETURNONEQUITYTOTAL", method = 'ale')
plot(eff_4)

eff_5 <- FeatureEffect$new(mod, feature = "RETURNONASSETS", method = 'ale')
plot(eff_5)

eff_6 <- FeatureEffect$new(mod, feature = "NETSALESORREVENUES", method = 'ale')
plot(eff_6)

# 3. Shapley values - local predictions
shapley <- Shapley$new(mod, x.interest = testing_data[8,])
plot(shapley)



results <- data.frame(Row = 1:nrow(testing_data), Actual = testing_data$HighS75perc, Predicted = predicted_test, row.names = NULL)

results$Outcome <- with(results, ifelse(Actual == Predicted & Actual == "1", "TP",
                                        ifelse(Actual == Predicted & Actual == "0", "TN",
                                               ifelse(Actual != Predicted & Actual == "1", "FN", "FP"))))

# one row from each scenario
TP_row <- subset(results, Outcome == "TP")$Row[1]
TN_row <- subset(results, Outcome == "TN")$Row[1]
FP_row <- subset(results, Outcome == "FP")$Row[1]
FN_row <- subset(results, Outcome == "FN")$Row[1]

predictor <- Predictor$new(rf_model_train, data = testing_data, y = testing_data$HighS75perc)

plot_shapley_for_instance <- function(instance_index) {
  shapley <- Shapley$new(predictor, x.interest = testing_data[instance_index,])
  plot(shapley)
}

plot_shapley_for_instance(TP_row)
plot_shapley_for_instance(TN_row)
plot_shapley_for_instance(FP_row)
plot_shapley_for_instance(FN_row)