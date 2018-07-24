library(data.table)
library(dplyr)
library(randomForest)
library(lime)
library(ranger)
install.packages('pdp')
housing <- fread('/Users/luajiongwei/Documents/GitHub/HDB/combined_data.csv')
housing <- housing %>% select(-street_name, -V1, -month, -street_name, -block) 
#Create train set i.e. pre 2015
housing_train <- housing %>% filter(month < 2015) %>% select(-street_name, -V1, -month, -street_name, -block) 
housing_train <- housing_train %>% na.omit()
housing_train <- housing_train %>% mutate_if(sapply(housing_train, is.character), as.factor) %>%  mutate_if(sapply(housing_train, is.factor), toupper) 

#Create test set i.e. post 2015
housing_test <- housing %>% filter(month > 2015) %>% select(-street_name, -V1, -month, -street_name, -block, -flat_type)
housing_test <- housing_test %>% mutate_if(sapply(housing_test,is.character), as.factor) %>% mutate_if(sapply(housing_test, is.factor), toupper) 

rf_classifier <- train( formula = flat_type ~ ., ntree = 100, data = housing_train, importance = T)

importances  <- as.data.table(rf_classifier$importance)

importances %>% View()

rf_classifier

explainer <- lime(housing_train %>% select(-flat_type), as_classifier(rf_classifier))
housing_train_predictors <- housing_train %>% select(-flat_type)
explanation <- explain(housing_train_predictors[1:10,], explainer, n_labels = 1, n_features = 5)
housing_train[1:10, ] %>% View()
plot_explanations(explanation)


#Regression Problem
housing <- fread('/Users/luajiongwei/Documents/GitHub/HDB/combined_data.csv')
housing <- housing %>% select(-street_name, -V1, -month, -street_name, -block) 
idx <- sample(1:nrow(housing), 0.8 * nrow(housing), replace = FALSE )
housing_train <- housing[idx,]
housing_test <- housing[-idx,]
housing_train <- housing_train %>% na.omit()
housing_train <- housing_train %>% mutate_if(sapply(housing_train, is.character), as.factor) %>%  mutate_if(sapply(housing_train, is.factor), toupper) 
housing_test <- housing_test %>% na.omit()
housing_test <- housing_test %>% mutate_if(sapply(housing_test, is.character), as.factor) %>% mutate_if(sapply(housing_test, is.factor), toupper)


#Train Regressor
rf_regressor <- ranger(formula = resale_price ~ ., num.trees = 100, data = housing_train, importance = 'impurity', verbose = T)

#Define model type and incorporate predict_model 
model_type.ranger <- function(x, ...) {
  return("regression")
}

model_type(rf_regressor)

predict_model.ranger <- function(x, newdata, ...) {
  # Function performs prediction and returns data frame with Response
  pred <- predict(x, newdata)
  return(as.data.frame(pred$predictions))
}

# LIME
explainer_regressor <- lime(housing_train %>% select(-resale_price), rf_regressor)
housing_test_predictors <- housing_test %>% select(-resale_price)
explanation_regressor <- explain(housing_test_predictors[1:10, ], explainer_regressor, n_features = 5)
explanation_regressor
plot_explanations(explanation_regressor)

colnames(housing_test_predictors)
#
library(pdp)
pd <-rf_regressor %>% partial(pred.var = 'floor_area_sqm', progress = 'text')
p1 <- plotPartial(pd, rug = TRUE, train = housing_train)
pd2 <- rf_regressor %>% partial(pred.var = c ('floor_area_sqm','lease_commence_date'), progress = 'text')

