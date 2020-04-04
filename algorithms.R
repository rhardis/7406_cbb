###
# This file is for algorithms to try and compare in the final report
###
library(glmnet)

regression_simp = function(features_train, features_test, target_train, target_test){
  x_vars = as.matrix(features_train)
  y_var = target_train
  set.seed(7406)
  
  train.control <- trainControl(method = "repeatedcv", 
                                number = 10, repeats = 10)
  Regression <- train(y_var ~ (x_vars)^2,              #we can do this, or stepAIC for all comb.
                      data = cbind(x_vars, y_var), 
                      method = "lm",
                      trControl = train.control)
  
  prediction = predict(Regression, target_test)
  return(prediction)
}

lasso_reg = function(features_train, features_test, target_train, target_test){
  x_vars = as.matrix(features_train)
  y_var = target_train
  
  lambda_seq = 10^seq(2,-2,by=-.1)
  
  cv_output = cv.glmnet(x_vars, y_var, alpha=1, lambda = lambda_seq)
  best_lam = cv_output$lambda.min
  
  best_model = glmnet(x_vars, y_var, alpha=1, lambda=best_lam)
  
  x_test = as.matrix(features_test)
  y_test = target_test
  
  prediction = predict(best_model, s=best_lam, newx=x_test)
  return(prediction)
}

ridge_reg = function(features_train, features_test, target_train, target_test){
  x_vars = as.matrix(features_train)
  y_var = target_train
  
  lambda_seq = 10^seq(2,-2,by=-.1)
  
  cv_output = cv.glmnet(x_vars, y_var, alpha=0, lambda = lambda_seq)
  best_lam = cv_output$lambda.min
  
  best_model = glmnet(x_vars, y_var, alpha=0, lambda=best_lam)
  
  x_test = as.matrix(features_test)
  y_test = target_test
  
  prediction = predict(best_model, s=best_lam, newx=x_test)
  return(prediction)
}

boosted_reg = function(features_train, features_test, target_train, target_test){
  x_vars = as.matrix(features_train)
  y_var = target_train
  set.seed(7406)
  train.control <- trainControl(method = "repeatedcv", 
                                number = 10)
  # Train the model
  BoostedRegressionTree <- train(y_var ~ x_vars, 
                                 data = cbind(features_train, target_train), 
                                 method = "xgbTree",
                                 trControl = train.control)
  
  prediction <- BoostedRegressionTree %>% predict(features_test)
  return(prediction)
}

MARS = function(features_train, features_test, target_train, target_test){
  x_vars = as.matrix(features_train)
  y_var = target_train
  set.seed(7406)
  train.control <- trainControl(method = "repeatedcv", 
                                number = 10, repeats = 10)
  # Train the model
  MARScv <- train(y_var ~ x_vars,
                      data = cbind(features_train, target_train),
                      method = "earth",
                      trControl = train.control)
  
  prediction <- MARScv %>% predict(features_test)
  return(prediction)
}