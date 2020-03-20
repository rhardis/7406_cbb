###
# This file is for algorithms to try and compare in the final report
###
library(glmnet)

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