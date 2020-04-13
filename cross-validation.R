library(doParallel)


kfolds_cv = function(kfolds, full_df, run_boost=T){
  full_df = na.omit(full_df)
  
  registerDoParallel(detectCores()-2)
  len = nrow(full_df)
  indices = sample(1:len, len, replace=F)
  
  remove.size = floor(len/kfolds)
  
  # Get best parameters for the different models
  library(glmnet)
  get_best_lasso = function(full_df){
    features_train = as.matrix(full_df[,-which(names(full_df) %in% c("POSTSEASON"))])
    target_train = full_df[,which(names(full_df) %in% c("POSTSEASON"))]
    
    x_vars = features_train
    y_var = target_train
    
    lambda_seq = 10^seq(2,-2,by=-.1)
    
    cv_output = cv.glmnet(x_vars, y_var, alpha=1, lambda = lambda_seq)
    return(cv_output$lambda.min)
  }
  best_lasso_lambda = get_best_lasso(full_df)
  
  get_best_ridge = function(full_df){
    features_train = as.matrix(full_df[,-which(names(full_df) %in% c("POSTSEASON"))])
    target_train = full_df[,which(names(full_df) %in% c("POSTSEASON"))]
    
    x_vars = features_train
    y_var = target_train
    
    lambda_seq = 10^seq(2,-2,by=-.1)
    
    cv_output = cv.glmnet(x_vars, y_var, alpha=0, lambda = lambda_seq)
    return(cv_output$lambda.min)
  }
  best_ridge_lambda = get_best_ridge(full_df)

  
  
  # Run kfolds
  k_metrics = foreach(k = 1:kfolds, .combine=rbind) %dopar% {
  #k_metrics = matrix(0, 10, 4)
  #for (k in 1:kfolds){
    library(glmnet)
    library(caret)
    library(earth)
    library(MLmetrics)
    library(dplyr)
    library(xgboost)
    library(randomForest)
    
    # Correct games scoring function
    correct_games = function(preds, ytrue){
      # Returns the decimal value of game predictions strictly within 0.5 games of the true number of wins
      return(sum(abs(preds-ytrue)<0.5)/length(ytrue))
    }
    
    
    # Split train and test data for this fold
    if (k != kfolds){
      remove.index = seq(1+(k-1)*remove.size,k*remove.size)
    }
    else{
      remove.index = seq(1+(k-1)*remove.size, len)
    }
    
    test.data = full_df[remove.index,]
    train.data = full_df[-remove.index,]
    
    train.x.mat = as.matrix(train.data[,-which(names(full_df) %in% c("POSTSEASON"))])
    train.y.mat = train.data[,which(names(full_df) %in% c("POSTSEASON"))]
    test.x.mat = as.matrix(test.data[,-which(names(full_df) %in% c("POSTSEASON"))])
    test.y.mat = test.data[,which(names(full_df) %in% c("POSTSEASON"))]
    
    
    
    # Get predictions for each model
    lasso.model = glmnet(train.x.mat, train.y.mat, alpha=1, lambda = best_lasso_lambda)
    pred.best_lasso = predict(lasso.model, s=best_lasso_lambda, newx=test.x.mat)
    
    ridge.model = glmnet(train.x.mat, train.y.mat, alpha=0, lambda = best_ridge_lambda)
    pred.best_ridge = predict(ridge.model, s=best_ridge_lambda, newx=test.x.mat)
      
    linreg.model = lm(POSTSEASON~., data = train.data)
    pred.linreg = predict(linreg.model, newdata = as.data.frame(test.x.mat))
    
    mars.model = train(POSTSEASON~., data=train.data, method="earth")
    pred.mars = mars.model %>% predict(as.data.frame(test.x.mat))
    
    if (run_boost){
      boost.model = train(POSTSEASON~., data=train.data, method="xgbTree")
      pred.boost = boost.model %>% predict(as.data.frame(test.x.mat))
      mse.boost = MSE(pred.boost, test.y.mat)
      acc.boost = correct_games(pred.boost, test.y.mat)
    }
    else{
      mse.boost = 0.872
      acc.boost = .469
    }
    
    rf = randomForest(POSTSEASON~.,data=train.data)
    pred.rf = predict(rf, newdata=test.data[,-which(names(full_df) %in% c("POSTSEASON"))])
    
    # Score each model
    mse.lasso = MSE(pred.best_lasso, test.y.mat)
    acc.lasso = correct_games(pred.best_lasso, test.y.mat)
    
    mse.ridge = MSE(pred.best_ridge, test.y.mat)
    acc.ridge = correct_games(pred.best_ridge, test.y.mat)
    
    mse.linreg = MSE(pred.linreg, test.y.mat)
    acc.linreg = correct_games(pred.linreg, test.y.mat)
    
    mse.mars = MSE(pred.mars, test.y.mat)
    acc.mars = correct_games(pred.mars, test.y.mat)
    
    mse.rf = MSE(pred.rf, test.y.mat)
    acc.rf = correct_games(pred.rf, test.y.mat)
    
    #k_metrics[k] = c(mse.lasso, mse.linreg, acc.lasso, acc.linreg)
    to.k_metrics = c(mse.lasso, mse.ridge, mse.linreg, mse.mars, mse.boost, mse.rf, acc.lasso, acc.ridge, acc.linreg, acc.mars, acc.boost, acc.rf)
    to.k_metrics
  }
  
  num_models = ncol(k_metrics)/2
  k_mse.mat = k_metrics[,1:num_models]
  k_acc.mat = k_metrics[,(num_models+1):ncol(k_metrics)]
  
  k_mse.mean = colMeans(k_mse.mat)
  k_acc.mean = colMeans(k_acc.mat)
  
  k_metrics = as.data.frame(rbind(k_mse.mean, k_acc.mean))
  
  return(k_metrics)
}

# Correct games scoring function
correct_games = function(preds, ytrue){
  # Returns the decimal value of game predictions strictly within 0.5 games of the true number of wins
  return(sum(abs(preds-ytrue)<0.5)/length(ytrue))
}
