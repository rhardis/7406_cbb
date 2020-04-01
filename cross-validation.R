library(doParallel)
library(glmnet)

kfolds_cv = function(kfolds, full_df){
  registerDoParallel(cores=5)
  len = nrow(full_df)
  indices = sample(1:len, len, replace=F)
  
  remove.size = floor(len/kfolds)
  
  k_metrics = foreach(k = 1:kfolds, .combine=rbind) %dopar% {
    
    # Correct games scoring function
    correct_games = function(preds, ytrue){
      # Returns the decimal value of game predictions strictly within 0.5 games of the true number of wins
      return(sum(abs(preds-ytrue)<0.5)/length(ytrue))
    }
    
    library(MLmetrics)
    
    # Split train and test data for this fold
    if (k != kfolds){
      remove.index = seq(1+(k-1)*remove.size,k*remove.size)
    }
    else{
      remove.index = seq(1+(k-1)*remove.size, len)
    }
    
    test.data = full_df[remove.index,-(ncol(full_df)-5)]
    train.data = full_df[-remove.index,]
    
    train.x.mat = as.matrix(train.data[,-(ncol(train.data)-5)])
    train.y.mat = train.data[,(ncol(train.data)-5)]
    test.x.mat = as.matrix(test.data[,-(ncol(test.data)-5)])
    test.y.mat = test.data[,(ncol(test.data)-5)]
    
    # Get predictions for each model
    
    
    best_lambda = .2
    lasso.model = glmnet(train.x.mat, train.y.mat, alpha=1, lambda = best_lambda)
    pred.best_lasso = predict(lasso.model, s=best_lambda, newx=test.x.mat)
      
    linreg.model = lm(POSTSEASON~., data = train.data)
    pred.linreg = predict(linreg.model, newdata = test.data)
    
    #pred.best_lasso = pred.linreg
    
    # Score each model
    mse.lasso = MSE(pred.best_lasso, test.y.mat)
    acc.lasso = correct_games(pred.best_lasso, test.y.mat)
    
    mse.linreg = MSE(pred.linreg, test.y.mat)
    acc.linreg = correct_games(pred.linreg, test.y.mat)
    
    to.k_metrics = c(mse.lasso, mse.linreg, acc.lasso, acc.linreg)
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
