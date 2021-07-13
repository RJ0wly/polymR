library(tidyverse)
library(glmnet)
library(Hmisc)
library(pec)
library(RegParallel)

#' bootstrap_lasso
#' 
#' Generate a list from selected covariates for each bootstrap applied to a
#' logistic regression or cox lasso. (inspire from SparseLearner package)
#' @usage bootstrap_lasso(x,y,bs=10,kfold=10,family,standardize)
#' @param x Dataframe, containing the features of interest.
#' @param y Vector, containing status or the response.
#' @param bs Integer, number of boostrap.
#' @param kfold Integer, K-Folds cross-validator. Split dataset 
#' into k consecutive folds.
#' @param family String, currently two options cox or binomial, 
#' not all model available.
#' @param standardize Boolean, columns of the data matrix x are standardized, 
#' i.e. each column of x has mean 0 and standard deviation 1. 
#' @export
bootstrap_lasso=function(x, y, bs=10, kfold=10,family,standardize){
  rowx <- nrow(x)
  n <- length(y)
  if (rowx!=n){
    stop("The number of rows in x is not equal to the length of y!")
  }
  res = lapply(1:bs,function(i){
    start_time= as.numeric(Sys.time())
    repeat{ 
      s <- sample(n, replace=TRUE)
      # repeat while it does not have at least two discrete value from each group
      if(length(table(y[s])) >= 2 & length(table(y[-s])) >= 2)
        break
    }
    # selecting row
    BoostrapX <- as.matrix(x[s, ])
    colnames(BoostrapX) <- colnames(x)
    BoostrapY <- y[s]
    
    # logistic regression lasso
    if(family == "binomial"){
      cvfit <- cv.glmnet(x=BoostrapX, 
                         y=BoostrapY, 
                         type.measure="deviance", 
                         nfolds=kfold, 
                         family="binomial",
                         alpha=1,
                         maxit=1000,
                         standardize=standardize)
    }
    # cox lasso
    if(family == "cox"){
      cvfit <- cv.glmnet(x=BoostrapX, 
                         y=BoostrapY, 
                         type.measure="C", 
                         nfolds=kfold, 
                         family="cox",
                         alpha=1,
                         maxit=1000,
                         standardize=standardize)
    }
    
    
    model_final <- cvfit$glmnet.fit
    coeffs_zero <- as.matrix(coef(model_final, s=cvfit$lambda.min))
    coeffs_zero <- names(coeffs_zero[coeffs_zero[,1]!=0,])
    end_time = as.numeric(Sys.time())
    cat("Boostrap ", i,"/",bs, "time taken",end_time - start_time,"s\n")
    tmp <- coeffs_zero[which(coeffs_zero!="(Intercept)")]
  })
  return(res)
}