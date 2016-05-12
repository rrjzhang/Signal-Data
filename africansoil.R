library('readr')
library('dplyr')
library('ggplot2')
library('caret')

df = read_csv('C:/Users/User/Documents/GitHub/Signal-Data-Science/training.csv')

soil = dplyr::select(df,starts_with('m'))
targets = dplyr::select(df,Ca:Sand)

caret_fit_vec = vector(mode="list",5)
min_ind_vec = vector(mode="numeric",5)
best_lambda_vec = vector(mode="numeric",5)
best_alpha_vec = vector(mode="numeric",5)
lowest_RMSE_vec = vector(mode="numeric",5)  

for (i in 3:5){

  
# lambda_range = seq(0,0.4,0.005)
param_grid = expand.grid(.alpha = 1:10 * 0.1,
                         .lambda=10^seq(-4, -1, length.out=10))

control = trainControl(method = 'repeatedcv', number = 10, repeats=3, verboseIter = TRUE)
# rated = activities_df[[ma2]]
caret_fit = train(x=soil,
                  y = targets[[i]], 
                  method="glmnet",
                  tuneGrid=param_grid,
                  trControl=control)

min_ind = which.min(caret_fit$results$RMSE)
best_lambda = caret_fit$results$lambda[min_ind]
best_alpha = caret_fit$results$alpha[min_ind]
lowest_RMSE = caret_fit$results$RMSE[min_ind]

caret_fit_vec[[i]] = caret_fit
min_ind_vec[i] = min_ind
best_lambda_vec[i] = best_lambda
best_alpha_vec[i] = best_alpha
lowest_RMSE_vec[i] = lowest_RMSE  

}
##arbitrary function!
arb_rating = function(df, rating){
  df_gender = dplyr::filter(df,gender==g)
  ma = match(rating,colnames(df))
  print (ma)
  activities_df = dplyr::select(df_gender,sports:yoga,ma)
  #find lambda range
  lambda_range = (cv.glmnet(scale(select(activities_df,sports:yoga)),
                            activities_df$attr_o, alpha = 1))$lambda
  
  #do the caret
  param_grid = expand.grid(.alpha = 1:10 * 0.1,
                           .lambda=10^seq(-4, -1, length.out=10))
  
  control = trainControl(method = 'repeatedcv', number = 10, repeats=3, verboseIter = TRUE)
  
  print("welp")
  
  ma2 = match(rating,colnames(activities_df))
  predictors = (scale(select(activities_df,sports:yoga)))
  rated = activities_df[[ma2]]
  
  
  caret_fit = train(x=predictors,
                    y = rated, 
                    method="glmnet",
                    tuneGrid=param_grid,
                    trControl=control)
  
  print('here')
  
  min_ind = caret_fit$results$lambda[which.min(caret_fit$results$RMSE)]
  best_lambda = caret_fit$results$lambda[min_ind]
  best_alpha = caret_fit$results$lambda[min_ind]
  lowest_RMSE = caret_fit$results$RMSE[min_ind]
  
  return(c(best_lambda,best_alpha,lowest_RMSE))}

abnorm = list(min_ind,best_lambda,best_alpha,lowest_RMSE)

