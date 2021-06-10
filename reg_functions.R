# Custom Bootstrap Lasso Function

boot_lasso_function <- function(b, alpha, x_train, y_train, x_test, y_test){
  boot_betas = matrix(0, nrow=ncol(x), ncol=b)
  pred_train = numeric(b)
  pred_test = numeric(b)
  for (i in 1:b){
    boot_sample_id = sample(1:nrow(x_train), replace = TRUE)
    x_boot = x_train[boot_sample_id, ]
    y_boot = y_train[boot_sample_id]
    cv_lasso=cv_glmnet(x =  as_matrix(x_boot), y =  y_boot, alpha=alpha, intercept = FALSE, family = "gaussian", type_measure="mse",standardize = TRUE)
    optimum_Lasso=glmnet(x = as_matrix(x_boot) , y =  y_boot, alpha=alpha, intercept = FALSE, family = "gaussian", type_measure="mse", lambda=cv_lasso$lambda_min,standardize = TRUE)
    boot_betas[, i] = as_vector(optimum_Lasso$beta)
    pred_train[i] = sum((y_train - predict(optimum_Lasso, as_matrix(x_train)))^2)
    pred_test[i] = sum((y_test - predict(optimum_Lasso, as_matrix(x_test)))^2)
  }
  return(list(boot_betas, pred_train, pred_test))
}

# Function to get the variables that remained throughout the boostrap replications

active_variables<-function(bootstrap_result, percentage){
  # bootstrap_result: the output of boot_lasso_function
  # percentage: value represent the percentage of bootstrap replications that the variables were present_      #Value of _9 means we only take those variables that are present in at least 90% of the bootstrap 
  # replications_
  val = c()
  for (j in 1:nrow(bootstrap_result)){
    if (mean(bootstrap_result[j,]!=0)>=percentage){
      val= c(val, j)
    }
  }
  return(val)
}

