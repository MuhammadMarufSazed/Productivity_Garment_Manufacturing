production = read.csv("Data\\garments_worker_productivity.csv")


#Spelling error correction
production$department = replace(production$department, production$department=='finishing ', 'finishing')
production$department = replace(production$department, production$department=='sweing ', 'sewing')

#format for rest of the columns
production$date = as.Date(production$date, "%m/%d/%Y")
production$quarter = as.factor(production$quarter)
production$department = as.factor(production$department)
production$day = as.factor(production$day)
production$team = as.factor(production$team)



#normalizing function
normlize_fn = function(x){
  z = (x - mean(x))/sd(x)
  return (z)
}

# normalizing numeric columns
prod_norm = apply(production[, -c(1:5)], 2, normlize_fn)

#combining with factor columns
production_norm = cbind(production[, c(1:5)], prod_norm)



#Missing value imputation (WIP column) - with Expectation Maximization


library(norm)
s <- prelim.norm(prod_norm) #do preliminary manipulations
thetahat <- em.norm(s) #find the mle
rngseed(1234567) #set random number generator seed
ximp <- imp.norm(s,thetahat,prod_norm) #impute missing data under the MLE

prod_norm_imp = as.data.frame(ximp)

prod_norm_final = cbind(production[, c(1:5)], prod_norm_imp)


#saving imputed data

write.csv(prod_norm_final, file = "Data\\imputed_formatted_data.csv")


