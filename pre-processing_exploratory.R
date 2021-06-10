production = read.csv("garments_worker_productivity.csv")

summary(production)
dim(production)

colnames(production)

#normalizing data
normlize_fn = function(x){
  z = (x - mean(x))/sd(x)
  return (z)
}

prod_norm = apply(production[, -c(1:5)], 2, normlize_fn)

production = cbind(production[, c(1:5)], prod_norm)

boxplot(production[, -c(1:5)])


#format for rest of the columns
production$date = as.Date(production$date, "%m/%d/%Y")
production$quarter = as.factor(production$quarter)
production$department = as.factor(production$department)
production$day = as.factor(production$day)
production$team = as.factor(production$team)


