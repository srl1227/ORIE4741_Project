#CV
#indic_subset is the data from only the countries for the 23 indicators selected from the random forest
set.seed(5)
train_ind <- sample(1:nrow(indic_subset), 2/3*nrow(indic_subset))
train <- indic_subset[train_ind,]
test <- indic_subset[-train_ind,]

#train.fit = regsubsets(`CO2 emissions (metric tons per capita)`~.,data=train ,nvmax=20, method = "backward")
#summary(train.fit)

#test.mt = model.matrix(`CO2 emissions (metric tons per capita)`~.,data=test)

#val.errors = rep(NA,20)
#for(i in 1:20){
#  coefi = coef(train.fit, id=i)
#  pred = test.mt[,names(coefi)]%*%coefi
#  val.errors[i] = mean((test$`CO2 emissions (metric tons per capita)`-pred)^2)
#}
#val.errors #select 15 vars

trfit = regsubsets(`CO2 emissions (metric tons per capita)`~.,data=train ,nvmax=23) #model fit on training set
train.mt = model.matrix(`CO2 emissions (metric tons per capita)`~.,data=train)
ve = rep(NA,23)
for(i in 1:23){
  ci = coef(trfit, id=i)
  pred = train.mt[,names(ci)]%*%ci
  ve[i] = mean((train$`CO2 emissions (metric tons per capita)`-pred)^2)
}
ve #training error

#bestfit = regsubsets(`CO2 emissions (metric tons per capita)`~.,data=train ,nvmax=20, method = "backward")
#coef(bestfit,15)

#our cv predict function
predict.regsubsets = function(object,newdata,id,...){
  mat = model.matrix(`CO2 emissions (metric tons per capita)`~.,data=newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

#10 fold CV
k = 10
folds = sample(1:k, nrow(indic_subset), replace=TRUE)
cv.er = matrix(NA, k, 23, dimnames = list(NULL, paste(1:23)))
rsq = rep(NA,10)
for(j in 1:k){
  fit = regsubsets(`CO2 emissions (metric tons per capita)`~.,data=indic_subset[folds!=j,], nvmax = 23)
  rsq[j] = summary(fit)$adjr2[21]
  for(i in 1:23){
    pred = predict.regsubsets(fit, indic_subset[folds==j,], id=i)
    cv.er[j,i] = mean((indic_subset$`CO2 emissions (metric tons per capita)`[folds==j]-pred)^2)
  }
}

mean(rsq) #the mean adjusted rsqured for the 21 feature model is 0.8493457, for the training 21 ftr model 0.8567084

mean.cv.errors = apply(cv.er,2,mean) #the mean errors from our CV
plot(mean.cv.errors, type='o', xlab="Number of Variables ",
     ylab="Mean CV Error (MSE)", xlim = c(1,23), ylim = c(6.5,12.5), main = "Best Subset Selection")

min_var = which.min(mean.cv.errors) #lowest error at 21 vars

#sdme <- sd(mean.cv.errors)
#sd of mean cv errors is 1.27
#sderror <- subset(mean.cv.errors, mean.cv.errors < (6.876516+1.273025))
#with the one-standard-error rule, we can use the 5 variable model

bestmodel = regsubsets(`CO2 emissions (metric tons per capita)`~.,data=indic_subset, nvmax = 23)
bmsummary <- summary(bestmodel)
plot(bmsummary$adjr2, xlab="Number of Variables ",
     ylab="Adjusted RSq", main = 'Best Subset Selection',type="o",xlim = c(0,23), ylim = c(0.70,0.90))

coef(bestmodel,21)
#Our best model is the 21 variable model: which predicts C02 Emissions (Metric Tons Per Capita) based on 20
#predictors:


plot(ve, type = 'l', col = 'blue', lwd = 2, xlab = '# Features', ylab = "Mean Sq. Error", main = "Best Subset Selection Error")
lines(mean.cv.errors, col = 'red', lwd = 2)
legend('topright', c("Training Error", "Mean CV Error"), fill = c('blue','red'))
