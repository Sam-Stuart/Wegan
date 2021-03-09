#'Perform Penalized Linear Regression'
#'@description Build a penalized regression model for one user selected predictor variable
#'@usage pen.reg.anal(mSetObj=NA, method="ridge", facA=NULL, weights=NULL)
#'@param mSetObj Input the name of the created mSetObj
#'@param method Set penalized regression method, default is ridge
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

#install.packages(c("caret", "glmnet", "Metrics", "dplyr"))
library("caret")
library("glmnet")
library("Metrics")
library("dplyr")

pen.reg.anal <- function(mSetObj=NA, method="ridge", facA=NULL, weights=NULL){
  
  mSetObj <- .get.mSet(mSetObj)
  
  #Text will be visible to user. 
  cat("One dependent variable and two or more independent variables will be tested for correlation. Only numeric variables will be used.")
  cat("Note that all variables other than the dependent variable will be treated as independent variables.") 
  #"The penalized regression models will constrain the contribution of the independent variables on the dependent variable, in some cases removing a variable from the model all together, so as to maximize the fit of the model."
  
  #Subset data to select only numeric variables
  data <- select_if(mSetObj$dataSet$norm, is.numeric)
  
  #Data check
  if (ncol(data)<3) {
      stop("Your data set only has 2 variables! Try using linear, polynomial or SVM regression.")
  }
  
  #Set response variable name
  if (facA=="NULL"){
    facA <- colnames(mSetObj$dataSet$norm)[1] #facA is the name of the response variable name. Default is first column.
  } else {
    facA <- facA #Determined using numeric.columns() function below (java will present options in drop down menu)
  }
  
  #Generate test and train data for model building
  set.seed(37) #Ensures same selction of data for test and train each time
  index <- sample(1:nrow(data), 0.7*nrow(data)) #Select 70% of dataset
  train_data <- data[index,] #70% of dataset
  test_data <- data[-index,] #30% of dataset
  resp.col.num <- which(colnames(data)==facA)
  predictors_train <- train_data[,-resp.col.num]
  predictors_test <- test_data[,-resp.col.num]
  response_train <- train_data[,facA] # response data for train dataset
  cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
  
  if (is.null(weights)==TRUE) { #No weights for model building
    
    if (method == "elastic net") {
      
      #Build model
      params <- train(x=predictors_train, y=response_train, weights=NULL, method="glmnet", 
                      trControl=trainControl("cv", number=10), tuneLength=5) #testing variour parameters
      model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                      weights=NULL, family="gaussian") #Build model with "best" parameters
      bestLambda <- params$bestTune$lambda #Extract best parameters
      bestAlpha <- params$bestTune$alpha
      method <- "Elastic Net Regression"
      
      #File name for summary download
      fileName <- "elastic_net_regression_summary.txt"
      
      #Cross validation results for plotting
      cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
      
    } else if (method == "lasso") {
      
      #Build model
      lambda <- 10^seq(-3, 3, length = 100)
      params <- train(predictors_train, response_train, weights=NULL, method="glmnet", 
                      trControl=trainControl("cv", number=10),
                      tuneGrid = expand.grid(alpha=1, lambda=lambda)) #testing variour parameters
      model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                      weights=NULL, family="gaussian") #Build model with "best" parameters
      bestAlpha <- 1
      bestLambda <- params$bestTune$lambda #Extract best parameter
      method <- "Lasso Regression"
      
      #File name for summary download
      fileName <- "lasso_regression_summary.txt"
      
      #Cross validation results for plotting
      cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
      
    } else {
      
      #Build model for ridge regression
      lambda <- 10^seq(-3, 3, length = 100)
      params <- train(predictors_train, response_train, weights=NULL, method="glmnet", 
                      trControl=trainControl("cv", number=10),
                      tuneGrid = expand.grid(alpha=0, lambda=lambda)) #testing variour parameters
      model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                      weights=NULL, family="gaussian")#Build model with "best" parameters
      bestAlpha <- 0
      bestLambda <- params$bestTune$lambda #Extract best parameter
      method <- "Ridge Regression"
      
      #File name for summary download
      fileName <- "ridge_regression_summary.txt" 
      
      #Cross validation results for plotting
      cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
      
    }
    
  } else {
    weights <- weights #Java upload weights as a vector of numeric values
    
    if (length(weights) == nrow(data)) { #There must be one weight for every row in the data set
      if (method == "elastic net") {
        
        #Build model
        params <- train(x=predictors_train, y=response_train, weights=weights, method="glmnet", 
                        trControl=trainControl("cv", number=10), tuneLength=5) #testing variour parameters
        model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                        weights=weights, family="gaussian") #Build model with "best" parameters
        bestLambda <- params$bestTune$lambda #Extract best parameters
        bestAlpha <- params$bestTune$alpha
        method <- "Elastic Net Regression"
        
        #File name for summary download
        fileName <- "elastic_net_regression_summary.txt"
        
        #Cross validation results for plotting
        cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
        
      } else if (method == "lasso") {
        
        #Build model
        lambda <- 10^seq(-3, 3, length = 100)
        params <- train(predictors_train, response_train, weights=weights, method="glmnet", 
                        trControl=trainControl("cv", number=10),
                        tuneGrid = expand.grid(alpha=1, lambda=lambda)) #testing variour parameters
        model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                        weights=weights, family="gaussian") #Build model with "best" parameters
        bestAlpha <- 1
        bestLambda <- params$bestTune$lambda #Extract best parameter
        method <- "Lasso Regression"
        
        #File name for summary download
        fileName <- "lasso_regression_summary.txt"
        
        #Cross validation results for plotting
        cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
        
      } else {
        
        #Build model for ridge regression
        lambda <- 10^seq(-3, 3, length = 100)
        params <- train(predictors_train, response_train, weights=weights, method="glmnet", 
                        trControl=trainControl("cv", number=10),
                        tuneGrid = expand.grid(alpha=0, lambda=lambda)) #testing variour parameters
        model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                        weights=weights, family="gaussian")#Build model with "best" parameters
        bestAlpha <- 0
        bestLambda <- params$bestTune$lambda #Extract best parameter
        method <- "Ridge Regression"
        
        #File name for summary download
        fileName <- "ridge_regression_summary.txt" 
        
        #Cross validation results for plotting
        cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
        
      }
    } else {
      stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
    }
  }
  
  cat("The penalized regression model was optimized using alpha = ", bestAlpha, " and lambda = ", bestLambda, ".", sep="") #Text will be visible to user.
  
  #Extract results
  summary <- params 
  resp.col.num <- which(colnames(data)==facA)
  fitted <- predict(model, newx=as.matrix(data[,-resp.col.num]))
  colnames(fitted) <- "Predicted values"
  call <- model[["call"]]
  formula <- as.formula(paste(facA, "~", paste(colnames(predictors_train), collapse="+", sep="")))
  coef <- as.data.frame(summary(coef(model)))
  coef$variable <- "variable"
  x <- 1
  for (col.num in coef[,"i"]) {
    coef$variable[x] <- colnames(data)[col.num]
    x <- x+1
  }
  coef$variable[1] <- "intercept"
  coef <- data.frame(coef$variable, coef$x)
  colnames(coef) <- c("Variables", "Coefficients")
  overall.rsme <- rmse(data[,facA], fitted)

  #Obtain test RMSE for plotting
  test_prediction <- predict(model, newx=as.matrix(predictors_test))
  test_rmse <- rmse(test_data[,facA], test_prediction)
  
  #Store results in mSetObj$analSet$penReg
  mSetObj$analSet$penReg$mod <- list(model_name=method, model=model, response=facA, predictors=colnames(predictors_train), alpha=bestAlpha, lambda=bestLambda)
  mSetObj$analSet$penReg$res <- list(response=facA, predictors=colnames(predictors_train), predictors.test.data=predictors_test, predictors.train.data=predictors_train, summary=summary, coefficients=coef, predicted.values=fitted, overall.rmse=overall.rsme, train_data=train_data, test_data=test_data, test.rmse=test_rmse, cross.validation=cv, method=method, fileName=fileName) 

  #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
  sink(fileName) 
  cat("Formula:\n")
  print(formula)
  cat("\nMethod:\n")
  cat(paste0(method))
  cat("\n\nCall:\n")
  print(call)
  cat("\nSummary:\n")
  print(summary)
  cat("\nCoefficients:\n")
  print(coef)
  cat("\nPredicted values using trained model:\n")
  print(fitted)
  cat("\nOverall RMSE:\n")
  cat(paste0(overall.rsme))
  sink()
  
  return(.set.mSet(mSetObj))
}


#'Produce predicted/actual plot for penalized regression
#'@description Scatter plot, where actual variables are y and predicted values are x
#'@usage plot.pred.penReg(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

plot.pred.penReg <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #install.packages("Metrics")
  library("Metrics")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  model <- mSetObj$analSet$penReg$mod$model
  method <- mSetObj$analSet$penReg$res$method
  predictors_test <- mSetObj$analSet$penReg$res$predictors.test.data
  test_prediction <- predict(model, newx=as.matrix(predictors_test))
  facA <- mSetObj$analSet$penReg$res$response
  test_data <- mSetObj$analSet$penReg$res$test_data

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.penReg <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(x=test_prediction, y=test_data[,facA], xlab="Predicted", ylab="Actual", main=method, yaxt="n")
  axis(2, las=2)
  abline(a=0,b=1)
  dev.off()
  
  return(.set.mSet(mSetObj))
}

#'Produce cross validation plot for penalized regression
#'@description
#'@usage plot.cv.penReg(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

plot.cv.penReg <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  cv <- mSetObj$analSet$penReg$res$cross.validation
  method <- mSetObj$analSet$penReg$res$method
  
  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.cv.penReg <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(cv, yaxt="n", xlab="Log(Lambda)", main=paste0(method, " Cross Validation Plot", "\n"))
  axis(2, las=2)
  dev.off()
  
  return(.set.mSet(mSetObj))
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################


#'Determine number and names of numeric variables for multivariate linear regression'
#'@description Java will use the results to enable user options for selecting the dependent variable
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

pen.numeric.columns <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  
  #install.packages("dplyr")
  library("dplyr")
  
  data <- select_if(mSetObj$dataSet$norm, is.numeric)
  count.all.numeric.cols <- ncol(data)
  name.all.numeric.cols <- colnames(data)
  
  num.col.results <- list(
    count=count.all.numeric.cols,
    names=name.all.numeric.cols
  )
  
  return(name.all.numeric.cols)
  
}

penal.reg.get.results <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj)
  lin.reg.result <- c(mSetObj$analSet$penReg$mod$alpha, mSetObj$analSet$penReg$mod$lambda, mSetObj$analSet$penReg$res$test.rmse)
  return(lin.reg.result)

}
