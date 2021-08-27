#'Perform Machine Learning Regression'
#'@description Build a linear regression model for one user selected predictor variable
#'@usage reg.machine.anal(mSetObj=NA, method=method)
#'@param mSetObj Input the name of the created mSetObj
#'@param method Set ML regression method, default is random forest
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

reg.machine.anal <- function(mSetObj=NA, method="random forest") {
  
  #install.packages(c("e1071", "randomForest"))
  library("e1071")
  library("randomForest")
  library("Metrics")
  
  mSetObj <- .get.mSet(mSetObj)

  #Text should be visable to user
  AddErrMsg("The first column will be the response variable. The remaining columns will be the predictor variables.")
  AddErrMsg("Response variable must be numeric for machine regression analysis. Predictor variables can be numeric or categorical.") 
  AddErrMsg("For categorical variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the level labels to I, II and III.")
  
  #Generate test and train data for model building
  set.seed(37)
  index <- sample(1:nrow(mSetObj$dataSet$norm), 0.7*nrow(mSetObj$dataSet$norm))
  train_data <- mSetObj$dataSet$norm[index,]
  test_data <- mSetObj$dataSet$norm[-index,]
  predictors_train <- model.matrix(train_data[,1] ~ ., train_data)[,-1] # Train predictor variables, creating dummy variables for categorical variables
  predictors_test <- model.matrix(test_data[,1] ~ ., test_data)[,-1] # Test predictor variables, creating dummy variables for categorical variables
  response_train_name <- colnames(mSetObj$dataSet$norm)[1] #response_train variable name
  predictors_train_name <- colnames(predictors_train)[-1] #response_train variable name
  #Text should be visable to user
  cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.")
  
  #Generate formula
  formula <- as.formula(paste(response_train_name, "~", paste(predictors_train_name, collapse="+")))
  
  if (method == "SVM") {
    
    #Build model
    model <- tune(svm, formula,  data=as.data.frame(predictors_train), ranges=list(epsilon=seq(0,1,0.1), cost=2^(seq(0.5,8,.5))))
    tunedModel <-model$best.model
    model_name <- "SVM Regression"
    
    #Extract predicted values
    prediction <- predict(tunedModel, newdata=as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
    
    #Store results for plotting
    mSetObj$analSet$svmReg$meth <- model_name
    mSetObj$analSet$svmReg$pred <- prediction
    mSetObj$analSet$svmReg$test <- test_data
    
    #Generate and download summary of parameter testing and write to txt document
    summary <- summary(tunedModel) 
    residuals <- residuals(tunedModel)
    decision_values <- tunedModel[["decision.values"]]
    fitted <- predict(tunedModel)
    svm_RMSE <- rmse(predictors_train[,1], fitted)
    fileName <- "SVM_regression_summary.txt"
    
    #Store results
    mSetObj$analSet$svmReg$res <- list(summary=summary, predicted.values=fitted, residuals=residuals, decision.values=decision_values, RSME=svm_RMSE, fileName=fileName)       
    mSetObj$analSet$svmReg$mod <- list(model_name=model_name, model=model, response=response_train_name, predictor=predictors_train_name)
    
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    # cat("\nReference category:\n")
    # cat(paste0(reference))
    print(summary)
    cat("Residuals:\n")
    print(residuals)
    cat("\nDecision values:\n")
    print(decision_values)
    cat("\nPredicted values:\n")
    print(fitted)
    cat("\nRMSE:\n")
    cat(paste0(svm_RMSE))
    sink()
    
  } else { #Method is random forest
    
    #Build model
    model <- tuneRF(y=train_data[,1], x=predictors_train[,-1], ntreeTry=500, stepFactor=2, improve=0.05, trace=TRUE, doBest=TRUE, plot=FALSE, importance=TRUE)
    model_name <- "Random Forest Regression"
    
    #Extract predicted values
    prediction <- predict(model, newdata=as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
    
    #Store results for plotting
    mSetObj$analSet$rfReg$meth <- model_name
    mSetObj$analSet$rfReg$pred <- prediction
    mSetObj$analSet$rfReg$test <- test_data
    
    #Generate and download summary of parameter testing and write to txt document
    summary <- model 
    predictor_importance <- importance(model)
    fitted <- predict(model)
    svm_RMSE <- rmse(predictors_train[,1], fitted)
    fileName <- "random_forest_regression_summary.txt"
    
    #Store results
    mSetObj$analSet$rfReg$res <- list(summary=summary, predicted.values=fitted, RSME=svm_RMSE, predictor.importance=predictor_importance, fileName=fileName)       
    mSetObj$analSet$rfReg$mod <- list(model_name=model_name, model=model, response=response_train_name, predictor=predictors_train_name)
    
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    # cat("\nReference category:\n")
    # cat(paste0(reference))
    print(model)
    cat("\nPredicted values:\n")
    print(fitted)
    cat("\nRMSE:\n")
    cat(paste0(svm_RMSE, "\n"))
    cat("\nPredictor variable importance:\n")
    print(predictor_importance)
    sink()
    
  } 
  
  return(.set.mSet(mSetObj))
  
}  

#'Plot svm predicted vs actual data plot with line of best fit
#'@description Scatter plot with line of best fit, where response variable is y and predictor variable is x
#'@usage plot.pred.svmReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param method Set ML regression method, default is random forest
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

plot.pred.MLReg <- function(mSetObj=NA, method="random forest", imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
  if (method=="SVM") {
    method <- mSetObj$analSet$svmReg$meth
    prediction <- mSetObj$analSet$svmReg$pred
    test_data <- mSetObj$analSet$svmReg$test
    
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
    mSetObj$imgSet$plot.pred.svmReg <- imgName
    
    #Generate plot
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    plot(prediction, test_data[,1], xlab="Predicted", ylab="Actual", main=method, yaxt="n")
    axis(2, las=2)
    abline(a=0,b=1)
    dev.off()
    
    return(.set.mSet(mSetObj))
  
  } else { #random forest is default
    
    method <- mSetObj$analSet$rfReg$meth
    prediction <- mSetObj$analSet$rfReg$pred
    test_data <- mSetObj$analSet$rfReg$test
    
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
    mSetObj$imgSet$plot.pred.rfReg <- imgName
    
    #Generate plot
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    plot(prediction, test_data[,1], xlab="Predicted", ylab="Actual", main=method, yaxt="n")
    axis(2, las=2)
    abline(a=0,b=1)
    dev.off()
    
    return(.set.mSet(mSetObj))
  }
}

