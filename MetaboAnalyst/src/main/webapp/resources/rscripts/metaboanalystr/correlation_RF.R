#'Perform Random Forest Regression'
#'@description Use random forest for regression analysis
#'@usage reg.rf.anal(mSetObj=NA, facA=NULL, pred.text=NULL)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param pred.text Input predictor column names (java uses text box to obtain string)
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

reg.rf.anal <- function(mSetObj=NA, facA=NULL, pred.text=NULL) {
  
  #install.packages(c("randomForest", "Metrics"))
  library("randomForest")
  library("Metrics")
  
  mSetObj <- .get.mSet(mSetObj)
  
  #Text will be visible to user. Dependent var default is first column. Independent var default is second column.
  cat("One dependent variable and two or more independent variables will be tested for correlation. The dependent variable must be numeric. The independent variables can be numeric or categorical.")
  cat("For categorical independent variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the level labels to I, II and III prior to upload.")

  #Data check
  if (ncol(mSetObj$dataSet$norm)<3) {
    col.classes <- sapply(mSetObj$dataSet$norm,class)
    if ("factor" %in% col.classes) {
    stop("Your data set only has 2 variables! Try using logistic regression.")
    } else {
      stop("Your data set only has 2 variables! Try using linear or polynomial regression.")
    }
  }
  
  #Set dependent (response) variable name
  if (facA=="NULL") {
    for (i in 1:ncol(mSetObj$dataSet$norm)) {
      if (is.factor(mSetObj$dataSet$norm[,i])==FALSE) {
        facA <- colnames(mSetObj$dataSet$norm)[i]# Default is to choose the first numeric column as response column
        break
      }
    }
  } else {
    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
  }
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default pred.text is second column.
  cat("Indicate independent variables using the column names with commas in between.")
  
  #Set right side of formula with predictor variables
  if (pred.text=="NULL") {
    resp.col.num <- which(colnames(mSetObj$dataSet$norm)==facA)
    data <- mSetObj$dataSet$norm[,-resp.col.num]
    pred.text <- paste0(colnames(data)[1], ",", colnames(data)[2]) #Default is the first 2 potential predictor columns
  } else {
    pred.text <- pred.text #taken from text box by java, fed as string into R code
  }
  
  #Currate right side of formula, and extract character vector of predictors
  pred.text <- gsub("\n", "", pred.text, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
  pred.text <- gsub(",", "+", pred.text, fixed=TRUE) 
  pred.text <- gsub(";", "+", pred.text, fixed=TRUE)
  pred.text <- gsub(" ", "", pred.text, fixed=TRUE)
  pred.text <- gsub(":", "+", pred.text, fixed=TRUE)
  pred.text <- gsub("*", "+", pred.text, fixed=TRUE)
  
  #Generate formula
  formula <- as.formula(paste(facA, "~", pred.text))
  #Text should be visible to user
  cat(paste0("You have created this formula for model building: ", facA, " ~ ", pred.text))
  cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
  cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
  #Subset data using predictor column names
  predictors <- unlist(strsplit(pred.text, "+", fixed=TRUE))
  pred_data <- mSetObj$dataSet$norm[,which(colnames(mSetObj$dataSet$norm) %in% predictors)]
  model_data <- data.frame(mSetObj$dataSet$norm[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors)
  
  #Generate test and train data for model building
  set.seed(37) #Insures same selction of data for test and train each time
  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset
  train_data <- model_data[index,] #70% of dataset
  test_data <- model_data[-index,] #30% of dataset
  predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors
  predictors_test <- predictors_test[,-1] # predictor data for test dataset
  predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-1] # Predictor variables in train dataset, creating dummy vars for categorical predictors
  predictors_train <- predictors_train[,-1] # predictor data for train dataset
  response_train <- train_data[,facA] # response data for train dataset
  response_test <- test_data[,facA] # response data for test dataset
  cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
  
  #Build model
  model <- tuneRF(y=response_train, x=predictors_train, ntreeTry=500, stepFactor=2, improve=0.05, trace=FALSE, doBest=TRUE, plot=FALSE, importance=TRUE)
  model_name <- "Random Forest Regression"
  
  #Generate results
  summary <- model 
  predictor_importance <- importance(model, type=1)
  fitted <- predict(model)  
  train_rmse <- rmse(response_train, fitted)
  fileName <- "random_forest_regression_summary.txt"
  
  #Obtain test RMSE for plotting
  test_prediction <- predict(model, newdata=predictors_test)
  test_rmse <- rmse(response_test, test_prediction)
  
  #Store results
  mSetObj$analSet$rfReg$res <- list(summary=summary, response=facA, predictors=colnames(predictors_train), pred.text=pred.text, predicted.values=fitted, train.RMSE=train_rmse, test.prediction=test_prediction, test.RMSE=test_rmse, predictor.importance=predictor_importance, train_data=train_data, test_data=test_data, predictors.test.data=predictors_test, predictors.train.data=predictors_train, method=model_name, fileName=fileName)       
  mSetObj$analSet$rfReg$mod <- list(model_name=model_name, model=model, response=facA, predictors=colnames(predictors_train))

  #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink(fileName) 
  cat("Formula:\n")
  print(formula)
  print(model)
  cat("\nPredicted values:\n")
  print(fitted)
  cat("\nModel RMSE:\n")
  cat(paste0(train_rmse, "\n"))
  cat("\nPredictor variable importance:\n")
  print(predictor_importance)
  sink()
  
  return(.set.mSet(mSetObj))
  
}


#'Plot random forest predicted vs actual data plot using test data
#'@description Scatter plot where actual data is y and predicted data is x
#'@usage plot.pred.svmReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
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

plot.pred.RFReg <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
  method <- mSetObj$analSet$rfReg$res$method
  prediction <- mSetObj$analSet$rfReg$res$test.prediction
  test_data <- mSetObj$analSet$rfReg$res$test_data
  facA <- mSetObj$analSet$rfReg$res$response
  
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
  plot(x=prediction, y=test_data[,facA], xlab=paste0("Predicted ", facA), ylab=paste0("Actual ", facA), main=method, yaxt="n")
  axis(2, las=2)
  abline(a=0,b=1)
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


#'Plot random forest error rates
#'@description Plots error rate as a function of forest size 
#'@usage plot.pred.rfReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
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

plot.pred.rfError <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
  method <- mSetObj$analSet$rfReg$res$method
  model <- mSetObj$analSet$rfReg$mod$model
  
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
  mSetObj$imgSet$plot.err.rfReg <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(model, main=method, yaxt="n")
  axis(2, las=2)
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


#'Plot relative importance of predictors
#'@description Bar graph for relative importance of independent variables
#'@usage plot.relimpo.rfReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
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

plot.relimpo.rfReg <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
  predictor_importance <- mSetObj$analSet$rfReg$res$predictor.importance
  
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
  mSetObj$imgSet$plot.err.rfReg <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  barplot(t(predictor_importance), xlab="Independent Variables (Predictors)", ylab="% Increase Mean Square Error", main="Relative Importance of Predictors \nRandom Forest Regression", yaxt="n")
  axis(2, las=2)
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number and names of numeric columns for random forest regression'
#'@description Java will use the numeric columns to enable user options for selecting dependent variable
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

numeric.columns <- function(mSetObj=NA){
  
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

return(num.col.results)
  
}
