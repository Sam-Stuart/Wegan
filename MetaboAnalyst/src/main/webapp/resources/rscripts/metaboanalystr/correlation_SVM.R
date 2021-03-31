#'Perform SVM Regression'
#'@description Use SVM for regression analysis
#'@usage reg.rf.anal(mSetObj=NA, facA=NULL)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param pred.text Input predictor column names (java uses text box to obtain string)
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

reg.svm.anal <- function(mSetObj=NA, facA=NULL, pred.text=NULL) {

  #install.packages(c("e1071", "Metrics"))
  library("e1071")
  library("Metrics")
  mSetObj <- .get.mSet(mSetObj)
  
  mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
  print(mSetObj$dataSet$norm)
  #Text should be visable to user
  cat("One dependent variable and one or more independent variables will be tested for correlation. The dependent variable must be numeric. The independent variables can be numeric or categorical.")
  cat("For categorical independent variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the level labels to I, II and III prior to upload.")
  
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
    pred.text <- colnames(data)[1] #Default is the first potential predictor column
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
  response_train <- train_data[,facA] # response data for train dataset
  response_test <- test_data[,facA] # response data for test dataset
  cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
  
  #Build model
  model <- tune(svm, formula, data=train_data, ranges=list(epsilon=seq(0,1,0.1), cost=2^(seq(0.5,8,.5))))
  tunedModel <- model$best.model
  model_name <- "SVM Regression"
  
  #Extract results
  summary <- summary(tunedModel) 
  residuals <- residuals(tunedModel)
  decision_values <- tunedModel[["decision.values"]]
  fitted <- predict(tunedModel)
  train_rmse <- rmse(response_train, fitted)
  fileName <- "SVM_regression_summary.txt"
  
  #Obtain test RMSE for plotting
  test_prediction <- predict(tunedModel, newdata=test_data)
  test_rmse <- rmse(response_test, test_prediction)
  
  #Store results
  mSetObj$analSet$rfReg$res <- list(summary=summary, response=facA, predictors=pred_data, pred.text=pred.text, predicted.values=fitted, train.RMSE=train_rmse, test.prediction=test_prediction, test.RMSE=test_rmse, train_data=train_data, test_data=test_data, method=model_name, fileName=fileName)       
  mSetObj$analSet$rfReg$mod <- list(model_name=model_name, model=model, response=facA, predictors=predictors)
  mSetObj$analSet$rfReg$res$test_this <- test_data
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
  cat("\nModel RMSE:\n")
  #cat(paste0(svm_RMSE))
  sink()
  
  return(.set.mSet(mSetObj))
    
}  


#'Plot svm predicted vs actual data plot with line of best fit
#'@description Scatter plot where response variable is y and predictor variable is x
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

plot.pred.svmReg <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)

  method <- mSetObj$analSet$rfReg$res$method
  prediction <- mSetObj$analSet$rfReg$res$test.prediction
  test_data <- mSetObj$analSet$rfReg$res$test_this
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
