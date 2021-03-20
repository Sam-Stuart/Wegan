#'Perform Multivariate Linear Regression'
#'@description Build a linear regression model for multiple user selected predictor variables
#'@param mSetObj Input the name of the created mSetObj
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

lin.reg.anal.multi <- function(mSetObj=NA, pred.text=NULL, weights=NULL){

  mSetObj <- .get.mSet(mSetObj)
  
  #Text should be visable to user
  cat("All variables must be numeric values.")
  cat("The first column will be the response variable.") 
  cat("Text box instructions for selecting predictor variables: Indicate predictor variables using the column names with commas in between. If user is interested in an interaction between particular variables, indicate it with a colon rather than a comma. Spaces do not matter. Text box should be interactive, meaning any change in text alters the result in real time.")
  
  #Set right side of formula, with predictor variables and interactions
  if (is.null(pred.text)) {
    pred.text <- colnames(mSetObj$dataSet$norm)[1] #Default is the second column name, no interactions
  } else {
    pred.text <- pred.text #taken from text box by java
  }
  
  #Currate right side of formula, with predictor variables and interactions
  pred.text <- gsub("\n", "", pred.text, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
  pred.text <- gsub(",", "+", pred.text, fixed=TRUE) 
  pred.text <- gsub(";", "+", pred.text, fixed=TRUE)
  pred.text <- gsub(" ", "", pred.text, fixed=TRUE)

  #Define formula
  formula <- as.formula(paste(colnames(mSetObj$dataSet$norm)[1], "~", pred.text))
    
  #Generate model with or without weights
  if (is.null(weights)==TRUE) {
    model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=NULL) #Create linear model, no weights
  } else {
    weights <- weights #Java upload weights as a vector of numeric values
    if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
      model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=weights) #Create linear model, with weights
    } else {
      cat("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error should be thrown so user can see it 
    }
  }
  
  #Store model
  mSetObj$analSet$linRegMulti$mod <- model
  
  #Extract results
  fitted <- fitted(model) #Predicted values
  summary <- summary(model) #Summary includes coefficients, residuals and fit parameters
  conf.int <- confint(model, level=0.95) #Confidence intervals for predictor variables
  covar <- vcov(model) #Covariance matrix for preductor variables
  fileName <- "multivariate_linear_regression_summary.txt" #File name for summary, used by save.linReg1.summary()
  
  #Store results
  mSetObj$analSet$linRegMulti$res <- list(summary=summary, predicted.values=fitted, confidence.intervals=conf.int, covariance.matrix=covar, fileName=fileName) 
  
  #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink(fileName) 
  cat("Formula:\n")
  print(formula)
  print(summary)
  cat("\nConfidence intervals for predictor variables:")
  print(conf.int)
  cat("\nPredicted values:")
  print(fitted)
  cat("\nCovariance matrix for predictor variables:")
  print(covar)
  sink()
  
  return(.set.mSet(mSetObj))
  
}


#'Produce diagnostic plots for multivariate linear regression
#'@description
#'@usage plot.linRegMulti.diagnostic(mSetObj, imgName, format="png", dpi=72, width=NA)
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

plot.linRegMulti.diagnostic <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  model <- mSetObj$analSet$linRegMulti$mod #Obtained by lin.reg.anal.one()

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #Set plot name for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.linRegMulti.diagnostic  <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  layout(matrix(c(1,2,3,4),2,2))
  plot(model)
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}






##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

