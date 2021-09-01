#'Perform Linear Regression'
#'@description Build a linear regression model for one user selected predictor variable
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses Columns() to give user options)
#'@param facB Input the name of the predictor column (java uses Columns() to give user options)
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

lin.reg.anal.one <- function(mSetObj=NA, facA="NULL", facB="NULL", weights=NULL){
  
  mSetObj <- .get.mSet(mSetObj)
  mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
  #Dependent var default is first column. Independent var default is second column.


  #Set dependent (response) variable name
  if (facA == "NULL"){
    facA <- colnames(mSetObj$dataSet$norm)[1] #Default is first column.
  } else {
    facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
  }
  
  #Set independent (predictor) variable name
  if (facB == "NULL"){
    facB <- colnames(mSetObj$dataSet$norm)[2] #Default is second column.
  } else {
    facB <- facB #Determined using Columns() function below (java will present options in drop down menu)
  }

  #Variable type check
  if (is.factor(mSetObj$dataSet$norm[,facA] || mSetObj$dataSet$norm[,facB])==TRUE){
    #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
    stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
  }
  
  #Define formula
  formula <- as.formula(paste0(facA, "~", facB)) 
  
  #Generate model with or without weights
  if (is.null(weights)==TRUE) {
    model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=NULL) #Create linear model, no weights
  } else {
    weights <- weights #Java upload weights as a vector of numeric values
    if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
      model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=weights) #Create linear model, with weights
    } else {
      #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
      stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
    }
  }
  
  #Store model
  mSetObj$analSet$linReg1$mod <- model
  
  #Extract results
  fitted <- fitted(model) #Predicted values
  summary <- summary(model) #Summary includes coefficients, residuals and fit parameters
  residuals <- model$residuals # Get the residuals 
  conf.int <- confint(model, level=0.95) #Confidence intervals for predictor variables
  covar <- vcov(model) #Covariance matrix for preductor variables
  fileName <- paste0("linear_regession_summary_", facA, "~", facB, ".txt") #File name for summary
  coeffs <- summary[["coefficients"]] #Extract model coefficients
  beta <- round(coeffs[2], 2)
  alpha <- round(coeffs[1], 2)
  equation <- paste(facA, " = ", paste(paste(beta, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
  r.squared <- summary[["adj.r.squared"]] #Extract R^2 value
  r_sq <- round(r.squared, 2)
  r.squared.eq <- paste("R-squared = ", r_sq) #Generate R^2 equation
  r.squared.adj <- summary[["adj.r.squared"]] #Extract adjusted R^2 value
  r_sq_adj <- round(r.squared.adj, 2)
  r.squared.adj.eq <- paste("R-squared adjusted = ", r_sq_adj) #Generate adjusted R^2 equation
  
  # #Test residuals for normality. Error will be visable to user.
  # norm_resid <- shapiro.test(residuals) 
  # if (norm_resid$p.value < 0.05){
  #   norm_resid_text <- paste0("The residuals are normally distributed. This model is valid.") #To be used in summary, not to be displayed
  # } else {
  #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  #   stop("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  # }
  
  #Store results
  mSetObj$analSet$linReg1$res <- list(response=facA, predictor=facB, summary=summary, predicted.values=fitted, confidence.intervals=conf.int, covariance.matrix=covar, equation=equation, r.squared.eq=r.squared.eq, r.squared.adj.eq=r.squared.adj.eq, fileName=fileName, formula=formula) #Note where the summary is stored
  
  #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink(fileName) 

  cat("Formula:\n")
  print(formula)

  print(summary)
  # print(norm_resid)
  # cat("Normality of residuals result:\n")
  # cat(paste0(norm_resid_text, "\n"))

  cat("\nConfidence intervals for predictor variables:")
  print(conf.int)

  cat("\nPredicted values:")
  print(fitted)

  cat("\nCovariance matrix for predictor variables:")
  print(covar)
  sink()
  
  
  return(.set.mSet(mSetObj))
  
}


#'Plot line of best fit for linear regression with one predictor variable
#'@description Scatter plot with line of best fit, where response variable is y and predictor variable is x
#'@usage Plot.linReg1(mSetObj, imgName, format="png", dpi=72, width=NA)
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

plot.linReg1 <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  facA <- mSetObj$analSet$linReg1$res$response #For x-axis label
  facB <- mSetObj$analSet$linReg1$res$predictor #For y-axis label
  response <- mSetObj$dataSet$norm[,facA] #First column is reponse variable by default
  predictor <- mSetObj$dataSet$norm[,facB] #User selects predictor variable in drop down menu
  model <- mSetObj$analSet$linReg1$mod
  
  #Set plot dimensions
  if(is.na(width)){
    w <- 7.2
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.linReg1 <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(response, predictor, xlab=facA, ylab=facB, main="Univariate Linear Regression Line of Best Fit", yaxt="n")
  axis(2, las=2)
  abline(model)
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number and names of columns in dataset'
#'@description Java will use the number of columns to enable user options
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

lin.reg.columns <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  
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

lin.reg.get.results <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj)
  lin.reg.result <- c(mSetObj$analSet$linReg1$res$equation, mSetObj$analSet$linReg1$res$r.squared.eq, mSetObj$analSet$linReg1$res$r.squared.adj.eq)
  return(lin.reg.result)

}
