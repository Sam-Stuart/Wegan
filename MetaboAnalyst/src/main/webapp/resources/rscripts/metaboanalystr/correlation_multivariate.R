#'Perform Multivariate Linear Regression'
#'@description Build a linear regression model for multiple user selected predictor variables
#'@param mSetObj Input the name of the created mSetObj
#'@param pred.text Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

lin.reg.anal.multi <- function(mSetObj=NA, pred.text="NULL", facA="NULL", weights=NULL){

  #install.packages(c("relaimpo"))
  library("dplyr")
  library("Metrics")
  library("relaimpo")

  mSetObj <- .get.mSet(mSetObj)
  
  #Text will be visable to user. Default dependent var is first column. Default independent vars is second column.
  cat("One dependent variable and multiple independent variables will be tested for correlation. All must have numeric values.")
  
  #Choose response (dependent) variable for modeling
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
  cat("Indicate independent variables using the column names with commas in between. If interested in an interaction between particular variables, indicate it with a colon rather than a comma.")
  
  #Set right side of formula with predictor variables
  if (pred.text=="NULL") {
    resp.col.num <- which(colnames(mSetObj$dataSet$norm)==facA)
    data <- mSetObj$dataSet$norm[,-resp.col.num]
    num.data <- select_if(data, is.numeric)
    pred.text <- colnames(num.data)[1] #Default is the first potential predictor column
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
  predictors1 <- unlist(strsplit(pred.text, "+", fixed=TRUE))
  predictors2 <- unlist(strsplit(predictors1, ":", fixed=TRUE))
  pred_data <- as.data.frame(mSetObj$dataSet$norm[,which(colnames(mSetObj$dataSet$norm) %in% predictors2)])
  model_data <- data.frame(mSetObj$dataSet$norm[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)
  
  #Determine if any predictor variables are categorical
  for (i in 1:ncol(pred_data)) {
    if (is.factor(pred_data[,i])==TRUE) {
      #AddErrMsg("You have chosen a categorical independent variable! Please adjust your independent variables appropriately. You can also try other regression models such as logistic, SVM or random forest.")
      stop("You have chosen a categorical independent variable! Please adjust your independent variables appropriately. You can also try other regression models such as logistic, SVM or random forest.") 
    }
  }  
  
  #Generate model with or without weights
  if (is.null(weights)==TRUE) {
    model <- lm(formula=formula, data=model_data, weights=NULL) #Create linear model, no weights
  } else {
    weights <- weights #Java upload weights as a vector of numeric values
    if (length(weights) == nrow(model_data)) { #There must be one weight for every row in the data set
      model <- lm(formula=formula, data=model_data, weights=weights) #Create linear model, with weights
    } else {
      #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.")
      stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") 
    }
  }
  
  #Store model
  model_name <- "Multivariate Linear Regression"
  mSetObj$analSet$linRegMulti$mod <- list(model=model, model_name=model_name, model.data=model_data, response=facA, predictor=predictors2)
  
  #Extract results
  fitted <- fitted(model) #Predicted values
  summary <- summary(model) #Summary includes coefficients, residuals and fit parameters
  residuals <- model$residuals # Get the residuals 
  conf.int <- confint(model, level=0.95) #Confidence intervals for predictor variables
  covar <- vcov(model) #Covariance matrix for preductor variables
  fileName <- "multivariate_linear_regression_summary.txt" #File name for summary, used by save.linReg1.summary()
  
  #Create equations and tabels
  coeffs <- as.data.frame(summary[["coefficients"]]) #Extract model coefficients
  coeffs$significance <- coeffs$`Pr(>|t|)`
  coeffs$significance[coeffs$significance < 0.001] <- "***"
  coeffs$significance[coeffs$significance < 0.01 & coeffs$significance >= 0.001] <- "**"
  coeffs$significance[coeffs$significance < 0.05 & coeffs$significance >= 0.01] <- "*"
  coef_values <- as.data.frame(round(coeffs[,1], 2))
  colnames(coef_values) <- "Coefficient"
  row.names(coef_values) <- row.names(coeffs)
  variables <- rownames(coef_values)
  intercept <- round(coef_values[1,1], 2)
  equations <- list()
  for (i in 2:nrow(coef_values)) {
    equation <- paste0(coef_values[i,1], "*", variables[i])
    equations[[i]] <- equation
  }
  equation <- paste(unlist(equations), collapse=" + ")
  equation <- paste0(facA, " = ", equation, " + ", intercept)
  r.squared <- summary[["adj.r.squared"]] #Extract R^2 value
  r_sq <- round(r.squared, 2)
  r.squared.eq <- paste("R-squared = ", r_sq) #Generate R^2 equation
  r.squared.adj <- summary[["adj.r.squared"]] #Extract adjusted R^2 value
  r_sq_adj <- round(r.squared.adj, 2)
  r.squared.adj.eq <- paste("R-squared adjusted = ", r_sq_adj) #Generate adjusted R^2 equation
  
  #Obtain test RMSE for plotting
  plot_prediction <- predict(model)
  plot_rmse <- rmse(model_data[,facA], plot_prediction)
  
  #Obtain relative importance of predictors for plotting
  if (ncol(model_data)==2) {
    importance <- data.frame(V1=100)
    colnames(importance) <- colnames(model_data)[2]
  } else {
    importance <- calc.relimp(model, type= "lmg", rela=TRUE)
  }

  #Test residuals for normality. Error will be visable to user.
  norm_resid <- shapiro.test(residuals)
  # if (norm_resid$p.value > 0.05){
  #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try choosing other independent and/or dependent variables, other data preprocessing options, or other regression models such as SVM or random forest.")
  #   stop("The residuals are NOT normally distributed. This model is invalid. Try choosing other independent and/or dependent variables, other data preprocessing options, or other regression models such as SVM or random forest.")
  # }
  
  #Store results
  mSetObj$analSet$linRegMulti$res <- list(summary=summary, response=facA, predictors=pred.text, model.data=model_data, equation=equation, r.squared.eq=r.squared.eq, r.squared.adj.eq=r.squared.adj.eq, r.squared=r_sq,
                                          coeff.tabel=coeffs, residuals=residuals, predicted.values=fitted, confidence.intervals=conf.int, covariance.matrix=covar, plot.rmse=plot_rmse, importance=importance, fileName=fileName) 
  
  #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink(fileName) 
  cat("Formula:\n")
  print(formula)
  cat("\nEquation:\n")
  cat(paste0(equation, "\n"))
  print(summary)
  cat("Residuals:\n")
  print(residuals)
  print(norm_resid)
  cat("Predicted values:\n")
  print(fitted)
  cat("\nConfidence intervals for predictor variables:\n")
  print(conf.int)
  cat("\nCovariance matrix for predictor variables:\n")
  print(covar)
  sink()
  
  return(.set.mSet(mSetObj))
  
}


#'Produce predicted/actual plot for multivariate regression
#'@description Scatter plot, where actual variables are y and predicted values are x
#'@usage plot.pred.linRegMulti(mSetObj, imgName, format="png", dpi=72, width=NA)
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

plot.pred.linRegMulti <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  model_name <- mSetObj$analSet$linRegMulti$mod$model_name
  model_data <- mSetObj$analSet$linRegMulti$mod$model.data
  prediction <- mSetObj$analSet$linRegMulti$res$predicted.values
  facA <- mSetObj$analSet$linRegMulti$res$response

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
  mSetObj$imgSet$plot.pred.linRegMulti <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(x=prediction, y=model_data[,facA], xlab=paste0("Predicted ", facA), ylab=paste0("Actual ", facA), main=model_name, yaxt="n")
  axis(2, las=2)
  abline(a=0,b=1)
  dev.off()
  
  return(.set.mSet(mSetObj))
}



#'Produce relative importance of predictors bar plot for multivariate regression
#'@description 
#'@usage plot.relaimpo.linRegMulti(mSetObj, imgName, format="png", dpi=72, width=NA)
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

plot.relaimpo.linRegMulti <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  facA <- mSetObj$analSet$linRegMulti$res$response
  model_data <- mSetObj$analSet$linRegMulti$mod$model.data
  importance <- mSetObj$analSet$linRegMulti$res$importance
  
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
  mSetObj$imgSet$plot.relaimpo.linRegMulti <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  if (ncol(model_data)==2) {
    barplot(as.matrix(importance), ylim=c(0,1), ylab="Proportion of R Squared", xlab="Predictors (Independent Variables)", yaxt="n", main=paste0("Relative Importance of Predictors for ", facA))
    axis(2, las=2)
  } else {
    barplot(importance@lmg, ylim=c(0,1), ylab="Proportion of R Squared", xlab="Predictors (Independent Variables)", yaxt="n", main=paste0("Relative Importance of Predictors for ", facA))
    axis(2, las=2)
  }
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

