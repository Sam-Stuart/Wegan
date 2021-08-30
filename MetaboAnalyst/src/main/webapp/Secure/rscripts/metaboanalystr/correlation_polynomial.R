#'Perform Polynomial Regression'
#'@description Build polynomial regression models of various degrees for one user selected predictor variable
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param facB Input the name of the predictor column (java uses numeric.columns() to give user options)
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

poly.reg.anal <- function(mSetObj=NA, facA=NULL, facB=NULL, weights=NULL){
  
  mSetObj <- .get.mSet(mSetObj)
  
  #Text should be visable to user
  cat("Two variables will be tested for correlation, a dependent variable and an independent variable. Both must have numeric values.")
  mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
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
  
  #Set independent (predictor) variable name
  if (facB=="NULL") {
    for (i in 1:ncol(mSetObj$dataSet$norm)) {
      if (is.factor(mSetObj$dataSet$norm[,i+1])==FALSE) {
        facB <- colnames(mSetObj$dataSet$norm)[i+1]# Default is to choose the second numeric column as response column
        break
      }
    }
  } else {
    facB <- facB #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
  }

  #Determine maximum degree
  facB.unique <- unique(mSetObj$dataSet$norm[,facB])
  max.deg <- length(facB.unique)-1
  if (is.null(max.deg)) {
    max.deg <- 2 #Default max.deg is 2
  } else if (max.deg > 10){ #max.deg cannot exceed 10 to avoid over fitting
    max.deg <- 10 
  } else {
    max.deg <- max.deg 
  }

  
  #Define containers for results
  mSetObj$analSet$polyReg$mod <- list()
  mSetObj$analSet$polyReg$res <- list()
  df.R.sq <- c(V1="polynomial_degree", V2="R_squared", V3="adjusted_R_squared")
  
  for (i in 2:max.deg){
    
    #Generate model
    degree <- i
    formula <- as.formula(paste0(facA, " ~ poly(", facB, ", ", degree, ")"))
    model <- lm(formula=formula, weights=NULL, data=mSetObj$dataSet$norm) #Create polynomial regression model
    
    model_name <- paste0("polynomial.degree.", degree, ".model")
    
    #Extract results
    summary <- summary(model) #Summary includes coefficients, residuals and fit parameters
    conf.int <- confint(model, level=0.95) #Confidence intervals for predictor variables
    residuals <- model$residuals # Get the residuals 
    
    
    coeffs <- summary[["coefficients"]] #Extract model coefficients

    #Generate equation
    if (degree==2) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.2, paste0(facB, "^", degree), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree==3) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.3, paste0(facB, "^", degree), sep="*"), paste(beta.2, paste0(facB, "^", degree-1), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree==4) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      beta.4 <- round(coeffs[5], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.4, paste0(facB, "^", degree), sep="*"), paste(beta.3, paste0(facB, "^", degree-1), sep="*"), paste(beta.2, paste0(facB, "^", degree-2), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree==5) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      beta.4 <- round(coeffs[5], 2) 
      beta.5 <- round(coeffs[6], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.5, paste0(facB, "^", degree), sep="*"), paste(beta.4, paste0(facB, "^", degree-1), sep="*"), paste(beta.3, paste0(facB, "^", degree-2), sep="*"), paste(beta.2, paste0(facB, "^", degree-3), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree==6) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      beta.4 <- round(coeffs[5], 2) 
      beta.5 <- round(coeffs[6], 2) 
      beta.6 <- round(coeffs[7], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.6, paste0(facB, "^", degree), sep="*"), paste(beta.5, paste0(facB, "^", degree-1), sep="*"), paste(beta.4, paste0(facB, "^", degree-2), sep="*"), paste(beta.3, paste0(facB, "^", degree-3), sep="*"), paste(beta.2, paste0(facB, "^", degree-4), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree==7) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      beta.4 <- round(coeffs[5], 2) 
      beta.5 <- round(coeffs[6], 2) 
      beta.6 <- round(coeffs[7], 2) 
      beta.7 <- round(coeffs[8], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.7, paste0(facB, "^", degree), sep="*"), paste(beta.6, paste0(facB, "^", degree-1), sep="*"), paste(beta.5, paste0(facB, "^", degree-2), sep="*"), paste(beta.4, paste0(facB, "^", degree-3), sep="*"), paste(beta.3, paste0(facB, "^", degree-4), sep="*"), paste(beta.2, paste0(facB, "^", degree-5), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree==8) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      beta.4 <- round(coeffs[5], 2) 
      beta.5 <- round(coeffs[6], 2) 
      beta.6 <- round(coeffs[7], 2) 
      beta.7 <- round(coeffs[8], 2) 
      beta.8 <- round(coeffs[9], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.8, paste0(facB, "^", degree), sep="*"), paste(beta.7, paste0(facB, "^", degree-1), sep="*"), paste(beta.6, paste0(facB, "^", degree-2), sep="*"), paste(beta.5, paste0(facB, "^", degree-3), sep="*"), paste(beta.4, paste0(facB, "^", degree-4), sep="*"), paste(beta.3, paste0(facB, "^", degree-5), sep="*"), paste(beta.2, paste0(facB, "^", degree-6), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree==9) {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      beta.4 <- round(coeffs[5], 2) 
      beta.5 <- round(coeffs[6], 2) 
      beta.6 <- round(coeffs[7], 2) 
      beta.7 <- round(coeffs[8], 2) 
      beta.8 <- round(coeffs[9], 2) 
      beta.9 <- round(coeffs[10], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.9, paste0(facB, "^", degree), sep="*"), paste(beta.8, paste0(facB, "^", degree-1), sep="*"), paste(beta.7, paste0(facB, "^", degree-2), sep="*"), paste(beta.6, paste0(facB, "^", degree-3), sep="*"), paste(beta.5, paste0(facB, "^", degree-4), sep="*"), paste(beta.4, paste0(facB, "^", degree-5), sep="*"), paste(beta.3, paste0(facB, "^", degree-6), sep="*"), paste(beta.2, paste0(facB, "^", degree-7), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    } else {
      alpha <- round(coeffs[1], 2)
      beta.1 <- round(coeffs[2], 2)
      beta.2 <- round(coeffs[3], 2) 
      beta.3 <- round(coeffs[4], 2) 
      beta.4 <- round(coeffs[5], 2) 
      beta.5 <- round(coeffs[6], 2) 
      beta.6 <- round(coeffs[7], 2) 
      beta.7 <- round(coeffs[8], 2) 
      beta.8 <- round(coeffs[9], 2) 
      beta.9 <- round(coeffs[10], 2) 
      beta.10 <- round(coeffs[11], 2) 
      equation <- paste(facA, " = ", paste(paste(beta.10, paste0(facB, "^", degree), sep="*"), paste(beta.9, paste0(facB, "^", degree-1), sep="*"), paste(beta.8, paste0(facB, "^", degree-2), sep="*"), paste(beta.7, paste0(facB, "^", degree-3), sep="*"), paste(beta.6, paste0(facB, "^", degree-4), sep="*"), paste(beta.5, paste0(facB, "^", degree-5), sep="*"), paste(beta.4, paste0(facB, "^", degree-6), sep="*"), paste(beta.3, paste0(facB, "^", degree-7), sep="*"), paste(beta.2, paste0(facB, "^", degree-8), sep="*"), paste(beta.1, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
    }
    
    fitted <- fitted(model) #Predicted values
    r.squared <- summary[["adj.r.squared"]] #Extract R^2 value
    r_sq <- round(r.squared, 2)
    r.squared.eq <- paste("R-squared = ", r_sq) #Generate R^2 equation
    r.squared.adj <- summary[["adj.r.squared"]] #Extract adjusted R^2 value
    r_sq_adj <- round(r.squared.adj, 2)
    r.squared.adj.eq <- paste("R-squared adjusted = ", r_sq_adj) #Generate adjusted R^2 equation
    R.sq <- summary$r.squared #Extract R^2 value
    R.sq.eq <- paste("R-squared = ", round(R.sq,2)) #Generate R^2 equation, to be called upon later as needed
    adj.R.sq <- summary$adj.r.squared #Extract R^2 value
    adj.R.sq.eq <- paste("R-squared adjusted = ", round(adj.R.sq,2)) #Generate R^2 equation, to be called upon later as needed
    data.R.sq <- c(i, R.sq, adj.R.sq)
    df.R.sq <- rbind(df.R.sq, data.R.sq)

    
    fileName <- paste0("polynomial_regession_summary_degree_", degree, "_", facA, "~", facB, ".txt") #File name for summary, used by save.polyReg.summary()

    #Store results in mSetObj$analSet$polyReg
    list_index <- i-1
    mSetObj$analSet$polyReg$res[[list_index]] <- list(response=facA, predictor=facB, degree=degree, summary=summary, predicted.values=fitted, confidence.intervals=conf.int, fileName=fileName, equation=equation, R.sq.eq=R.sq.eq, adj.R.sq.eq=adj.R.sq.eq)
    mSetObj$analSet$polyReg$mod[[list_index]] <- list(model_name=model_name, model=model, response=facA, predictor=facB)

    #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    print(summary)
    # print(norm_resid)
    # cat("Normality of errors result:\n")
    # cat(paste0(norm_resid_text, "\n"))
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    sink()
    
  } 

  #Create table comparing R stats for all models produced
  colnames(df.R.sq) <- df.R.sq[1,]
  write.csv(df.R.sq[-1,], file=paste0("polynomial_regression_R_squared_values_",  facA, "~", facB, ".csv"), row.names=FALSE)
  
  return(.set.mSet(mSetObj))
  
}


#'Plot line of best fit for polynomial regression with one predictor variable
#'@description Scatter plot with line of best fit, where response variable is y and predictor variable is x
#'@usage Plot.polyReg(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input the image name
#'@param degree Input the polynomial degree (determined by dropdown menu selection)
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

plot.polyReg <- function(mSetObj=NA, degree=NULL, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
  #Assign degree
  if (is.null(degree)){
    degree <- 2 #Default degree is 2
  } else{
    degree <- degree #Value needs to be user defined in drop down menu. See Poly.Reg.Degrees() for more information
  }

  #Extract plot components
  list_index <- degree - 1
  facA <- mSetObj$analSet$polyReg$res[[list_index]][["response"]]
  facB <- mSetObj$analSet$polyReg$res[[list_index]][["predictor"]]
  #facA <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["response"]]#For x-axis label
  #facB <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["predictor"]] #For y-axis label
  response <- mSetObj$dataSet$norm[,facA] #First column is reponse variable by default
  predictor <- mSetObj$dataSet$norm[,facB] #Predictor column used for model construction by poly.reg.anal()
  #model <- mSetObj[["analSet"]][["polyReg"]][["mod"]][[1]][["model"]] #Obtained by poly.reg.anal()
  model <- mSetObj$analSet$polyReg$mod[[list_index]][["model"]]
  
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
  mSetObj$imgSet$plot.polyReg <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(y=response, x=predictor, ylab=facA, xlab=facB, main=paste0("Polynomial Regression Degree ", degree), yaxt="n")
  axis(2, las=2)
  lines(sort(predictor), fitted(model)[order(predictor)])
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


#'Produce predicted/actual plot for polynomial regression
#'@description Scatter plot, where actual variables are y and predicted values are x
#'@usage plot.pred.polyReg(mSetObj, imgName, format="png", dpi=72, width=NA)
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

plot.pred.polyReg <- function(mSetObj=NA, degree=NULL, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)

  #Assign degree
  if (is.null(degree)){
    degree <- 2 #Default degree is 2
  } else{
    degree <- degree #Value needs to be user defined in drop down menu. See Poly.Reg.Degrees() for more information
  }

  list_index <- degree-1
  method <- "Polynomial Regression"
  prediction <- mSetObj$analSet$polyReg$res[[list_index]][["predicted.values"]]
  #facA <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["response"]] #For x-axis label
  facA <- mSetObj$analSet$polyReg$res[[list_index]][["response"]]
  
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
  plot(x=prediction, y=mSetObj$dataSet$norm[,facA], xlab="Predicted", ylab="Actual", main=method, yaxt="n")
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

#'Determine number and names of numeric variables for polynomial regression'
#'@description Java will use this function to enable user options for selecting dependent and independent variables
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

poly.numeric.columns <- function(mSetObj=NA){
  
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


#'Determine number of possible polynomial degrees'
#'@description Java will use the number of possible degrees to enable user options
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

Poly.Reg.Degrees <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  facB <- mSetObj$analSet$polyReg$res[[1]][["predictor"]]
  facB.unique <- unique(mSetObj$dataSet$norm[,facB])
  max.deg <- length(facB.unique)-1  
  degree.list <- 2:max.deg
  
  return(degree.list)
  
}

poly.reg.get.results <- function(mSetObj=NA, degree=NULL){

  mSetObj <- .get.mSet(mSetObj)

  #Assign degree
  if (is.null(degree)){
    degree <- 2 #Default degree is 2
  } else{
    degree <- degree #Value needs to be user defined in drop down menu. See Poly.Reg.Degrees() for more information
  }

  #Extract plot components
  list_index <- degree - 1

  lin.reg.result <- c(mSetObj$analSet$polyReg$res[[list_index]][["equation"]], mSetObj$analSet$polyReg$res[[list_index]][["R.sq.eq"]], mSetObj$analSet$polyReg$res[[list_index]][["adj.R.sq.eq"]])
  return(lin.reg.result)

}