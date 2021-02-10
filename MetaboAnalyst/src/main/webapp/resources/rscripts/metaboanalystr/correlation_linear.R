#'Perform Linear Regression'
#'@description Build a linear regression model for one term
#'@param mSetObj Input name of the created mSetObject 
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotLinearTable<-function(mSetObj=NA, weights=NULL){
  
  mSetObj <- .get.mSet(mSetObj)

  cat(paste0(mSetObj$dataSet$norm))

  formula <- mSetObj$dataSet$norm[,1] ~ mSetObj$dataSet$norm[,2] #Default response is first column. Terms should also be user selected once results are displayed. The # of columns will be determined by Col.Count(). Javascript will then display columns in drop down.
  
  if (is.null(weights)==TRUE) { #Weights is an optional secondary file uploaded by user, separate from data
    weights <- NULL
  }

  else {
    weights <- mSetObj$dataSet$weights
  }
  
  model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=NULL)
  
  fileName <- "linear_regression_summary_one_term.txt"
  
  results <-list(
    model = model,
    summary = summary(model), #actual table needing to be shown 
    tableName = fileName 
  )
  
  mSetObj$analSet$linReg1 <- results #results is in msetboject (holds summary table and table name)
  return(.set.mSet(mSetObj))

}
#returning a table

#'Plot line of best fit for linear regression with one term results
#'@description 
#'@usage Plot.linReg1(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotLinearGraph<-function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){

  mSetObj <- .get.mSet(mSetObj);
  response <- mSetObj$dataSet$norm[,1]
  #factor <- mSetObj$dataSet$facA
  factor <- mSetObj$dataSet$norm[,2]
  model <- mSetObj$analSet$linReg1$model
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");

  if(is.na(width)){
    w <- 7;
  }

  else if(width == 0){
    w <- 7;
  }

  else{
    w <- width;
  }

  mSetObj$imgSet$Plot.linReg1 <- imgName;

  h <- w;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(factor, response)
  abline(model)
  dev.off();
  return(.set.mSet(mSetObj));
  
}

#'Perform Linear Regression'
#'@description Build a linear regression model for one user selected predictor variable
#'@param mSetObj Input the name of the created mSetObj
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

lin.reg.anal.one <- function(mSetObj=NA, weights=NULL){
  
  mSetObj <- .get.mSet(mSetObj)
  #facA <- colnames(mSetObj$dataSet$norm)[2] #facA is the name of the first predictor variable (ie the column name)
  facA <- "Agrostol"
  cat("The first column will be the response variable.") #This should be visible to user
  cat(paste0(facA))  

  formula <- mSetObj$dataSet$norm[,1] ~ mSetObj$dataSet$norm[,"Agrostol"] #facA predictor variable should be user selected once results are displayed. The # and names of columns will be determined by Columns(). Javascript will then display columns in drop down.

  cat("After formula line")  

  if (is.null(weights)==TRUE) {
    model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=NULL) #Create linear model, no weights
    cat("Model was just created")
  } else {
    weights <- mSetObj$linReg$weights #Weights is held separate from dataSet, and can therefore only be accessed in the linear regression analysis pathway.
    weights <- unlist(weights) #Change weights from a dataframe into a vector of numeric values
    if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
      model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=weights) #Create linear model, with weights
    } else {
        cat("The length of the weights vector does not equal the number of rows in the data set!") #Error should be thrown so user can see it 
      }
    }
  cat("After everything")
  mSetObj$analSet$linReg1$mod <- model #Note where the model is stored
  summary <- summary(model) #Generate summary
  #fileName <- paste0("linear_regression_summary_", colnames(mSetObj$dataSet$norm)[1], "~", facA, ".txt") #File name for summary, used by save.linReg1.summary()
  #mSetObj$analSet$linReg1$sum <- list(summary=summary, fileName=fileName) #Note where the summary is stored
  
  return(.set.mSet(mSetObj))
  
}

plot.linReg1 <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  response_name <- colnames(mSetObj$dataSet$norm)[1] #For x-axis label
  predictor_name <- mSetObj$dataSet$facA #For y-axis label
  response <- mSetObj$dataSet$norm[,1] #First column is reponse variable by default
  predictor <- mSetObj$dataSet$norm[,facA] #User selects predictor variable in drop down menu
  model <- mSetObj$analSet$linReg1$mod #Obtained by lin.reg.anal.one()

  #Obtain equations for line of best fit and R^2 which will be displayed next to the plot
  coeffs <- mSetObj$analSet$linReg1$sum$summary$coefficients #Extract model coefficients
  equation <- paste("Y =", paste(round(coeffs[1],2), paste(round(coeffs[2],2), predictor_name, sep=" * ", collapse=" + "), sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
  equation <- gsub('\\+ -', '- ', gsub(' \\* ', '*', equation)) #Tidy equation
  r.squared <- mSetObj$analSet$linReg1$sum$summary$r.squared #Extract R^2 value
  r.squared.eq <- paste("R^2 = ", round(r_sq,2)) #Generate R^2 equation
  mSetObj$analSet$linReg1$eq <- list(equation=equation, r.squared.eq=r.squared.eq) #Note where the equations are stored
  
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  
  h <- w
  mSetObj$imgSet$Plot.linReg1 <- imgName
 
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(response, predictor, xlab=response_name, ylab=predictor_name, main="Univariate Linear Regression")
  abline(model)
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number of columns in dataset'
#'@description Jave will use the number of columns to enable user options
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

Col.Count <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  col.count <- ncol(mSetObj$dataSet$norm)
  return(col.count)
  
}

#'Determine number and names of columns in dataset'
#'@description Java will use the number of columns to enable user options
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

Columns <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  column.count <- ncol(mSetObj$dataSet$norm)-1 #Removing the first column (response variable) from the count
  column.names <- colnames(mSetObj$dataSet$norm)[-1] #Removing the first column (response variable) from the names
  
  column.results <- list(
    count=col.count,
    names=col.names
  )
  
  return(column.results)
  
}


#'Save linear regression model summary in txt file'
#'@description Saves linear regression model summary to the current working directory
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

save.linReg1.summary <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  summary <- mSetObj$analSet$linReg1$sum$summary 
  fileName <- mSetObj$analSet$linReg1$sum$fileName
  
  sink(fileName)
  print(summary)
  sink()
  
}