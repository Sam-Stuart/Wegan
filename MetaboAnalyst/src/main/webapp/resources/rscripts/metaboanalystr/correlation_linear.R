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
  formula <- mSetObj$dataSet$norm[,1] ~ mSetObj$dataSet$facA #Default response is first column. Terms should also be user selected once results are displayed. The # of columns will be determined by Col.Count(). Javascript will then display columns in drop down.
  
  if (is.null(weights)==TRUE) { #Weights is an optional secondary file uploaded by user, separate from data
    weights <- NULL
  }

  else {
    weights <- mSetObj$dataSet$weights
  }
  
  model <- lm(formula=formula, data=mSetObj, weights=NULL)
  
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
  factor <- mSetObj$dataSet$facA
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
  plot(response, factor)
  abline(model)
  dev.off();
  return(.set.mSet(mSetObj));
  
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