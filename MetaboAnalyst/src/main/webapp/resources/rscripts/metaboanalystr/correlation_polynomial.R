#'Return formula for a polynomial model, utility function used in poly.reg.anal'
#'@description Create equation from coefficients of a polynomial model
#'@param coeff coefficients from polynomial model
#'@param pwr highest power of polynomial model
#'@author Gina Sykes\email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

poly_eqn <- function(coeff, pwr){

#  if(length(coeff) != (pwr+1)){
#    stop("Length of input coefficients needs to be one more than input power")
#  }
  
  # a <- coeff
  i <- 1
  pp <- rev(seq(0, pwr, 1))
  yy <- vector(mode = "list", length = length(coeff))
  
while(i < (pwr+2)){

    if (coeff[i] > 0 & i!=1){
     ye<-   sprintf("+ ")
      }else if(a[i] < 0){
      ye<-  sprintf("- ")
      } else{
       ye <- sprintf("")
      }
  
  if(pp[i] == 0){
    yy[[i]] <- paste0(ye, sprintf("%.2f ", abs(coeff[i])) )
  }else if(pp[i] == 1 ){
    yy[[i]] <- paste0(ye, sprintf("%.2fx ",  abs(coeff[i]) ) )
   } else {
   yy[[i]] <- paste0(ye, sprintf("%.2fx^%d ", abs(coeff[i]), pp[i]) )
  }
   i <- i + 1
  }

 return( trimws(paste(unlist(yy, use.names=FALSE), collapse = "")) )
 
}


#'Perform Polynomial Regression'
#'@description Build polynomial regression models of various degrees for one user selected predictor variable
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses Columns() to give user options)
#'@param facB Input the name of the predictor column (java uses Columns() to give user options)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

poly.reg.anal <- function(mSetObj=NA, 
                          facA="NULL",
                          facB="NULL",
                          data="false"
                          
                          ){
  
  mSetObj <- .get.mSet(mSetObj)
  
  #Text should be visible to user
  cat("Two variables will be tested for correlation, a dependent variable and an independent variable. Both must have numeric values.")
  
 ### SET DATA (whether to use original data or not)
  if (data == "false") { 
   #  mSetObj$dataSet$norm <-  #default use norm
   #    mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
     input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  ### SET VARIABLES
  #SET DEPENDENT (RESPONSE) VARIABLE NAME
  if (facA == "NULL") {
    for (i in seq_len(ncol(input)) ){
      if (is.factor(input[,i+1]) == FALSE || is.character(input[,i+1]) == FALSE ) {
        facA <- colnames(input)[i]# Default: choose the 1st numeric column as response column
        break
      }
    }
  } else {
    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
  }
  
  #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
  if (facB == "NULL") {
    for (i in 1:ncol(input)) {
      if (is.factor(input[,i+1]) == FALSE || is.character(input[,i+1]) == FALSE ) {
        facB <- colnames(input)[i+1]# Default: choose the 2nd numeric column as response column
        break
      }
    }
  } else {
    facB <- facB #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
  }

  #DETERMINE MAX DEGREE
  facB.unique <- unique(input[,facB])
  max.deg <- length(facB.unique) - 1
  if (is.null(max.deg)) {
    max.deg <- 2 #Default max.deg is 2
  } else if (max.deg > 10){ #max.deg cannot exceed 10 to avoid over fitting
    max.deg <- 10 
  } else {
    max.deg <- max.deg 
  }
  
  #PRE-ALLOCATE CONTAINERS FOR RESULTS
  mSetObj$analSet$polyReg$mod <- list()
  mSetObj$analSet$polyReg$res <- list()
  #### CHANGE: V3 FROM name 'adjusted_R_squared' to 'R_squared_adjusted'
  df.R.sq <- c(V1="polynomial_degree", V2="R_squared", V3="R_squared_adjusted")
  
  for (i in 2:max.deg){
    
    #GENERATE MODEL
## change names from 'formula','model','summary', 'fitted' to 'form', 'mod','summ', 'fitt' to avoid scoping errors
    ### alpha(yint) + beta(slope) * x
    degree <- i
    form <- as.formula(paste0(facA, " ~ poly(", facB, ", ", degree, ")"))
    mod <- lm(formula = form, weights = NULL, data = input) #Create polynomial regression model
    model_name <- paste0("polynomial.degree.", degree, ".model")
    
    #EXTRACT RESULTS
    summ <- summary(mod) #PRINT #Summary w coeffs, resids & fit
    conf.int <- confint(mod, level = 0.95) #PRINT  #Conf intervals for predictor variables
    fitt <- fitted(mod) # PRINT #Predicted values
    coeffs <- summ[["coefficients"]] #Extract model coefficients

# GENERATE EQUATION using internal function
equation <- gsub("x", facB, 
            poly_eqn(coeff = coeffs[,"Estimate"], pwr = degree)  )

# #Generate equation
# if (degree == 2) {
#   alpha <- round(coeffs[1], digits = 2) #yint
#   beta.1 <- round(coeffs[2], digits = 2) #slope
#   beta.2 <- round(coeffs[3], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.2, paste0(facB, "^", degree), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create eqn: w/ intercept, coeff, pred name
# } else if (degree == 3) {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.3, paste0(facB, "^", degree), sep = "*"),
#                           paste(beta.2, paste0(facB, "^", degree-1), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# } else if (degree == 4) {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   beta.4 <- round(coeffs[5], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.4, paste0(facB, "^", degree), sep = "*"),
#                           paste(beta.3, paste0(facB, "^", degree-1), sep = "*"),
#                           paste(beta.2, paste0(facB, "^", degree-2), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# } else if (degree == 5) {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   beta.4 <- round(coeffs[5], digits = 2) 
#   beta.5 <- round(coeffs[6], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.5, paste0(facB, "^", degree), sep = "*"),
#                           paste(beta.4, paste0(facB, "^", degree-1), sep = "*"),
#                           paste(beta.3, paste0(facB, "^", degree-2), sep = "*"),
#                           paste(beta.2, paste0(facB, "^", degree-3), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# } else if (degree == 6) {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   beta.4 <- round(coeffs[5], digits = 2) 
#   beta.5 <- round(coeffs[6], digits = 2) 
#   beta.6 <- round(coeffs[7], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.6, paste0(facB, "^", degree), sep = "*"),
#                           paste(beta.5, paste0(facB, "^", degree-1), sep = "*"),
#                           paste(beta.4, paste0(facB, "^", degree-2), sep = "*"),
#                           paste(beta.3, paste0(facB, "^", degree-3), sep = "*"),
#                           paste(beta.2, paste0(facB, "^", degree-4), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# } else if (degree == 7) {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   beta.4 <- round(coeffs[5], digits = 2) 
#   beta.5 <- round(coeffs[6], digits = 2) 
#   beta.6 <- round(coeffs[7], digits = 2) 
#   beta.7 <- round(coeffs[8], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.7, paste0(facB, "^", degree), sep = "*"),
#                           paste(beta.6, paste0(facB, "^", degree-1), sep = "*"),
#                           paste(beta.5, paste0(facB, "^", degree-2), sep = "*"),
#                           paste(beta.4, paste0(facB, "^", degree-3), sep = "*"),
#                           paste(beta.3, paste0(facB, "^", degree-4), sep = "*"),
#                           paste(beta.2, paste0(facB, "^", degree-5), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# } else if (degree == 8) {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   beta.4 <- round(coeffs[5], digits = 2) 
#   beta.5 <- round(coeffs[6], digits = 2) 
#   beta.6 <- round(coeffs[7], digits = 2) 
#   beta.7 <- round(coeffs[8], digits = 2) 
#   beta.8 <- round(coeffs[9], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.8, paste0(facB, "^", degree-1), sep = "*"),
#                           paste(beta.7, paste0(facB, "^", degree-2), sep = "*"),
#                           paste(beta.6, paste0(facB, "^", degree-3), sep = "*"),
#                           paste(beta.5, paste0(facB, "^", degree-4), sep = "*"),
#                           paste(beta.4, paste0(facB, "^", degree-5), sep = "*"),
#                           paste(beta.3, paste0(facB, "^", degree-6), sep = "*"),
#                           paste(beta.2, paste0(facB, "^", degree-7), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# } else if (degree == 9) {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   beta.4 <- round(coeffs[5], digits = 2) 
#   beta.5 <- round(coeffs[6], digits = 2) 
#   beta.6 <- round(coeffs[7], digits = 2) 
#   beta.7 <- round(coeffs[8], digits = 2) 
#   beta.8 <- round(coeffs[9], digits = 2) 
#   beta.9 <- round(coeffs[10], digits = 2) 
#   equation <- paste(facA, " = ", 
#                     paste(paste(beta.9, paste0(facB, "^", degree), sep = "*"),
#                           paste(beta.8, paste0(facB, "^", degree-1), sep = "*"),
#                           paste(beta.7, paste0(facB, "^", degree-2), sep = "*"),
#                           paste(beta.6, paste0(facB, "^", degree-3), sep = "*"),
#                           paste(beta.5, paste0(facB, "^", degree-4), sep = "*"),
#                           paste(beta.4, paste0(facB, "^", degree-5), sep = "*"),
#                           paste(beta.3, paste0(facB, "^", degree-6), sep = "*"),
#                           paste(beta.2, paste0(facB, "^", degree-7), sep = "*"),
#                           paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# } else {
#   alpha <- round(coeffs[1], digits = 2)
#   beta.1 <- round(coeffs[2], digits = 2)
#   beta.2 <- round(coeffs[3], digits = 2) 
#   beta.3 <- round(coeffs[4], digits = 2) 
#   beta.4 <- round(coeffs[5], digits = 2) 
#   beta.5 <- round(coeffs[6], digits = 2) 
#   beta.6 <- round(coeffs[7], digits = 2) 
#   beta.7 <- round(coeffs[8], digits = 2) 
#   beta.8 <- round(coeffs[9], digits = 2) 
#   beta.9 <- round(coeffs[10], digits = 2) 
#   beta.10 <- round(coeffs[11], digits = 2) 
#   equation <- paste(facA, " = ", paste(paste(beta.10, paste0(facB, "^", degree), sep = "*"), paste(beta.9, paste0(facB, "^", degree-1), sep = "*"), paste(beta.8, paste0(facB, "^", degree-2), sep = "*"), paste(beta.7, paste0(facB, "^", degree-3), sep = "*"), paste(beta.6, paste0(facB, "^", degree-4), sep = "*"), paste(beta.5, paste0(facB, "^", degree-5), sep = "*"), paste(beta.4, paste0(facB, "^", degree-6), sep = "*"), paste(beta.3, paste0(facB, "^", degree-7), sep = "*"), paste(beta.2, paste0(facB, "^", degree-8), sep = "*"), paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
# }
    
     r_sq <- summ[["r.squared"]] #Extract R^2 # was stored in r.squared #was using 'adj.r.squared' ?
     r_sq_adj <- summ[["adj.r.squared"]]#Extract adjusted R^2 value # was in r.squared.adj
    # r.squared.eq <- paste("R-squared = ", r_sq) #Generate R^2 equation
    # r.squared.adj.eq <- paste("R-squared adjusted = ", r_sq_adj)
    # R.sq <- summary$r.squared #Extract R^2 value
    # R.sq.eq <- paste("R-squared = ", round(R.sq, digits = 2)) 
    # adj.R.sq <- summary$adj.r.squared #Extract R^2 value
    # adj.R.sq.eq <- paste("R-squared adjusted = ", round(adj.R.sq, digits = 2)) 
    # data.R.sq <- c(i, r_sq, r_sq_adj) # was c(i, R.sq, adj.R.sq)
 df.R.sq <- c(i, r_sq, r_sq_adj)
    df.R.sq <- rbind(df.R.sq, c(i, r_sq, r_sq_adj)  )

    
    fileName <- paste("polynomial_regession_summary",
 # degree, "_", facA, "-", facB, 
".txt", sep = "") #File name for summary, used by save.polyReg.summary()

    #Store results in mSetObj$analSet$polyReg
    list_index <- i-1
    mSetObj$analSet$polyReg$res[[list_index]] <- list(response = facA, predictor = facB, degree = degree, summary = summ, predicted.values = fitt, confidence.intervals = conf.int,  equation = equation,  r.squared.eq=paste("R-squared = ", round(r_sq, digits = 2)), r.squared.adj.eq=paste("R-squared adjusted = ", round(r_sq_adj, digits = 2)), fileName = fileName  )

     mSetObj$analSet$polyReg$mod[[list_index]] <- list(model.name = model_name, model = mod, formula = form, response = facA, predictor = facB)
    
    # mSetObj$analSet$polyReg$res[[list_index]] <- list(response = facA, predictor = facB, degree = degree, summary = summ, predicted.values = fitt, confidence.intervals = conf.int, fileName = fileName, equation = equation, R.sq.eq = R.sq.eq, adj.R.sq.eq = adj.R.sq.eq)
  
   
    ### Printing Values ## R CODE ERROR CHECK: MAKE SURE BACKSLASH IS FOLLOWED BY N
    # Text document, goes into wd; accessible as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(form)
    cat("Equation:\n")
    print(equation)
    print(summ)
    # cat("\nLinear Model Assumption Check:")
    # print(df)
    # print(failed)
    # print(norm_resid)
    # cat("Normality of errors result:\n")
    # cat(paste0(norm_resid_text, "\n"))
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    # cat("\nPredicted values:")
    # print(fitt)
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
#'@param degree Input the polynomial degree (determined by dropdown menu selection)
#'@param facA Input the name of the response column (java uses Columns() to give user options)
#'@param facB Input the name of the predictor column (java uses Columns() to give user options)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
###@param weights Set weight values, default is NULL
#' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
#'@param plot_eq Boolean, "false" (default) to show linear model equation on plot, "true" for plot without annotation of model equation (at top); y is 0.75*max(y) (checkbox)
#'@param plot_rsq Boolean, "false" (default) to show linear model rsq value on plot, "true" for plot without annotation of rsq value (at top); y is 0.75*max(y) (checkbox)
#'@param plot_rsq_adj Boolean, "true" to show linear model adjusted rsq value on plot, "false" (default) for plot without annotation of adjusted rsq value (at top); y is 0.75*max(y) (checkbox)
#'@param plot_title Input the name of the title (default: "Polynomial Regression: (formula);, textbox)
#'@param plot_xlab Input the name to use for x-axis label (default: facB, textbox)
#'@param plot_ylab Input the name to use for y-axis label (default: facA, textbox)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Gina Sykes\email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

poly.reg.plot <- function(mSetObj=NA, # was called plot.polyReg
                         degree="NULL", 
                         facA = "NULL",
                         facB="NULL",
                         data="false",
  col_dots="NULL",
  col_line="NULL", 
  plot_ci="false",
  plot_eq="false", 
  plot_rsq="false", 
  plot_rsq_adj="false",
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  imgName, format="png", dpi=72, width=NA){
  
library("ggplot2")
library("ggpmisc")
# library("stringr")
library("RJSONIO")

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
  ### SET DATA (whether to use original data or not)
  if (data=="false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #ASSIGN DEGREE
  if (degree == "NULL"){
    degree <- 2 #Default degree is 2
  } else{
    degree <- as.numeric(degree) #user defined in drop down menu. See Poly.Reg.Degrees() for more information
  }

#Extract plot components
 facA <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["response"]]#For x-axis label
 facB <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["predictor"]] #For y-axis label
 mod <- mSetObj$analSet$polyReg$mod[[list_index]][["model"]]
 form <- formula(mod)
 formula2 <- as.formula( gsub(facB, "x",
      gsub(facA, "y", deparse(form) ) 
         ))  
response <- input[,facA] #First column is reponse variable by default
predictor <- input[,facB] #Predictor column used for model construction by poly.reg.anal()


#    mSetObj$analSet$polyReg$res[[list_index]] <- list(response = facA, predictor = facB, degree = degree, summary = summ, predicted.values = fitt, confidence.intervals = conf.int,  equation = equation,  r.squared.eq=paste("R-squared = ", round(r_sq, digits = 2)), r.squared.adj.eq=paste("R-squared adjusted = ", round(r_sq_adj, digits = 2)), formula = formula, fileName = fileName  )   
  #     mSetObj$analSet$polyReg$mod[[list_index]] <- list(model.name = model_name, model = mod, response = facA, predictor = facB)



## ASSIGN LIST INDEX FOR SUBSETTING MSET 
list_index <- degree - 1

# ### SET VARIABLES
#   #SET DEPENDENT (RESPONSE) VARIABLE NAME
#   if (facA == "NULL") {
#      if( !"res" %in% names(mSetObj$analSet$polyReg) ){
#         facA <- mSetObj$analSet$polyReg$res[[list_index]][["response"]] #y
#      } else { 
# 
#     for (i in seq_along(colnames(input)) ){
#       if (is.factor(input[,i]) == FALSE) {
#         facA <- colnames(input)[i]# Default: choose the 1st numeric column as response column
#         break
#       }
#     }
# }
#   } else {
#     facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
#   }
#   
#   #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
#   if (facB == "NULL") {
#  if( !"res" %in% names(mSetObj$analSet$polyReg) ){
#         facB <- mSetObj$analSet$polyReg$res[[list_index]][["predictor"]] #x
#      } else { 
#     for (i in seq_along(colnames(input)) ) {
#       if (is.factor(input[,i+1]) == FALSE) {
#         facB <- colnames(input)[i+1]# Default: choose the 2nd numeric column as response column
#         break
#       }
#     }
#   }
# } else {
#     facB <- facB #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
#   }
# 
#    #GENERATE MODEL
#   list_index <- degree - 1     
#   formula <- as.formula(paste(facA, " ~ poly(", facB, ", ", degree, ")", sep = ""))
#   model <- lm(formula = formula, weights = NULL, data = input) #Create polynomial regression model
#   formula2 <- as.formula(
#    stringr::str_replace_all(deparse(formula), facA, "y") %>%
#      stringr::str_replace_all(., facB, "x") )
 


  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
 #NAME PLOT FOR DOWNLOAD
  # must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.polyReg <- imgName
  
#SET POINT COLOR
  col_dots1 <- 
				switch(
					col_dots,
					"NULL" = "black",
					"black" = "black",
					"blue" = "blue",
					"red" = "red",
					"green" = "green",
					"grey" = "grey",
					NULL
				)
  #SET LINE COLOR
   col_line1 <- 
				switch(
					col_line,
					"NULL" = "black",
					"black" = "black",
					"blue" = "blue",
					"red" = "red",
					"green" = "green",
					"grey" = "grey",
					NULL
				)
  
  # CONF INT (95%)
  if (plot_ci == "false") {
      plot_ci1 <- FALSE # default
    } else {
      plot_ci1 <- TRUE
    }

  # PLOT TITLE
  if(plot_title == " "){

## plotmath: r-bloggers.com/2018/03/math-notation-for-r-plot-titles-expression-and-bquote/
   # plot_title1 <- bquote("Polynomial Regression: "~.(facA) ~ "~"~.(facB)^.(degree)) 
  plot_title1 <- paste("Polynomial Regression: ", as.expression(form), sep = "")
  } else {
    plot_title1 <- plot_title
  }
  
  # PLOT YAXIS
  if(plot_ylab == " "){
    plot_ylab1 <- facA
  } else {
    plot_ylab1 <- plot_ylab
  }
  
  # PLOT XAXIS
  if(plot_xlab == " "){
    plot_xlab1 <- facB
  } else {
    plot_xlab1 <- plot_xlab
  }
  
  
  ## NOTE THE .data ARGUMENT; 
  ##  To avoid a note from CMD check about .data, use #' @importFrom rlang .data in any roxygen code block (typically in package documentation as generated by usethis::use_package_doc()) ; ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
  a0 <- ggplot(data =  input, #data.frame(fpred = prediction, fA = input[,facA]),aes(x = fpred, y = fA)) + # aes(x = #.data[[facB]], y = .data[[facA]]) ) +
   aes_(x = as.name(facB), y = as.name(facA)) )+
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, method = "lm", fullrange = TRUE, formula = formula2) +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )
  
 ## for JSON object:
if(plot_ci1 == TRUE){
aj <- a0
} else{
aj <- ggplot(data = input,
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
     aes_(x = as.name(facA), y = as.name(facB)) )+
     labs(title = plot_title1) + ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = TRUE, color = col_line1, fullrange = TRUE, method = 'lm', formula = formula2) +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text = element_text(size = 12, colour = "black"),  axis.title = element_text(size = 12), plot.title = element_text(face = 'bold', hjust = 0.5)  )
} 

### ADD 2ND LINE FOR EQUATION IF >> DEGREE POLYNOMIAL
#if(degree > 3){
#eq_sep <- "\n"
#} else {
#eq_sep <- "*\"  |  \"*"
#}
 
## {GGPMISC} ANNOTATION LOOP   
   if (plot_rsq_adj == "false"){ # default
  ### EQUATION, RSQ
   if (plot_eq != "false" && plot_rsq != "false") { 
      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label),  sep = "*\"  |  \"*")),
      formula = formula2,
      # eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
      # eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facA]) )
  ### EQUATION
    } else if (plot_eq != "false" && plot_rsq == "false") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
    paste0(after_stat(eq.label) )),
    formula = formula2,
    # eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
    # eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
    rr.digits = 2, coef.digits = 2, parse = TRUE,
    label.y = 0.75 * max(input[,facA]) ) 
  ### NOTHING
    } else if (plot_eq == "false" && plot_rsq == "false") { # default
      a0 <- a0
  ### RSQ
    } else if (plot_eq == "false" && plot_rsq != "false") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
      paste0(after_stat(rr.label) )),
      formula = formula2,
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facA]) ) 
    }
   
   } else { # RSQ_ADJ
  ### EQUATION, RSQ, RSQ_ADJ     
   if (plot_eq != "false" && plot_rsq != "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(adj.rr.label),
      sep = "*\"  |  \"*")), size = 3,
      formula = formula2,
      # eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),  
      # eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facA]) ) 
  ### EQUATION, RSQ_ADJ 
    } else if (plot_eq != "false" && plot_rsq == "false") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "*\"  |  \"*")),
      formula = formula2,
      # eq.with.lhs =  paste0("italic(`", facB, "`)~`=`~"),
      # eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facA]) ) 
  ### RSQ_ADJ 
    } else if (plot_eq == "false" && plot_rsq == "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
       paste0(after_stat(adj.rr.label) )),
       formula = formula2,
       rr.digits = 2, coef.digits = 2, parse = TRUE,
       label.y = 0.75 * max(input[,facA]) ) 
  ### RSQ, RSQ_ADJ 
    } else if (plot_eq == "false" && plot_rsq != "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(rr.label), after_stat(adj.rr.label),  sep = "*\"  |  \"*")),
       formula = formula2,
       rr.digits = 2, coef.digits = 2, parse = TRUE,
       label.y = 0.75 * max(input[,facA]) ) 
    } 
     
   }
   
  ### before making the plot
  # plot(y=response, x=predictor, ylab=facA, xlab=facB, main=paste0("Polynomial Regression Degree ", degree), yaxt="n"); axis(2, las=2)
  # lines(sort(predictor), fitted(model)[order(predictor)])
  
  #STORE IN mset
  mSetObj$analSet$polyReg$plot <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

#GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")

  print(a0)
  # a0
  dev.off()

# JSON MAKING
build <- ggplot_build(aj)
linear_plot_json <- list()

build_line <- build$data[[1]] ### line is 1
build_points <- build$data[[2]] # points is 2
linear_plot_json <- list()

linear_plot_json$main <- plot_title1 #title
linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
linear_plot_json$points$size <- build_points[,c("size")]#[,7]
linear_plot_json$lines$coords <- build_line[,c("x","y")]
linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
linear_plot_json$lines$size <- build_line[,c("size")]
 ## linear_plot_json$label <- build$data[[3]][,c("label")]
 ## linear_plot_json$lines$ci <- build$data[[1]][,c("se")]
### it should have CI
  if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 }   
  

## BOOLEANS
 linear_plot_json$bool_ci <- plot_ci1

if (plot_eq == "false"){ # default
 linear_plot_json$bool_eq <- FALSE
} else{
linear_plot_json$bool_eq <- TRUE
}

if (plot_rsq == "false"){ # default
 linear_plot_json$bool_rsq <- FALSE
} else{
linear_plot_json$bool_rsq <- TRUE
}

if (plot_rsq_adj == "false"){ # default
 linear_plot_json$bool_rsq_adj <- FALSE
} else{
linear_plot_json$bool_rsq_adj <- TRUE
}

#### MODEL VARS FOR LINE
  linear_plot_json$r_sq <-
    summary(mod)[["r.squared"]] #Extract R^2
  linear_plot_json$r_sq_adj <-
    summary(mod)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$slope <-
    summary(mod)[["coefficients"]][2] # beta
  linear_plot_json$yint <-
    summary(mod)[["coefficients"]][1] # alpha

 
 json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
 sink(imgName2)
 cat(json.obj)
 sink()
print(json.obj)
print(paste("PLOT1 | facA: ", facA, " | facB: ", facB, " | degree: ", degree, sep = ""))
print("I'm JSON the highway to hell")

if(!.on.public.web){
  return(.set.mSet(mSetObj))
    }
  
}


#'Produce predicted/actual plot for polynomial regression
#'@description Scatter plot, where actual variables are y and predicted values are x
#'@usage poly.pred.plot(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param degree Input the polynomial degree (determined by dropdown menu selection)
#'@param facA Input the name of the response column (java uses Columns() to give user options)
#'@param facB Input the name of the predictor column (java uses Columns() to give user options)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
###@param weights Set weight values, default is NULL
#' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
#'@param plot_title Input the name of the title (default: "Polynomial Regression Predicted vs Actual: (formula);, textbox)
#'@param plot_xlab Input the name to use for x-axis label (default: facB, textbox)
#'@param plot_ylab Input the name to use for y-axis label (default: facA, textbox)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.
#'@author Gina Sykes\email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export


poly.pred.plot <- function(mSetObj=NA,
                                degree="NULL", 
                         facA = "NULL",
                         facB="NULL",
                         data="false",
  col_dots="NULL",
  col_line="NULL", 
  plot_ci="false",
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  imgName, format="png", dpi=72, width=NA){
  
library("dplyr")
library("ggplot2")
#library("RJSONIO")

  ## was called: plot.pred.polyReg
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)

### SET DATA (whether to use original data or not)
  if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

 #ASSIGN DEGREE
  if (degree == "NULL"){
    degree <- 2 #Default degree is 2
  } else{
    degree <- as.numeric(degree) #user defined in drop down menu. See Poly.Reg.Degrees() for more information
  }

#Extract plot components
 facA <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["response"]]#For x-axis label
 facB <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["predictor"]] #For y-axis label
 mod <- mSetObj$analSet$polyReg$mod[[list_index]][["model"]]
 form <- formula(mod)
 formula2 <- as.formula( gsub(facB, "x",
      gsub(facA, "y", deparse(form) ) 
         ))  
response <- input[,facA] #First column is reponse variable by default
predictor <- input[,facB] #Predictor column used for model construction by poly.reg.anal()


#    mSetObj$analSet$polyReg$res[[list_index]] <- list(response = facA, predictor = facB, degree = degree, summary = summ, predicted.values = fitt, confidence.intervals = conf.int,  equation = equation,  r.squared.eq=paste("R-squared = ", round(r_sq, digits = 2)), r.squared.adj.eq=paste("R-squared adjusted = ", round(r_sq_adj, digits = 2)), formula = formula, fileName = fileName  )   
  #     mSetObj$analSet$polyReg$mod[[list_index]] <- list(model.name = model_name, model = mod, response = facA, predictor = facB)



## ASSIGN LIST INDEX FOR SUBSETTING MSET 
list_index <- degree - 1

# ### SET VARIABLES
#   #SET DEPENDENT (RESPONSE) VARIABLE NAME
#   if (facA == "NULL") {
#      if( !"res" %in% names(mSetObj$analSet$polyReg) ){
#         facA <- mSetObj$analSet$polyReg$res[[list_index]][["response"]] #y
#      } else { 
# 
#     for (i in seq_along(colnames(input)) ){
#       if (is.factor(input[,i]) == FALSE) {
#         facA <- colnames(input)[i]# Default: choose the 1st numeric column as response column
#         break
#       }
#     }
# }
#   } else {
#     facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
#   }
#   
#   #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
#   if (facB == "NULL") {
#  if( !"res" %in% names(mSetObj$analSet$polyReg) ){
#         facB <- mSetObj$analSet$polyReg$res[[list_index]][["predictor"]] #x
#      } else { 
#     for (i in seq_along(colnames(input)) ) {
#       if (is.factor(input[,i+1]) == FALSE) {
#         facB <- colnames(input)[i+1]# Default: choose the 2nd numeric column as response column
#         break
#       }
#     }
#   }
# } else {
#     facB <- facB #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
#   }
# 
#    #GENERATE MODEL
#   list_index <- degree - 1     
#   form <- as.formula(paste(facA, " ~ poly(", facB, ", ", degree, ")", sep = ""))
#   mod <- lm(formula = formula, weights = NULL, data = input) #Create polynomial regression model
#   formula2 <- as.formula(
#    stringr::str_replace_all(deparse(formula), facA, "y") %>%
#      stringr::str_replace_all(., facB, "x") )
   

 #GENERATE MODEL
  method <- "Polynomial Regression"
  formula1 <- as.formula(paste(facA, " ~ poly(", facB, ", ", degree, ")", sep = ""))
model1 <- lm(formula = formula1, weights = NULL, data = input) #Create polynomial regression model
prediction <- fitted(mod)

   dfpred <- data.frame(fpred = prediction, fA = input[,facA])
   formula <- as.formula("fA~fpred")
   model <- lm(formula = formula, data = dfpred, weights = NULL)

  # response <- input[,facA] #First column is reponse variable by default
  # predictor <- input[,facB] #Predictor column used for model construction by poly.reg.anal()
  # formula2 <- as.formula(
  # stringr::str_replace_all(deparse(formula1), facA, "y") %>%
  #   stringr::str_replace_all(., facB, "x") )
  # prediction <- mSetObj$analSet$polyReg$res[[list_index]][["predicted.values"]]  


  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
 #NAME PLOT FOR DOWNLOAD
  # must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.linRegMulti <- imgName 
  
#SET POINT COLOR
  col_dots1 <- 
				switch(
					col_dots,
					"NULL" = "black",
					"black" = "black",
					"blue" = "blue",
					"red" = "red",
					"green" = "green",
					"grey" = "grey",
					NULL
				)
  #SET LINE COLOR
   col_line1 <- 
				switch(
					col_line,
					"NULL" = "black",
					"black" = "black",
					"blue" = "blue",
					"red" = "red",
					"green" = "green",
					"grey" = "grey",
					NULL
				)
  
  # CONF INT (95%)
  if (plot_ci == "false") {
      plot_ci1 <- FALSE # default
    } else {
      plot_ci1 <- TRUE
    }

  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste("Polynomial Regression: Predicted vs Actual (", as.expression(formula1), ")", sep = "")
  } else {
    plot_title1 <- plot_title
  }
  
  ## y actual input[,facA] fA
  ## x prediction fpred
  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Actual"
  } else { # facA, response
    plot_ylab1 <- plot_ylab
  }

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Predicted"
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
  
    #   plot(x=prediction, y=mSetObj$dataSet$norm[,facA], xlab="Predicted", ylab="Actual", main=method, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
  
  ## y actual input[,facA] fA
  ## x prediction fpred
 # a0 <- ggplot(data =  data.frame(
 #   fpred = prediction, fA = input[,facA]),
 # aes(x = fpred, y = fA)) +

 a0 <- ggplot(data =  dfpred, 
  aes(x = fpred, y = fA)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, method = "lm", fullrange = TRUE) +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )


## for JSON object:
if(plot_ci1 == TRUE){
aj <- a0
} else{
aj <- ggplot(data = dfpred,
  aes(x = fpred, y = fA)) +
     labs(title = plot_title1) + ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = TRUE, color = col_line1, fullrange = TRUE, method = 'lm') +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text = element_text(size = 12, colour = "black"),  axis.title = element_text(size = 12), plot.title = element_text(face = 'bold', hjust = 0.5)  )
} 


     #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   
#STORE IN mset
  mSetObj$analSet$linReg1$plotpred <- list(plot= a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  print(a0)
  # a0
  dev.off()


# JSON MAKING
build <- ggplot_build(aj)
linear_plot_json <- list()

build_line <- build$data[[1]] ### line is 1
build_points <- build$data[[2]] # points is 2
linear_plot_json <- list()

linear_plot_json$main <- plot_title1 #title
linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
linear_plot_json$points$size <- build_points[,c("size")]#[,7]
linear_plot_json$lines$coords <- build_line[,c("x","y")]
linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
linear_plot_json$lines$size <- build_line[,c("size")]
 ## linear_plot_json$label <- build$data[[3]][,c("label")]
 ## linear_plot_json$lines$ci <- build$data[[1]][,c("se")]
### it should have CI
  if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 }   


## BOOLEANS
if(plot_ci1 == TRUE){
 linear_plot_json$bool_ci <- TRUE
 } else{
linear_plot_json$bool_ci <- FALSE
}

#### MODEL VARS FOR LINE
  linear_plot_json$r_sq <-
    summary(model)[["r.squared"]] #Extract R^2
  linear_plot_json$r_sq_adj <-
    summary(model)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$slope <-
    summary(model)[["coefficients"]][2] # beta
  linear_plot_json$yint <-
    summary(model)[["coefficients"]][1] # alpha

 json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
 sink(imgName2)
 cat(json.obj)
 sink()
print(json.obj)
print(paste("PLOT2| facA: ", facA, " | facB: ", facB, " | degree: ", degree , sep = ""))
print("Hey little sister who's the only json")

if(!.on.public.web){
  return(.set.mSet(mSetObj))
    }
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
  
  data <- dplyr::select_if(mSetObj$dataSet$norm, is.numeric)
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
  list_index <- as.numeric(degree) - 1

  lin.reg.result <- c(mSetObj$analSet$polyReg$res[[list_index]][["equation"]],
                  mSetObj$analSet$polyReg$res[[list_index]][["R.sq.eq"]], 
                  mSetObj$analSet$polyReg$res[[list_index]][["adj.R.sq.eq"]])
  return(lin.reg.result)

}