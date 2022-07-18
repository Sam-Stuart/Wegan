#'Generate linear regression model
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses Columns() to give user options)
#'@param facB Input the name of the predictor column (java uses Columns() to give user options)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#' @importFrom rlang .data
#'@export
lin.reg.anal <- function(mSetObj = NA,
                         facA = "NULL",
                         facB = "NULL",
             data = "false"
             
  ){

  library("lmtest")
  
  mSetObj <- .get.mSet(mSetObj)
  
  # mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
  
 ### SET DATA (whether to use original data or not)
  if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  ### SET VARIABLES
  #Set dependent (response) variable name
  if (facA == "NULL"){
    facA <- colnames(input)[1] #Default is 1st column.
  } else {
    facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
  }
  #Set independent (predictor) variable name
  if (facB == "NULL"){
    facB <- colnames(input)[2] #Default is 2nd column.
  } else {
    facB <- facB #Determined using Columns() function below (java will present options in drop down menu)
  }
  
  # VARIABLE TYPE CHECK
  if (is.factor(input[,facA] || input[,facB]) == TRUE){
    #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
    stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
  }

  #DEFINE FORMULA
  formula <- as.formula(paste0(facA, "~", facB)) 

  #GENERATE MODEL, with or without weights
  # if (is.null(weights)==TRUE) {
    model <- lm(formula = formula, data = input, weights = NULL) #Create linear model, no weights
 #  } else {
 #    weights <- weights #Java upload weights as a vector of numeric values
 #    if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
 #      model <- lm(formula=formula, data=input, #mSetObj$dataSet$norm,
 # weights=weights) #Create linear model, with weights
 #    } else {
 #      #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
 #      stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
 #    }
 #  }
 
    
  ## alpha(yint) + beta(slope) * x
  # EXTRACTING MODEL RESULTS
  # later, print()/cat() these:
  summary <- summary(model) #PRINT #Summary w coeff, resid & fit
  fitted <- fitted(model) #PRINT
  covar <- vcov(model) #PRINT
  conf.int <- confint(model, level=0.95) #PRINT
  
  fileName <- paste0("linear_regression_summary", ".txt") #File name for summary
# fileName <- paste0("linear_regression_summary_", facA, "~", facB, ".txt") #File name for summary
  coeffs <- summary[["coefficients"]] #Extract model coefficients
  beta <- round(coeffs[2], digits = 2)
  alpha <- round(coeffs[1], digits = 2)
  equation <- paste(facA, " = ",
 paste( paste(beta, facB, sep="*"), alpha, sep=" + ") ) # equation with intercept, coefficient and predictor variable name
  r_sq <- round(summary[["r.squared"]], digits = 2) #Extract R^2
  r_sq_adj <- round(summary[["adj.r.squared"]], digits = 2) #Extract adjusted R^2 value

  ##### - MODEL ASSUMPTIONS TESTS - #####
  mod_shp <- stats::shapiro.test(model$residuals)$p.value
  mod_bp <- lmtest::bptest(model)$p.value
  mod_dw <- lmtest::dwtest(model)$p.value

  mod <- c(mod_shp, mod_bp, mod_dw)
  names(mod) <- c("Shapiro-Wilk test for Normality of Residuals", "Beusch-Pagan test for Homoscedasticity", "Durbin-Watson test for Autocorrelation (Independent Residual Errors)")
  fix<-c("Try other preprocessing options, or try other regression models such as SVM or random forest.",
  "Try transforming the dependent variable (e.g., log transformation), or try redefining the dependent variable as a rate.",
  "Is this time series data? Try looking into adding a lag in the independent/dependent variable, or adding a seasonal dummy variable to the model."
  )
  mod1 <- mod # numeric
  n_fail <- sum(mod < 0.05) # was: nassump_fail
  
  if(any(mod < 0.001)){ # format numbers for printing; 3 digits after decimal
  mod[mod < 0.001] <-
   formatC( mod[mod < 0.001] , format = "e", digits = 3)
  
  mod[mod1 > 0.001] <-
   round( mod1[mod1 > 0.001], digits = 3)
  }
   
  if(n_fail > 0){ # if any failed tests
   mod <- mod[mod1 < 0.05] # subset for failed tests
  
   f0 <- paste0(n_fail, " linear model assumption test(s) failed: \n")
   
 failed<-c(f0, paste0( 
         names(mod), " (P-Value: ", mod, ")/n", fix[mod1 < 0.05], "\n" ) )
      # "Please be advised that conforming to these assumptions is necessary for use of the linear model. If the goal is to visually explore your data, try the Plotting module." 
    #AddErrMsg(failed)
    message(failed)
    } else {
    failed <- paste0("No model assumption tests failed.")
    }
  
### based on: # https://github.com/rempsyc/rempsyc/blob/main/R/nice_assumptions.R
  df <- data.frame("Normality (Shapiro-Wilk)..." = mod_shp,
          "Homoscedasticity (Breusch-Pagan)..." = mod_bp,
           "Autocorrelation of Residuals (Durbin-Watson)..." = mod_dw,
           "N Assumptions Failed..." = n_fail,
                   check.names = FALSE)
  row.names(df) <- NULL
  ##### - MODEL ASSUMPTIONS TEST DONE - ####
  
  # #Test residuals for normality. Error will be visible to user.
  # norm_resid <- shapiro.test(residuals) 
  # if (norm_resid$p.value < 0.05){
  #   norm_resid_text <- paste0("The residuals are normally distributed. This model is valid.") #To be used in summary, not to be displayed
  # } else {
  #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  #   stop("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  # }
 
   # STORE MODEL
  mSetObj$analSet$linReg1$mod <- model

  #STORE RESULTS: x/y var names, model summary, fitted values,conf intervals, covar mat, equation, rsq, rsq_adj, filename, formula
  ### change response to xlab, predictor to ylab
  mSetObj$analSet$linReg1$res <- list(response = facA, predictor = facB, summary = summary, predicted.values = fitted, confidence.intervals = conf.int, covariance.matrix = covar, equation = equation, r.squared.eq = paste("R-squared = ", r_sq), r.squared.adj.eq = paste("R-squared adjusted = ", r_sq_adj),
          assumptions = df,  fileName = fileName, formula = formula ) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  
    
### Printing Values ## ERROR CHECK: BACKSLASH FOLLOWED BY N
  sink(fileName)
  cat("Formula:\n")
  print(formula)
  cat("Model:\n")
  print(equation)
  cat("\nLinear Model Assumption Check:")
  print(df)
  print(failed)
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



#'Generate linear regression plot
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#'@param mSetObj Input the name of the created mSetObj
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)

#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
###@param weights Set weight values, default is NULL
#' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
#'@param plot_eq Boolean, "false" (default) to show linear model equation on plot, "true" for plot without annotation of model equation (at top); y is 0.75*max(y) (checkbox)
#'@param plot_rsq Boolean, "false" (default) to show linear model rsq value on plot, "true" for plot without annotation of rsq value (at top); y is 0.75*max(y) (checkbox)
#'@param plot_rsq_adj Boolean, "true" to show linear model adjusted rsq value on plot, "false" (default) for plot without annotation of adjusted rsq value (at top); y is 0.75*max(y) (checkbox)
#'@param plot_title Input the name of the title (default: "Univariate Linear Regression Line of Best Fit", textbox)
#'@param plot_xlab Input the name to use for x-axis label (default: facA, textbox)
#'@param plot_ylab Input the name to use for y-axis label (default: facB, textbox)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#' @importFrom rlang .data
#'@export
lin.reg.plot <- function(mSetObj=NA,
                        # facA = "NULL",
                        # facB = "NULL",
             data = "false",
             
  col_dots = "NULL",
  col_line = "NULL",
  plot_ci = "false",# weights=NULL,
  plot_eq = "false",
  plot_rsq = "false",
  plot_rsq_adj = "false",
  plot_title = " ",
  plot_ylab = " ",
  plot_xlab = " ",
  
  imgName,
  format = "png",
  dpi = 72,
  width = NA
  ){

  library("ggpmisc")
  library("ggplot2")
  
  mSetObj <- .get.mSet(mSetObj)
  
  ## DATA: NORMAL OR NOT
   if (data=="false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  
  #RETRIEVE RESULTS: 
 facA <- mSetObj$analSet$linReg1$res$response
 facB <-  mSetObj$analSet$linReg1$res$predictor
   
   fileName <- mSetObj$analSet$linReg1$res$fileName
   # predicted.values <- mSetObj$analSet$linReg1$res$predicted.values
   formula <- mSetObj$analSet$linReg1$res$formula
   model <- mSetObj$analSet$linReg1$mod   
 
  
  # PLOT

  #SET PLOT DIMENSIONS
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
  
    #SET POINT COLOR
  if (col_dots == "NULL") {
      col_dots1 <- "black" # default
    } else if (col_dots == "blue") {
      col_dots1 <- "blue"
    } else if (col_dots == "red") {
      col_dots1 <- "red"
    } else if(col_dots == "green"){
      col_dots1 <- "green"
    } else if(col_dots == "grey"){
      col_dots1 <- "grey"
    }
  
  #SET LINE COLOR
  if (col_line == "NULL") {
      col_line1 <- "black" # default
    } else if (col_line == "blue") {
      col_line1 <- "blue"
    } else if (col_line == "red") {
      col_line1 <- "red"
    } else if(col_line == "green"){
      col_line1 <- "green"
    } else if(col_line == "grey"){
      col_line1 <- "grey"
    }
  
  #SET WHETHER TO ADD 95% CONF INT
  if (plot_ci == "false") {
      plot_ci1 <- FALSE # default
    } else {
      plot_ci1 <- TRUE
    }

  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- "Univariate Linear Regression Line of Best Fit"
  } else {
    plot_title1 <- plot_title
  }
  
  # PLOT YAXIS
  if(plot_ylab == " "){
    plot_ylab1 <- facB
  } else {
    plot_ylab1 <- plot_ylab
  }
  
  # PLOT XAXIS
  if(plot_xlab == " "){
    plot_xlab1 <- facA
  } else {
    plot_xlab1 <- plot_xlab
  }
  
  
  # #IF THERE IS A LINE COLOR, OVERRIDE DOT COLOR TO BLACK
  # if(!all.equal(col_line1, "black")){
  #   col_dots1 <- "black"
  # }
 
  # THEME 
#  theme_lineplot <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size=12, colour="black"),    axis.title=element_text(size=12), legend.title=element_text(12), legend.text=element_text(size=12), plot.title=element_text(face='bold',hjust = 0.5)
#   )
  ## NOTE THE .data ARGUMENT; 
  ##  To avoid a note from CMD check about .data, use #' @importFrom rlang .data in any roxygen code block (typically in package documentation as generated by usethis::use_package_doc()) ; https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
   a0 <- ggplot(data = input,
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
     aes_(x = as.name(facA), y = as.name(facB)) )+
     labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE, method = 'lm') +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )

     #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   
  
# stat_poly_eq(aes(label = paste0("atop(", ..eq.label..,",",..rr.label..,")")), output.type = "text" 

## {GGPMISC} ANNOTATION LOOP   
   if (plot_rsq_adj == "false"){ # default
  ### EQUATION, RSQ
   if (plot_eq != "false" && plot_rsq != "false") { 
    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label),  sep = "*\"  |  \"*")),
    # a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0("atop(", after_stat(eq.label),",", after_stat(rr.label),")")), output.type = "text",
      eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
      eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) )
  ### EQUATION
    } else if (plot_eq != "false" && plot_rsq == "false") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0(after_stat(eq.label) )),
# a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0("atop(", after_stat(eq.label),",", after_stat(rr.label),")")), output.type = "text",
    eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
    eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
    rr.digits = 2, coef.digits = 2, parse = TRUE,
    label.y = 0.75 * max(input[,facB]) ) 
  ### NOTHING
    } else if (plot_eq == "false" && plot_rsq == "false") { # default
      a0 <- a0
  ### RSQ
    } else if (plot_eq == "false" && plot_rsq != "false") {
    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0(after_stat(rr.label) )),
# a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0("atop(", after_stat(rr.label),")")), output.type = "text",
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) ) 
    }
   
   } else { # RSQ_ADJ
  ### EQUATION, RSQ, RSQ_ADJ     
   if (plot_eq != "false" && plot_rsq != "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(adj.rr.label),
     sep = "*\"  |  \"*")), size = 3,
# a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0("atop(", after_stat(eq.label),",", after_stat(rr.label), ",", after_stat(adj.rr.label),")")), 
# output.type = "text", size = 3,
      eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),  
      eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) ) 
  ### EQUATION, RSQ_ADJ 
    } else if (plot_eq != "false" && plot_rsq == "false") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "*\"  |  \"*")),
# a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0("atop(", after_stat(eq.label), ",", after_stat(adj.rr.label),")")), output.type = "text",
      eq.with.lhs =  paste0("italic(`", facB, "`)~`=`~"),
      eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) ) 
  ### RSQ_ADJ 
    } else if (plot_eq == "false" && plot_rsq == "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label =  paste0(after_stat(adj.rr.label) )),
# a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0("atop(", after_stat(adj.rr.label),")")), output.type = "text",
       rr.digits = 2, coef.digits = 2, parse = TRUE,
       label.y = 0.75 * max(input[,facB]) ) 
  ### RSQ, RSQ_ADJ 
    } else if (plot_eq == "false" && plot_rsq != "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(rr.label), after_stat(adj.rr.label),  sep = "*\"  |  \"*")),
# a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste0("atop(", after_stat(rr.label), ",", after_stat(adj.rr.label),")")), output.type = "text",
       rr.digits = 2, coef.digits = 2, parse = TRUE,
       label.y = 0.75 * max(input[,facB]) ) 
    } 
     
   }
   
#STORE IN mset

  mSetObj$analSet$linReg1$plotted <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  # mSetObj$analSet$linReg1$plot <- a0
  # mSetObj$analSet$linReg1$plot_title <- plot_title1
  # mSetObj$analSet$linReg1$plot_ylab <- plot_ylab1
  # mSetObj$analSet$linReg1$plot_xlab <- plot_xlab1

  print(a0)
  # a0
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


### PREDICTED PLOTTING FUNCTION

#'Generate prediction/actual linear regression plot
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#'@param mSetObj Input the name of the created mSetObj
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)

#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
###@param weights Set weight values, default is NULL
#' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
#'@param plot_eq Boolean, "false" (default) to show linear model equation on plot, "true" for plot without annotation of model equation (at top); y is 0.75*max(y) (checkbox)
#'@param plot_rsq Boolean, "false" (default) to show linear model rsq value on plot, "true" for plot without annotation of rsq value (at top); y is 0.75*max(y) (checkbox)
#'@param plot_rsq_adj Boolean, "true" to show linear model adjusted rsq value on plot, "false" (default) for plot without annotation of adjusted rsq value (at top); y is 0.75*max(y) (checkbox)
#'@param plot_title Input the name of the title (default: "Univariate Linear Regression Line of Best Fit", textbox)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#' @importFrom rlang .data
#'@export
lin.pred.plot <- function(mSetObj=NA,
                         # facA = "NULL",
                         # facB = "NULL",
             data = "false",
             
             col_dots = "NULL",
             col_line = "NULL",
             plot_ci = "false",
  plot_eq = "false",
  plot_rsq = "false",
  plot_rsq_adj = "false",
  plot_title = " ",
  plot_ylab = " ",
  plot_xlab = " ",
  imgName,
  format = "png",
  dpi = 72,
  width = NA
  ){

  library("ggpmisc")
  library("ggplot2")
  
  mSetObj <- .get.mSet(mSetObj)
  
  ## DATA: NORMAL OR NOT
   if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  
  #RETRIEVE RESULTS:
 facA <- mSetObj$analSet$linReg1$res$response
 facB <-  mSetObj$analSet$linReg1$res$predictor
   
   fileName <- mSetObj$analSet$linReg1$res$fileName
   prediction <- mSetObj$analSet$linReg1$res$predicted.values
   # formula <- mSetObj$analSet$linReg1$res$formula
   # model <- mSetObj$analSet$linReg1$mod   
 

  # PLOT

  #SET PLOT DIMENSIONS
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
  
    #SET POINT COLOR
  if (col_dots == "NULL") {
      col_dots1 <- "black" # default
    } else if (col_dots == "blue") {
      col_dots1 <- "blue"
    } else if (col_dots == "red") {
      col_dots1 <- "red"
    } else if(col_dots == "green"){
      col_dots1 <- "green"
    } else if(col_dots == "grey"){
      col_dots1 <- "grey"
    }
  
   #SET LINE COLOR
   if (col_line == "NULL") {
       col_line1 <- "black" # default
     } else if (col_line == "blue") {
       col_line1 <- "blue"
     } else if (col_line == "red") {
       col_line1 <- "red"
     } else if(col_line == "green"){
       col_line1 <- "green"
     } else if(col_line == "grey"){
       col_line1 <- "grey"
     }
   
   #SET WHETHER TO ADD 95% CONF INT
   if (plot_ci == "false") {
       plot_ci1 <- FALSE # default
     } else {
       plot_ci1 <- TRUE
     }

  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste0("Linear Regression: Predicted vs Actual (", facA, ")")
  } else {
    plot_title1 <- plot_title
  }
  
  ## y actual input[,facA] fA
  ## x prediction fpred
  # # PLOT YAXIS
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
 
  
    # plot(x = prediction, y = input[,facA], xlab = "Predicted", ylab = "Actual", main = method, yaxt = "n")
  
  ## y actual input[,facA] fA
  ## x prediction fpred
  a0 <- ggplot(data =  data.frame(
    fpred = prediction, fA = input[,facA]),
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
   # aes_(x = as.name(facA), y = as.name(facB)) )+
  aes(x = fpred, y = fA)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE, method='lm') +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )

     #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   
  
# ## {GGPMISC} ANNOTATION LOOP
#    if (plot_rsq_adj == "false"){ # default
#   ### EQUATION, RSQ
#    if (plot_eq != "false" && plot_rsq != "false") {
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label),  sep = "*\"  |  \"*")),
#       eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
#       eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) )
#   ### EQUATION
#     } else if (plot_eq != "false" && plot_rsq == "false") {
#    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label =
#     paste0(after_stat(eq.label) )),
#     eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
#     eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
#     rr.digits = 2, coef.digits = 2, parse = TRUE,
#     label.y = 0.75 * max(input[,facB]) )
#   ### NOTHING
#     } else if (plot_eq == "false" && plot_rsq == "false") { # default
#       a0 <- a0
#   ### RSQ
#     } else if (plot_eq == "false" && plot_rsq != "false") {
#    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label =
#       paste0(after_stat(rr.label) )),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) )
#     }
# 
#    } else { # RSQ_ADJ
#   ### EQUATION, RSQ, RSQ_ADJ
#    if (plot_eq != "false" && plot_rsq != "false") {
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(adj.rr.label),
#       sep = "*\"  |  \"*")), size = 3,
#       eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
#       eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) )
#   ### EQUATION, RSQ_ADJ
#     } else if (plot_eq != "false" && plot_rsq == "false") {
#    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "*\"  |  \"*")),
#       eq.with.lhs =  paste0("italic(`", facB, "`)~`=`~"),
#       eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) )
#   ### RSQ_ADJ
#     } else if (plot_eq == "false" && plot_rsq == "false") {
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label =
#        paste0(after_stat(adj.rr.label) )),
#        rr.digits = 2, coef.digits = 2, parse = TRUE,
#        label.y = 0.75 * max(input[,facB]) )
#   ### RSQ, RSQ_ADJ
#     } else if (plot_eq == "false" && plot_rsq != "false") {
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(rr.label), after_stat(adj.rr.label),  sep = "*\"  |  \"*")),
#        rr.digits = 2, coef.digits = 2, parse = TRUE,
#        label.y = 0.75 * max(input[,facB]) )
#     }
# 
#    }
   
#STORE IN mset
  
  mSetObj$analSet$linReg1$plotPred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  print(a0)
  # a0
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}



#'Generate Normality of Residuals (qqplot) plot for model
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#'@param mSetObj Input the name of the created mSetObj
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param plot_title Input the name of the title (default: "Univariate Linear Regression Line of Best Fit", textbox)
#'@param plot_xlab Input the name to use for x-axis label (default: facA, textbox)
#'@param plot_ylab Input the name to use for y-axis label (default: facB, textbox)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#' @importFrom rlang .data
#'@export
lin.qq.plot <- function(mSetObj=NA,
                         facA = "NULL",
                         facB = "NULL",
             data = "false",
 col_dots = "NULL",
 col_line = "NULL",

  plot_title = " ",
  plot_ylab = " ",
  plot_xlab = " ",
  imgName,
  format = "png",
  dpi = 72,
  width = NA
  ){

  library("ggplot2")
  
  mSetObj <- .get.mSet(mSetObj)
  
  ## DATA: NORMAL OR NOT
   if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
#    ### iF VARIABLES ARE SET
#   #Set dependent (response) variable name
#   if (facA == "NULL"){
#     if( !"res" %in% names(mSetObj$analSet$linReg1) ){
#        facA <- mSetObj$analSet$linReg1$res$response
#     } else {
#     facA <- colnames(input)[1] #Default is 1st column.
#   }
# } else {
#     facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
#   }
#   #Set independent (predictor) variable name
#   if (facB == "NULL"){
#     if( !"res" %in% names(mSetObj$analSet$linReg1) ){
#        facB <-  mSetObj$analSet$linReg1$res$predictor
#     } else {
#     facB <- colnames(input)[2] #Default is 2nd column.
#   }
# } else {
#     facB <- facB #Determined using Columns() function below (java will present options in drop down menu)
#   }
#   
#   # VARIABLE TYPE CHECK
#   if (is.factor(input[,facA] || input[,facB]) == TRUE){
#     #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
#     stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
#   }
  
    # EXTRACT PLOT COMPONENTS
   
   facA <- mSetObj$analSet$linReg1$res$response
   facB <-  mSetObj$analSet$linReg1$res$predictor
   fileName <- mSetObj$analSet$linReg1$res$fileName
   # formula <- mSetObj$analSet$linReg1$res$formula
   # model <- mSetObj$analSet$linReg1$mod

  # PLOT

  #SET PLOT DIMENSIONS
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
  
    #SET POINT COLOR
  if (col_dots == "NULL") {
      col_dots1 <- "black" # default
    } else if (col_dots == "blue") {
      col_dots1 <- "blue"
    } else if (col_dots == "red") {
      col_dots1 <- "red"
    } else if(col_dots == "green"){
      col_dots1 <- "green"
    } else if(col_dots == "grey"){
      col_dots1 <- "grey"
    }
  
  #SET LINE COLOR
  if (col_line == "NULL") {
      col_line1 <- "black" # default
    } else if (col_line == "blue") {
      col_line1 <- "blue"
    } else if (col_line == "red") {
      col_line1 <- "red"
    } else if(col_line == "green"){
      col_line1 <- "green"
    } else if(col_line == "grey"){
      col_line1 <- "grey"
    }


  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste0("Normality of Residuals (", facA, ")")
  } else {
    plot_title1 <- plot_title
  }
  
  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Emprical"
  } else { # facA, response
    plot_ylab1 <- plot_ylab
  }

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Theoretical"
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
 
  a0 <- ggplot(data =  input,
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
   aes_(sample =  as.name(facA)) )+
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
    stat_qq(shape = 16, color = col_dots1) +
    stat_qq_line(color = col_line1, fullrange = TRUE) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )

     #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   
  
#STORE IN mset

  mSetObj$analSet$linReg1$plotNorm <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  print(a0)
  # a0
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}




#'JSON object conversion for linear regression plot
#'@description Build JSON object from linear regression plot. mSetObj was used with lin.reg.plot so that 'response', 'predictor', and plot are already stored in the object
#'@param mSetObj Input the name of the created mSetObj
#'@param which_plot Designate string representing which plot to convert; options are "plot" or "plotted" (or "NULL", default) for the default correlation plot; "pred" for Predicted vs Actual plot; "norm" for Normality of Residuals plot or "fit" for Fitted vs Residuals plot

#'@author  Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export


lin.reg.plot.json <- function(mSetObj=NA, which_plot = "NULL"){

  library("ggplot2")
  library("RJSONIO")
  
  mSetObj <- .get.mSet(mSetObj)
 
if(which_plot == "NULL" | which_plot == "plot" | which_plot == "plotted"){
  a0 <- mSetObj$analSet$linReg1$plotted$plot
  plot_title1 <- mSetObj$analSet$linReg1$plotted$title
  plot_ylab1 <- mSetObj$analSet$linReg1$plotted$ylab
  plot_xlab1 <- mSetObj$analSet$linReg1$plotted$xlab
} else if(which_plot == "pred"){
  a0 <- mSetObj$analSet$linReg1$plotPred$plot
  plot_title1 <- mSetObj$analSet$linReg1$plotPred$title
  plot_ylab1 <- mSetObj$analSet$linReg1$plotPred$ylab
  plot_xlab1 <- mSetObj$analSet$linReg1$plotPred$xlab
} else if(which_plot == "norm"){
  a0 <- mSetObj$analSet$linReg1$plotNorm$plot
  plot_title1 <- mSetObj$analSet$linReg1$plotNorm$title
  plot_ylab1 <- mSetObj$analSet$linReg1$plotNorm$ylab
  plot_xlab1 <- mSetObj$analSet$linReg1$plotNorm$xlab
} else if(which_plot == "fit"){
  a0 <- mSetObj$analSet$linReg1$plotFit$plot
  plot_title1 <- mSetObj$analSet$linReg1$plotFit$title
  plot_ylab1 <- mSetObj$analSet$linReg1$plotFit$ylab
  plot_xlab1 <- mSetObj$analSet$linReg1$plotFit$xlab
}
  imgName <- mSetObj$imgSet$plot.linReg1
  facA <- mSetObj$analSet$linReg1$res$response
  facB <- mSetObj$analSet$linReg1$res$predictor


  build <- ggplot_build(a0)
  # colnames(build$data[[1]]);  c("x", "y", "flipped_aes", "PANEL", "group", "colour", "fill",  "size", "linetype", "weight", "alpha")
  linear_plot_json <- list()
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
  linear_plot_json$points$coords <- build$data[[1]][,c("x","y")] #[,1:2]
  linear_plot_json$points$cols <- build$data[[1]][,grepl("col",colnames(build$data[[1]]))] #[,6] #colours
  linear_plot_json$points$shape <- build$data[[1]][,c("group")]#[,5]
  linear_plot_json$points$size <- build$data[[1]][,c("size")]#[,7]
  linear_plot_json$lines$cols <- build$data[[2]][,grepl("col",colnames(build$data[[2]]))]
  linear_plot_json$label <- build$data[[3]][,c("label")]
  # linear_plot_json$lines$ci <- build$data[[1]][,c("se")]
  ci<- build$data[[1]][,c("x","y", "ymin", "ymax")]
  colnames(ci) <- c("x","y","CI_down", "CI_up")
  linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]

  imgName <- paste(imgName, ".json", sep="")
  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName)
  cat(json.obj)
  sink()

  if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }
  
}






## OLD ALL IN ONE FUNCTION
# #'Generate linear regression plot
# #'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
# #'@param mSetObj Input the name of the created mSetObj
# #'@param facA Input the name of the response column (java uses Columns() to give user options)
# #'@param facB Input the name of the predictor column (java uses Columns() to give user options)
# #'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
# #'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
# #'@param col_line Set color for line (default "NULL" is black); (static dropdown)
# ###@param weights Set weight values, default is NULL
# #' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
# #'@param plot_eq Boolean, "false" (default) to show linear model equation on plot, "true" for plot without annotation of model equation (at top); y is 0.75*max(y) (checkbox)
# #'@param plot_rsq Boolean, "false" (default) to show linear model rsq value on plot, "true" for plot without annotation of rsq value (at top); y is 0.75*max(y) (checkbox)
# #'@param plot_rsq_adj Boolean, "true" to show linear model adjusted rsq value on plot, "false" (default) for plot without annotation of adjusted rsq value (at top); y is 0.75*max(y) (checkbox)
# #'@param plot_title Input the name of the title (default: "Univariate Linear Regression Line of Best Fit", textbox)
# #'@param plot_xlab Input the name to use for x-axis label (default: facA, textbox)
# #'@param plot_ylab Input the name to use for y-axis label (default: facB, textbox)
# #'@param imgName Input the image name
# #'@param format Select the image format, "png" or "pdf", default is "png" 
# #'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
# #'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
# #'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
# #'The second default is width=0, where the width is 7.2. Otherwise users can input their own width. 
# #'@author Gina Sykes \email{gsykes@ualberta.ca}
# #'University of Alberta, Canada
# #'License: GNU GPL (>= 2)
# #' @importFrom rlang .data
# #'@export
# lin.reg.plot <- function(mSetObj=NA,
  #                         facA = "NULL",
  #                         facB = "NULL",
  #            data = "false",
  #            
  # col_dots = "NULL",
  # col_line = "NULL",
  # plot_ci = "false",# weights=NULL,
  # plot_eq = "false",
  # plot_rsq = "false",
  # plot_rsq_adj = "false",
  # plot_title = "NULL",
  # plot_ylab = "NULL",
  # plot_xlab = "NULL",
  # imgName,
  # format = "png",
  # dpi = 72,
  # width = NA
#   ){
# 
#   library("ggpmisc")
#   library("ggplot2")
#   
#   mSetObj <- .get.mSet(mSetObj)
#   
#   # mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
#   
#  ### SET DATA (whether to use original data or not)
#   if (data == "false") { 
#     input <- mSetObj$dataSet$norm #default use norm
#   } else {
#     input <- mSetObj$dataSet$orig
#   }
#   
#   ### SET VARIABLES
#   #Set dependent (response) variable name
#   if (facA == "NULL"){
#     facA <- colnames(input)[1] #Default is 1st column.
#   } else {
#     facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
#   }
#   #Set independent (predictor) variable name
#   if (facB == "NULL"){
#     facB <- colnames(input)[2] #Default is 2nd column.
#   } else {
#     facB <- facB #Determined using Columns() function below (java will present options in drop down menu)
#   }
#   
#   # VARIABLE TYPE CHECK
#   if (is.factor(input[,facA] || input[,facB])==TRUE){
#     #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
#     stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
#   }
# 
#   #DEFINE FORMULA
#   formula <- as.formula(paste0(facA, "~", facB)) 
# 
#   #GENERATE MODEL, with or without weights
#   # if (is.null(weights) == TRUE) {
#     model <- lm(formula = formula, data = input, weights = NULL) #Create linear model, no weights
#  #  } else {
#  #    weights <- weights #Java upload weights as a vector of numeric values
#  #    if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
#  #      model <- lm(formula=formula, data=input, #mSetObj$dataSet$norm,
#  # weights=weights) #Create linear model, with weights
#  #    } else {
#  #      #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
#  #      stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
#  #    }
#  #  }
#  
#     
#   ## alpha(yint) + beta(slope) * x
#   # EXTRACTING MODEL RESULTS
#   # later, print()/cat() these:
#   summary <- summary(model) #PRINT #Summary w coeff, resid & fit
#   fitted <- fitted(model) #PRINT
#   covar <- vcov(model) #PRINT
#   conf.int <- confint(model, level=0.95) #PRINT
#   
#   fileName <- paste0("linear_regession_summary_", facA, "~", facB, ".txt") #File name for summary
#   coeffs <- summary[["coefficients"]] #Extract model coefficients
#   beta <- round(coeffs[2], digits = 2)
#   alpha <- round(coeffs[1], digits = 2)
#   equation <- paste(facA, " = ",
#  paste( paste(beta, facB, sep="*"), alpha, sep=" + ") ) # equation with intercept, coefficient and predictor variable name
#   r_sq <- round(summary[["r.squared"]], digits = 2) #Extract R^2
#   r_sq_adj <- round(summary[["adj.r.squared"]], digits = 2) #Extract adjusted R^2 value
# 
#  ##### - MODEL ASSUMPTIONS TESTS - #####
#   mod_shp <- stats::shapiro.test(model$residuals)$p.value
#   mod_bp <- lmtest::bptest(model)$p.value
#   mod_dw <- lmtest::dwtest(model)$p.value
# 
#   mod <- c(mod_shp, mod_bp, mod_dw)
#   names(mod) <- c("Shapiro-Wilk test for Normality of Residuals", "Beusch-Pagan test for Homoscedasticity", "Durbin-Watson test for Autocorrelation (Independent Residual Errors)")
#   fix<-c("Try other preprocessing options, or try other regression models such as SVM or random forest.",
#   "Try transforming the dependent variable (e.g., log transformation), or try redefining the dependent variable as a rate.",
#   "Is this time series data? Try looking into adding a lag in the independent/dependent variable, or adding a seasonal dummy variable to the model."
#   )
#   mod1 <- mod # numeric
#   n_fail <- sum(mod < 0.05) # was: nassump_fail
#   
#   if(any(mod < 0.001)){ # format numbers for printing
#   mod[mod < 0.001] <-
#    formatC( mod[mod < 0.001] , format = "e", digits = 3)
#   
#   mod[mod1 > 0.001] <-
#    round( mod1[mod1 > 0.001], digits = 3)
#   }
#    
#   if(n_fail > 0){
#    mod <- mod[mod1 < 0.05]
#   
# f0 <- paste0(n_fail, " linear model assumption test(s) failed: \n ")
# 
#  failed<-c(f0, paste0(
#       names(mod), " (P-Value: ", mod, ")\n", fix[mod1 < 0.05], "\n"  ) )
#       # "Please be advised that conforming to these assumptions is necessary for use of the linear model.  If the goal is to visually explore your data, try the Plotting module." 
#     # AddErrMsg(failed)
#     message(failed)
#     } else {
#     failed <- paste0("No model assumption tests failed.")
#     }
#   
#   df <- data.frame("Normality (Shapiro-Wilk)..." = mod_shp,
#           "Homoscedasticity (Breusch-Pagan)..." = mod_bp,
#            "Autocorrelation of Residuals (Durbin-Watson)..." = mod_dw,
#            "N Assumptions Failed..." = n_fail,
#                    check.names = FALSE)
#   row.names(df) <- NULL
#   ##### - MODEL ASSUMPTIONS TEST DONE - ####
#   
#   # #Test residuals for normality. Error will be visible to user.
#   # norm_resid <- shapiro.test(residuals) 
#   # if (norm_resid$p.value < 0.05){
#   #   norm_resid_text <- paste0("The residuals are normally distributed. This model is valid.") #To be used in summary, not to be displayed
#   # } else {
#   #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
#   #   stop("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
#   # }
#  
#    # STORE MODEL
#   mSetObj$analSet$linReg1$mod <- model
# 
#   #STORE RESULTS: x/y var names, model summary, fitted values,conf intervals, covar mat, equation, rsq, rsq_adj, filename, formula
#   ### change response to xlab, predictor to ylab
#   # mSetObj$analSet$linReg1$res <- list(response=facA, predictor=facB, summary=summary, predicted.values=fitted, confidence.intervals=conf.int, covariance.matrix=covar, equation=equation, r.squared.eq=paste("R-squared = ", r_sq), r.squared.adj.eq=paste("R-squared adjusted = ", r_sq_adj), fileName=fileName, formula=formula ) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
#   
#   # PLOT
# 
#   #SET PLOT DIMENSIONS
#   if(is.na(width)){
#     w <- 7.2
#   } else if(width == 0){
#     w <- 7.2
#   } else{
#     w <- width
#   }
#   h <- w
#   
#   #Name plot for download
#   imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
#   mSetObj$imgSet$plot.linReg1 <- imgName
#   
#     #SET POINT COLOR
#   if (col_dots == "NULL") {
#       col_dots1 <- "black" # default
#     } else if (col_dots == "blue") {
#       col_dots1 <- "blue"
#     } else if (col_dots == "red") {
#       col_dots1 <- "red"
#     } else if(col_dots == "green"){
#       col_dots1 <- "green"
#     } else if(col_dots == "grey"){
#       col_dots1 <- "grey"
#     }
#   
#   #SET LINE COLOR
#   if (col_line == "NULL") {
#       col_line1 <- "black" # default
#     } else if (col_line == "blue") {
#       col_line1 <- "blue"
#     } else if (col_line == "red") {
#       col_line1 <- "red"
#     } else if(col_line == "green"){
#       col_line1 <- "green"
#     } else if(col_line == "grey"){
#       col_line1 <- "grey"
#     }
#   
#   #SET WHETHER TO ADD 95% CONF INT
#   if (plot_ci == "false") {
#       plot_ci1 <- FALSE # default
#     } else {
#       plot_ci1 <- TRUE
#     }
# 
#   # PLOT TITLE
#   if(plot_title == "NULL"){
#     plot_title1 <- "Univariate Linear Regression Line of Best Fit"
#   } else {
#     plot_title1 <- plot_title
#   }
#   
#   # PLOT YAXIS
#   if(plot_ylab == "NULL"){
#     plot_ylab1 <- facB
#   } else {
#     plot_ylab1 <- plot_ylab
#   }
#   
#   # PLOT XAXIS
#   if(plot_xlab == "NULL"){
#     plot_xlab1 <- facA
#   } else {
#     plot_xlab1 <- plot_xlab
#   }
#   
#   
#   # #IF THERE IS A LINE COLOR, OVERRIDE DOT COLOR TO BLACK
#   # if(!all.equal(col_line1, "black")){
#   #   col_dots1 <- "black"
#   # }
#  
#   # THEME 
# #  theme_lineplot <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size=12, colour="black"),    axis.title=element_text(size=12), legend.title=element_text(12), legend.text=element_text(size=12), plot.title=element_text(face='bold',hjust = 0.5)
# #   )
#   ## NOTE THE .data ARGUMENT; 
#   ##  To avoid a note from CMD check about .data, use #' @importFrom rlang .data in any roxygen code block (typically in package documentation as generated by usethis::use_package_doc()) ; https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
#    a0 <- ggplot(data = input,
#    # aes(x = .data[[facA]], y = .data[[facB]]) ) +
#     aes_(x = as.name(facA), y = as.name(facB)) )+
#    labs(title = plot_title1) +
#      ylab(plot_ylab1)+ xlab(plot_xlab1) +
#      geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE, method = 'lm') +
#      geom_point(shape = 16, color = col_dots1) +
#      theme_bw() + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
# axis.text = element_text(size = 12, colour = "black"), 
#    axis.title = element_text(size = 12),
#  # legend.title=element_text(12), legend.text=element_text(size=12), 
# plot.title=element_text(face = 'bold',hjust = 0.5)
#   )
# 
#      #GENERATE PLOT
#   Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
#    
#   
# ## {GGPMISC} ANNOTATION LOOP   
#    if (plot_rsq_adj == "false"){ # default
#   ### EQUATION, RSQ
#    if (plot_eq != "false" && plot_rsq != "false") { 
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label),  sep = "*\"  |  \"*")),
#       eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
#       eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) )
#   ### EQUATION
#     } else if (plot_eq != "false" && plot_rsq == "false") {
#    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
#     paste0(after_stat(eq.label) )),
#     eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),
#     eq.x.rhs =  paste0("~italic(`*` ~`", facA,"`)"),
#     rr.digits = 2, coef.digits = 2, parse = TRUE,
#     label.y = 0.75 * max(input[,facB]) ) 
#   ### NOTHING
#     } else if (plot_eq == "false" && plot_rsq == "false") { # default
#       a0 <- a0
#   ### RSQ
#     } else if (plot_eq == "false" && plot_rsq != "false") {
#    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
#       paste0(after_stat(rr.label) )),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) ) 
#     }
#    
#    } else { # RSQ_ADJ
#   ### EQUATION, RSQ, RSQ_ADJ     
#    if (plot_eq != "false" && plot_rsq != "false") {
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(adj.rr.label),
#       sep = "*\"  |  \"*")), size = 3,
#       eq.with.lhs =  paste0("italic(`", facB,"`)~`=`~"),  
#       eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) ) 
#   ### EQUATION, RSQ_ADJ 
#     } else if (plot_eq != "false" && plot_rsq == "false") {
#    a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "*\"  |  \"*")),
#       eq.with.lhs =  paste0("italic(`", facB, "`)~`=`~"),
#       eq.x.rhs =  paste0("~italic(`*` ~`", facA, "`)"),
#       rr.digits = 2, coef.digits = 2, parse = TRUE,
#       label.y = 0.75 * max(input[,facB]) ) 
#   ### RSQ_ADJ 
#     } else if (plot_eq == "false" && plot_rsq == "false") {
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
#        paste0(after_stat(adj.rr.label) )),
#        rr.digits = 2, coef.digits = 2, parse = TRUE,
#        label.y = 0.75 * max(input[,facB]) ) 
#   ### RSQ, RSQ_ADJ 
#     } else if (plot_eq == "false" && plot_rsq != "false") {
#      a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(rr.label), after_stat(adj.rr.label),  sep = "*\"  |  \"*")),
#        rr.digits = 2, coef.digits = 2, parse = TRUE,
#        label.y = 0.75 * max(input[,facB]) ) 
#     } 
#      
#    }
#    
# #STORE IN mset
#   mSetObj$analSet$linReg1$plot <- a0
# 
#   print(a0)
#   # a0
#   dev.off()
#     
# ### Printing Values ## ERROR CHECK: BACKSLASH FOLLOWED BY N
#   sink(fileName)
#   cat("Formula:\n")
#   print(formula)
#   cat("Model:\n")
#   print(equation)
#   cat("\nLinear Model Assumption Check:")
#   print(df)
#   print(failed)
#   print(summary)
#   # print(norm_resid)
#   # cat("Normality of residuals result:\n")
#   # cat(paste0(norm_resid_text, "\n"))
#   cat("\nConfidence intervals for predictor variables:")
#   print(conf.int)
#   cat("\nPredicted values:")
#   print(fitted)
#   cat("\nCovariance matrix for predictor variables:")
#   print(covar)
#   sink()
#   
#   return(.set.mSet(mSetObj))
#   
# }



# #'Perform Linear Regression'
# #'@description Build a linear regression model for one user selected predictor variable
# #'@param mSetObj Input the name of the created mSetObj
# #'@param facA Input the name of the response column (java uses Columns() to give user options)
# #'@param facB Input the name of the predictor column (java uses Columns() to give user options)
# #'@param weights Set weight values, default is NULL
# #'@author Louisa Normington\email{normingt@ualberta.ca}
# #'University of Alberta, Canada
# #'License: GNU GPL (>= 2)
# #'@export
# 
# lin.reg.anal.one <- function(mSetObj=NA, facA="NULL", facB="NULL", weights=NULL){
#   
#   mSetObj <- .get.mSet(mSetObj)
#   mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
#   #Dependent var default is first column. Independent var default is second column.
# 
# 
#   #Set dependent (response) variable name
#   if (facA == "NULL"){
#     facA <- colnames(mSetObj$dataSet$norm)[1] #Default is first column.
#   } else {
#     facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
#   }
#   
#   #Set independent (predictor) variable name
#   if (facB == "NULL"){
#     facB <- colnames(mSetObj$dataSet$norm)[2] #Default is second column.
#   } else {
#     facB <- facB #Determined using Columns() function below (java will present options in drop down menu)
#   }
# 
#   #Variable type check
#   if (is.factor(mSetObj$dataSet$norm[,facA] || mSetObj$dataSet$norm[,facB])==TRUE){
#     #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
#     stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
#   }
#   
#   #Define formula
#   formula <- as.formula(paste0(facA, "~", facB)) 
#   
#   #Generate model with or without weights
#   if (is.null(weights)==TRUE) {
#     model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=NULL) #Create linear model, no weights
#   } else {
#     weights <- weights #Java upload weights as a vector of numeric values
#     if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
#       model <- lm(formula=formula, data=mSetObj$dataSet$norm, weights=weights) #Create linear model, with weights
#     } else {
#       #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
#       stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
#     }
#   }
#   
#   #Store model
#   mSetObj$analSet$linReg1$mod <- model
#   
#   #Extract results
#   fitted <- fitted(model) #Predicted values
#   summary <- summary(model) #Summary includes coefficients, residuals and fit parameters
#   residuals <- model$residuals # Get the residuals 
#   conf.int <- confint(model, level=0.95) #Confidence intervals for predictor variables
#   covar <- vcov(model) #Covariance matrix for preductor variables
#   fileName <- paste0("linear_regession_summary_", facA, "~", facB, ".txt") #File name for summary
#   coeffs <- summary[["coefficients"]] #Extract model coefficients
#   beta <- round(coeffs[2], 2)
#   alpha <- round(coeffs[1], 2)
#   equation <- paste(facA, " = ", paste(paste(beta, facB, sep="*"), alpha, sep=" + ")) #Create equation with intercept, coefficient and predictor variable name
#   r.squared <- summary[["adj.r.squared"]] #Extract R^2 value
#   r_sq <- round(r.squared, 2)
#   r.squared.eq <- paste("R-squared: ", r_sq) #Generate R^2 equation
#   r.squared.adj <- summary[["adj.r.squared"]] #Extract adjusted R^2 value
#   r_sq_adj <- round(r.squared.adj, 2)
#   r.squared.adj.eq <- paste("R-squared adjusted: ", r_sq_adj) #Generate adjusted R^2 equation
#   
#   # #Test residuals for normality. Error will be visable to user.
#   # norm_resid <- shapiro.test(residuals) 
#   # if (norm_resid$p.value < 0.05){
#   #   norm_resid_text <- paste0("The residuals are normally distributed. This model is valid.") #To be used in summary, not to be displayed
#   # } else {
#   #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
#   #   stop("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
#   # }
#   
#   #Store results
#   mSetObj$analSet$linReg1$res <- list(response=facA, predictor=facB, summary=summary, predicted.values=fitted, confidence.intervals=conf.int, covariance.matrix=covar, equation=equation, r.squared.eq=r.squared.eq, r.squared.adj.eq=r.squared.adj.eq, fileName=fileName, formula=formula) #Note where the summary is stored
#   
#   #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
#   sink(fileName) 
# 
#   cat("Formula:\n")
#   print(formula)
# 
#   print(summary)
#   # print(norm_resid)
#   # cat("Normality of residuals result:\n")
#   # cat(paste0(norm_resid_text, "\n"))
# 
#   cat("\nConfidence intervals for predictor variables:")
#   print(conf.int)
# 
#   cat("\nPredicted values:")
#   print(fitted)
# 
#   cat("\nCovariance matrix for predictor variables:")
#   print(covar)
#   sink()
#   
#   
#   return(.set.mSet(mSetObj))
#   
# }
# 
# 
# #'Plot line of best fit for linear regression with one predictor variable
# #'@description Scatter plot with line of best fit, where response variable is y and predictor variable is x
# #'@usage Plot.linReg1(mSetObj, imgName, format="png", dpi=72, width=NA)
# #'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
# #'@param imgName Input the image name
# #'@param format Select the image format, "png" or "pdf", default is "png" 
# #'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
# #'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
# #'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
# #'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
# #'@author Louisa Normington\email{normingt@ualberta.ca}
# #'University of Alberta, Canada
# #'License: GNU GPL (>= 2)
# #'@export
# 
# plot.linReg1 <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
#   
#   #Extract necessary objects from mSetObj
#   mSetObj <- .get.mSet(mSetObj)
#   facA <- mSetObj$analSet$linReg1$res$response #For x-axis label
#   facB <- mSetObj$analSet$linReg1$res$predictor #For y-axis label
#   response <- mSetObj$dataSet$norm[,facA] #First column is reponse variable by default
#   predictor <- mSetObj$dataSet$norm[,facB] #User selects predictor variable in drop down menu
#   model <- mSetObj$analSet$linReg1$mod
#   
#   #Set plot dimensions
#   if(is.na(width)){
#     w <- 7.2
#   } else if(width == 0){
#     w <- 7.2
#   } else{
#     w <- width
#   }
#   h <- w
#   
#   #Name plot for download
#   imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
#   mSetObj$imgSet$plot.linReg1 <- imgName
#   
#   #Generate plot
#   Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
#   plot(response, predictor, xlab=facA, ylab=facB, main="Univariate Linear Regression Line of Best Fit", yaxt="n")
#   axis(2, las=2)
#   abline(model)
#   dev.off()
#   
#   return(.set.mSet(mSetObj))
#   
# }


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
# library("tidyselect")
  
  data <- select_if(mSetObj$dataSet$norm, is.numeric)
# data <- select(mSetObj$dataSet$norm, tidyselect::where(is.numeric)) 
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

#   lin.reg.result <- c(mSetObj$analSet$linReg1$res$equation,
# mSetObj$analSet$linReg1$res$r.squared.eq,
# mSetObj$analSet$linReg1$res$r.squared.adj.eq)

lin.reg.result <- mSetObj$analSet$linReg1$plot

  return(lin.reg.result)

}
