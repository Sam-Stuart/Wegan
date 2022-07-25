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
  
  #Text should be visable to user
  cat("Two variables will be tested for correlation, a dependent variable and an independent variable. Both must have numeric values.")
  
 ### SET DATA (whether to use original data or not)
  if (data == "false") { 
     mSetObj$dataSet$norm <-  #default use norm
       mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
     input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  ### SET VARIABLES
  #SET DEPENDENT (RESPONSE) VARIABLE NAME
  if (facA == "NULL") {
    for (i in 1:ncol(input)) {
      if (is.factor(input[,i]) == FALSE) {
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
      if (is.factor(input[,i+1]) == FALSE) {
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
  
  #pre-allocate containers for results
  mSetObj$analSet$polyReg$mod <- list()
  mSetObj$analSet$polyReg$res <- list()
  #### CHANGE: V3 FROM name 'adjusted_R_squared' to 'R_squared_adjusted'
  df.R.sq <- c(V1="polynomial_degree", V2="R_squared", V3="R_squared_adjusted")
  
  for (i in 2:max.deg){
    
    #GENERATE MODEL
    ### alpha(yint) + beta(slope) * x
    degree <- i
    formula <- as.formula(paste0(facA, " ~ poly(", facB, ", ", degree, ")"))
    model <- lm(formula = formula, weights = NULL, data = input) #Create polynomial regression model
    model_name <- paste0("polynomial.degree.", degree, ".model")
    
    #EXTRACT RESULTS
    summary <- summary(model) #PRINT #Summary w coeffs, resids & fit
    conf.int <- confint(model, level = 0.95) #PRINT  #Conf intervals for predictor variables
    fitted <- fitted(model) # PRINT #Predicted values
    coeffs <- summary[["coefficients"]] #Extract model coefficients

    #Generate equation
    if (degree == 2) {
      alpha <- round(coeffs[1], digits = 2) #yint
      beta.1 <- round(coeffs[2], digits = 2) #slope
      beta.2 <- round(coeffs[3], digits = 2) 
      equation <- paste(facA, " = ", 
       paste(paste(beta.2, paste0(facB, "^", degree), sep = "*"),
       paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree == 3) {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      equation <- paste(facA, " = ", 
        paste(paste(beta.3, paste0(facB, "^", degree), sep = "*"),
        paste(beta.2, paste0(facB, "^", degree-1), sep = "*"),
        paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree == 4) {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      beta.4 <- round(coeffs[5], digits = 2) 
      equation <- paste(facA, " = ", 
        paste(paste(beta.4, paste0(facB, "^", degree), sep = "*"),
        paste(beta.3, paste0(facB, "^", degree-1), sep = "*"),
        paste(beta.2, paste0(facB, "^", degree-2), sep = "*"),
        paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree == 5) {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      beta.4 <- round(coeffs[5], digits = 2) 
      beta.5 <- round(coeffs[6], digits = 2) 
      equation <- paste(facA, " = ", 
        paste(paste(beta.5, paste0(facB, "^", degree), sep = "*"),
        paste(beta.4, paste0(facB, "^", degree-1), sep = "*"),
        paste(beta.3, paste0(facB, "^", degree-2), sep = "*"),
        paste(beta.2, paste0(facB, "^", degree-3), sep = "*"),
        paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree == 6) {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      beta.4 <- round(coeffs[5], digits = 2) 
      beta.5 <- round(coeffs[6], digits = 2) 
      beta.6 <- round(coeffs[7], digits = 2) 
      equation <- paste(facA, " = ", 
        paste(paste(beta.6, paste0(facB, "^", degree), sep = "*"),
        paste(beta.5, paste0(facB, "^", degree-1), sep = "*"),
        paste(beta.4, paste0(facB, "^", degree-2), sep = "*"),
        paste(beta.3, paste0(facB, "^", degree-3), sep = "*"),
        paste(beta.2, paste0(facB, "^", degree-4), sep = "*"),
        paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree == 7) {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      beta.4 <- round(coeffs[5], digits = 2) 
      beta.5 <- round(coeffs[6], digits = 2) 
      beta.6 <- round(coeffs[7], digits = 2) 
      beta.7 <- round(coeffs[8], digits = 2) 
      equation <- paste(facA, " = ", 
        paste(paste(beta.7, paste0(facB, "^", degree), sep = "*"),
        paste(beta.6, paste0(facB, "^", degree-1), sep = "*"),
        paste(beta.5, paste0(facB, "^", degree-2), sep = "*"),
        paste(beta.4, paste0(facB, "^", degree-3), sep = "*"),
        paste(beta.3, paste0(facB, "^", degree-4), sep = "*"),
        paste(beta.2, paste0(facB, "^", degree-5), sep = "*"),
        paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree == 8) {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      beta.4 <- round(coeffs[5], digits = 2) 
      beta.5 <- round(coeffs[6], digits = 2) 
      beta.6 <- round(coeffs[7], digits = 2) 
      beta.7 <- round(coeffs[8], digits = 2) 
      beta.8 <- round(coeffs[9], digits = 2) 
       equation <- paste(facA, " = ", 
        paste(paste(beta.8, paste0(facB, "^", degree-1), sep = "*"),
        paste(beta.7, paste0(facB, "^", degree-2), sep = "*"),
        paste(beta.6, paste0(facB, "^", degree-3), sep = "*"),
        paste(beta.5, paste0(facB, "^", degree-4), sep = "*"),
        paste(beta.4, paste0(facB, "^", degree-5), sep = "*"),
        paste(beta.3, paste0(facB, "^", degree-6), sep = "*"),
        paste(beta.2, paste0(facB, "^", degree-7), sep = "*"),
        paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else if (degree == 9) {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      beta.4 <- round(coeffs[5], digits = 2) 
      beta.5 <- round(coeffs[6], digits = 2) 
      beta.6 <- round(coeffs[7], digits = 2) 
      beta.7 <- round(coeffs[8], digits = 2) 
      beta.8 <- round(coeffs[9], digits = 2) 
      beta.9 <- round(coeffs[10], digits = 2) 
      equation <- paste(facA, " = ", 
        paste(paste(beta.9, paste0(facB, "^", degree), sep = "*"),
        paste(beta.8, paste0(facB, "^", degree-1), sep = "*"),
        paste(beta.7, paste0(facB, "^", degree-2), sep = "*"),
        paste(beta.6, paste0(facB, "^", degree-3), sep = "*"),
        paste(beta.5, paste0(facB, "^", degree-4), sep = "*"),
        paste(beta.4, paste0(facB, "^", degree-5), sep = "*"),
        paste(beta.3, paste0(facB, "^", degree-6), sep = "*"),
        paste(beta.2, paste0(facB, "^", degree-7), sep = "*"),
        paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    } else {
      alpha <- round(coeffs[1], digits = 2)
      beta.1 <- round(coeffs[2], digits = 2)
      beta.2 <- round(coeffs[3], digits = 2) 
      beta.3 <- round(coeffs[4], digits = 2) 
      beta.4 <- round(coeffs[5], digits = 2) 
      beta.5 <- round(coeffs[6], digits = 2) 
      beta.6 <- round(coeffs[7], digits = 2) 
      beta.7 <- round(coeffs[8], digits = 2) 
      beta.8 <- round(coeffs[9], digits = 2) 
      beta.9 <- round(coeffs[10], digits = 2) 
      beta.10 <- round(coeffs[11], digits = 2) 
      equation <- paste(facA, " = ", paste(paste(beta.10, paste0(facB, "^", degree), sep = "*"), paste(beta.9, paste0(facB, "^", degree-1), sep = "*"), paste(beta.8, paste0(facB, "^", degree-2), sep = "*"), paste(beta.7, paste0(facB, "^", degree-3), sep = "*"), paste(beta.6, paste0(facB, "^", degree-4), sep = "*"), paste(beta.5, paste0(facB, "^", degree-5), sep = "*"), paste(beta.4, paste0(facB, "^", degree-6), sep = "*"), paste(beta.3, paste0(facB, "^", degree-7), sep = "*"), paste(beta.2, paste0(facB, "^", degree-8), sep = "*"), paste(beta.1, facB, sep = "*"), alpha, sep = " + ")) #Create equation with intercept, coefficient and predictor variable name
    }
    
     r_sq <- summary[["r.squared"]] #Extract R^2 # was stored in r.squared #was using 'adj.r.squared' ?
     r_sq_adj <- summary[["adj.r.squared"]]#Extract adjusted R^2 value # was in r.squared.adj
    # r.squared.eq <- paste("R-squared = ", r_sq) #Generate R^2 equation
    # r.squared.adj.eq <- paste("R-squared adjusted = ", r_sq_adj)
    # R.sq <- summary$r.squared #Extract R^2 value
    # R.sq.eq <- paste("R-squared = ", round(R.sq, digits = 2)) 
    # adj.R.sq <- summary$adj.r.squared #Extract R^2 value
    # adj.R.sq.eq <- paste("R-squared adjusted = ", round(adj.R.sq, digits = 2)) 
    # data.R.sq <- c(i, r_sq, r_sq_adj) # was c(i, R.sq, adj.R.sq)
 df.R.sq <- c(i, r_sq, r_sq_adj)
    df.R.sq <- rbind(df.R.sq, c(i, r_sq, r_sq_adj)  )

    
    fileName <- paste0("polynomial_regession_summary_degree_", degree, "_", facA, "~", facB, ".txt") #File name for summary, used by save.polyReg.summary()

    #Store results in mSetObj$analSet$polyReg
    list_index <- i-1
    mSetObj$analSet$polyReg$res[[list_index]] <- list(response = facA, predictor = facB, degree = degree, summary = summary, predicted.values = fitted, confidence.intervals = conf.int,  equation = equation,  r.squared.eq=paste("R-squared = ", round(r_sq, digits = 2)), r.squared.adj.eq=paste("R-squared adjusted = ", round(r_sq_adj, digits = 2)), formula = formula, fileName = fileName  )
    
     mSetObj$analSet$polyReg$mod[[list_index]] <- list(model_name = model_name, model = model, response = facA, predictor = facB)
    
    # mSetObj$analSet$polyReg$res[[list_index]] <- list(response = facA, predictor = facB, degree = degree, summary = summary, predicted.values = fitted, confidence.intervals = conf.int, fileName = fileName, equation = equation, R.sq.eq = R.sq.eq, adj.R.sq.eq = adj.R.sq.eq)
  
   
    ### Printing Values ## R CODE ERROR CHECK: MAKE SURE BACKSLASH IS FOLLOWED BY N
    # Text document, goes into wd; accessible as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    cat("Equation:\n")
    print(equation)
    print(summary)
    # cat("\nLinear Model Assumption Check:")
    # print(df)
    # print(failed)
    # print(norm_resid)
    # cat("Normality of errors result:\n")
    # cat(paste0(norm_resid_text, "\n"))
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    # cat("\nPredicted values:")
    # print(fitted)
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
                         # facA = "NULL", facB="NULL",
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

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
  ### SET DATA (whether to use original data or not)
  if (data=="false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #Assign degree
  if (degree == "NULL"){
    degree <- 2 #Default degree is 2
  } else{
    degree <- degree #user defined in drop down menu. See Poly.Reg.Degrees() for more information
  }

  #Extract plot components
  list_index <- degree - 1
  facA <- mSetObj$analSet$polyReg$res[[list_index]][["response"]] #y
  facB <- mSetObj$analSet$polyReg$res[[list_index]][["predictor"]] # x
  #facA <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["response"]]#For x-axis label
  #facB <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["predictor"]] #For y-axis label
  response <- input[,facA] #First column is reponse variable by default
  predictor <- input[,facB] #Predictor column used for model construction by poly.reg.anal()
  model <- mSetObj$analSet$polyReg$mod[[list_index]][["model"]]
  formula <- formula(model)
  # formula2 <- as.formula( gsub(facB, "x",
  #     gsub(facA, "y", deparse(formula) ) 
  #       ))  
formula2 <- as.formula(
    stringr::str_replace_all(deparse(formula), facA, "y") %>%
      stringr::str_replace_all(., facB, "x") )

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
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep = "")
  mSetObj$imgSet$plot.polyReg <- imgName
  
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
    plot_title1 <- paste0("Polynomial Regression: ", as.expression(formula))
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
  ##  To avoid a note from CMD check about .data, use #' @importFrom rlang .data in any roxygen code block (typically in package documentation as generated by usethis::use_package_doc()) ; https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
  a0 <- ggplot(data =  input, #data.frame(fpred = prediction, fA = input[,facA]),aes(x = fpred, y = fA)) + # aes(x = .data[[facB]], y = .data[[facA]]) ) +
   aes_(x = as.name(facB), y = as.name(facA)) )+
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE, formula = formula2) +
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

  print(a0)
  # a0
  dev.off()
  
  return(.set.mSet(mSetObj))
  
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
                         # facA = "NULL", facB="NULL",
                         data="false",
             col_dots="NULL",
             col_line="NULL", 
             plot_ci="false",
  # plot_eq="false", 
  # plot_rsq="false", 
  # plot_rsq_adj="false",
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  imgName, format="png", dpi=72, width=NA){
  
library("ggplot2")

  ## was called: plot.pred.polyReg
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)

### SET DATA (whether to use original data or not)
  if (data=="false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

  #Assign degree
  if (degree== "NULL"){
    degree <- 2 #Default degree is 2
  } else{
    degree <- degree #Value needs to be user defined in drop down menu. See Poly.Reg.Degrees() for more information
  }

  list_index <- degree-1
  method <- "Polynomial Regression"
  prediction <- mSetObj$analSet$polyReg$res[[list_index]][["predicted.values"]]
  #facA <- mSetObj[["analSet"]][["polyReg"]][["res"]][[list_index]][["response"]] #For x-axis label
  facA <- mSetObj$analSet$polyReg$res[[list_index]][["response"]]
  facB <- mSetObj$analSet$polyReg$res[[list_index]][["predictor"]]
  
  model <- mSetObj$analSet$polyReg$mod[[list_index]][["model"]]
  formula <- formula(model)
  # formula2 <- as.formula( gsub(facB, "x",
  #                             gsub(facA, "y", deparse(formula) ) 
  #                             )) 
formula2 <- as.formula(
    stringr::str_replace_all(deparse(formula), facA, "y") %>%
      stringr::str_replace_all(., facB, "x") )
 

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
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep = "")
  mSetObj$imgSet$plot.pred.linRegMulti <- imgName
  
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
    plot_title1 <- paste0("Polynomial Regression: Predicted vs Actual (", as.expression(formula), ")")
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
  a0 <- ggplot(data =  data.frame(
    fpred = prediction, fA = input[,facA]),
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
   # aes_(x = as.name(facA), y = as.name(facB)) )+
  aes(x = fpred, y = fA)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE, formula = formula2) +
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
   
#STORE IN mset
  mSetObj$analSet$linReg1$plotpred <- list(plot= a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  print(a0)
  # a0
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