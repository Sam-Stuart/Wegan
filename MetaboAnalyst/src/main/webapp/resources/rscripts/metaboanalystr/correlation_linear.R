#' @title ggplot2 theme for scientific publications
#'
#' @description
#' from {envalysis}: github.com/zsteinmetz/envalysis/blob/main/R/theme_publish.R
#' Themes set the general aspect of the plot such as the color of the
#' background, grid lines, the size and color of fonts. This particular theme is
#' based on the classic dark-on-light ggplot2 \code{\link[ggplot2]{theme_bw}}
#' and has been used for scientific publications.
#'
#' @param base_size base font size
#' @param base_family base font family
#' @param base_line_size base line size for, e.g. for ticks and axes
#' @param \dots further arguments to be passed to \code{\link[ggplot2]{theme_bw}}
#'
#' @author 
#' Zacharias Steinmetz
#' 
#' @examples
#' require(ggplot2)
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour=factor(gear))) + facet_wrap( ~ am)
#' p
#' p + theme_bww()
#'
#' @seealso
#' \code{\link[ggplot2]{ggtheme}}
#' \code{\link[ggplot2]{theme_bw}}
#'
#' @import ggplot2
#' @export
theme_bww <- function(base_size = 12, base_family = "",
                          base_line_size = 0.25, ...) {
  half_line <- base_size / 2
  small_rel <- 0.8
  small_size <- small_rel * base_size
  
  # TODO: replace size with linewidth in `element_rect()`
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(
      rect = element_rect(fill = "transparent", colour = NA, color = NA,
                          size = 0, linetype = 0),
      text = element_text(family = base_family, face = "plain",
                          colour = "black", size = base_size, hjust = 0.5,
                          vjust = 0.5, angle = 0, lineheight = 0.9,
                          margin = ggplot2::margin(), debug = F),
      
      axis.text = element_text(size = small_size),
      axis.text.x = element_text(margin = ggplot2::margin(t = small_size/4),
                                 vjust = 1),
      axis.text.y = element_text(margin = ggplot2::margin(r = small_size/4), 
                                 hjust = 1),
      axis.title.x = element_text(margin = ggplot2::margin(t = small_size,
                                                           b = small_size)),
      axis.title.y = element_text(angle = 90,
                                  margin = ggplot2::margin(r = small_size,
                                                           l = small_size/4)),
      axis.ticks = element_line(colour = "black", size = base_line_size),
      axis.ticks.length = unit(0.25, 'lines'),
      
      axis.line = element_line(colour = "black", size = base_line_size),
      axis.line.x = element_line(colour = "black", size = base_line_size), 
      axis.line.y = element_line(colour = "black", size = base_line_size), 
      
      legend.spacing = unit(base_size/4, "pt"),
      legend.key = element_blank(),
      legend.key.size = unit(1 * base_size, "pt"),
      legend.key.width = unit(1.5 * base_size, 'pt'),
      legend.text = element_text(size = rel(small_rel)),
      legend.title = element_text(size = rel(small_rel), face = 'bold'),
      legend.position = 'bottom',
      legend.box = 'horizontal',
      
      panel.spacing = unit(1, "lines"),
      panel.background = element_blank(),
      panel.border = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      strip.text = element_text(size = base_size),
      strip.background = element_rect(fill = NA, colour = "black",
                                      size = 0.125),
      strip.text.x = element_text(face = 'bold', hjust = 0,
                                  margin = ggplot2::margin(b = small_size/2,
                                                           t = small_size/4)),
      strip.text.y = element_text(angle = -90, face = 'bold',
                                  margin = ggplot2::margin(l = small_size/2,
                                                           r = small_size/4)),
      
      plot.margin = unit(c(5,5,0,0), "pt"),
      plot.background = element_blank(),
      plot.title = element_text(face = "bold", size = 1.2 * base_size, 
                                margin = ggplot2::margin(b = half_line),
                                hjust = 0)
    )
}







#'Generate linear regression model
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#' generates a text doc summary of the model named: "corr_linear_model_summary.txt"
#' results from calculations are stored in mSet in analSet in 'linReg' as 'res' (response name, predictor name, model summary, predicted values, confidence intervals, covariance matrix, model equation, R squared value, adjusted R squared value, the filename) and 'mod' (regression type, model, formula, model assumptions, response name, predictor name)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses Columns() to give user options of numeric columns)
#'@param facB Input the name of the predictor column (java uses Columns() to give user options of numeric columns)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#' @importFrom rlang .data
#'@export

lin.reg.anal <- function(mSetObj = NA,
 facA = "NULL", # response, dropdown
 facB = "NULL", # predictor, dropdown
 data = "false" # checkbox
             
  ){

  library("lmtest")
  
  mSetObj <- .get.mSet(mSetObj)
  
  # mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]
  
 ### SET DATA (whether to use original data or not)
  if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
    # mSetObj$dataSet$usenorm <- TRUE
  } else {
    input <- mSetObj$dataSet$orig
   # mSetObj$dataSet$usenorm <- FALSE
  }
  
  ### SET VARIABLES
  #SET DEPENDENT (RESPONSE) VARIABLE NAME
  if (facA == "NULL"){
    facA <- colnames(input)[1] #Default is 1st column.
  } else {
    facA <- facA #Determined using Columns() function (java will present numeric column options in drop down menu)
  }
  #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
  if (facB == "NULL"){
    facB <- colnames(input)[2] #Default is 2nd column.
  } else {
    facB <- facB #Determined using Columns() function (java will present numeric column options in drop down menu)
  }
  
  # VARIABLE TYPE CHECK
  if (is.factor(input[,facA]) || is.factor(input[,facB]) ){
    AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
    stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
  }

  #DEFINE FORMULA
  form <- as.formula(paste(facA, "~", facB, sep="")) 

  #GENERATE MODEL, without weights
  # if (is.null(weights) == TRUE) {
    mod <- lm(formula = form, data = input, weights = NULL) #Create linear model, no weights
 #  } else {
 #    weights <- weights #Java upload weights as a vector of numeric values
 #    if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
 #      mod <- lm(formula=form, data=input, #mSetObj$dataSet$norm,
 # weights=weights) #Create linear model, with weights
 #    } else {
 #      #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
 #      stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
 #    }
 #  }
 
    
  ## alpha(yint) + beta(slope) * x
  # EXTRACTING MODEL RESULTS
  # later, print()/cat() these:
  summ <- summary(mod) #PRINT #Summary w coeff, resid & fit
  fitt <- fitted(mod) #PRINT
  covar <- vcov(mod) #PRINT
  conf.int <- confint(model, level=0.95) #PRINT
  
  fileName <- paste0("corr_linear_model_summary", ".txt") #File name for summary
# fileName <- paste0("linear_regression_summary_", facA, "~", facB, ".txt") #File name for summary
  coeffs <- summ[["coefficients"]] #Extract model coefficients
  beta <- round(coeffs[2], digits = 2) # slope
  alpha <- round(coeffs[1], digits = 2) # yint
  equation <- paste(facA, " = ",
 paste( paste(beta, facB, sep="*"), alpha, sep=" + ") ) # equation with intercept, coefficient and predictor variable name
  r_sq <- round(summ[["r.squared"]], digits = 2) #Extract R^2
  r_sq_adj <- round(summ[["adj.r.squared"]], digits = 2) #Extract adjusted R^2 value

  ##### - MODEL ASSUMPTIONS TESTS - #####
  mod_shp <- stats::shapiro.test(model$residuals)$p.value
  mod_bp <- lmtest::bptest(model)$p.value
  mod_dw <- lmtest::dwtest(model)$p.value
  mod_res <- lmtest::resettest(model)$p.value #linearity

  #   Ramsey Regression Equation Specification Error Test (RESET) to detect specification errors in the model (created in 1968 by a student for their dissertation). The RESET performs a nested model comparison with the current model and the current model plus some polynomial terms, and then returns the result of an F-test. The idea is, if the added non-linear terms explain variance in the outcome, then there is a specification error of some kind, such as the failure to include some curvilinear term or the use of a general linear model where a generalized linear model should have been used.
  # sscc.wisc.edu/sscc/pubs/RegDiag-R/linearity.html
  # A significant p-value is an indication that the relationship between the predictors and the outcome needs to be further investigated.
  
  mod_asmp <- c(mod_shp, mod_bp, mod_dw
           # , mod_res
           )
  names(mod_asmp) <- c("Shapiro-Wilk test for Normality of Residuals", "Beusch-Pagan test for Homoscedasticity", "Durbin-Watson test for Autocorrelation (Independent Residual Errors)"
                  # , "RESET for Linearity"
                  )
  fix<-c("Try other preprocessing options, or try other regression models such as SVM or random forest.",
  "Try transforming the dependent variable (e.g., log transformation), or try redefining the dependent variable as a rate.",
  "Is this time series data? Try looking into adding a lag in the independent/dependent variable, or adding a seasonal dummy variable to the model."
  # ,"Try modifying the model or otherwise accounting for endogeneity (unobserved confounding)"
  ) # fit a generalized linear model
  mod_num <- mod_asmp # numeric
  n_fail <- formatC( sum(mod_asmp < 0.05), drop0trailing = TRUE)# was: nassump_fail
  
  if(any(mod_asmp < 0.001)){ # format numbers for printing; 3 decimals after decimal
  mod_asmp[mod_asmp < 0.001] <-
   formatC( mod_asmp[mod_asmp < 0.001] , format = "e", digits = 3)
  
  mod_asmp[mod_num > 0.001] <-
   round( mod_num[mod_num > 0.001], digits = 3)
  }
  
  mod_df <- mod_asmp
  names(mod_df) <- c('mod_shp', 'mod_bp', 'mod_dw'
           # , 'mod_res'
           )
   
  if(n_fail > 0){ # subset for failed tests
   mod_asmp <- mod_asmp[mod_num < 0.05]
  
   f0 <- paste0(n_fail, " linear model assumption test(s) failed: \n")
   
 failed<-c(f0, paste( 
         "TEST: ", names(mod_asmp), " (P-Value: ", mod_asmp, ")\n",
         "TRY: ", fix[mod_num < 0.05], "\n", sep = "" ) )
      # "Please be advised that conforming to these assumptions is necessary for use of the linear model. The results of these tests provide an indication as to how appropriately these assumptions are met. If the goal is to visually explore your data, try the Plotting module." 
    #AddErrMsg(failed)
    message(failed)
    } else {
    failed <- ("No model assumption tests failed.")
    }
  
  df_asmp <- data.frame("Normality (Shapiro-Wilk)..." = mod_df['mod_shp'], #mod_df[mod_num %in% mod_shp]
          "Homoscedasticity (Breusch-Pagan)..." = mod_df['mod_bp'], #mod_df[mod_num %in% mod_bp]
           "Autocorrelation of Residuals\n (Durbin-Watson)..." = mod_df['mod_dw'], #mod_df[mod_num %in% mod_dw]
          # "Linearity (RESET)..." = mod_df['mod_res'], #mod_df[mod_num %in% mod_res]
           "N Assumptions Failed..." = n_fail,
                   check.names = FALSE)
  row.names(df_asmp) <- NULL
# name of df_asmp was df, name of mod_asmp was mod! 202212-13
 ##### - MODEL ASSUMPTIONS TEST DONE - ####
  
  # #Test residuals for normality. Error will be visible to user.
  # norm_resid <- shapiro.test(residuals) 
  # if (norm_resid$p.value < 0.05){
  #   norm_resid_text <- paste0("The residuals are normally distributed. This model is valid.") #To be used in summary, not to be displayed
  # } else {
  #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  #   stop("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  # }
 
   # STORE MODEL ('mod') : model name, model, formula, model assumptions, x var name, y var name
  mSetObj$analSet$linReg$mod <- list(model.name = "Linear Correlation", model = mod, formula = form,  assumptions = df_asmp, response = facA, predictor = facB)

  #STORE RESULTS ('res'): x/y var names, model summary, fitted values,conf intervals, covar mat, equation, rsq, rsq_adj, filename, formula
  ### change response to xlab, predictor to ylab
  mSetObj$analSet$linReg$res <- list(response = facA, predictor = facB, summary = summ, predicted.values = fitt, confidence.intervals = conf.int, covariance.matrix = covar, equation = equation, r.squared.eq = paste("R-squared = ", r_sq), r.squared.adj.eq = paste("R-squared adjusted = ", r_sq_adj), response.vector = input[,facA, drop = TRUE], fileName = fileName) 
  #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  
    
### Printing Values ## ERROR CHECK: BACKSLASH FOLLOWED BY N
  sink(fileName)
  cat("Formula:\n")
  print(deparse(form))
  cat("Model:\n")
  print(equation)
  cat("\nLinear Model Assumption Check:")
  print(t(df_asmp))
  cat(failed)
  print(summ)
  # print(norm_resid)
  # cat("Normality of residuals result:\n")
  # cat(paste0(norm_resid_text, "\n"))
  cat("\nConfidence intervals for predictor variables:")
  print(conf.int)
  cat("\nPredicted values:")
  print(fitt)
  cat("\nCovariance matrix for predictor variables:")
  print(covar)
  sink()
  
  return(.set.mSet(mSetObj))
  
}



#'Generate linear regression plot
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#' default beginning name of plot is: 'corr_linear' (set in LinearView)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
#'@param facB Input the name of the predictor column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
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
#'@param size_title Set size for text of title (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_xlab Set size for x axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ylab Set size for y axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_xtick Set size of text for x axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ytick Set size of text for y axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
## importFrom rlang .data
#'@export

lin.reg.plot <- function(mSetObj=NA,
 facA = "NULL", # response, dropdown
 facB = "NULL", # predictor, dropdown
 data = "false" # checkbox
             
  col_dots = "NULL",
  col_line = "NULL",
  plot_ci = "false",# weights=NULL,
  plot_eq = "false",
  plot_rsq = "false",
  plot_rsq_adj = "false",
  plot_title = " ",
  plot_ylab = " ",
  plot_xlab = " ",

  size_title = "NULL",
  size_xlab = "NULL",
  size_ylab = "NULL",
  size_xtick = "NULL",
  size_ytick = "NULL",
  
  imgName,
  format = "png",
  dpi = 72,
  width = NA
  ){

  library("ggpmisc")
  library("ggplot2")
# library("RJSONIO")
  
  mSetObj <- .get.mSet(mSetObj)
  
  ## DATA: NORMAL OR NOT
  #   if (mSetObj$dataSet$usenorm == TRUE) { 
   if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

 #  ### SET VARIABLES
 #  #SET DEPENDENT (RESPONSE) VARIABLE NAME
 #   if (facA == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facA <- mSetObj$analSet$linReg$res$response
 #     } else {
 #     facA <- colnames(input)[1] #Default is 1st column.
 #    }
 #   } else {
 #     facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
 #   }
 #  #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
 #   if (facB == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facB <-  mSetObj$analSet$linReg$res$predictor
 #     } else {
 #     facB <- colnames(input)[2] #Default is 2nd column.
 #    }
 #   } else {
 #     facB <- facB #Determined using Columns() function (java will present options in drop down menu)
 #   }

  #RETRIEVE RESULTS: 
 # form <- as.formula(paste(facA, "~", facB, sep = ""))
 # mod <- lm(formula = form, data = input, weights = NULL)
 fileName <- mSetObj$analSet$linReg$res$fileName
 facA <- mSetObj$analSet$linReg$res$response
 facB <-  mSetObj$analSet$linReg$res$predictor
 form <- mSetObj$analSet$linReg$mod$formula 
 mod <- mSetObj$analSet$linReg$mod$model 
  
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
  
  #NAME PLOT FOR DOWNLOAD
  # must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.linReg <- imgName
  
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
    plot_title1 <- paste("Univariate Linear Regression (", facA, " ~ ", facB, ")", sep = "")
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

# PLOT TEXT SIZE
# size_base <- theme_bw()$text$size # 11
# stackoverflow.com/questions/53560599/how-to-change-the-default-font-size-in-ggplot2-including-geom-text
 size_base <- 12
  #SET TITLE SIZE
  size_title1 <-
				switch(
					size_title,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X LABEL SIZE
  size_xlab1 <-
				switch(
					size_xlab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)
 #SET Y LABEL SIZE
  size_ylab1 <-
				switch(
					size_ylab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET Y TICK SIZE
  size_ytick1 <-
				switch(
					size_ytick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X TICK SIZE
  size_xtick1 <-
				switch(
					size_xtick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

if(any(!c(size_xlab, size_ylab) %in% "NULL")){
if(all(!c(size_xlab, size_ylab) %in% "NULL")){
size_leg <- size_xlab
} else {
size_leg <- c(size_xlab, size_ylab)[!c(size_xlab, size_ylab) %in% "NULL"]
}
} else {
size_leg <- size_xlab
}

  # #IF THERE IS A LINE COLOR, OVERRIDE DOT COLOR TO BLACK
  # if(!all.equal(col_line1, "black")){
  #   col_dots1 <- "black"
  # }

# PLOT 
 # THEME 
#  theme_lineplot <- function(){theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size=12, colour="black"),    axis.title=element_text(size=12), legend.title=element_text(12), legend.text=element_text(size=12), plot.title=element_text(face='bold',hjust = 0.5)
#   )}
  ## NOTE THE .data ARGUMENT; 
  ##  To avoid a note from CMD check about .data, use #' @importFrom rlang .data in any roxygen code block (typically in package documentation as generated by usethis::use_package_doc()) ; ggplot2.tidyverse.org/articles/ggplot2-in-packages.html
   
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
        #axis.text = element_text(size = 12, colour = "black"), 
        axis.title.x = element_text(size = size_xlab1),
        axis.title.y = element_text(size = size_ylab1),
        axis.text.x = element_text(size = size_xtick1),
        axis.text.y = element_text(size = size_ytick1),
        # legend.title=element_text(size_leg), legend.text=element_text(), 
        plot.title = element_text(size = size_title1, face = 'bold', hjust = 0.5)
  )

## for JSON object:
if(plot_ci1 == TRUE){
aj <- a0
} else{ #set se to TRUE to make confidence interval regardless
aj <- ggplot(data = input,
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
     aes_(x = as.name(facA), y = as.name(facB)) )+
     labs(title = plot_title1) + ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = TRUE, color = col_line1, fullrange = TRUE, method = 'lm') +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text = element_text(size = 12, colour = "black"),  axis.title = element_text(size = 12), plot.title = element_text(face = 'bold', hjust = 0.5)  )
} 

  
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

  mSetObj$analSet$linReg$plotted <- list(
   plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

    #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")

  print(a0)
  # a0
  dev.off()
  
# JSON MAKING
build <- ggplot_build(aj)
build_line <- build$data[[1]] ### line is 1
build_points <- build$data[[2]]
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
 ## linear_plot_json$label <- build$data[[3]][,c("label")]; # build$data[[1]][,c("se")]
  if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x=build_line[,c("x")], y=build_line[,c("y")], CI_down = 0, CI_up = 0)
 }   
  

## BOOLEANS
if(plot_ci1 == TRUE){
 linear_plot_json$bool_ci <- TRUE
 } else{
linear_plot_json$bool_ci <- FALSE
}

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


#   #### PASTE VERSION
#    df <- data.frame(x=build$data[[1]][,c("x")],
#                   y=build$data[[1]][,c("y")],
#               point_cols = build$data[[1]][,grepl("col",colnames(build$data[[1]]))],
#               point_shape = build$data[[1]][,c("group")],
#               point_size = build$data[[1]][,c("size")],
#               line_cols = build$data[[2]][,grepl("col",colnames(build$data[[2]]))][1] ) 
#   
#   if(grepl("ymin", colnames(build$data[[1]])) && grepl("ymax", colnames(build$data[[1]])) ){
#   df$CI_down <-build$data[[1]][,c("ymin")] 
#   df$CI_up <- build$data[[1]][,c("ymax")]
#  } else{
#    df$CI_down <- NULL
#    df$CI_up <- NULL
# }
# 
# aa<-apply(df, 1, function(x){
#   paste(x, sep= paste0(colnames(df),": ") )
# })
# bb<-apply(aa, 2, function(x){ paste(
#     paste0(colnames(df),": "), x
#   ) })
# cc<- apply(bb, 2,  function(x){
#           paste(x, collapse=",")
#         } )
# dd<-paste( seq_along(cc),": {",cc, "}" )
# ee<- paste(
#   "data: {", 
#   paste(dd, collapse=","), "}"
# )
# 
#   linear_plot_json$main <- plot_title1 #title
#   linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
#   linear_plot_json$data <- ee
 
  
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
 print(paste("PLOT1: facA: ", facA, " | facB: ", facB,  sep = ""))
 print("we built this city JSON rock and roll")

# lin.reg.plot.json(mSetObj, "plotted")
# lin.reg.plot.json(mSetObj)

 if(!.on.public.web){
  return(.set.mSet(mSetObj))
    }
}




### PREDICTED PLOTTING FUNCTION
#'Generate prediction/actual linear regression plot
#' default beginning name of plot is: 'corr_linear_pred' (set in LinearView)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
#'@param facB Input the name of the predictor column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
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
#'@param size_title Set size for text of title (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_xlab Set size for x axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ylab Set size for y axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_xtick Set size of text for x axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ytick Set size of text for y axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
## importFrom rlang .data
#'@export

lin.pred.plot <- function(mSetObj=NA,
  facA = "NULL", # response, dropdown
  facB = "NULL", # predictor, dropdown
  data = "false" # checkbox
             
  col_dots = "NULL",
  col_line = "NULL",
  plot_ci = "false",
  plot_eq = "false",
  plot_rsq = "false",
  plot_rsq_adj = "false",
  plot_title = " ",
  plot_ylab = " ",
  plot_xlab = " ",

  size_title = "NULL",
  size_xlab = "NULL",
  size_ylab = "NULL",
  size_xtick = "NULL",
  size_ytick = "NULL",

  imgName,
  format = "png",
  dpi = 72,
  width = NA
  ){

  library("ggpmisc")
  library("ggplot2")
# library("RJSONIO")
  
  mSetObj <- .get.mSet(mSetObj)
  
  ## DATA: NORMAL OR NOT
  #  if (mSetObj$dataSet$usenorm == TRUE) { 
   if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
    #  mSetObj$dataSet$usenorm <- TRUE
  } else {
    input <- mSetObj$dataSet$orig
    # mSetObj$dataSet$usenorm <- FALSE
  }
  
  #RETRIEVE RESULTS:
 #  ### SET VARIABLES
 #  #SET DEPENDENT (RESPONSE) VARIABLE NAME
 #   if (facA == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facA <- mSetObj$analSet$linReg$res$response
 #     } else {
 #     facA <- colnames(input)[1] #Default is 1st column.
 #    }
 #   } else {
 #     facA <- facA #Determined using Columns() function below (java will present numeric column options in drop down menu)
 #   }
 #   #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
 #   if (facB == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facB <-  mSetObj$analSet$linReg$res$predictor
 #     } else {
 #     facB <- colnames(input)[2] #Default is 2nd column.
 #    }
 #   } else {
 #     facB <- facB #Determined using Columns() function (java will present numeric column options in drop down menu)
 #   }

   
   # formula1 <- as.formula(paste(facA, "~", facB, sep = ""))
   # model1 <- lm(formula = form, data = input, weights = NULL) 
   # prediction <- fitted(model1) 

   facA <- mSetObj$analSet$linReg$res$response
   facB <-  mSetObj$analSet$linReg$res$predictor
   form <- mSetObj$analSet$linReg$mod$formula 
   mod <- mSetObj$analSet$linReg$mod$model 
   prediction <- mSetObj$analSet$linReg$res$predicted.values
   fileName <- mSetObj$analSet$linReg$res$fileName

   dfpred <- data.frame( fpred = prediction, fA = input[,facA])
   formula2 <- as.formula("fA ~ fpred")
   model2 <- lm(formula = formula2, data = dfpred, weights = NULL)

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
  
  #NAME PLOT FOR DOWNLOAD
  # must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.linReg1 <- imgName
  
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
 
# PLOT TEXT SIZE
# size_base <- theme_bw()$text$size # 11
# stackoverflow.com/questions/53560599/how-to-change-the-default-font-size-in-ggplot2-including-geom-text
 size_base <- 12
  #SET TITLE SIZE
  size_title1 <-
				switch(
					size_title,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X LABEL SIZE
  size_xlab1 <-
				switch(
					size_xlab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)
 #SET Y LABEL SIZE
  size_ylab1 <-
				switch(
					size_ylab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET Y TICK SIZE
  size_ytick1 <-
				switch(
					size_ytick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X TICK SIZE
  size_xtick1 <-
				switch(
					size_xtick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

if(any(!c(size_xlab, size_ylab) %in% "NULL")){
if(all(!c(size_xlab, size_ylab) %in% "NULL")){
size_leg <- size_xlab
} else {
size_leg <- c(size_xlab, size_ylab)[!c(size_xlab, size_ylab) %in% "NULL"]
}
} else {
size_leg <- size_xlab
}

# PLOT
  #baseR: plot(x = prediction, y = input[,facA], xlab = "Predicted", ylab = "Actual", main = method, yaxt = "n")
  ## y actual input[,facA] fA |  x prediction fpred
  a0 <- ggplot(data =  dfpred, #data.frame(fpred = prediction, fA = input[,facA]),
  aes(x = fpred, y = fA)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE, method='lm') +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #axis.text = element_text(size = 12, colour = "black"), 
        axis.title.x = element_text(size = size_xlab1),
        axis.title.y = element_text(size = size_ylab1),
        axis.text.x = element_text(size = size_xtick1),
        axis.text.y = element_text(size = size_ytick1),
        # legend.title=element_text(size_leg), legend.text=element_text(), 
        plot.title = element_text(size = size_title1, face = 'bold', hjust = 0.5)
  )

  
#STORE IN mset
  
  mSetObj$analSet$linReg$plotPred <- list(
    plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

 #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")

  print(a0)
  # a0
  dev.off()
  
# lin.reg.plot.json(mSetObj=mSetObj, which_plot = "pred")

# JSON MAKING

if(plot_ci1 == TRUE){
 aj <- a0
 } else { # include se regardless, need it for json making
aj <- ggplot(data = dfpred, # data.frame(fpred = prediction, fA = input[,facA]),
  aes(x = fpred, y = fA)) +
     labs(title = plot_title1) + ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = TRUE, color = col_line1, fullrange = TRUE, method = 'lm') +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text = element_text(size = 12, colour = "black"),  axis.title = element_text(size = 12), plot.title = element_text(face = 'bold', hjust = 0.5)  )
}

build <- ggplot_build(aj)
build_line <- build$data[[1]] ### line is 1
build_points <- build$data[[2]]
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
    summary(mod)[["r.squared"]]#Extract R^2
  linear_plot_json$r_sq_adj <-
   summary(mod)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$slope <-
    summary(mod)[["coefficients"]][2] # beta
  linear_plot_json$yint <-
   summary(mod)[["coefficients"]][1] # alpha

## WRITE JSON
 json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
 sink(imgName2)
 cat(json.obj)
 sink()
 print(json.obj)
 print(paste("PLOT2: facA: ", facA, " | facB: ", facB,  sep = ""))
 print("carry on my wayward JSON")

# lin.reg.plot.json(mSetObj, "plotted")
# lin.reg.plot.json(mSetObj)

 if(!.on.public.web){
  return(.set.mSet(mSetObj))
    }
  
}


#'Generate Normality of Residuals (qqplot) plot for model
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#' default beginning name of plot is: 'corr_linear_normres' (set in LinearView)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
#'@param facB Input the name of the predictor column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param col_line Set color for line (default "NULL" is blue); (static dropdown)
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param plot_title Input the name of the title (default: "Normality of Residuals (response ~ predictor)" where predictor is predictor name and response is response name, textbox)
#'@param size_title Set size for text of title (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_xlab Set size for x axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ylab Set size for y axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)s
#'@param size_xtick Set size of text for x axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ytick Set size of text for y axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
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
### importFrom rlang .data
#'@export

lin.qq.plot <- function(mSetObj=NA,

  facA = "NULL", # response, dropdown
  facB = "NULL", # predictor, dropdown
  data = "false" # checkbox
             
  col_dots = "NULL",
  col_line = "NULL",
  
  plot_title = " ",
  plot_ylab = " ",
  plot_xlab = " ",

  size_title = "NULL",
  size_xlab = "NULL",
  size_ylab = "NULL",
  size_xtick = "NULL",
  size_ytick = "NULL",

  imgName,
  format = "png",
  dpi = 72,
  width = NA
  ){

## ADD CONFIDENCE INTERVALS:
## tjmahr.com/quantile-quantile-plots-from-scratch/
## QQPLOTS WITH FACETS: mgimond.github.io/ES218/Week06a.html

  library("ggplot2")
# library("RJSONIO")
  
  mSetObj <- .get.mSet(mSetObj)
  
  ## DATA: NORMAL OR NOT
  # if(mSetObj$dataSet$usenorm == TRUE) {
   if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

  #RETRIEVE RESULTS:
 #  ### SET VARIABLES
 #  #SET DEPENDENT (RESPONSE) VARIABLE NAME
 #   if (facA == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facA <- mSetObj$analSet$linReg$res$response
 #     } else {
 #     facA <- colnames(input)[1] #Default is 1st column.
 #    }
 #   } else {
 #     facA <- facA #Determined using Columns() function below (java will present numeric column options in drop down menu)
 #   }
 #   #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
 #   if (facB == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facB <-  mSetObj$analSet$linReg$res$predictor
 #     } else {
 #     facB <- colnames(input)[2] #Default is 2nd column.
 #    }
 #   } else {
 #     facB <- facB #Determined using Columns() function (java will present numeric column options in drop down menu)
 #   }


#  # VARIABLE TYPE CHECK
#  if (is.factor(input[,facA] || input[,facB]) == TRUE){
#    #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
#    stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
#  }
  
  # EXTRACT PLOT COMPONENTS
   # form <- as.formula(paste(facA, "~", facB, sep = "")) 
   # mod <- lm(formula = formula, data = input, weights = NULL)
    facA <- mSetObj$analSet$linReg$res$response
    facB <- mSetObj$analSet$linReg$res$predictor
    form <- mSetObj$analSet$linReg$mod$formula
    mod <- mSetObj$analSet$linReg$mod$model 
    fileName <- mSetObj$analSet$linReg$res$fileName


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
  
  #NAME PLOT FOR DOWNLOAD
  # must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.linReg1 <- imgName
  
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
					"NULL" = "blue",
					"black" = "black",
					"blue" = "blue",
					"red" = "red",
					"green" = "green",
					"grey" = "grey",
					NULL
				)

  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste("Normality of Residuals (", facA, " ~ ", facB, ")", sep = "")
  } else {
    plot_title1 <- plot_title
  }

  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Empirical"
  } else { # facA, response
    plot_ylab1 <- plot_ylab
  }

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Theoretical"
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
 
# PLOT TEXT SIZE
# size_base <- theme_bw()$text$size # 11
# stackoverflow.com/questions/53560599/how-to-change-the-default-font-size-in-ggplot2-including-geom-text
 size_base <- 12
  #SET TITLE SIZE
  size_title1 <-
				switch(
					size_title,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X LABEL SIZE
  size_xlab1 <-
				switch(
					size_xlab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)
 #SET Y LABEL SIZE
  size_ylab1 <-
				switch(
					size_ylab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET Y TICK SIZE
  size_ytick1 <-
				switch(
					size_ytick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X TICK SIZE
  size_xtick1 <-
				switch(
					size_xtick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

if(any(!c(size_xlab, size_ylab) %in% "NULL")){
if(all(!c(size_xlab, size_ylab) %in% "NULL")){
size_leg <- size_xlab
} else {
size_leg <- c(size_xlab, size_ylab)[!c(size_xlab, size_ylab) %in% "NULL"]
}
} else {
size_leg <- size_xlab
}

  y_val <- quantile(stats::rstandard(mod), c(0.25, 0.75))
  x_val <- qnorm(c(0.25, 0.75))
  slope_val <- diff(y_val)/diff(x_val)
  int_val <- y_val[1L] - slope_val * x_val[1L]
  
## FOR JSON MAKING
 q1_q3 <- data.frame(      ## tjmahr.com/quantile-quantile-plots-from-scratch/
   x = qnorm(c(.25, .75)),
   y = quantile(stats::rstandard(mod), c(.25, .75)),
   quantile = c(1,3)
 )


# PLOT

a0 <- ggplot() +
  geom_qq(aes(sample = stats::rstandard(mod) ),  shape = 16, color = col_dots1) +
  geom_abline(color = col_line1, slope = slope_val, intercept = int_val) +
  labs(title = plot_title1) +
  ylab(plot_ylab1)+ xlab(plot_xlab1) +
     # stat_qq(shape = 16, color = col_dots1) +
     # stat_qq_line(aes(sample = stats::rstandard(mod) ), color = col_line1) +
     # geom_qq_line(aes(sample = stats::rstandard(mod) ), color = col_line1)+
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #axis.text = element_text(size = 12, colour = "black"), 
        axis.title.x = element_text(size = size_xlab1),
        axis.title.y = element_text(size = size_ylab1),
        axis.text.x = element_text(size = size_xtick1),
        axis.text.y = element_text(size = size_ytick1),
        # legend.title=element_text(size_leg), legend.text=element_text(), 
        plot.title = element_text(size = size_title1, face = 'bold', hjust = 0.5)
  ) #+ geom_point(aes(x = x, y = y), data = q1_q3,  shape = 3, size = 5,   stroke = 1.1)


#STORE IN mset
  mSetObj$analSet$linReg$plotNorm <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

 #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
 
  print(a0)
  # a0
  dev.off()


# JSON MAKING
build <- ggplot_build(a0)
build_line <- build$data[[2]] ### NOTE line is 2 in list, points is 1
build_points <- build$data[[1]]
linear_plot_json <- list()

linear_plot_json$main <- plot_title1 #title
linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles

linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
linear_plot_json$points$size <- build_points[,c("size")]#[,7]
linear_plot_json$lines$coords_q1q3 <- q1_q3
linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
linear_plot_json$lines$size <- build_line[,c("size")]
  
  #### MODEL VARS FOR LINE
  linear_plot_json$r_sq <-"NA"
   # summary(mod)[["r.squared"]] #Extract R^2
  linear_plot_json$r_sq_adj <-"NA"
   # summary(mod)[["adj.r.squared"]]  #Extract adjusted R^2 
  linear_plot_json$slope <- build_line[,c("slope")] #slope_val 
    # summary(mod)[["coefficients"]][2] # beta
  linear_plot_json$yint <- build_line[,c("intercept")] #int_val 
   # summary(mod)[["coefficients"]][1] # alpha

## WRITE JSON 
 json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
 sink(imgName2)
 cat(json.obj)
 sink()
 print(json.obj)
 print(paste("PLOT3: facA: ", facA, " | facB: ", facB,  sep = ""))
 print("livin json a prayer")

# lin.reg.plot.json(mSetObj)
# lin.reg.plot.json(mSetObj=mSetObj, which_plot = "norm")


 if(!.on.public.web){
  return(.set.mSet(mSetObj))
    }
  
}



#'Generate Residuals vs. Fitted values plot for model
#'@description Plot residuals and predicted (fitted or yhat values) of linear regression model on 2 variables in data (check linearity assumption)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
#'@param facB Input the name of the predictor column (default 1st column, dropdown) (java uses Columns() to give user numeric column options)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param col_line Set color for line (default "NULL" is blue); (static dropdown)
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param plot_title Input the name of the title (default: "Univariate Linear Regression Line of Best Fit", textbox)
#'@param plot_xlab Input the name to use for x-axis label (default: facA, textbox)
#'@param plot_ylab Input the name to use for y-axis label (default: facB, textbox)
#'@param size_title Set size for text of title (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_xlab Set size for x axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ylab Set size for y axis label (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)s
#'@param size_xtick Set size of text for x axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
#'@param size_ytick Set size of text for y axis ticks (default: 'NULL' = 'medium', options: very small, small, medium, large, very large; dropdown)
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
lin.resfit.plot <- function(mSetObj=NA,

  facA = "NULL", # response, dropdown
  facB = "NULL", # predictor, dropdown
  data = "false" # checkbox

  col_dots = "NULL",
  col_line = "NULL",

  plot_title = " ",
  plot_ylab = " ",
  plot_xlab = " ",

  size_title = "NULL",
  size_xlab = "NULL",
  size_ylab = "NULL",
  size_xtick = "NULL",
  size_ytick = "NULL",

  imgName,
  format = "png",
  dpi = 72,
  width = NA
  ){

  library("ggplot2")
# library("RJSONIO")
  
  mSetObj <- .get.mSet(mSetObj)
  
  ## DATA: NORMAL OR NOT
  #  if (mSetObj$dataSet$usenorm == TRUE) { 
   if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

  
  #RETRIEVE RESULTS:
 #  ### SET VARIABLES
 #  #SET DEPENDENT (RESPONSE) VARIABLE NAME
 #   if (facA == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facA <- mSetObj$analSet$linReg$res$response
 #     } else {
 #     facA <- colnames(input)[1] #Default is 1st column.
 #    }
 #   } else {
 #     facA <- facA #Determined using Columns() function below (java will present numeric column options in drop down menu)
 #   }
 #   #SET INDEPENDENT (PREDICTOR) VARIABLE NAME
 #   if (facB == "NULL"){
 #     if( !"res" %in% names(mSetObj$analSet$linReg) ){
 #        facB <-  mSetObj$analSet$linReg$res$predictor
 #     } else {
 #     facB <- colnames(input)[2] #Default is 2nd column.
 #    }
 #   } else {
 #     facB <- facB #Determined using Columns() function (java will present numeric column options in drop down menu)
 #   }
   
   # VARIABLE TYPE CHECK
   # if (is.factor(input[,facA]) || is.factor(input[,facB]) ){
   #  #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
   #  stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
   # }
  
   # EXTRACT PLOT COMPONENTS
   # form <- as.formula(paste0(facA, "~", facB)) 
   # mod <- lm(formula = formula, data = input, weights = NULL)   

   facA <- mSetObj$analSet$linReg$res$response
   facB <-  mSetObj$analSet$linReg$res$predictor
   form <- mSetObj$analSet$linReg$mod$formula
   mod <- mSetObj$analSet$linReg$mod$model
   fileName <- mSetObj$analSet$linReg$res$fileName

   dfres <- data.frame(resid = residuals(mod), fit = fitted(mod))
   formula2 <- as.formula(paste0("fit~resid"))
   model2 <- lm(formula = formula2, data = dfres, weights = NULL)

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
  
  #NAME PLOT FOR DOWNLOAD
  # must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.linReg <- imgName
  
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
					"NULL" = "blue",
					"black" = "black",
					"blue" = "blue",
					"red" = "red",
					"green" = "green",
					"grey" = "grey",
					NULL
				)

  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste0("Residuals vs. Fitted (", facA," ~ ", facB, ")")
  } else {
    plot_title1 <- plot_title
  }
  
  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Residuals"
  } else { # facA, response
    plot_ylab1 <- plot_ylab
  }

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Fitted"
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
  
# PLOT TEXT SIZE
# size_base <- theme_bw()$text$size # 11
# stackoverflow.com/questions/53560599/how-to-change-the-default-font-size-in-ggplot2-including-geom-text
 size_base <- 12
  #SET TITLE SIZE
  size_title1 <-
				switch(
					size_title,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X LABEL SIZE
  size_xlab1 <-
				switch(
					size_xlab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)
 #SET Y LABEL SIZE
  size_ylab1 <-
				switch(
					size_ylab,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET Y TICK SIZE
  size_ytick1 <-
				switch(
					size_ytick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

 #SET X TICK SIZE
  size_xtick1 <-
				switch(
					size_xtick,
					"NULL" = size_base,
					"medium" = size_base,
					"small" = 0.8*size_base,
					"extrasmall" = 0.6*size_base,
                                      "large" = 1.2*size_base,
					"extralarge" = 1.4*size_base,
					NULL
				)

if(any(!c(size_xlab, size_ylab) %in% "NULL")){
if(all(!c(size_xlab, size_ylab) %in% "NULL")){
size_leg <- size_xlab
} else {
size_leg <- c(size_xlab, size_ylab)[!c(size_xlab, size_ylab) %in% "NULL"]
}
} else {
size_leg <- size_xlab
}


# PLOT
   # a0 <- ggplot(data =  input,
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
   # aes_(sample =  as.name(facA)) )+
  a0 <- ggplot(data = dfres, #data.frame(resid = residuals(mod), fit = fitted(mod)),  
     aes(x = fit, y = resid)) +
     labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_point(shape = 16, color = col_dots1) + 
     geom_hline(yintercept = 0) +
     geom_smooth(color = col_line1, fullrange = TRUE, se = FALSE) +
     theme_bw() + 
theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #axis.text = element_text(size = 12, colour = "black"), 
        axis.title.x = element_text(size = size_xlab1),
        axis.title.y = element_text(size = size_ylab1),
        axis.text.x = element_text(size = size_xtick1),
        axis.text.y = element_text(size = size_ytick1),
        # legend.title=element_text(size_leg), legend.text=element_text(), 
        plot.title = element_text(size = size_title1, face = 'bold', hjust = 0.5)
  ) 

#STORE IN mset
  mSetObj$analSet$linReg$plotFit <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   
  print(a0)
  # a0
  dev.off()
  
 # JSON MAKING
build <- ggplot_build(a0)
build_line <- build$data[[3]] ### line is 2 and 3! 2 is horizontal black line
build_line2 <- build$data[[2]]
build_points <- build$data[[1]]
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

# horizontal black line
linear_plot_json$lines2$cols <- build_line2[,grepl("col",colnames(build_line2))]
linear_plot_json$lines2$size <- build_line2[,c("size")]
linear_plot_json$lines2$yint <- build_line2[,c("yintercept")]
linear_plot_json$lines2$slope <- 0
 
  
  #### MODEL VARS FOR LINE
  linear_plot_json$r_sq <-
    summary(model2)[["r.squared"]] #Extract R^2
  linear_plot_json$r_sq_adj <-
    summary(model2)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$slope <-
    summary(model2)[["coefficients"]][2] # beta
  linear_plot_json$yint <-
    summary(model2)[["coefficients"]][1] # alpha

 json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
 sink(imgName2)
 cat(json.obj)
 sink()
 print(json.obj)
 print(paste("PLOT4: facA: ", facA, " | facB: ", facB,  sep = ""))
 print("json the road again")

# lin.reg.plot.json(mSetObj, "plotted")
# lin.reg.plot.json(mSetObj)
# lin.reg.plot.json(mSetObj=mSetObj, which_plot = "fit")


 if(!.on.public.web){
  return(.set.mSet(mSetObj))
    }

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
 

  mod <- mSetObj$analSet$linReg$mod$model
  # colnames(build$data[[1]]);  c("x", "y", "flipped_aes", "PANEL", "group", "colour", "fill",  "size", "linetype", "weight", "alpha")
  linear_plot_json <- list()



if(which_plot == "NULL" || which_plot == "plot" || which_plot == "plotted"){
  a0 <- mSetObj$analSet$linReg$plotted$plot
  plot_title1 <- mSetObj$analSet$linReg$plotted$title
  plot_ylab1 <- mSetObj$analSet$linReg$plotted$ylab
  plot_xlab1 <- mSetObj$analSet$linReg$plotted$xlab
} else if(which_plot == "pred"){

  # a0 <- mSetObj$analSet$linReg$plotPred$plot
  plot_title1 <- mSetObj$analSet$linReg$plotPred$title
  plot_ylab1 <- mSetObj$analSet$linReg$plotPred$ylab
  plot_xlab1 <- mSetObj$analSet$linReg$plotPred$xlab

### IF CONFIDENCE INTERVALS  PRESENT
if(any(grepl("ymin", colnames( ggplot_build(mSetObj$analSet$linReg$plotPred$plot)$data[[1]] )))){
 plot_ci1 <- TRUE
 aj <- mSetObj$analSet$linReg$plotPred$plot
 } else { # include se regardless, need it for json making
 plot_ci1 <- FALSE
 aj <- ggplot(data = data.frame(fpred = mSetObj$analSet$linReg$res$predicted.values, fA = mSetObj$analSet$linReg$res$response.vector,
  aes(x = fpred, y = fA)) +
     labs(title = plot_title1) + ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = TRUE, color = col_line1, fullrange = TRUE, method = 'lm') +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.text = element_text(size = colour = "black"),  axis.title = element_text(colour = "black"), plot.title = element_text(face = 'bold', hjust = 0.5)  )
}

build <- ggplot_build(aj)
build_line <- build$data[[1]] ### line is 1
build_points <- build$data[[2]]
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
if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 }
## BOOLEANS
 linear_plot_json$bool_ci <- plot_ci1

  #### MODEL VARS FOR LINE
  linear_plot_json$r_sq <-
    summary(mod)[["r.squared"]]#Extract R^2
  linear_plot_json$r_sq_adj <-
   summary(mod)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$slope <-
    summary(mod)[["coefficients"]][2] # beta
  linear_plot_json$yint <-
   summary(mod)[["coefficients"]][1] # alpha

} else if(which_plot == "norm"){
# QQ NORM 
  a0 <- mSetObj$analSet$linReg$plotNorm$plot
  plot_title1 <- mSetObj$analSet$linReg$plotNorm$title
  plot_ylab1 <- mSetObj$analSet$linReg$plotNorm$ylab
  plot_xlab1 <- mSetObj$analSet$linReg$plotNorm$xlab
 build <- ggplot_build(a0)

build_line <- build$data[[2]] ### NOTE line is 2 in list, points is 1
build_points <- build$data[[1]]

linear_plot_json$main <- plot_title1 #title
linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles

linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
linear_plot_json$points$size <- build_points[,c("size")]#[,7]
linear_plot_json$lines$coords_q1q3 <- data.frame(      
   x = qnorm(c(.25, .75)),
   y = quantile(stats::rstandard(mod), c(.25, .75)),
   quantile = c(1,3)
 )
linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
linear_plot_json$lines$size <- build_line[,c("size")]
  
  #### MODEL VARS FOR LINE
  linear_plot_json$r_sq <-"NA"
   # summary(mod)[["r.squared"]] #Extract R^2
  linear_plot_json$r_sq_adj <-"NA"
   # summary(mod)[["adj.r.squared"]]  #Extract adjusted R^2 
  linear_plot_json$slope <- build_line[,c("slope")] #slope_val 
    # summary(mod)[["coefficients"]][2] # beta
  linear_plot_json$yint <- build_line[,c("intercept")] #int_val 
   # summary(mod)[["coefficients"]][1] # alpha


} else if(which_plot == "fit"){

 # RESIDUALS vs FITTED
  a0 <- mSetObj$analSet$linReg$plotFit$plot
  plot_title1 <- mSetObj$analSet$linReg$plotFit$title
  plot_ylab1 <- mSetObj$analSet$linReg$plotFit$ylab
  plot_xlab1 <- mSetObj$analSet$linReg$plotFit$xlab
  build <- ggplot_build(a0)

   dfres <- data.frame(resid = residuals(mod), fit = fitted(mod))
   formula2 <- as.formula(paste0("fit~resid"))
   model2 <- lm(formula = formula2, data = dfres, weights = NULL)

build_line <- build$data[[3]] ### line is 2 and 3! 2 is horizontal black line
build_line2 <- build$data[[2]]
build_points <- build$data[[1]]

linear_plot_json$main <- plot_title1 
linear_plot_json$axis <- c(plot_xlab1, plot_ylab1)
linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
linear_plot_json$points$size <- build_points[,c("size")]#[,7]
linear_plot_json$lines$coords <- build_line[,c("x","y")]
linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
linear_plot_json$lines$size <- build_line[,c("size")]
# horizontal black line
linear_plot_json$lines2$cols <- build_line2[,grepl("col",colnames(build_line2))]
linear_plot_json$lines2$size <- build_line2[,c("size")]
linear_plot_json$lines2$yint <- build_line2[,c("yintercept")]
linear_plot_json$lines2$slope <- 0
  #### MODEL VARS FOR LINE
  linear_plot_json$r_sq <-
    summary(model2)[["r.squared"]] #Extract R^2
  linear_plot_json$r_sq_adj <-
    summary(model2)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$slope <-
    summary(model2)[["coefficients"]][2] # beta
  linear_plot_json$yint <-
    summary(model2)[["coefficients"]][1] # alpha

}
  imgName <- mSetObj$imgSet$plot.linReg
  # facA <- mSetObj$analSet$linReg$res$response
  # facB <- mSetObj$analSet$linReg$res$predictor


  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles

  linear_plot_json$points$coords <- build$data[[1]] ["x"]["y"] #[,c("x","y")] #[,1:2]
  linear_plot_json$points$cols <- build$data[[1]][,grepl("col",colnames(build$data[[1]]))] #[,6] #colours
  linear_plot_json$points$shape <- build$data[[1]][,c("group")]#[,5]
  linear_plot_json$points$size <- build$data[[1]][,c("size")]#[,7]
  linear_plot_json$lines$cols <- build$data[[2]][,grepl("col",colnames(build$data[[2]]))]


  linear_plot_json$r_sq <- round(summary(mod)[["r.squared"]], digits = 2) #Extract R^2
  linear_plot_json$r_sq_adj <- round(summary(mod)[["adj.r.squared"]], digits = 2) #Extract adjusted R^2 
  linear_plot_json$slope <- round(summary(mod)[["coefficients"]][2], digits = 2) # beta
  linear_plot_json$yint <- round(summary(mod)[["coefficients"]][1], digits = 2) # alpha

  imgName2 <- paste(imgName, ".json", sep="")
  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName2)
  cat(json.obj)
  sink()
print(json.obj)
print("got it")

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
#   mSetObj$imgSet$plot.linReg <- imgName
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
  
  data <- select_if(mSetObj$dataSet$norm, is.numeric)
# data <- input[,sapply(input, is.numeric), drop = FALSE]
  count.all.numeric.cols <- ncol(data)
  name.all.numeric.cols <- colnames(data)
  
  num.col.results <- list(
    count = count.all.numeric.cols,
    names = name.all.numeric.cols
  )
  
#  return(num.col.results) # list: count, names 
  return(name.all.numeric.cols)
  
}

#'Return filename for summary textfile generated'
#'@description Java get textfile name, to be able to be put on HTML page as a separate downloadable link
#'@param mSetObj Input name of the created mSetObject 
#'@author Gina Sykes\email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

lin.reg.get.results <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj)

#   lin.reg.result <- c(mSetObj$analSet$linReg$res$equation,
# mSetObj$analSet$linReg$res$r.squared.eq,
# mSetObj$analSet$linReg$res$r.squared.adj.eq)

lin.reg.result <- mSetObj$analSet$linReg$res$fileName

  return(lin.reg.result)

}

