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


multi.reg.anal <- function(mSetObj=NA, 
                               facA="NULL",
                               predtext="NULL",
                               data="false" 
                               # weights=NULL
                           ){

  #install.packages(c("relaimpo"))
  library("dplyr")
  library("Metrics")
  library("relaimpo")

  mSetObj <- .get.mSet(mSetObj)
  
   ### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #TEXT IS VISIBLE TO USER. Default dependent var is 1st column
  cat("One dependent variable and multiple independent variables will be tested for correlation. All must have numeric values.")
  
  #SET RESPONSE (DEPENDENT) VARIABLE
  if (facA == "NULL") {
    for (i in seq_along(colnames(input)) ) {
      if (is.factor(input[,i]) == FALSE) {
        facA <- colnames(input)[i]# Default is to choose the first numeric column as response column
        break
      }
    }
  } else {
    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
  }
  
  #Textbox instructions for selecting predictor variables. Textbox should be interactive, ie changes in text alter results in real time. Default predtext = 2nd col
  cat("Indicate independent variables withcomma-separated column names. If interested in an interaction, indicate it between particular variables with a colon.")
  
  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
  if (predtext == "NULL") {
    #resp.col.num <- which(colnames(input) == facA); data <- input[,-resp.col.num]
    dat <- input[ , colnames(input) != facA]
    num.data <- dplyr::select_if(dat, is.numeric)
    predtext <- colnames(num.data)[1] #Default is the 1st potential predictor column
    # predtext <- paste0(predtext, ",")
  } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
#CHECK PREDTEXT FOR COMMAS
   if( !any(grepl(",", predtext, fixed = TRUE, perl=TRUE)) ){ # if there are no commas in input predictor name(s)
if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ # can't be >1 other cols to use, so if there is, error
warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
}}

  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
predtext1 <- predtext

  predtext <- gsub("\n", "", predtext, fixed = TRUE) #fixed=TRUE means dealing with 1 string, vs a vector of strings (fixed=FALSE)
  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
  predtext <- gsub(";", "+", predtext, fixed = TRUE)
  predtext <- gsub(" ", "", predtext, fixed = TRUE)
  predtext <- gsub(":", "+", predtext, fixed = TRUE)
  predtext <- gsub("*", "+", predtext, fixed = TRUE)  

  #GENERATE FORMULA
  formula <- as.formula(paste(facA, "~", predtext))
  #Text should be visible to user
  cat(paste0("You have created this formula for model building: ", facA, " ~ ", predtext, " ."))
  # cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
   cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
   ### CHECK: are all input predictor names in data
  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
  
  # if(any(!colnames(data) %in% predictors2)){
if(!all(predictors2 %in% colnames(data)) ){
   stop(paste0( "'", predictors2[!predictors2 %in% colnames(data)],
  "' not found in data variables ('",
  paste(colnames(data), collapse = "', '"),
  "'): check spelling of text box input."))
}

  #SUBSET DATA USING PREDICTOR COLUMN NAMES
  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2]) # [,which(colnames(input) %in% predictors2)])
  model_data <- data.frame(input[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)

  #DETERMINE IF ANY PREDICTORS ARE CATEGORICAL 
  for (i in seq_along(colnames(pred_data)) ){
    if (is.factor(pred_data[,i]) || is.character(pred_data[,i]) ) {
      #AddErrMsg("You have chosen a categorical independent variable! Please adjust your independent variables appropriately. You can also try other regression models such as logistic, SVM or random forest.")
      stop("You have chosen a categorical independent variable! Please adjust your independent variables appropriately. You can also try other regression models such as logistic, SVM or random forest.") 
    }
  }  
  
  #MODEL WITH/WO WEIGHTS
  # if (is.null(weights) == TRUE) {
    model <- lm(formula = formula, data = model_data, weights = NULL) #MAKE MODEL 
  # } #else {
  #   weights <- weights #Java upload weights as a vector of numeric values
  #   if (length(weights) == nrow(model_data)) { #There must be one weight for every row in the data set
  #     model <- lm(formula = formula, data = model_data, weights = weights) #Create linear model, with weights
  #   }
  #   else {
  #     #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.")
  #     stop("The length of the input weights does not equal the number of rows in the data set! Check that the input weights is correct.") 
  #   }
  # }
  
  #STORE MODEL
  model_name <- "Multivariate Linear Regression"
  mSetObj$analSet$linRegMulti$mod <- list(model = model, model_name = model_name, model.data = model_data, response = facA, predictor = predictors2)
  
  #EXTRACT RESULTS
  fitted <- fitted(model) #Predicted values
  summary <- summary(model) # includes coefficients, residuals, fit 
  residuals <- model$residuals # Get the residuals 
  conf.int <- confint(model, level = 0.95) #Conf intervals for predictor variables
  covar <- vcov(model) #Covariance matrix for predIctor variables
  fileName <- "multivariate_regression_summary.txt" #File name for summary, used by save.linReg1.summary()
  
  #CREATE EQUATIONS AND TABLES 
  coeffs <- as.data.frame(summary[["coefficients"]]) #Extract model coefficients
  coeffs$significance <- coeffs$`Pr(>|t|)`
  coeffs$significance[coeffs$significance < 0.001] <- "***"
  coeffs$significance[coeffs$significance < 0.01 & coeffs$significance >= 0.001] <- "**"
  coeffs$significance[coeffs$significance < 0.05 & coeffs$significance >= 0.01] <- "*"
  coef_values <- as.data.frame(round(coeffs[,1], 2))
  colnames(coef_values) <- "Coefficient"
  row.names(coef_values) <- row.names(coeffs)
  variables <- rownames(coef_values)
  intercept <- round(coef_values[1,1], digits = 2)
  
  equations <- list()
  for (i in 2:nrow(coef_values)) {
    equation <- paste0(coef_values[i,1], " * ", variables[i])
    equations[[i]] <- equation
  }
  equation <- paste0(facA, " = ", paste(unlist(equations), collapse=" + "),
                     " + ", intercept)
  r_sq <- round(summary[["adj.r.squared"]], digits = 2)
  r_sq_adj <- round(summary[["adj.r.squared"]], digits = 2)
  
  #RMSE (FOR PLOTTING)
  plot_prediction <- predict(model)
  plot_rmse <- Metrics::rmse(model_data[,facA], plot_prediction)
  
  #PREDICTOR RELATIVE IMPORTANCE (for plotting)
  if (ncol(model_data) == 2) {
      ## if there is only 1 predictor, it comports 100% importance
    importance <- data.frame(V1 = 100)
    colnames(importance) <- colnames(model_data)[2]
  } else {
    importance <- relaimpo::calc.relimp(model, type = "lmg", rela = TRUE)
  }

  # NORMALITY OF RESIDUALS
  # #Test residuals for normality. Error will be visable to user.
  # norm_resid <- shapiro.test(residuals)
  # # if (norm_resid$p.value > 0.05){
  # #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try choosing other independent and/or dependent variables, other data preprocessing options, or other regression models such as SVM or random forest.")
  # #   stop("The residuals are NOT normally distributed. This model is invalid. Try choosing other independent and/or dependent variables, other data preprocessing options, or other regression models such as SVM or random forest.")
  # # }
  
  #STORE RESULTS 
  mSetObj$analSet$linRegMulti$res <- list(summary = summary, response = facA, predictors = predtext, predtext = predtext1, model.data = model_data, equation = equation, r.squared.eq = paste("R-squared = ", r_sq), r.squared.adj.eq = paste("R-squared adjusted = ", r_sq_adj), r.squared = r_sq,  r.squared.adjusted = r_sq_adj,
     coeff.table = coeffs, residuals = residuals, predicted.values = fitted, confidence.intervals = conf.int, covariance.matrix = covar, plot.rmse = plot_rmse, importance = importance, fileName = fileName) 
  
  
  #DOWNLOD TEXT DOC, CONTAINING SUMMARY called fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink(fileName) 
  cat("Formula:\n")
  print(formula)
  cat("\nEquation:\n")
  cat(paste0(equation, "\n"))
  print(summary)
  # cat("Residuals:\n")
  # print(residuals)
  # print(norm_resid)
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
#'@usage multi.pred.plot (mSetObj, facA='NULL', predtext='NULL', data='false', col_dots='NULL', col_line='NULL', plot_ci='false', plot_title=' ', plot_ylab=' ', plot_xlab=' ', imgName, format='png', dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
###param weights Set weight values, default is NULL
#' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
#'@param plot_title Input the name of the title (default: "Predicted vs Actual: (formula);, textbox)
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

multi.pred.plot <- function(mSetObj=NA, 
  facA = "NULL",
  predtext = "NULL",
  data="false",
  
  col_dots="NULL",
  col_line="NULL", 
  plot_ci="false",
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  imgName, format="png", dpi=72, width=NA){
  
  
  library("ggplot2")
  # library("JSONIO")
  
  ## name was: plot.pred.linRegMulti
  mSetObj <- .get.mSet(mSetObj)
  
   ### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  
 #Extract necessary objects from mSetObj
  model_name <- mSetObj$analSet$linRegMulti$mod$model_name
  # model_data <- mSetObj$analSet$linRegMulti$mod$model.data
  # prediction <- mSetObj$analSet$linRegMulti$res$predicted.values
  
  ### TROUBLESHOOTING:
  ##   col_dots1<-"blue"
  ##   col_line1<-"red"
  ##   plot_ci1<-TRUE
  ##   plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(formula), ")")
  ##   plot_ylab1 <- "Actual"
  ##   plot_xlab1<- "Predicted"
  
  ##### WITH facA and predtext options
  ##### [CRUNCH]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
  if (facA == "NULL") { 
    if("res" %in% names(mSetObj$analSet$linRegMulti) ){ #if there is a results made already, take that response
        facA <- mSetObj$analSet$linRegMulti$res$response
     } else {
    for (i in seq_along(colnames(input)) ) {
      if (is.factor(input[,i]) == FALSE) {
        facA <- colnames(input)[i]# Default is to choose the 1st numeric column as response column
        break
      }
    }
 }
} else {
    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu
  }
  
  #Textbox instructions for selecting predictor variables COMMENTED OUT FOR NOW
  # cat("Indicate independent variables using the column names with commas in between. If interested in an interaction between particular variables, indicate it with a colon rather than a comma.")
  
  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
  if (predtext == "NULL") {
    if("res" %in% names(mSetObj$analSet$linRegMulti) ){#if there is a results made already, take that predictor
        predtext <- mSetObj$analSet$linRegMulti$res$predtext
     } else {
    dat <- input[ , colnames(input) != facA] #drop=FALSE means it will be a df
    num.data <- dplyr::select_if(dat, is.numeric)
    predtext <- colnames(num.data)[1] #Default is the 1st potential predictor column
    # predtext <- paste0(predtext, ",") # for error check later
 } 
    } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
  #CHECK PREDTEXT FOR COMMAS
 if( !any(grepl(",", predtext, fixed = TRUE, perl=TRUE)) ){ # if there are no commas in input predictor name(s)
   if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ # can't be >1 other cols to use, so if there is, error  (not counting the default)
warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
    } }

  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
  predtext <- gsub("\n", "", predtext, fixed = TRUE)
  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
  predtext <- gsub(";", "+", predtext, fixed = TRUE)
  predtext <- gsub(" ", "", predtext, fixed = TRUE)
  predtext <- gsub(":", "+", predtext, fixed = TRUE)
  predtext <- gsub("*", "+", predtext, fixed = TRUE)
   
  #GENERATE FORMULA
  formula <- as.formula(paste(facA, "~", predtext))
  #Text should be visible to user # SOME COMMENTED OUT FOR NOW
  cat(paste0("You have created this formula for prediction plot: ", facA, " ~ ", predtext))
  # cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
  # cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
   ### CHECK: are all input predictor names in data
  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
 # if(any(!colnames(data) %in% predictors2)){
if(!all(predictors2 %in% colnames(data)) ){
   stop(paste0( "'", predictors2[!predictors2 %in% colnames(data)],
  "' not found in data variables ('",
  paste(colnames(data), collapse = "', '"),
  "'): check spelling of text box input."))
}

  #SUBSET DATA USING PREDICTOR COLUMN NAMES
  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2])
  model_data <- data.frame(input[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)

  #DETERMINE IF ANY PREDICTORS ARE CATEGORICAL 
  for (i in seq_along(colnames(pred_data)) ){
    if (is.factor(pred_data[,i]) || is.character(pred_data[,i]) ) {
      stop("You have chosen a categorical independent variable! Please adjust your independent variables appropriately. You can also try other regression models such as logistic, SVM or random forest.") 
    }
  }
  
# MAKE MODEL
    model <- lm(formula = formula, data = model_data, weights = NULL)
  
  #########
  ######### [CRUNCH]

# MAKE DF WITH PREDICTED VALUES

pred_model <- predict(model, newdata = model_data, interval = "confidence")
  # pred_fit <- fitted(model)  
  dfpred <- data.frame(fpred = pred_fit[,"fit"],#fitted(model),
     fA = input[,facA], lwr = pred_fit[,"lwr"], upr = pred_fit[,"upr"] )
  formula2 <- as.formula("fA ~ fpred")
  model2 <- lm(formula = formula2, data = dfpred)

  
  #SET PLOT DIMENSIONS
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 1st, bc overwriting imgName var in next line
imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.linRegMulti <- imgName
  
    #SET POINT COLOR
  col_dots1 <- 
				switch(
					col_dots,
					"NULL" = "black",
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
					"blue" = "blue",
					"red" = "red",
					"green" = "green",
					"grey" = "grey",
					NULL
				)

  #SET WHETHER TO ADD 95% CONF INT
  if (plot_ci == "false") {
      plot_ci1 <- FALSE # default
    } else {
      plot_ci1 <- TRUE
    }
  
  # PLOT TITLE
  if(plot_title == " "){ 
    plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(formula), ")")
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
 

  if (plot_ci == "false") {
      plot_ci1 <- FALSE # default
      
  a1 <- ggplot(data =  dfpred, aes(x = fpred, y = fA)) +
     geom_smooth(se = FALSE, color = col_line1, fullrange = TRUE) +#, formula = formula2) +
     geom_point(shape = 16, color = col_dots1)
    } else {
      plot_ci1 <- TRUE
  a1 <- ggplot(data =  dfpred, aes(x = fpred, y = fA)) +
     geom_smooth(se = FALSE, color = col_line1, fullrange = TRUE) +#, formula = formula2) +
     geom_point(shape = 16, color = col_dots1)+
     geom_line(aes(model, color=col_line1)) +
     geom_ribbon(aes(fpred, ymin = lwr, ymax = upr), alpha = .2) 
    }

  ## MAKE PLOT  
   a0 <- a1 +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )




  ## y actual input[,facA] fA
  ## x prediction fpred
#  a0 <- ggplot(data =  dfpred, aes(x = fpred, y = fA)) +
#    labs(title = plot_title1) +
#     ylab(plot_ylab1)+ xlab(plot_xlab1) +
#     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE) +#, formula = formula2) +
#     geom_point(shape = 16, color = col_dots1) +
#     theme_bw() + 
#  theme(panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        axis.text = element_text(size = 12, colour = "black"), 
#        axis.title = element_text(size = 12),
#        # legend.title=element_text(12), legend.text=element_text(size=12), 
#        plot.title = element_text(face = 'bold', hjust = 0.5)
#  )
  
  #GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  print(a0)
  # plot(x=prediction, y=model_data[,facA], xlab=paste0("Predicted ", facA), ylab=paste0("Actual ", facA), main=model_name, yaxt="n"); axis(2, las=2); abline(a=0,b=1) #Old Base R
  dev.off()
  
  #JSON OBJECT MAKING
  build <- ggplot_build(a0)
  build_line <- build$data[[1]]
  build_points <- build$data[[2]]
  linear_plot_json <- list()
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles

### WITH DROP=FALSE
 #linear_plot_json$points$coords <- build_points[,c("x","y"), drop=FALSE] #[,1:2]
 #linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points)),drop=TRUE] #[,6] #colours
 #linear_plot_json$points$shape <- build_points[,c("group"),drop=TRUE] #[,5]
 #linear_plot_json$points$size <- build_points[,c("size"),drop=TRUE] #[,7]
 #linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line)),drop=TRUE]
 #
 #if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
 # ci<- build_line[,c("x","y", "ymin", "ymax"),drop=FALSE] 
 # colnames(ci) <- c("x","y","CI_down", "CI_up")
 # linear_plot_json$lines$ci <- ci # build_line[,c("ymin", "ymax"),drop=FALSE]
 #} else{
 #   linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x"),drop=TRUE], y = build_line[,c("y"),drop=TRUE], CI_down = 0, CI_up = 0)
 #}   
  
  linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
  linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
  linear_plot_json$points$shape <- build_points[,c("group")] #[,5]
  linear_plot_json$points$size <- build_points[,c("size")] #[,7]
  linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
  
  if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build_line[,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 }   


  ## BOOLEANS
  if(plot_ci1 == TRUE){
    linear_plot_json$bool_ci <- TRUE
    } else{
     linear_plot_json$bool_ci <- FALSE
    }

  
  linear_plot_json$model$r_sq <-
   summary(model2)[["r.squared"]] #Extract R^2
  linear_plot_json$model$r_sq_adj <-
    summary(model2)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$model$slope <-
    summary(model2)[["coefficients"]][2] # beta
  linear_plot_json$model$yint <-
    summary(model2)[["coefficients"]][1] # alpha

  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName2)
  cat(json.obj)
  sink()
  
   if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }
}


#'Produce relative importance of predictors bar plot for multivariate regression
#'@description 
#'@usage multi.relaimpo.plot(mSetObj, facA='NULL', predtext='NULL', data='false', plot_palette='NULL', plot_label='false', plot_title=' ', plot_ylab=' ', plot_xlab=' ', imgName, format='png', dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param plot_palette Set color palette from RColorBrewer paelettes (default "NULL" is Set2); (static dropdown)
#'@param plot_label Add label over bar with relimpo value (rounded 3 dig) (default "NULL" is no label); (checkbox)
###@param weights Set weight values, default is NULL
#'@param plot_title Input the name of the title (default: "Relative Importance of Predictors for  (facA);, textbox)
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

multi.relaimpo.plot <- function(mSetObj=NA, 
  facA = "NULL",
  predtext = "NULL",
  data="false",
  plot_palette = "NULL", 
  plot_label="false", 
  
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  imgName, format="png", dpi=72, width=NA){
  
  
  library("ggplot2")
  library("relaimpo")
  # library("JSONIO")
  
  ## name was: plot.relimpo.linRegMulti
  mSetObj <- .get.mSet(mSetObj)
  
   ### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
#SET PLOT DIMENSIONS
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w

  ##### WITH facA and predtext options
  ##### [CRUNCH]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
  if (facA == "NULL") {
    if("res" %in% names(mSetObj$analSet$linRegMulti) ){ #if there is a results made already, take that response
        facA <- mSetObj$analSet$linRegMulti$res$response
     } else {
    for (i in seq_along(colnames(input)) ) {
      if (is.factor(input[,i]) == FALSE) {
        facA <- colnames(input)[i]# Default is to choose the 1st numeric column as response column
        break
      }
    }
 }
} else {
    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu
  }
  
  #Textbox instructions for selecting predictor variables SOME COMMENTED OUT FOR NOW
  # cat("Indicate independent variables using comma-separated column names. If interested in an interaction between particular variables, use a colon rather than a comma.")
  
  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
  if (predtext == "NULL") {
    if("res" %in% names(mSetObj$analSet$linRegMulti) ){
        predtext <- mSetObj$analSet$linRegMulti$res$predtext #if a results is made already, take that predictor
     } else {
    dat <- input[ , colnames(input) != facA]
    num.data <- dplyr::select_if(dat, is.numeric)
    predtext <- colnames(num.data)[1]
    # predtext <- paste0(predtext, ",") # for comma error check later
    #predtext <- paste(colnames(num.data)[c(1,2)], collapse="+") #Default is the 1st potential predictor column
 } 
    } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
# CHECK IN PREDTEXT FOR COMMAS
 if( !any(grepl(",", predtext, fixed = TRUE, perl=TRUE)) ){ # if there are no commas in input predictor name(s)
    if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ # can't be >1 other cols to use, so if there is, error
warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
    } }

  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
  predtext <- gsub("\n", "", predtext, fixed = TRUE)
  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
  predtext <- gsub(";", "+", predtext, fixed = TRUE)
  predtext <- gsub(" ", "", predtext, fixed = TRUE)
  predtext <- gsub(":", "+", predtext, fixed = TRUE)
  predtext <- gsub("*", "+", predtext, fixed = TRUE)

  #GENERATE FORMULA
  formula <- as.formula(paste(facA, "~", predtext))
  #Text should be visible to user COMMENTED OUT FOR NOW
  cat(paste0("For the relative importance plot, you created this formula: ", facA, " ~ ", predtext))
  #cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
  #cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
   ### CHECK: are all input predictor names in data
  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
  # if(any(!colnames(data) %in% predictors2)){
if(!all(predictors2 %in% colnames(data)) ){ # if any of the input predictors2 are not in colnames of data
   stop(paste0( "'", predictors2[!predictors2 %in% colnames(data)], #predictors2 not in data
  "' not found in data variables ('",
  paste(colnames(data), collapse = "', '"), #list data variables, so user can see what the actual names are
  "'): check spelling of text box input."))
}

  #SUBSET DATA USING PREDICTOR COLUMN NAMES
  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2])
  model_data <- data.frame(input[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)

  #DETERMINE IF ANY PREDICTORS ARE CATEGORICAL 
  for (i in seq_along(colnames(pred_data)) ){
    if (is.factor(pred_data[,i]) || is.character(pred_data[,i]) ) {
      stop("You have chosen a categorical independent variable! Please adjust your independent variables appropriately. You can also try other regression models such as logistic, SVM or random forest.") 
    }
  }
 
# MAKE MODEL 
    model <- lm(formula = formula, data = model_data, weights = NULL)
  
#PREDICTOR RELATIVE IMPORTANCE (MAKE DF for plotting)
  if (ncol(model_data) == 2) {
      ## if there is only 1 predictor, comports 100% importance
    importance <- data.frame(V1=1.00)
    colnames(importance) <- colnames(model_data)[2]
    dfrel <- data.frame(predictors = colnames(importance), relaimp = importance[,1])
    
  } else {
    importance <- relaimpo::calc.relimp(model, type = "lmg", rela = TRUE)
    dfrel <- data.frame(predictors = names(importance@lmg), relaimp = importance@lmg)
  }
  
# TROUBLESHOOTING
# print(dfrel)

  #########
  ######### [CRUNCH]
  
  ### TROUBLESHOOTING 
  ## plot_palette = "Dark2"; 
  ###  plot_title1 <- paste0("Relative Importance of Predictors for ", facA); plot_xlab1 <- "Predictors (Independent Variables)" ;plot_ylab1 <- "Proportion of R Squared" 
    
   #Extract necessary objects from mSetObj
  # facA <- mSetObj$analSet$linRegMulti$res$response
  # model_data <- mSetObj$analSet$linRegMulti$mod$model.data
  # importance <- mSetObj$analSet$linRegMulti$res$importance
    
    
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
   imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.relaimpo.linRegMulti <- imgName
  
     
 # scale_fill_brewer(palette = plot_palette1)
#c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdYlBu", "Dark2", "Paired", "Set2")
   
   # plot_palette1 <- "Dark2"
   plot_palette1 <- 
     switch(
       plot_palette,
       "NULL" = "Set2",
       ### FOR TROUBLESHOOTING
       "RdYlBu" = "RdYlBu", 
       "RdBu" = "RdBu", 
       "PuOr" = "PuOr", 
       "PRGn" = "PRGn", 
       "PiYG" = "PiYG", 
       "BrBG" = "BrBG", 
       
       "Dark2" = "Dark2", 
       "RedYellowlBue" = "RdYlBu", 
       "RedBlue" = "RdBu", 
       "PurpleOrange" = "PuOr", 
       "PurpleGreen" = "PRGn", 
       "PinkYellowGreen" = "PiYG",
       "BrownBlueGreen" = "BrBG",
       NULL
     )

  # PLOT TITLE
  if(plot_title == " "){ 
    plot_title1 <- paste0("Relative Importance of Predictors for ", facA)
  } else {
    plot_title1 <- plot_title
  }
  
  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Proportion of R Squared"
  plot_ylab1 <- expression(paste("Proportion of ", R^2))
  } else { # facA, response
    plot_ylab1 <- plot_ylab
  }

   
  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Predictors (Independent Variables)" 
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
 
   
# TROUBLESHOOTING   
# dfrel <- mtcars %>% dplyr::mutate(predictors = as.character(carb)) %>%  dplyr::mutate(relaimp = gear/10) 
# barplot(as.matrix(importance) #    barplot(importance@lmg,

  #GENERATE PLOT
  a0 <- ggplot(data = dfrel, aes(x = predictors, y = relaimp)) +
    labs(title = plot_title1) + ylab(plot_ylab1)+ xlab(plot_xlab1) +
    geom_col(aes(fill = predictors), alpha = 0.5, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,1)) +
    scale_fill_brewer(palette = plot_palette1) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )
  
  #ADD RELAIMP VALUE LABEL
  if (plot_label == "false") {
     a0 <- a0
    } else {
    #  a0 <- a0 +  geom_label(aes(label = round(relaimp, digits = 3)) )
 a0 <- a0 +  geom_label(aes(label = paste0( 
                                   round(relaimp*100, digits = 3), '%'
                                 )
                             ))
    }
  
  
  #JSON OBJECT MAKING
  build <- ggplot_build(a0)
  build_bar <- build$data[[1]]
  linear_plot_json <- list()
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles

 if(any(grepl("ymin", colnames(build_bar))) && any(grepl("ymax", colnames(build_bar))) ){
   ys<- build_bar[,c("ymin", "ymax")] # ys<- build_bar[,c("ymin", "ymax"), drop=FALSE] 
 } else{
    ys <- data.frame(ymin = build_bar[,"y"], ymax = build_bar[,"y"] )
   #  ys <- data.frame(ymin = build_bar[,"y",drop=TRUE], ymax = build_bar[,"y",drop=TRUE] )
 }
  if(any(grepl("xmin", colnames(build_bar))) && any(grepl("xmax", colnames(build_bar))) ){
   xs<- build_bar[,c("xmin", "xmax")] 
   # xs<- build_bar[,c("xmin", "xmax"), drop=FALSE] 
 } else{
    xs <- data.frame(xmin = build_bar[,"x"], xmax = build_bar[,"x"] )
 }

  linear_plot_json$bar$coords <- cbind(build_bar[, c("x","y")], xs, ys )
 # linear_plot_json$bar$coords <- cbind(build_bar[, c("x","y"),drop=FALSE], xs, ys )
  linear_plot_json$bar$cols <- build_bar[,"fill"] #[,6] #colours
  
  ## BOOLEANS
  if(plot_label == TRUE){
    linear_plot_json$bool_label_onbar <- TRUE
    } else{
    linear_plot_json$bool_label_onbar <- FALSE
    }

  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  print(a0)
  # plot(x=prediction, y=model_data[,facA], xlab=paste0("Predicted ", facA), ylab=paste0("Actual ", facA), main=model_name, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
  dev.off()
  
  
  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName2)
  cat(json.obj)
  sink()
  
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }
  
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
  
  data <- dplyr::select_if(mSetObj$dataSet$norm, is.numeric)
  count.all.numeric.cols <- ncol(data)
  name.all.numeric.cols <- colnames(data)
  
  num.col.results <- list(
    count=count.all.numeric.cols,
    names=name.all.numeric.cols
  )
  
  return(num.col.results)
  
}

