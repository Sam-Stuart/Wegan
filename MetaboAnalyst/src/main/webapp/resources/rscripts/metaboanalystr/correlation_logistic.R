prep_boxtext <- function(textbox_text, purpose_ordtext = NULL 
                      ## 202211-06 # was named: prep_text4formula
                      # purpose = c("predtext", "ordtext") 
                      ){
 if(is.null(purpose_ordtext)){ 
   ### PURPOSE PREDICTORS
   #(fixed=T) deals w/ one string, vs (fixed=F) deals w/ vector of strings
    x <- gsub("\n", "", textbox_text, fixed=TRUE) 
    x <- gsub(" ", "", x, fixed=TRUE)
    x <- gsub(",", "+", x, fixed=TRUE) 
    x <- gsub(";", "+", x, fixed=TRUE)
    x <- gsub(":", "+", x, fixed=TRUE)
    x <- gsub("*", "+", x, fixed=TRUE)
 } else {
   ## PURPOSE ORDINAL:
    x <- gsub("\n", "", textbox_text, fixed=TRUE) 
    x <- gsub(" ", "", x, fixed=TRUE)
 }
    x
}


# log.effects.plot <- function(mSetObj=NA,
#                              # facA = "NULL", 
#                              # predtext = "NULL", 
#                              data = "false",
#                              type = "NULL",  # was multinomial before # can maybe remove the type argument?
#                              #   plot_linear = "false", # if false, plot error bars; if true, plot linear (even categorical vars)
#                              #  var_viewby1 = "NULL",
#                              #  var_viewby2 = "NULL",
#                              #  var_viewby3 = "NULL",
#                              plot_ci="false", #checkbox
#                              plot_xangle = "false", #checkbox
#                              plot_palette = "NULL", #dropdown
#                              plot_leg_horiz = "false", #checkbox
#                              plot_leg_pos = "NULL", #dropdown
#                              
#                              plot_title=" ",
#                              plot_ylab=" ",
#                              plot_xlab=" ",
#                              size_title = "NULL",
#                              size_xlab = "NULL",
#                              size_ylab = "NULL",
#                              size_xtick = "NULL",
#                              size_ytick = "NULL",
#                              
#                              imgName, format="png", dpi=72, width=NA)
#
# log.ROC.plot <- function(mSetObj=NA, 
#  # facA = "NULL", 
#  # predtext = "NULL",
#  data = "false",
#  type="NULL", # was multinomial 
#  plot_palette = "NULL", #dropdown  
#
#  plot_title = " ",
#  size_title = "NULL",
#  size_xlab = "NULL",
#  size_ylab = "NULL",
#  size_xtick = "NULL",
#  size_ytick = "NULL",


#'Perform Multivariate Logistic Regression'
#'@description Build a multivariate logistic regression model for user selected predictor variables
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses factor.columns() to give user options)
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param type Type of logistic regression (default "NULL" is for binomial; options are ordinal, multinomial or binomial)
#'@param reference Response variable level to be used as reference (java uses log.ref.level() to give user options)
#'@param ordertext Input order of dependent variable levels in ascending order, taken from text box by java, entered into R code as one character value (string)
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

log.reg.anal <- function(mSetObj=NA,
                         facA="NULL", # dropdown
                         predtext="NULL", # textbox
                         data = "false",
                         type="NULL",
                         reference="NULL", # dropdown
                         ordertext="NULL" # textbox; order of response levels
                         # weights=weights
                         ) {
  
  library("nnet") #For multinomial regression
  library("MASS") #For ordinal regression (polr)
  # library("assertr")
  library("dplyr") #For data manipulation # `%>%` should be exported for piping 
  # library("tidyselect") # tidyselect helper 'where' is not exported to namespace, but workaround exists (so still need package) ; to replace deprecated select_ variant `select_if`

  mSetObj <- .get.mSet(mSetObj)

   ### SET DATA (whether to use original data or not)
  if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
 
# 202211-03: change data back to input 
#  data <- input

  #TEXT SHOULD BE VISIBLE TO USER 
#  cat("One categorical dependent variable and one or more independent variables will be tested for correlation. Independent variables can be categorical or numeric." )
#  cat("By default, the 1st categorical (cat.) variable (var.) is treated as the dependent (depnt). var. If the dataset has more than 1 cat. var., you may change the depnt var. using the drop down menu.")
#  cat("Choose model type=binomial if there are 2 depnt var. levels (unordered); levels are unique values for a var.. Choose type=multinomial if there are more than 2 depnt var. levels (unordered). Choose type=ordinal if there are more than 2 depnt var. levels and they are ordered." )
#  cat("For cat. vars., make sure to use characters for the levels and not numbers. Eg., if you have levels 1, 2 and 3, change the labels to I, II and III.")
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default predtext is second column.
  # cat("Indicate independent variables using the column names with commas in between. If interested in an interaction between particular variables, indicate it with a colon rather than a comma.")
  
  #CHOOSE RESPONSE (DEPENDENT) VARIABLE FOR MODELING
  #  facData <- input %>% dplyr::select(tidyselect::vars_select_helpers$where(is.character))
  # facData <- input %>%  dplyr::select(tidyselect::vars_select_helpers$where(is.character) | tidyselect::vars_select_helpers$where(is.factor))
  # facData <- input %>% dplyr::select_if(is.character) %>% dplyr:: select_if(is.factor)# | is.factor)
 # facData <- input %>% dplyr::select_if(function(col) {is.character(col) | is.factor(col)})    
## compare time for apply and dplyr: (apply way faster)
# microbenchmark::microbenchmark(
#  facData1 <- input[,sapply(input, is.factor) | sapply(input, is.character), drop = FALSE],
#  facData2 <- input %>% dplyr::select_if(function(col) {is.character(col) | is.factor(col)})
#)
#  facData <- input[,sapply(input, is.factor) | sapply(input, is.character), drop = FALSE]

if(!any( sapply(input, is.factor) | sapply(input, is.character)) ){
errm <- c("No categorical variables in data; did you input your categorical variables as numbers (for example, as hot-one coded)? Try using SVM, multivariate, or linear univariate methods, which don't require categorical variables, or try splitting your numeric variable of interest into categories.")
AddErrMsg(errm)
stop(errm)
}


# CHOOSE RESPONSE VARIABLE
  if (facA == "NULL") {
# if( any(sapply(input, is.factor) | sapply(input, is.character)) ){ # already checked above for factor column presence
  facA <- colnames(
        input[,sapply(input, is.factor) | sapply(input, is.character), drop = FALSE]
        )[1]
#  } else{
#    AddErrMsg("No categorical variables in data; did you input your categorical variables as numbers (for example, as hot-one coded)?")
#    stop("No categorical variables in data")
#  }
  } else {
    facA <- facA #User selected, java uses function factor.columns() to provide options in drop down menu (only categorical columns are available)
  }

# FILENAME
  # fileName <- paste0("corr_logistic_model_summary.txt") #File name for results
  fileName <- paste0("logistic_regression_summary_", facA, ".txt");

## predtext is input into R as one string from textbox on webpage
#CHECK PREDTEXT FOR COMMAS
   if( !any(grepl(",", predtext, fixed = TRUE)) && predtext != "" && predtext != "NULL" ){ 
    # if there are no commas in input predictor name(s)
   if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ 
    # can't be >1 other cols to use, so if there is, warning
    warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
  }}


#### PREDICTORS (using dynamic checkboxes)
  #SET RIGHT SIDE OF FORMULA WITH PREDICTOR VARIABLES (ie not facA)
  if (predtext == "NULL") {
    predtext <- colnames( input[, !(colnames(input) == facA), drop = FALSE] )[1] #Default is 1st column
  } else {

    if("list" %in% class(predtext) ){
   predtext1 <- vector(mode = "character",  length = length(predtext) )
    for(i in seq_along(predtext) ){ 
   predtext1[i] <- predtext[[i]]
      }
       predtext <- paste(predtext1, collapse = ",")
    } else {
       predtext <- paste(predtext, collapse = ",")
  }
}

print(paste0("predtext: ", predtext))
print(paste0("class predtext: ", class(predtext)))
print(paste0("mode predtext: ", mode(predtext)))
print(paste0("typeof predtext: ", typeof(predtext)))


 # ## FORMULA SET UP
 # #SET RIGHT SIDE OF FORMULA WITH PREDICTOR VARIABLES (ie not facA)
 # if (predtext == "NULL") {
 #   # predData <- input[, !(colnames(input) == facA), drop = FALSE]
 #   predtext <- colnames( input[, !(colnames(input) == facA), drop = FALSE] )[1] #Default is 1st column
 #   # predtext <- paste(colnames(predData), collapse=" , ")
 # } else {
 #   predtext <- predtext #taken from text box by java, fed as string into R code
 # }

predtext1 <- predtext

  #CURATE RIGHT SIDE OF FORMULA; EXTRACT CHARACTER VECTOR OR PREDICTORS 
  predtext <- prep_boxtext(textbox_text = predtext, purpose_ordtext = NULL) 
  # predtext <- gsub("\n", "", predtext, fixed=TRUE) # predtext <- gsub(" ", "", predtext, fixed=TRUE) #predtext <- gsub(",", "+", predtext, fixed=TRUE)  # predtext <- gsub(";", "+", predtext, fixed=TRUE) # predtext <- gsub(":", "+", predtext, fixed=TRUE) # predtext <- gsub("*", "+", predtext, fixed=TRUE)


#GENERATE FORMULA 
form <- as.formula(paste0(facA, "~", predtext))   
## CHECK: are all input predictors in data
predictors <- unlist(strsplit(predtext, "+", fixed=TRUE), use.names = FALSE) ## use.names = F speeds unlist

   # input %>% assertr::verify(assertr::has_all_names(predictors), error_fun = justwarn)

   if( !all(is.element(predictors, colnames(input)) )){
     warning(paste0("log.reg.anal():", "'",
    paste( setdiff(predictors, colnames(input)), collapse = "', '" ),
    "' not found in data variables ('",   paste(colnames(input), collapse = "', '"), "'): check spelling of text box input."))
   }

#SUBSET DATA USING PREDICTOR COLUMN NAMES
pred_data <- input[, colnames(input) %in% predictors, drop = FALSE]
model_data <- data.frame(input[,facA], pred_data)
colnames(model_data) <- c(paste0(facA), predictors)

## MAKE RESPONSE VAR A FACTOR
if(!is.factor(model_data[,1])){
  model_data[,1] <- factor(model_data[,1])
  warning("response variable is a being coerced to factor")
}   

#CHECK NUMBER OF LEVELS 
facA_col <- model_data[,1]
levels.num <- nlevels(facA_col)
levels.facA <- levels(facA_col)

# SET LOGISTIC TYPE IF NOT SET
if(type == "NULL"){
 if(levels.num == 2){
  type <- "binomial"
 } else {
  type <- "multinomial"
 }
}

## STORE LOGISTIC TYPE, PREDICTORS, RESPONSE IN MSET
mSetObj$analSet$logRegInfo<- list(type = type, response = facA, predictor = predictors)

# REFERENCE LEVEL
if (reference == "NULL") {
  reference <- levels.facA[1] #Reference level defaults the 1st level of
} else {
  reference <- reference
}

# CHECK IF REFERENCE IS IN FACTOR LEVELS
   if( !all(is.element(reference, levels.facA) )){
     warning(paste0("log.reg.anal(): '", reference, 
    "', ' not found in response variable levels ('", 
     paste(levels.facA, collapse = "', '"), 
     "'): check spelling of text box input."))
   }

## ordertext: (ONLY USED FOR ORDINAL)
if (ordertext == "NULL") {
  ordertext <- levels( ordered(facA_col) )#Default is use order as inputted
} else { #Order is user inputted in ascending order, taken from text box by java, entered into R code as one character value (names separated by commas) (string)
  ordertext <- gsub(" ", "", ordertext, fixed = TRUE)
  ordertext <- unlist(strsplit(ordertext, ",", fixed = TRUE))
  ordertext <- trimws(ordertext)
}

### CHECK IF ORDERTEXT HAS VALID NAMES:
if(!all(is.element(ordertext, levels.facA))){
  warning(paste0( "'", ordertext[!ordertext %in% levels.facA], 
  "' not found in dependent variable levels of data (variable levels: '",
  paste(levels.facA, collapse = "', '"), "'): check spelling of text box input."))
}

 print(paste0("an: ", type))

### TYPE OF LOG REGRESSION LOOP:
## ORDINAL
  if (type == "ordinal") {

 if (levels.num < 3) {
      AddErrMsg("The dependent variable has less than 3 levels! Try binomial regression instead.")
      #stop("The dependent variable has less than 3 levels. Try binomial regression instead.")
    } 

    #REFERENCE LEVEL: SET FOR RESPONSE VARIABLE
    model_data[,1] <- relevel(as.factor(model_data[,1]), ref = reference) 
  
    #ORDER OF RESPONSE VARIABLE LEVELS
    #Text box instructions for selecting dependent variable levels. Text box should be interactive, meaning any change in text alters the result in real time. Default ordertext is no reorder.
   # cat("If performing ordinal regression, indicate the ascending order of the dependent variable levels using the level names with commas in between. For example, if your dependent variable is placement in a sports match, type bronze, silver, gold in the text box.")

    if (ordertext == "NULL") {
      model_data[,1] <- ordered(model_data[,1]) #Default is use order as inputted
     # ordertext <- levels(model_data[,1])
    } else { 
      ordertext <- ordertext #Order is user inputted in ascending order, taken from text box by java, entered into R code as one character value (names separated by commas) (string)
      model_data[,1] <- ordered(model_data[,1], levels = paste(ordertext, sep = ","))
    }
    
#### ADD CONFLICT BETWEEN REFERENCE AND LEVEL ORDER?

    #BUILD MODEL
    ## NOTE ABOUT ORDINAL MODEL CALLING with polr() from {MASS}:
     # Hess argument (T/F); whether the Hessian (observed info matrix) should be returned
     # (Used when calling summary() or vcov() on the fit (also returned for user) )
    mod <- MASS::polr(form, data = model_data, method = "logistic", Hess = TRUE)
    model_name <- "Ordinal Logistic Regression"
    
    #EXTRACT RESULTS
    summ <- summary(mod) #Summary of effects: Response/predictor odds ratios, SE and confidence intervals
    ord <- paste(ordertext, collapse = " < ")
    fitt <- fitted(mod) #Linear predicted values
    conf.int <- confint(mod, level = 0.95) #Confidence intervals for predictor variables
    oddsRatio <- exp(coef(mod))
    covar <- vcov(mod) #Covariance matrix for predictor variables
    logLik <- logLik(mod) 
    coeffs <- coef(mod)
    std.errors <- sqrt(diag(covar))
    zValues <- coeffs / std.errors
    pValues <- pnorm(abs(zValues), lower.tail = FALSE)*2
    Hessian <- mod[["Hessian"]]
    # fileName <- paste0("ordinal_logistic_regression_reference_", reference, "_summary.txt") #File name for results
    
    #STORE RESULTS

    mSetObj$analSet$logOrdReg$res <- list(summary = summ, model.data = model_data, response = facA, predictor = predictors, pretext = pretext1, level.reference = reference, level.resp.order = ordertext, predicted.values = fitt, confidence.intervals = conf.int, 
                                          Hessian = Hessian, oddsRatio = oddsRatio, covariance.matrix = covar, Loglikelihood = logLik, zValues = zValues, pValues = pValues, fileName = fileName)       
    mSetObj$analSet$logOrdReg$mod <- list(model.name = model_name, model = mod, formula = form, model.data = model_data, response = facA, predictor = predictors)
    
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(form)
    cat("\nDependent variable reference level:\n")
    cat(paste0(reference, "\n"))
    cat("\nDependent variable level order:\n")
    cat(paste0(ord))
    cat("\n\nSummary:\n")
    print(summ)
    cat("\nWald test/z-values:\n")
    print(zValues)
    cat("\nP-values:\n")
    print(pValues)
    cat("\nOdds ratio:\n")
    print(oddsRatio)
    cat("\nLog likelihood:\n") 
    print(logLik)
    cat("\nPredicted values:\n")
    print(fitt)
    cat("\nCovariance matrix for predictor variables:\n")
    print(covar)
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    cat("\nHessian matrix:\n")
    print(Hessian)
    sink()
    
  } else if (type == "multinomial") {

 ## MULTINOMIAL
    #CHECK NUMBER OF LEVELS
    if (levels.num < 3) {
      AddErrMsg("The dependent variable has less than 3 levels! Try binomial regression instead.")
      #stop("The dependent variable has less than 3 levels! Try binomial regression instead.")
    }

 ## SET REFERENCE
 model_data[,1] <- factor(model_data[,1], levels = c(reference, levels.facA[!levels.facA %in% reference]) )
  # c(reference, levels(model_data[,1])[!levels(model_data[,1]) %in% reference])

    #Set reference--ISNT WORKING!!!!!!!!!!! - gps - bc if the var isn't ordered then it won't take it?
    #model_data[,1] <- relevel(as.factor(model_data[,1]), ref=reference) 

    #Build model for multinomial regression
    mod <- nnet::multinom(form, data = model_data, Hess = TRUE, maxit = 1000, weights = NULL)
    model_name <- "Multinomial Logistic Regression"
    #Extract results
    summ <- summary(mod)
    resid <- mod[["residuals"]]
    fitt <- fitted(mod) #Predicted values
    conf.int <- confint(mod, level = 0.95) #Confidence intervals for predictor variables
    oddsRatio <- exp(coef(mod))
    covar <- vcov(mod) #Covariance matrix for preductor variables
    logLik <- logLik(mod)
    coeffs <- coef(mod)
    std.errors <- sqrt(diag(covar))
    zValues <- coeffs / std.errors
    pValues <- pnorm(abs(zValues), lower.tail = FALSE)*2
    Hessian <- mod[["Hessian"]]
    # fileName <- paste0("multinomial_logistic_regression_reference_", reference, "_summary.txt") #File name for results

    #STORE RESULTS
    mSetObj$analSet$logMultinomReg$res <- list(summary = summ, model.data = model_data,
 response = facA, predictor = predictors, predtext = predtext1, 
residuals = resid, predicted.values = fitt, 
confidence.intervals = conf.int, oddsRatio = oddsRatio, 
covariance.matrix = covar, Loglikelihood = logLik, 
zValues = zValues, pValues = pValues, fileName = fileName)       
    mSetObj$analSet$logMultinomReg$mod <- list(model.name = model_name, 
model = mod, formula = form, model.data = model_data, response = facA, predictor = predictors)
    cat("NUMBER 6")
    #Download text document containing the results, called the fileName.
    sink(fileName) 
    cat("Formula:\n")
    print(form)
    cat("\nDependent variable reference level:\n")
    cat(paste0(reference, "\n\n"))
    print(summ)
    cat("\nWald test/z-values:\n")
    print(zValues)
    cat("\nP-values:\n")
    print(pValues)
    cat("\nOdds ratio:\n")
    print(oddsRatio)
    cat("\nLog likelihood:\n") 
    print(logLik)
    cat("\nResiduals:\n")
    print(resid)
    cat("\nPredicted values:\n")
    print(fitt)
    cat("\nCovariance matrix for predictor variables:\n")
    print(covar)
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    cat("\nHessian (observed information) matrix:\n")
    print(Hessian)
    sink()
  
  } else { #Default type is binomial
    
## BINOMIAL
 # CHECK N LEVELS
    if (levels.num < 2) {
      AddErrMsg("The dependent variable has less than 2 levels! Make sure there are exactly 2 unique values in the dependent variable.")
      stop("The dependent variable has less than 2 levels! Make sure there are exactly 2 unique values in the dependent variable.")
    } else if (levels.num > 2) {
      AddErrMsg("The dependent variable has more than 2 levels! Try multinomial regression instead.")
      stop("The dependent variable has more than 2 levels! Try multinomial regression instead.")
    }

    #Set reference
    model_data[,1] <- relevel(as.factor(model_data[,1]), ref = reference) 
  
    #Build model
    mod <- glm(form, data = model_data, family = binomial("logit"), maxit = 1000, weights = NULL)
    model_name <- "Binomial Logistic Regression"
    
    #Extract results
    summ <- summary(mod)
    resid <- mod[["residuals"]]
    fitt <- fitted(mod) #Predicted values
    sefit <- predict(mod, se.fit = TRUE)$se.fit
    conf.int <- confint(mod, level = 0.95) #Confidence intervals for predictor variables
    oddsRatio <- exp(coef(mod))
    covar <- vcov(mod) #Covariance matrix for predictor variables
    testStat <- with(mod, null.deviance - deviance)
    testStatDF <- with(mod, df.null - df.residual)
    pValue <- with(mod, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    logLik <- logLik(mod)
    coeffs <- coef(mod)
    std.errors <- sqrt(diag(covar))
    zValues <- coeffs / std.errors
    pValues <- pnorm(abs(zValues), lower.tail = FALSE)*2
    # fileName <- paste0("binomial_logistic_regression_reference_", reference, "_summary.txt") #File name for results

    #STORE RESULTS
    mSetObj$analSet$logBinomReg$res <- list(summary = summ, model.data = model_data,
 response = facA, predictor = predictors, predtext = predtext1,
residuals = resid, predicted.values = fitt, standard.error = sefit, 
 confidence.intervals = conf.int, oddsRatio = oddsRatio,
covariance.matrix = covar, modelDiff = testStat,
modelDiffDF = testStatDF, pValue = pValue, 
Loglikelihood = logLik, zValues = zValues, pValues = pValues, fileName = fileName)       
    mSetObj$analSet$logBinomReg$mod <- list(model.name = model_name,
model = mod, formula = form, model.data = model_data, 
response = facA, predictor = predictors)
    
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(form)
    cat("\nDependent variable reference level:\n")
    cat(paste0(reference, "\n"))
    print(summ)
    cat("Difference between the null deviance and residual deviance:\n")
    cat(paste0("Test statistic=", testStat, "\n"))
    cat(paste0("Degrees of freedom=", testStatDF, "\n"))
    cat(paste0("P-value=", pValue, "\n"))
    cat("\nWald test/z-values:\n")
    print(zValues)
    cat("\nP-values:\n")
    print(pValues)
    cat("\nOdds ratio:\n")
    print(oddsRatio)
    cat("\nLog likelihood:\n") 
    print(logLik)
    cat("\nResiduals:\n")
    print(resid)
    cat("\nPredicted values:\n")
    print(fitt)
    cat("\nCovariance matrix for predictor variables:\n")
    print(covar)
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    sink()
      
  } 

  return(.set.mSet(mSetObj))
      
}






#'Effects plot for non-ordered logistic regression (conditional effects; non-focal terms are held constant at their mean value (if these are continuous) or at their reference level (for factors), and the effects are therefore conditional on these reference levels)
#'@description 
#'@usage plot.effects.logReg(mSetObj, type="binomial", imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param facA Input the name of the response column (java uses factor.columns() to give user options)
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param type Type of logistic regression (ordinal, multinomial or binomial), binomial is default
#'@param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
#'@param plot_title Input the name of the title (default: "Polynomial Regression Predicted vs Actual: (formula);, textbox)
#'@param plot_xlab Input the name to use for x-axis label (default: facB, textbox)
#'@param plot_ylab Input the name to use for y-axis label (default: facA, textbox)
#'@param plot_xangle Whether x-axis tick labels should be pivoted vertically (default: "false", checkbox)
#'@param plot_palette Static dropdown, select plot palette (default: "NULL" to palette 'metro')
#'@param plot_leg_horiz Whether the legend should be displayed horizontally (default: "false", checkbox)
#'@param plot_leg_pos Static dropdown, position of legend ('bottom', 'top', 'right', 'left') (default: "NULL" is the bottom)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can mSetObj$dataSet$norm their own width.   
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

log.effects.plot <- function(mSetObj=NA,
# facA = "NULL", 
# predtext = "NULL", 
  data = "false",
  type = "NULL",  # was multinomial before # can maybe remove the type argument?
#   plot_linear = "false", # if false, plot error bars; if true, plot linear (even categorical vars)
#  var_viewby1 = "NULL",
#  var_viewby2 = "NULL",
#  var_viewby3 = "NULL",
  plot_ci="false", #checkbox
  plot_xangle = "false", #checkbox
  plot_palette = "NULL", #dropdown
  plot_leg_horiz = "false", #checkbox
  plot_leg_pos = "NULL", #dropdown

  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  size_title = "NULL",
  size_xlab = "NULL",
  size_ylab = "NULL",
  size_xtick = "NULL",
  size_ytick = "NULL",
  
  imgName, format="png", dpi=72, width=NA){
  
  ## was named: plot.effects.logReg
  # library("effects")
  library("ggplot2")
  library("ggeffects")
  library("RJSONIO")

### marginal effects or conditional effects? CONDITIONAL
### from {ggeffects} vignette: strengejacke.github.io/ggeffects/articles/introduction_marginal_effects.html
# ggpredict() (used in this function) holds non-focal terms constant at their mean value (if these are continuous) or at their reference level (for factors). Thus, effects returned by ggpredict() are actually conditional effects (i.e. these are conditioned on certain (reference) levels of factors). However, ggeffect() and ggemmeans() return marginal effects, since the effects are “marginalized” (or “averaged”) over the levels of factors.
# SO from this function, it is the conditional effects returned instead of the marginal effects


  #EXTRACT OBJECTS FROM MSET
  mSetObj <- .get.mSet(mSetObj)
  
 ### SET DATA (whether to use original data or not)
 if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

# SET LOGISTIC TYPE (IF NOT SET), SET RESPONSE & PREDICTORS
type1 <- mSetObj$analSet$logRegInfo$type
print(paste0("eff: ", type1))
predictors <- mSetObj$analSet$logRegInfo$predictor
facA <- mSetObj$analSet$logRegInfo$response

### TROUBLESHOOTING (Q&D)
 # input <- iris; 
 # plot_xangle1 <- 0; plot_palette1 <- "blambus"; plot_leg_pos1 <- "bottom"; plot_leg_horiz1 <- "horizontal"
 # plot_xlab1 <- predictors[1];  plot_title <- paste0("Predicted Probabilities of ", facA) ; plot_ylab1 <- facA

### VARIABLES: STORE OUTPUT SOURCE BASED ON REGSN TYPE
  if (type1 == "ordinal") { 
    # main = "Ordinal Logistic Regression \nEffects Plot"
    # predictors <- mSetObj$analSet$logOrdReg$res$predictor
    # facA <- mSetObj$analSet$logOrdReg$res$response
    # output <- mSetObj$analSet$logOrdReg
    mod <- mSetObj$analSet$logOrdReg$mod$model
    reference <- mSetObj$analSet$logOrdReg$res$level.reference
    ordertext <- mSetObj$analSet$logOrdReg$res$level.resp.order
    main_end <- " ( Ordinal Logistic Regression)" #  # main = "Ordinal Logistic Regression \nEffects Plot"
  } else if (type1 == "multinomial") {
    # main = "Multinomial Logistic Regression \nEffects Plot"
    # predictors <- mSetObj$analSet$logMultinomReg$res$predictor
    # facA <- mSetObj$analSet$logMultinomReg$res$response
    # output <- mSetObj$analSet$logMultinomReg
    mod <- mSetObj$analSet$logMultinomReg$mod$model
    main_end <- " ( Multinomial Logistic Regression)" #  # main = "Multinomial Logistic Regression \nEffects Plot"
  } else { #Binomial
    # main = "Binomial Logistic Regression \nEffects Plot"
    # predictors <- mSetObj$analSet$logBinomReg$res$predictor
    # facA <- mSetObj$analSet$logBinomReg$res$response
    # output <- mSetObj$analSet$logBinomReg
    mod <- mSetObj$analSet$logBinomReg$mod$model
    main_end <- " ( Binomial Logistic Regression)" ## main = "Binomial Logistic Regression \nEffects Plot"
  }
  
# if(var_viewby1 == "NULL"){
#  var_viewby1 <- predictors[1]
# } 
#
# if(var_viewby2 == "NULL"){
#  if(length(predictors) > 1){
#   var_viewby2 <- predictors[2]
#   var_pred <- c(var_viewby1, var_viewby2)
#  } else {
#   var_pred <- var_viewby1
# }
# } else {
#  var_pred <- c(var_viewby1, var_viewby2)
# }
#
# if(var_viewby3 == "NULL"){
#  if(length(predictors) > 2){
#   var_viewby2 <- predictors[3]
#   var_pred <- c(var_viewby1, var_viewby2, var_viewby3)
#  } else {
### CHECK viewby2 
# if(var_viewby2 == "NULL"){
#   var_pred <- var_viewby1
# }
# } else {
#  var_pred <- c(var_viewby1, var_viewby2)
# }


## SET PREDICTORS TO PLOT
if(length(predictors) > 2){
   var_pred <- predictors[c(1:2)]
   plot_xlab_use <- paste(var_pred, collapse="\n")
  } else {
   var_pred <- predictors[1]
   plot_xlab_use <- var_pred
 }

  # AXIS TICK LABELS SIDEWAYS
  if(plot_xangle == "false"){
    plot_xangle1 <- 0
  } else {
    plot_xangle1 <- 90
  }
  
  # LEGEND DIRECTION: side-side, up-down
  plot_leg_horiz1 <- 
				switch(
					plot_leg_horiz,
					# norm = "normal",
					"false" = "vertical",
					"horizontal"
				)
  
  # LEGEND POSITION: top, bottom, right, left
  plot_leg_pos1 <- 
				switch(
					plot_leg_pos,
					# norm = "normal",
					"NULL" = "bottom",
					"bottom" = "bottom",
					"top" = "top",
					"right" = "right",
					"left" = "left",
					NULL
				)
 # PLOT PALETTE: blambus, metro, hero, ipsum, circus, viridis, breakfast.club, aqua 		  
   plot_palette1 <-   
                      switch(  plot_palette,
       ## blambus metro hero ipsum circus blambus viridis breakfast.club aqua
       "NULL" = "blambus",
       "blambus" = "blambus", 
       "metro" = "metro",
       "hero" = "hero",
       "ipsum" = "ipsum",
       "circus" = "circus",
       "viridis" = "viridis", #contin
       "breakfast.club" = "breakfast.club",
       "aqua" = "aqua" #contin 
     )
## - ## COLOURS FYI ## - ##
  #  # red bluegrey palecopperbrown black lightyellowlightorange
  # `blambus` = c("#E02E1F", "#5D8191", "#BD772D", "#494949", "#F2DD26"),
  #  red lighttealturquise brightgreen orange darkgrey yelloworange lightgrey
  # `metro` = c("#d11141", "#00aedb", "#00b159", "#f37735", "#8c8c8c", "#ffc425", "#cccccc"),
  #  red darktealblack palegoldorange palecoralbrown lightsummergreen darkolive
  # `hero` = c("#D2292B", "#165E88", "#E0BD1C", "#D57028", "#A5CB39", "#8D8F70"),
  #  violetblack lighttealturquoise darkblackgreen coral lightsummergreen lightmagentapink cadetblue pinksand palelightgreen
  # `ipsum` = c("#3f2d54", "#75b8d1", "#2d543d", "#d18975", "#8fd175", "#d175b8", "#758bd1", "#d1ab75", "#c9d175"),
  # red blue yellow maroon navy
  # `circus` = c("#C1241E", "#0664C9", "#EBD90A", "#6F130D", "#111A79"),
  #  CONTINUOUS: darkviolet darkteal teal turquoise-green green lightpalegreen yellow
  # `viridis` = c("#440154", "#46337E", "#365C8D", "#277F8E", "#1FA187", "#4AC16D", "#9FDA3A", "#FDE725"),
  #  red blue darkgreen pink palesand
  # `breakfast club` = c("#b6411a", "#4182dd", "#2d6328", "#eec3d8", "#ecf0c8")
  #  CONTINUOUS: light blue to purple mauve to light pink
  #  `aqua` = c("#BAF5F3", "#46A9BE", "#8B7B88", "#BD7688", "#F2C29E"),
  #  black purpley orangey redbrown light orange yellow
  # `warm` = c("#072835", "#664458", "#C45B46", "#F1B749", "#F8EB85"),

  
  # CONFIDENCE INTERVAL (95%)
  if (plot_ci == "false") {
      plot_ci1 <- FALSE # default
    } else {
      plot_ci1 <- TRUE
    }

  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste0("Predicted Probabilities of ", facA, main_end)
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
    plot_xlab1 <- plot_xlab_use
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

  #SET PLOT DIMENSIONS
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
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
  mSetObj$imgSet$ploteffectslogReg <- imgName
 
## if var_pred are 2 categorical, the 1st predictor is on x-axis, 2nd is colour variable
### ggeffects plot defaults to using continuous x axis, regardless of cat/contin variable: 
### to make an errorbar plot for a 1st categorical variable (will use discrete continuous bars)
#
## strengejacke.github.io/ggeffects/reference/ggpredict.html
# use categorical value on x-axis to make error bars
# data(efc)
# fitt <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)
# dat <- ggpredict(fitt, terms = c("c172code", "c161sex")) # cat vars
# ggplot(dat, aes(x, predicted, colour = group)) +
#  geom_point(position = position_dodge(.1)) +
#  geom_errorbar(
#    aes(ymin = conf.low, ymax = conf.high),
#    position = position_dodge(.1)
#  ) 
#  # + scale_x_discrete(breaks = 1:3, labels = get_x_labels(dat)) # doesn't work well
## continuous ribbon plot:
# ggplot(ggeffects::ggpredict(fitt, terms = "c12hour"),  # continuous var
#       aes(x, predicted)) +
# geom_line() +
# geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
#
# 3 vars, use facets & groups (contin, cat, cat)
# ggplot(ggeffects::ggpredict(fitt, terms = c("c12hour", "c161sex", "c172code")), 
#        aes(x = x, y = predicted, colour = group)) +
#  stat_smooth(method = "lm", se = plot_ci1) +
#  facet_wrap(~facet, ncol = 2)


## if(length(var_preds) == 3){ # if var_viewby3 is set, add shape term
# aes(shape = facet)}
#
# error bar plot
# if(plot_linear == "false"){
# a0 <- ggplot(ggeffects::ggpredict(mod, terms = var_preds),
#        aes(x = x, y = predicted, colour = group)) +
#  # stat_smooth(method = "lm", se = plot_ci1) +
#   geom_point(position = position_dodge(.1)) +
#   geom_errorbar(
#     aes(ymin = conf.low, ymax = conf.high),
#     position = position_dodge(.1)
#   )
# } else {
# a0 <- plot(
#     ggeffects::ggpredict(model = mod, terms = var_pred
#                          ), 
#     colors = plot_palette1,
#      # add.data = TRUE,
#      use.theme = FALSE,
#      dodge = 0.4,
#      one.plot = TRUE,
#      ci = plot_ci1  ) 
# }
# 
# if(length(var_preds) == 3){ # if var_viewby3 is set, add shape term to aes
# a0 <- a0 + facet_wrap(~facet)
# }
# 
# a0 <- a0 + 
#   #scale_color_brewer(palette = plot_palette1) +
#   # scale_fill_brewer(palette = plot_palette1) +
#  labs(
#      x = plot_xlab1, 
#      y = plot_ylab1, 
#      title = plot_title1
#   )  + theme(axis.text.x =  element_text(angle = plot_xangle1),
#       axis.title.x = element_text(size = size_xlab1),
#       axis.title.y = element_text(size = size_ylab1),
#       axis.text.x = element_text(size = size_xtick1),
#       axis.text.y = element_text(size = size_ytick1),
#       plot.title = element_text(size = size_title1, face = 'bold', hjust = 0.5)
#       legend.position = plot_leg_pos1, 
#       legend.direction = plot_leg_horiz1, 
#       legend.title = element_text(face = "bold", size = size_leg),
#       strip.background = element_rect(colour = "white", fill = "white"), 
#       strip.text = element_text(size = size_leg) )

  a0 <- plot(
    ggeffects::ggpredict(model = mod, terms = var_pred
                         ), 
    colors = plot_palette1,
     # add.data = TRUE,
     use.theme = FALSE,
     dodge = 0.4,
     one.plot = TRUE,
     ci = plot_ci1  ) +   labs(
     x = plot_xlab1, 
     y = plot_ylab1, 
     title = plot_title1
  )  + theme_bw() +
     theme(
      axis.title.x = element_text(size = size_xlab1),
      axis.title.y = element_text(size = size_ylab1),
      axis.text.x = element_text(size = size_xtick1, angle = plot_xangle1),
      axis.text.y = element_text(size = size_ytick1),
      plot.title = element_text(size = size_title1, face = 'bold', hjust = 0.5),
      legend.position = plot_leg_pos1, 
      legend.direction = plot_leg_horiz1, 
      legend.title = element_text(face = "bold", size = size_leg),
      strip.background = element_rect(colour = "white", fill = "white"),
      strip.text = element_text(size = size_leg) )
 
print("eff.make plot")

  # STORE IN mSET
  if (type == "ordinal") { #mSetObj$analSet$logOrdReg$res
  mSetObj$analSet$logOrdReg$plotEff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
# mSetObj$analSet$logRegInfo$logOrd$plotEff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
    } else if (type == "multinomial"){
  mSetObj$analSet$logMultinomReg$plotEff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)  
# mSetObj$analSet$logRegInfo$logMultinom$plotEff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  } else if (type == "binomial"){
    mSetObj$analSet$logBinomReg$plotEff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1) 
# mSetObj$analSet$logRegInfo$logBinom$plotEff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  }

print("eff.store in mSet")

  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  print(a0)
  # plot(effects::allEffects(mod=mod), ylim=c(0,1), rescale.axis=FALSE, main=main)
  dev.off()
  
  build <- ggplot_build(a0)
  linear_plot_json <- list()
  
  build_points <- build$data[[1]]
  # build_line <- build$data[[2]]
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
  linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
  linear_plot_json$points$cols <- build$data[[1]][,grepl("col",colnames(build_points))] #[,6] #colours
  linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
  linear_plot_json$points$size <- build_points[,c("size")]#[,7]
  # linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
  
  # ci<- build_line[,c("x","y", "ymin", "ymax")]
  # colnames(ci) <- c("x","y","CI_down", "CI_up")
  # linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
  
  linear_plot_json$model$r_sq <-
   summary(mod)[["r.squared"]] #Extract R^2
  linear_plot_json$model$r_sq_adj <-
    summary(mod)[["adj.r.squared"]] #Extract adjusted R^2 
  linear_plot_json$model$slope <-
    summary(mod)[["coefficients"]][2] # beta
  linear_plot_json$model$yint <-
    summary(mod)[["coefficients"]][1] # alpha

  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName2)
  cat(json.obj)
  sink()
  print(paste0("PLOT2 | facA: ", facA))

   if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }  

}



#'ROC curve for logistic regression
#'@description Display ROC curve with area under the curve (AUC) value for logistic regression
#'@usage plot.ROC.logReg(mSetObj, type="ordinal", imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param facA Input the name of the response column (java uses factor.columns() to give user options)
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param type Type of logistic regression (ordinal, multinomial or binomial), binomial is default
#'@param plot_title Input name of title
#'@param plot_palette 
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can mSetObj$dataSet$norm their own width.   
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

log.ROC.plot <- function(mSetObj=NA, 
  # facA = "NULL", 
  # predtext = "NULL",
  data = "false",
  type="NULL", # was multinomial 
  plot_palette = "NULL", #dropdown  

  plot_title = " ",
  size_title = "NULL",
  size_xlab = "NULL",
  size_ylab = "NULL",
  size_xtick = "NULL",
  size_ytick = "NULL",
  # size_lgd_title = "NULL",

  imgName, format="png", dpi=72, width=NA){
  
## named: plot.ROC.logReg
  library("pROC")
## ggplot alternatives for ROC plotting:
# library("precrec")
# library("plotROC")
  library("multiROC") # older package
  library("RJSONIO")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
 ### SET DATA (whether to use original data or not)
  if (data=="false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

# SET LOGISTIC TYPE (IF NOT SET), SET RESPONSE & PREDICTORS
type1 <- mSetObj$analSet$logRegInfo$type
 predictors <- mSetObj$analSet$logRegInfo$predictor
 facA <- mSetObj$analSet$logRegInfo$response

## ORDINAL ROC analysis: using volume under an r-dimensional surface (VUS) for r ordered categories math.ucdavis.edu/~saito/data/roc/roc-regression.pdf
# towardsdatascience.com/testing-an-alternative-visualisation-of-ordinal-data-and-regression-in-r-1dc838fcaa2d
##  Somers' Dxy rank correlation, a generalization of ROC area for ordinal or continuous Y
## It is computed for ordinal proportional odds regression in the lrm function in the rms package

print(paste0("roc: ", type1))

print("roc.before extracting")

### VARIABLES: STORE OUTPUT SOURCE BASED ON REGSN TYPE
  if (type1 == "ordinal") { 
    # main = "Ordinal Logistic Regression \nEffects Plot"
    # predictors <- mSetObj$analSet$logOrdReg$res$predictor
    # facA <- mSetObj$analSet$logOrdReg$res$response
    # output <- mSetObj$analSet$logOrdReg
    mod <- mSetObj$analSet$logOrdReg$mod$model
    model_data <- mSetObj$analSet$logOrdReg$res$model.data
    main_end <- " ( Ordinal Logistic Regression)" #  # main = "Ordinal Logistic Regression \nEffects Plot"
print("roc.extracted order")
  } else if (type1 == "multinomial") {
    # main = "Multinomial Logistic Regression \nEffects Plot"
    # predictors <- mSetObj$analSet$logMultinomReg$res$predictor
    # facA <- mSetObj$analSet$logMultinomReg$res$response
    # output <- mSetObj$analSet$logMultinomReg
    mod <- mSetObj$analSet$logMultinomReg$mod$model
    model_data <- mSetObj$analSet$logMultinomReg$res$model.data
    main_end <- " ( Multinomial Logistic Regression)" #  # main = "Multinomial Logistic Regression \nEffects Plot"
print("roc.extracted multinomial")
  } else { #Binomial
    # main = "Binomial Logistic Regression \nEffects Plot"
    # predictors <- mSetObj$analSet$logBinomReg$res$predictor
    # facA <- mSetObj$analSet$logBinomReg$res$response
    # output <- mSetObj$analSet$logBinomReg
    mod <- mSetObj$analSet$logBinomReg$mod$model
    model_data <- mSetObj$analSet$logBinomReg$res$model.data
    main_end <- " ( Binomial Logistic Regression)" ## main = "Binomial Logistic Regression \nEffects Plot"
print("roc.extracted binomial")
  }

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
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
  mSetObj$imgSet$plot.ROC.logReg <- imgName

print("roc.set image parames")

# c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdYlBu", "Dark2", "Paired", "Set2")
   # plot_palette1 <- "Dark2"
   plot_palette1 <- 
     switch(
       plot_palette,
       "NULL" = "Dark2",
       "Dark2" = "Dark2", 
       "Set2" = "Set2",
       "RdYlBu" = "RdYlBu",  #RedYellowBlue
       "PiYG" = "PiYG",  #PinkYellowGreen
       "BrBG" = "BrBG", # BrownBlueGreen
       NULL
     )

 # type <- "ordinal"  
   .simcap <- function(x) { # stackoverflow.com/questions/58350357/capitalizing-the-first-character-in-a-string-in-r#58350475
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
   }

  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste(.simcap(type1), " Logistic Regression \nROC Curve", sep="")
  } else {
    plot_title1 <- plot_title
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
   
 print("roc.before making plot")

  if (type1=="ordinal") {
 ## ORDINAL
    predicted <- fitted(summary(mod)) # mSetObj$analSet$logOrdReg$res$linear.predicted.values
    prob <- predict(mod, type="probs")
    
    ############ using {multiROC} - does it work for ordinal??
    
    # Micro-average ROC/AUC was calculated by stacking all groups together, thus converting the multi-class classification into binary classification. Macro-average ROC/AUC was calculated by averaging all groups results (one vs rest) and linear interpolation was used between points of ROC. Methods shows names of different classifiers.
    
   mn_pred <- data.frame(prob)  # mn_pred_col <- colnames(mn_pred)
   colnames(mn_pred) <- paste0(colnames(mn_pred), "_pred_MN")
   true_label <-data.frame(caret::class2ind(model_data[,facA])) # true_label_col <- colnames(true_label)
   colnames(true_label) <- paste0(true_label_col, "_true")

 print("roc.ord.make df for plotting")

 plot_roc_df <- multiROC::plot_roc_data(
    multiROC::multi_roc(cbind(true_label, mn_pred), force_diag = TRUE) )

 a0 <- ggplot(plot_roc_df, 
       aes(x = 1-Specificity, y = Sensitivity)) +
  geom_path(aes(color = Group, linetype = Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                colour='grey',size = 0.75, linetype = 'dotdash') 

 print("roc.ord.make plot")   
 
# pROC::ggroc( pROC::multiclass.roc(model_data[,facA]~prob , plot=TRUE, print.auc = TRUE, xlab = "Specificity ( True Negative)", ylab = "Sensitivity (True Positive)", main = main, yaxt = "n" ) ); axis(2, las=2)
  
  } else if (type1 == "multinomial") {
 ## MULTINOMIAL 
 
    predicted <- fitted(summary(mod)) # mSetObj$analSet$logMultinomReg$res$linear.predicted.values
    prob <- predict(mod, type="probs")
    
    ############ using {multiROC} github.com/WandeRum/multiROC
   mn_pred <- data.frame(prob)   #  mn_pred_col <- colnames(mn_pred)
   colnames(mn_pred) <- paste0(colnames(mn_pred), "_pred_MN")
   true_label <-data.frame(caret::class2ind(model_data[,facA, drop = TRUE])) # true_label_col <- colnames(true_label)
   colnames(true_label) <- paste0(colnames(true_label), "_true")

  plot_roc_df <- multiROC::plot_roc_data(
     multiROC::multi_roc(cbind(true_label, mn_pred), force_diag = TRUE) )

 print("roc.multinom.make df for plotting")

 a0 <- ggplot(plot_roc_df, 
       aes(x = 1-Specificity, y = Sensitivity)) +
  geom_path(aes(color = Group, linetype = Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                colour='grey',size = 0.75, linetype = 'dotdash') 

 print("roc.multinom.make plot")

   # multiclass.roc(model_data[,facA]~prob, plot=TRUE, print.auc=TRUE, xlab="Specificity (True Negative)", ylab="Sensitivity (True Positive)", main=main, yaxt="n"); axis(2, las=2)
    
  } else { 
 ## BINOMIAL 
    predicted <- fitted(summary(mod))
    prob <- predict(mod, type="response")
    
 print("roc.binom.make df for plotting")

  a0 <- pROC::ggroc(  pROC::roc(model_data[,facA]~prob )) # , plot=TRUE, print.auc=TRUE, xlab="Specificity (True Negative)", ylab="Sensitivity (True Positive)", main=main, yaxt="n"

 print("roc.binom.make plot")

   # pROC::ggroc(  pROC::roc(model_data[,facA]~prob
   #, plot=TRUE, print.auc=TRUE, xlab="Specificity (True Negative)", ylab="Sensitivity (True Positive)", main=main, yaxt="n"
   #   ) )
    
  }

a0 <- a0 +
  ggtitle(plot_title1) +
  scale_color_brewer(palette = plot_palette1) +
  # scale_fill_brewer(palette = plot_palette1) +
  theme_bw() + 
  theme(plot.title = element_text(size = size_title1, face = 'bold', hjust = 0.5),
#   legend.title = element_text(face = "bold", size = size_leg),
    legend.justification=c(1, 0), legend.position=c(.95, .05),
    legend.title=element_blank(), 
    legend.background = element_rect(fill=NULL, 
     size=0.5,linetype="solid", colour ="black"), 
    axis.title.x = element_text(size = size_xlab1, face = "bold"),
    axis.title.y = element_text(size = size_ylab1, face = "bold"),
    axis.text.x = element_text(size = size_xtick1, face = "bold"),
    axis.text.y = element_text(size = size_ytick1, face = "bold"),
)

  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   print(a0) 
   dev.off()
   
  plot_ylab1 <- "Sensitivity"
  plot_xlab1 <- "1-Specificity"

   # STORE IN mSET
  if (type1 == "ordinal") { #mSetObj$analSet$logOrdReg$res
  mSetObj$analSet$logOrdReg$plotRoc <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  # mSetObj$analSet$logRegInfo$logOrd$plotRoc <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
    } else if (type1 == "multinomial"){
  mSetObj$analSet$logMultinomReg$plotRoc <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  # mSetObj$analSet$logRegInfo$logMultinom$plotRoc <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  } else if (type1 == "binomial"){
  mSetObj$analSet$logBinomReg$plotRoc <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  # mSetObj$analSet$logRegInfo$logBinom$plotRoc <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
  }
   
  print("roc.store in mSet") 
   #JSON OBJECT MAKING
  build <- ggplot_build(a0)
  build_line <- build$data[[1]]
  # build_line2 <- build$data[[2]] #3 dotted line 
  linear_plot_json <- list()
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
  linear_plot_json$line$coords <- build_line[,c("x","y")] #[,1:2]
  linear_plot_json$line$cols <- build_line[,grepl("col",colnames(build_line))] #[,6] #colours
  linear_plot_json$line$group <- build_line[,c("group")]#[,5]
  linear_plot_json$line$size <- build_line[,c("size")]#[,7]
  linear_plot_json$line$linetype <- build_line[,c("linetype")]#[,7]
  # linear_plot_json$line2$coords <- build_line2[,c("x","xend","y","yend")] #[,1:2]
  # linear_plot_json$line2$cols <- build_line2[,grepl("col",colnames(build_line2))]
  # linear_plot_json$line2$group <- build_line2[,c("group")]#[,5]
  # linear_plot_json$line2$size <- build_line2[,c("size")]#[,7]
  # linear_plot_json$line2$linetype <- build_line2[,c("linetype")]#[,7]
  
  ## BOOLEANS
#  if(plot_ci1 == TRUE){
#    linear_plot_json$bool_ci <- TRUE
#    } else{
#      linear_plot_json$bool_ci <- FALSE
#   }

  
#  linear_plot_json$model$r_sq <-
#   summary(model2)[["r.squared"]] #Extract R^2
#  linear_plot_json$model$r_sq_adj <-
#    summary(model2)[["adj.r.squared"]] #Extract adjusted R^2 
#  linear_plot_json$model$slope <-
#    summary(model2)[["coefficients"]][2] # beta
#  linear_plot_json$model$yint <-
#   summary(model2)[["coefficients"]][1] # alpha

  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName2)
  cat(json.obj)
  print("take json me")
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


#'Determine predictor columns in dataset for logistic regression'
#'@description Java will use the names predictor columns to enable user options for predictor variable selection
#'@param mSetObj Input name of the created mSetObject 
#'@author Gina Sykes\email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export


#predictor.columns <- function(mSetObj=NA){
#
# mSetObj <- .get.mSet(mSetObj)
#
#
#   if( "logRegInfo" %in% names(mSetObj$analSet) ){
#     predictors <- mSetObj$analSet$logRegInfo$predictor
#     
#   } else {
#   
#   }
# 


#'Determine number and names of categorical columns in dataset for logistic regression'
#'@description Java will use the names and numbers of categorical columns to enable user options for response variable selection
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

factor.columns <- function(mSetObj=NA){
  load_dplyr()
  mSetObj <- .get.mSet(mSetObj)

  # fac.cols <- data[,sapply(mSetObj$dataSet$norm, is.factor) | sapply(mSetObj$dataSet$norm, is.character), drop = FALSE]
  # fac.cols <- dplyr::select_if(mSetObj$dataSet$norm, is.character)
  fac.cols <- mSetObj$dataSet$norm %>% 
             dplyr::select_if(function(col) {is.character(col) | is.factor(col)})   
  fac.names <- colnames(fac.cols)
  return(fac.names)
}


#'Determine number and names of levels in the response column for logistic regression'
#'@description Java will use the name and number of factor levels to enable user options for response variable levels selection
#'@param mSetObj Input name of the created mSetObject 
#'@param facA Column name of response variable (defined by user, java uses factor.columns())
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

log.response.levels <- function(mSetObj=NA, facA = "NULL"){  #  contained facA="NULL" 202212-31
#  load_dplyr()
## is this supposed to be equivalent to log.ref.level??
  mSetObj <- .get.mSet(mSetObj)
   
if(facA == "NULL") {
  if("logRegInfo" %in% names(mSetObj$analSet) ){
     facA <- mSetObj$analSet$logRegInfo$response
   } else {
     fac.cols <- mSetObj$dataSet$norm[,sapply(mSetObj$dataSet$norm, is.factor) | sapply(mSetObj$dataSet$norm, is.character), drop = FALSE]  # fac_cols <- mSetObj$dataSet$norm %>% dplyr::select_if(function(col) {is.character(col) | is.factor(col)})  
     facA <- colnames(fac_cols)[1] #Default is 1st factor column as response column
   }
  } else {
   facA <- facA #User selected, java uses function factor.columns() to obtain options
  }
   
  ## could be problematic if there are NAs in input data, could be removed in levels during data read-in conversion process - in order to have NA be converted as an explicit factor level, using factor(vector, exclude = NULL)
# right now it looks like NA values are imputed or excluded during pre-processing so there aren't any NA values that may be excluded as factor levels (general_proc_utils)
# or maybe, should tell user to explictly specify the grouping as "missing" if they want to view the effect of the group in the model as a seperate level - only with categorical variables
#  - if data is read in with strings as factors, then the NAs may not show up as levels 
#  - ie. if there are NAs, and someone wants that to count NA as a separate group (factor) level 
# (eg. for exploratory data analysis, if they plan to attribute that NA group to an unknown factor or give it a grouping name like 'batch')
# - as it looks like the data processing converts character columns to factors, and for factor variables it is the case (?) that NAs are not displayed as a factor level (unless explicitly specified with exclude = NULL),
# is there a place where that NA information is stored, if present? Are those NAs converted to a different value character, name something different like 'NAwegan'or something, so that they are stored somewhere?

  facA.levels <- unique(mSetObj$dataSet$norm[,facA, drop = TRUE]) #List names of levels in the response column
  facA.levels.names <- levels( as.factor(facA.levels) ) #Extract names in character vector
  return(facA.levels.names)
  
}
