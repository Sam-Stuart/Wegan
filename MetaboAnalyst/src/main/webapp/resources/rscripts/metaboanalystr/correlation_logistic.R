#'Perform Multivariate Logistic Regression'
#'@description Build a multivariate logistic regression model for user selected predictor variables
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses factor.columns() to give user options)
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param type Type of logistic regression (ordinal, multinomial or binomial)
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
                         type="multinomial",
                         reference="NULL", # dropdown
                         ordertext="NULL" # textbox; order of response levels
                         # weights=weights
                         ) {
  
  library("nnet") #For multinomial regression
  library("MASS") #For ordinal regression
  library("dplyr") #For data manipulation # `%>%` should be exported for piping 
  # library("tidyselect") # tidyselect helper 'where' is not exported to namespace, but workaround exists (so still need package) ; to replace deprecated select_ variant `select_if`
# library("JSONIO")


  mSetObj <- .get.mSet(mSetObj)

   ### SET DATA (whether to use original data or not)
  if (data == "false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  data <- input

  #TEXT SHOULD BE VISIBLE TO USER 
  cat("One categorical dependent variable and one or more independent variables will be tested for correlation. Independent variables can be categorical or numeric." )
  cat("By default, the first categorical variable is treated as the dependent variable. If the dataset has more than one categorical variable, you may change the dependent variable using the drop down menu.")
  cat("Choose model type=binomial if there are only 2 dependent variable levels and they are unordered.(Levels are unique values for a variable). Choose type=multinomial if there are more than 2 dependent variable levels and they are unordered. Choose type=ordinal if there are more than 2 dependent variable levels and they are ordered." )
  cat("For categorical variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the labels to I, II and III.")
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default predtext is second column.
  cat("Indicate independent variables using the column names with commas in between. If interested in an interaction between particular variables, indicate it with a colon rather than a comma.")
  
  #CHOOSE RESPONSE (DEPENDENT) VARIABLE FOR MODELING
  if (facA == "NULL") {
  #  facData <- data %>% dplyr::select(tidyselect::vars_select_helpers$where(is.character))
  # facData <- data %>%  dplyr::select(tidyselect::vars_select_helpers$where(is.character) | tidyselect::vars_select_helpers$where(is.factor))
    # facData <- data %>% dplyr::select_if(is.character) %>% dplyr:: select_if(is.factor)# | is.factor)
    # facData <- data[,sapply(data, is.factor) & sapply(data, is.character), drop = FALSE]

  facData <- data %>% dplyr::select_if(function(col) {is.character(col) | is.factor(col)})    

if (is.null(facData)) {
        stop("No categorical variables for analysis; did you input your categorical variables as numbers (for example, hot-one coded)?")
    } else {
        facA <- colnames(facData)[1]# Default is to choose the first factor column as response column
    }
  } else {
    facA <- facA #User selected, java uses function factor.columns() to provide options in drop down menu (only categorical columns are available)
  }

  ## FORMULA SET UP
  #SET RIGHT SIDE OF FORMULA WITH PREDICTOR VARIABLES (ie not facA)
  if (predtext == "NULL") {
    # resp.col.num <- which(colnames(data) == facA); predData <- data[,-resp.col.num]
    predData <- data[, !(colnames(data) == facA)]
    predtext <- colnames(predData)[2] #Default is 2nd predictor column
    # predtext <- paste(colnames(predData), collapse="+")
  } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
## predtext is input into R as one string (??)

  #CURATE RIGHT SIDE OF FORMULA; EXTRACT CHARACTER VECTOR OR PREDICTORS 
  predtext <- gsub("\n", "", predtext, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
  predtext <- gsub(" ", "", predtext, fixed=TRUE)
  predtext <- gsub(",", "+", predtext, fixed=TRUE) 
  predtext <- gsub(";", "+", predtext, fixed=TRUE)
  predtext <- gsub(":", "+", predtext, fixed=TRUE)
  predtext <- gsub("*", "+", predtext, fixed=TRUE)
  

  #GENERATE FORMULA 
  formula <- as.formula(paste0(facA, "~", predtext))   ## BEFORE IT WAS this
  # formula <- as.formula(paste(facA, paste(predtext, collapse=" + "), sep=" ~ "))
  #Text should be visible to user
  #cat(paste0("You have created this formula for model building: ", facA, " ~ ", predtext))
  #cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, the plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
  #cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  

## CHECK: are all input predictors in data
 predictors <- unlist(strsplit(predtext, "+", fixed=TRUE), use.names = FALSE) ## use.names = FALSE speeds up unlist
 if(any(!colnames(data) %in% predictors)){
stop(paste0( "'", predictors[!predictors %in% colnames(data)],
 "' not found in data variables ('", 
paste(colnames(data), collapse = "', '"), 
"'): check spelling of text box input."))
}

 #SUBSET DATA USING PREDICTOR COLUMN NAMES
  pred_data <- data[,which(colnames(data) %in% predictors)]
  model_data <- data.frame(data[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors)

## MAKE RESPONSE VAR A FACTOR
if(!is.factor(model_data[,1])){
    model_data[,1] <- factor(model_data[,1])
  }   
 
 #CHECK NUMBER OF LEVELS 
  levels.num <- nlevels(model_data[,1])

## ORDINAL
  if (type == "ordinal") {

 if (levels.num < 3) {
      #AddErrMsg("The dependent variable has less than 3 levels! Try binomial regression instead.")
      stop("The dependent variable has less than 3 levels. Try binomial regression instead.")
    } 

    #REFERENCE LEVEL: SET FOR RESPONSE VARIABLE
    #IDENTIFY DEFAULT REF LEVEL
    if (reference == "NULL") {
      ref.col <- as.data.frame(model_data[,1])
      print("ref.col") 
      print(ref.col) 
      ref.col.levels <- levels(model_data[,1])
      reference <- ref.col.levels[1] #Reference level defaults the 1st level of the response column
      print("reference")
      print(reference)
    } else {
      reference <- reference
    }
    
# ## CHECK IF REFERENCE INPUT IS VALID # how is reference level output?
#  if(any(!levels(model_data[,1]) %in% reference)){
#     stop(paste0("reference ('", reference, 
#     "') not found in dependent variable levels ('",
#     paste(levels(model_data[,1]), collapse = "', '"), 
#     "')"))
# }

    #SET REFERENCE LEVEL
    model_data[,1] <- relevel(as.factor(model_data[,1]), ref = reference) 
  
    #ORDER OF RESPONSE VARIABLE LEVELS
    #Text box instructions for selecting dependent variable levels. Text box should be interactive, meaning any change in text alters the result in real time. Default ordertext is no reorder.
    cat("If performing ordinal regression, indicate the ascending order of the dependent variable levels using the level names with commas in between. For example, if your dependent variable is placement in a sports match, type bronze, silver, gold in the text box.")

    if (ordertext == "NULL") {
      model_data[,1] <- ordered(model_data[,1]) #Default is use order as inputted
      ordertext <- levels(model_data[,1])
    } else { 
      ordertext <- ordertext #Order is user inputted in ascending order, taken from text box by java, entered into R code as one character value (names separated by commas) (string)
      ordertext <- gsub(" ", "", ordertext, fixed = TRUE)
      ordertext <- unlist(strsplit(ordertext, ",", fixed = TRUE))

    ### CHECK IF ORDERTEXT HAS VALID NAMES:
  if(any(!levels(model_data[,1]) %in% ordertext)){
   stop(paste0( "'", ordertext[!ordertext %in% levels(model_data[,1])], 
     "' not found in dependent variable levels of data (variable levels: '",
     paste(levels(model_data[,1]), collapse = "', '"), 
     "'): check spelling of text box input."))
     }
      model_data[,1] <- ordered(model_data[,1], levels = paste(ordertext, sep = ","))
    }
    
#### ADD CONFLICT BETWEEN REFERENCE AND LEVEL ORDER?

    #BUILD MODEL
    model <- MASS::polr(formula, data = model_data, method = "logistic", Hess = TRUE)
    model_name <- "Ordinal Logistic Regression"
    
    #EXTRACT RESULTS
    summary <- summary(model) #Summary of effects: Response/predictor odds ratios, SE and confidence intervals
    order <- paste0(ordertext, collapse = " < ")
    fitted <- fitted(model) #Linear predicted values
    conf.int <- confint(model, level = 0.95) #Confidence intervals for predictor variables
    oddsRatio <- exp(coef(model))
    covar <- vcov(model) #Covariance matrix for predictor variables
    logLik <- logLik(model) 
    coeffs <- coef(model)
    std.errors <- sqrt(diag(covar))
    zValues <- coeffs / std.errors
    pValues <- pnorm(abs(zValues), lower.tail = FALSE)*2
    Hessian <- model[["Hessian"]]
    fileName <- paste0("ordinal_logistic_regression_reference_", reference, "_summary.txt") #File name for results
    
    #STORE RESULTS
    mSetObj$analSet$logOrdReg$res <- list(summary = summary, model.data = model_data, response = facA, predictor = predictors, predicted.values = fitted, confidence.intervals = conf.int, 
                                          Hessian = Hessian, oddsRatio = oddsRatio, covariance.matrix = covar, Loglikelihood = logLik, zValues = zValues, pValues = pValues, fileName = fileName)       
    mSetObj$analSet$logOrdReg$mod <- list(model_name = model_name, model = model, model.data = model_data, response = facA, predictor = predictors)
    
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    cat("\nDependent variable reference level:\n")
    cat(paste0(reference, "\n"))
    cat("\nDependent variable level order:\n")
    cat(paste0(order))
    cat("\n\nSummary:\n")
    print(summary)
    cat("\nWald test/z-values:\n")
    print(zValues)
    cat("\nP-values:\n")
    print(pValues)
    cat("\nOdds ratio:\n")
    print(oddsRatio)
    cat("\nLog likelihood:\n") 
    print(logLik)
    cat("\nPredicted values:\n")
    print(fitted)
    cat("\nCovariance matrix for predictor variables:\n")
    print(covar)
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    cat("\nHessian matrix:\n")
    print(Hessian)
    sink()
    
  } else if (type=="multinomial") {

    
    #CHECK NUMBER OF LEVELS
    model_data_as_factor <- as.factor(model_data[,1]) 
    levels.num <- nlevels(model_data_as_factor)
    if (levels.num < 3) {
      #AddErrMsg("The dependent variable has less than 3 levels! Try binomial regression instead.")
      stop("The dependent variable has less than 3 levels! Try binomial regression instead.")
    }

    
    if (reference == "NULL") {
      ref.col <- as.data.frame(model_data[,1])
      ref.col.levels <- levels(as.factor(ref.col))
      reference <- ref.col.levels[1] #Ref level defaults to the 1st level of the response column
      print("reference")
      print(reference)
    } else {
      reference <- reference
    }
    
 ## CHECK IF REFERENCE INPUT IS VALID # how is reference level output?
 if(any(!levels(model_data[,1]) %in% reference){
    stop(paste0("reference ('", reference, 
    "') not found in dependent variable levels ('",
    paste(levels(model_data[,1]), collapse = "' , '"), 
    "')"))
}

 ## SET REFERENCE
 model_data[,1] <- factor(model_data[,1], levels = 
c(reference, levels(model_data[,1])[!levels(model_data[,1]) %in% reference])
)
    #Set reference--ISNT WORKING!!!!!!!!!!! - gps - bc if the var isn't ordered then it won't take it
    #model_data[,1] <- relevel(as.factor(model_data[,1]), ref=reference) 

    #Build model for multinomial regression
    model <- nnet::multinom(formula, data = model_data, Hess = TRUE, maxit = 1000, weights = NULL)
    model_name <- "Multinomial Logistic Regression"
    #Extract results
    summary <- summary(model)
    residuals <- model[["residuals"]]
    fitted <- fitted(model) #Predicted values
    conf.int <- confint(model, level = 0.95) #Confidence intervals for predictor variables
    oddsRatio <- exp(coef(model))
    covar <- vcov(model) #Covariance matrix for preductor variables
    logLik <- logLik(model)
    coeffs <- coef(model)
    std.errors <- sqrt(diag(covar))
    zValues <- coeffs / std.errors
    pValues <- pnorm(abs(zValues), lower.tail = FALSE)*2
    Hessian <- model[["Hessian"]]
    fileName <- paste0("multinomial_logistic_regression_reference_", reference, "_summary.txt") #File name for results
    #Store results
    mSetObj$analSet$logMultinomReg$res <- list(summary = summary, model.data = model_data,
 response = facA, predictor = predictors, 
residuals = residuals, predicted.values = fitted, 
confidence.intervals = conf.int, oddsRatio = oddsRatio, 
covariance.matrix = covar, Loglikelihood = logLik, 
zValues = zValues, pValues = pValues, fileName = fileName)       
    mSetObj$analSet$logMultinomReg$mod <- list(model_name = model_name, 
model = model, model.data = model_data, response = facA, predictor = predictors)
    cat("NUMBER 6")
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    cat("\nDependent variable reference level:\n")
    cat(paste0(reference, "\n\n"))
    print(summary)
    cat("\nWald test/z-values:\n")
    print(zValues)
    cat("\nP-values:\n")
    print(pValues)
    cat("\nOdds ratio:\n")
    print(oddsRatio)
    cat("\nLog likelihood:\n") 
    print(logLik)
    cat("\nResiduals:\n")
    print(residuals)
    cat("\nPredicted values:\n")
    print(fitted)
    cat("\nCovariance matrix for predictor variables:\n")
    print(covar)
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    cat("\nHessian matrix:\n")
    print(Hessian)
    sink()
  
  } else { #Default type is binomial
    
## BINOMIAL
 # CHECK N LEVELS
    if (levels.num < 2) {
      #AddErrMsg("The dependent variable has less than 2 levels! Check the levels of the variable and make sure there are only 2 unique values.")
      stop("The dependent variable has less than 2 levels! Check the levels of the variable and make sure there are only 2 unique values.")
    } else if (levels.num > 2) {
      #AddErrMsg("The dependent variable has more than 2 levels! Try multinomial regression instead.")
      stop("The dependent variable has more than 2 levels! Try multinomial regression instead.")
    }
    
    #Obtain reference level for response variable
    if (reference == "NULL") {
      ref.col <- as.data.frame(model_data[,1]) 
      reference <- levels(ref.col)[1] #Reference category is default the first level of the response column
    } else {
      reference <- reference
    }
    
## CHECK IF REFERENCE INPUT IS VALID # how is reference level output?
 if(any(!levels(model_data[,1]) %in% reference){
    stop(paste0("reference ('", reference, 
    "') not found in dependent variable levels ('",
    paste(levels(model_data[,1]), collapse = "' , '"), 
    "')"))
}

    #Set reference
    model_data[,1] <- relevel(as.factor(model_data[,1]), ref = reference) 
  
    #Build model
    model <- glm(formula, data = model_data, family = binomial("logit"), maxit = 1000, weights = NULL)
    model_name <- "Binomial Logistic Regression"
    
    #Extract results
    summary <- summary(model)
    residuals <- model[["residuals"]]
    fitted <- fitted(model) #Predicted values
    conf.int <- confint(model, level = 0.95) #Confidence intervals for predictor variables
    oddsRatio <- exp(coef(model))
    covar <- vcov(model) #Covariance matrix for preductor variables
    testStat <- with(model, null.deviance - deviance)
    testStatDF <- with(model, df.null - df.residual)
    pValue <- with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    logLik <- logLik(model)
    coeffs <- coef(model)
    std.errors <- sqrt(diag(covar))
    zValues <- coeffs / std.errors
    pValues <- pnorm(abs(zValues), lower.tail = FALSE)*2
    fileName <- paste0("binomial_logistic_regression_reference_", reference, "_summary.txt") #File name for results

    #Store results
    mSetObj$analSet$logBinomReg$res <- list(summary = summary, model.data = model_data,
 response = facA, predictor = predictors, 
residuals = residuals, predicted.values = fitted,
 confidence.intervals = conf.int, oddsRatio = oddsRatio,
covariance.matrix = covar, modelDiff = testStat,
modelDiffDF = testStatDF, pValue = pValue, 
Loglikelihood = logLik, zValues = zValues, pValues = pValues, fileName = fileName)       
    mSetObj$analSet$logBinomReg$mod <- list(model_name = model_name,
model = model, model.data = model_data, 
response = facA, predictor = predictors)
    
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    cat("\nDependent variable reference level:\n")
    cat(paste0(reference, "\n"))
    print(summary)
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
    print(residuals)
    cat("\nPredicted values:\n")
    print(fitted)
    cat("\nCovariance matrix for predictor variables:\n")
    print(covar)
    cat("\nConfidence intervals for predictor variables:\n")
    print(conf.int)
    sink()
      
  } 

  return(.set.mSet(mSetObj))
      
}






#'Effects plot for non-ordered logistic regression
#'@description 
#'@usage plot.effects.logReg(mSetObj, type="binomial", imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param type Type of logistic regression (ordinal, multinomial or binomial), binomial is default
#'@param 
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
                             type="binomial",  # was multinomial before
  plot_ci="false", #checkbox
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  plot_xangle = "false", #checkbox
  plot_palette = "NULL", #dropdown
  plot_leg_horiz = "false", #checkbox
  plot_leg_pos = "NULL", #dropdown
  imgName, format="png", dpi=72, width=NA){
  
  ## was named: plot.effects.logReg
  # library("effects")
  library("ggeffects")

  #EXTRACT OBJECTS FROM MSET
  mSetObj <- .get.mSet(mSetObj)
  
 # GET VARIABLES  
  if (type == "ordinal") { 
    # main = "Ordinal Logistic Regression \nEffects Plot"
    model <- mSetObj$analSet$logOrdReg$mod$model
    predictors <- mSetObj$analSet$logOrdReg$res$predictor
    facA <- mSetObj$analSet$logOrdReg$res$response
    main = paste0("Predicted Probabilities of ", facA, " ( Ordinal Logistic Regression)")
  } else if (type == "multinomial") {
    # main = "Multinomial Logistic Regression \nEffects Plot"
    model <- mSetObj$analSet$logMultinomReg$mod$model
    predictors <- mSetObj$analSet$logMultinomReg$res$predictor
    facA <- mSetObj$analSet$logMultinomReg$res$response
    main = paste0("Predicted Probabilities of ", facA, " ( Multinomial Logistic Regression)")
  } else { #Binomial
    # main = "Binomial Logistic Regression \nEffects Plot"
    model <- mSetObj$analSet$logBinomReg$mod$model
    predictors <- mSetObj$analSet$logBinomReg$res$predictor
    facA <- mSetObj$analSet$logBinomReg$res$response
    main = paste0("Predicted Probabilities of ", facA, " ( Binomial Logistic Regression)")
  }
  
### TROUBLESHOOTING (Q&D)
 input <- iris; 
 # plot_xangle1 <- 0; plot_palette1 <- "blambus"; plot_leg_pos1 <- "bottom"; plot_leg_horiz1 <- "horizontal"
 # plot_xlab1 <- predictors[1];  plot_title <- paste0("Predicted Probabilities of ", facA) ; plot_ylab1 <- facA


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
  # `breakfast club` = c("#b6411a", "#4182dd", "#2d6328", "#eec3d8", "#ecf0c8"),

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
    plot_title1 <- main
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
    plot_xlab1 <- predictors[1]#facB
  } else {
    plot_xlab1 <- plot_xlab
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
  mSetObj$imgSet$plot.effects.logReg <- imgName
 
  a0 <- plot(
    ggeffects::ggpredict(model = model, terms = predictors[1:2]
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
     theme(axis.text.x =  element_text(angle = plot_xangle1),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = plot_leg_pos1, 
      legend.direction = plot_leg_horiz1, 
      legend.title = element_text(face = "bold"),
    strip.background = element_rect(colour = "white", fill = "white"))
 
  # STORE IN mSET
  if (type == "ordinal") { #mSetObj$analSet$logOrdReg$res
  mSetObj$analSet$logOrdReg$ploteff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
    } else if (type == "multinomial"){
  mSetObj$analSet$logMultinomReg$ploteff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)  
  } else if (type == "binomial"){
    mSetObj$analSet$logBinomReg$ploteff <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1) 
  }


  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  print(a0)
  # plot(effects::allEffects(mod=model), ylim=c(0,1), rescale.axis=FALSE, main=main)
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}



#'ROC curve for logistic regression
#'@description Display ROC curve with area under the curve (AUC) value for logistic regression
#'@usage plot.ROC.logReg(mSetObj, type="ordinal", imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param type Type of logistic regression (ordinal, multinomial or binomial), binomial is default
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
type="multinomial", 
  plot_palette = "NULL", #dropdown  
        plot_title = " ",
imgName, format="png", dpi=72, width=NA){
  
## named: plot.ROC.logReg
  library("pROC")
## ggplot alternatives for ROC plotting:
# library("precrec")
# library("plotROC")
# library("multiROC") # older package
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
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


# c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdYlBu", "Dark2", "Paired", "Set2")
   # plot_palette1 <- "Dark2"
   plot_palette1 <- 
     switch(
       plot_palette,
       "NULL" = "Dark2",
       "Dark2" = "Dark2", 
       "Set2" = "Set2",
       "RedYellowBlue" = "RdYlBu", 
       "PinkYellowGreen" = "PiYG", 
       "BrownBlueGreen" = "BrBG",
       NULL
     )

 # type <- "ordinal"  
   .simcap <- function(x) { # https://stackoverflow.com/questions/58350357/capitalizing-the-first-character-in-a-string-in-r#58350475
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
   }
  # PLOT TITLE
  if(plot_title == " "){
    plot_title1 <- paste(.simcap(type), " Logistic Regression \nROC Curve", sep="")
  } else {
    plot_title1 <- plot_title
  }
   
  if (type=="ordinal") {
    model <- mSetObj$analSet$logOrdReg$mod$model
    model_data <- mSetObj$analSet$logOrdReg$res$model.data
    facA <- mSetObj$analSet$logOrdReg$res$response
    predicted <- fitted(summary(model))
# mSetObj$analSet$logOrdReg$res$linear.predicted.values
    prob <- predict(model, type="probs")
    
    ############ using {multiROC} - does it work or ordinal??
    
    # Micro-average ROC/AUC was calculated by stacking all groups together, thus converting the multi-class classification into binary classification. Macro-average ROC/AUC was calculated by averaging all groups results (one vs rest) and linear interpolation was used between points of ROC. Methodsshows names of different classifiers.
    
mn_pred <- data.frame(prob)
mn_pred_col <- colnames(mn_pred)
colnames(mn_pred) <- paste0(mn_pred_col, "_pred_MN")
true_label <-data.frame(caret::class2ind(model_data[,facA]))
true_label_col <- colnaes(true_label)
colnames(true_label) <- paste0(true_label_col, "_true")

plot_roc_df <- multiROC::plot_roc_data(
  multiROC::multi_roc(cbind(true_label, mn_pred),
                               force_diag = TRUE) )

a0 <- ggplot(plot_roc_df, 
       aes(x = 1-Specificity, y = Sensitivity)) +
  geom_path(aes(color = Group, linetype = Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                colour='grey',size = 0.75, linetype = 'dotdash') +
  ggtitle(plot_title1) +
  scale_color_brewer(palette = plot_palette1) +
  # scale_fill_brewer(palette = plot_palette1) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
  legend.justification=c(1, 0), legend.position=c(.95, .05),
 legend.title=element_blank(), 
legend.background = element_rect(fill=NULL, size=0.5,           linetype="solid", colour ="black"), 
axis.title = element_text(face = "bold", size = 12)
)
    
    #Generate plot
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    print(a0)
  ## https://github.com/WandeRum/multiROC
    # pROC::ggroc( pROC::multiclass.roc(model_data[,facA]~prob , plot=TRUE, print.auc = TRUE, xlab = "Specificity ( True Negative)", ylab = "Sensitivity (True Positive)", main = main, yaxt = "n" ) ); axis(2, las=2)
    dev.off()
    
  } else if (type == "multinomial") {

    model <- mSetObj$analSet$logMultinomReg$mod$model
    model_data <- mSetObj$analSet$logMultinomReg$res$model.data
    facA <- mSetObj$analSet$logMultinomReg$res$response
    predicted <- fitted(summary(model)) # mSetObj$analSet$logMultinomReg$res$linear.predicted.values
    prob <- predict(model, type="probs")
    
    ############ using {multiROC}
mn_pred <- data.frame(prob)
mn_pred_col <- colnames(mn_pred)
colnames(mn_pred) <- paste0(mn_pred_col, "_pred_MN")
true_label <-data.frame(caret::class2ind(model_data[,facA]))
true_label_col <- colnaes(true_label)
colnames(true_label) <- paste0(true_label_col, "_true")

plot_roc_df <- multiROC::plot_roc_data(
  multiROC::multi_roc(cbind(true_label, mn_pred),
                               force_diag = TRUE) )

a0 <- ggplot(plot_roc_df, 
       aes(x = 1-Specificity, y = Sensitivity)) +
  geom_path(aes(color = Group, linetype = Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                colour='grey',size = 0.75, linetype = 'dotdash') +
  ggtitle(plot_title1) +
  scale_color_brewer(palette = plot_palette1) +
  # scale_fill_brewer(palette = plot_palette1) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
  legend.justification=c(1, 0), legend.position=c(.95, .05),
 legend.title=element_blank(), 
legend.background = element_rect(fill=NULL, size=0.5,           linetype="solid", colour ="black"), 
axis.title = element_text(face = "bold", size = 12)
)
  a0  
    #Generate plot
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   print(a0)
     # multiclass.roc(model_data[,facA]~prob, plot=TRUE, print.auc=TRUE, xlab="Specificity (True Negative)", ylab="Sensitivity (True Positive)", main=main, yaxt="n"); axis(2, las=2)
    dev.off()
    
  } else { #binomial

    model <- mSetObj$analSet$logBinomReg$mod$model
    model_data <- mSetObj$analSet$logBinomReg$res$model.data
    facA <- mSetObj$analSet$logBinomReg$res$response
    predicted <- mSetObj$analSet$logBinomReg$res$predicted.values #fitted(summary(model))
    prob <- predict(model, type="response")
    
    #Generate plot
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    
  pROC::ggroc(  pROC::roc(model_data[,facA]~prob
      # , plot=TRUE, print.auc=TRUE, xlab="Specificity (True Negative)", ylab="Sensitivity (True Positive)", main=main, yaxt="n"
      ) )
    # axis(2, las=2)
    dev.off()
    
  }
  
  return(.set.mSet(mSetObj))

}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

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

# fac.cols <- data[,sapply(mSetObj$dataSet$norm, is.factor) & sapply(mSetObj$dataSet$norm, is.character), drop = FALSE]
  # fac.cols <- dplyr::select_if(mSetObj$dataSet$norm, is.character)
  fac.cols <- mSetObj$dataSet$norm %>% dplyr::select_if(function(col) {is.character(col) | is.factor(col)})   
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

log.response.levels <- function(mSetObj=NA, facA="NULL"){
  load_dplyr()
## is this supposed to be equivalent to log.ref.level??
  mSetObj <- .get.mSet(mSetObj)
  if (facA=="NULL") {
        fac_cols <- mSetObj$dataSet$norm %>% dplyr::select_if(function(col) {is.character(col) | is.factor(col)})  
        facA <- colnames(fac_cols)[1] # Choose the first factor column as response column
  } else {
    facA <- facA #User selected, java uses function factor.columns() to obtain options
  }
  resp.levels <- unique(mSetObj$dataSet$norm[,facA]) #List names of levels in the response column
  model_data_as_factor <- as.factor(resp.levels)
  resp.levels.names <- levels(model_data_as_factor) #Extract names in character vector
  return(resp.levels.names)
  
}
