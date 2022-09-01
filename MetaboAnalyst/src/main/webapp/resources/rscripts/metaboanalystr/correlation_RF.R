#'Perform Random Forest Regression'
#'@description Use random forest for regression analysis
#'@usage reg.rf.anal(mSetObj=NA, facA=NULL, pred.text=NULL)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param predtext Input predictor column names (java uses text box to obtain string)
#'@param data Whether or not to use normalized data (Boolean checkbox) ## UNNECESSARY?
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

reg.rf.anal <- function(mSetObj=NA, facA=NULL, predtext=NULL, data="false") {
  
  #install.packages(c("randomForest", "Metrics"))
  library("randomForest")
  library("Metrics")
  
  mSetObj <- .get.mSet(mSetObj)
  
### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

  #Text will be visible to user. Dependent var default is first column. Independent var default is second column.
  #cat("One dependent variable and two or more independent variables will be tested for correlation. The dependent variable must be numeric. The independent variables can be numeric or categorical.")
  #cat("For categorical independent variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the level labels to I, II and III prior to upload.")

  #Data check
  if (ncol(input)<3) {
    col.classes <- sapply(input,class)
    if ("factor" %in% col.classes) {
    stop("Your data set only has 2 variables! Try using logistic regression.")
    } else {
      stop("Your data set only has 2 variables! Try using linear or polynomial regression.")
    }
  }
  
  #Set dependent (response) variable name
  if (facA=="NULL") {
    for (i in seq_along(colnames(input)) ) {
      if (is.factor(input[,i])==FALSE) {
        facA <- colnames(input)[i]# Default is to choose the first numeric column as response column
        break
      }
    }
  } else {
    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
  }
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default pred.text is second column.
  cat("Indicate independent variables using the column names with commas in between.")
  
  #Set right side of formula with predictor variables
  if (predtext=="NULL") {
    data <- input[,colnames(input)!=facA]  # resp.col.num <- which(colnames(input)==facA)
    predtext <- paste0(colnames(data)[1], ",", colnames(data)[2]) #Default is the first 2 potential predictor columns
  } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
  #Curate right side of formula, and extract character vector of predictors
  predtext <- gsub("\n", "", predtext, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
  predtext <- gsub(",", "+", predtext, fixed=TRUE) 
  predtext <- gsub(";", "+", predtext, fixed=TRUE)
  predtext <- gsub(" ", "", predtext, fixed=TRUE)
  predtext <- gsub(":", "+", predtext, fixed=TRUE)
  predtext <- gsub("*", "+", predtext, fixed=TRUE)
  
  #Generate formula
  formula <- as.formula(paste(facA, "~", predtext))
  #Text should be visible to user
  cat(paste0("You have created this formula for model building: ", facA, " ~ ", pred.text))
  #cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
  #cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
  #Subset data using predictor column names
  predictors <- unlist(strsplit(pred.text, "+", fixed=TRUE))

 if(any(!colnames(input) %in% predictors)){
   stop(paste0( "'", predictors[!predictors %in% colnames(input)],
  "' not found in data variables ('",
  paste(colnames(input), collapse = "', '"),
  "'): check spelling of text box input."))
}


  pred_data <- input[,colnames(input) %in% predictors]
  model_data <- data.frame(input[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors)
  
  #Generate test and train data for model building
  set.seed(37) #Insures same selction of data for test and train each time
  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset
  train_data <- model_data[index,] #70% of dataset
  test_data <- model_data[-index,] #30% of dataset

  predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors # [,-1] removes intercept column of all 1's
  predictors_test <- predictors_test[,-1] # predictor data for test dataset # [,-1] removes facA from df
  predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-1] # Predictor variables in train dataset, creating dummy vars for categorical predictors
  predictors_train <- predictors_train[,-1] # predictor data for train dataset
  response_train <- train_data[,facA] # response data for train dataset # vector
  response_test <- test_data[,facA] # response data for test dataset #vector
  #cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
 
  
  #BUILD MODE, PREDICT ##
 model <- randomForest::tuneRF(y = response_train, x = predictors_train, ntreeTry = 500, stepFactor = 2, improve = 0.05, trace = FALSE, doBest = TRUE, plot = FALSE, importance = TRUE)
  model_name <- "Random Forest Regression" # train_data[,1] same as response_train
  
  #Generate results
  summary <- model 
  predictor_importance <- randomForest::importance(model, type=1) #type to sort by, 1=mean decrease in accuracy
  fitted <- predict(model)  
  train_rmse <- Metrics::rmse(response_train, fitted)
  fileName <- "rf_regression_summary.txt"
  
  #Obtain test RMSE for plotting
  test_prediction <- predict(model, newdata=predictors_test)
  prediction <- predict(model, newdata = as.matrix(predictors_test)) # from ml.R script

  test_rmse <- Metrics::rmse(response_test, test_prediction)
  rf_RMSE <- Metrics::rmse(predictors_train[,1], fitted) # was svm; from ml.R script
  
#STORE RESULTS
  ##Store results for plotting
  #mSetObj$analSet$rfReg$meth <- model_name
  #mSetObj$analSet$rfReg$pred <- prediction
  #mSetObj$analSet$rfReg$test <- test_data
  #mSetObj$analSet$rfReg$res <- list(summary = summary, predicted.values = fitted, RSME = svm_RMSE, predictor.importance = predictor_importance, fileName = fileName)   
  #mSetObj$analSet$rfReg$mod <- list(model_name = model_name, model = model, response = response_train_name, predictor = predictors_train_name)

  #STORE RESULTS
  mSetObj$analSet$rfReg$res <- list(summary=summary, response=facA, predictors=colnames(predictors_train), pred.text=pred.text, predicted.values=fitted, train.RMSE=train_rmse, test.prediction=test_prediction, test.RMSE=test_rmse, predictor.importance=predictor_importance, train_data=train_data, test_data=test_data, predictors.test.data=predictors_test, predictors.train.data=predictors_train, method=model_name, fileName=fileName)       
  mSetObj$analSet$rfReg$mod <- list(model_name=model_name, model=model, response=facA, predictors=colnames(predictors_train))

  #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink(fileName) 
  cat("Formula:\n")
  print(formula)
  print(model)
  cat("\nPredicted values:\n")
  print(fitted)
  cat("\nModel RMSE:\n")
  cat(paste0(train_rmse, "\n"))
  cat("\nPredictor variable importance:\n")
  print(predictor_importance)
  sink()
  
  return(.set.mSet(mSetObj))
  
}


#'Plot random forest predicted vs actual data plot using test data
#'@description Scatter plot where actual data is y and predicted data is x
#'@usage plot.pred.svmReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
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

rf.pred.plot <- function(mSetObj=NA,  #plot.pred.RFReg
facA = "NULL", 
predtext ="NULL",
 data="false",
  
  col_dots="NULL",
  col_line="NULL", 
  plot_ci="false",
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
## WAS THIS:
  #method <- mSetObj$analSet$rfReg$res$method
  #prediction <- mSetObj$analSet$rfReg$res$test.prediction
  #test_data <- mSetObj$analSet$rfReg$res$test_data
  #facA <- mSetObj$analSet$rfReg$res$response
  
### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

### GET FACA AND PREDTEXT

##### WITH facA and predtext options
  ##### [CRUNCH]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
  if (facA == "NULL") { 
    if("res" %in% names(mSetObj$analSet$rfReg) ){ #if there is a results made already, take that response
        facA <- mSetObj$analSet$rfReg$res$response
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

  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
  if (predtext == "NULL") {
    if("res" %in% names(mSetObj$analSet$rfReg) ){#if there is a results made already, take that predictor
        predtext <- mSetObj$analSet$rfReg$res$predictor
     } else {
    data <- input[ , colnames(input) != facA] #drop=FALSE means it will be a df
 predtext <- paste0(colnames(data)[1], ",", colnames(data)[2]) #Default is the first 2 potential predictor col 
#num.data <- dplyr::select_if(dat, is.numeric)
    #predtext <- colnames(num.data)[1] #Default is the 1st potential predictor column
 } 
    } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
  predtext <- gsub("\n", "", predtext, fixed = TRUE)
  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
  predtext <- gsub(";", "+", predtext, fixed = TRUE)
  predtext <- gsub(" ", "", predtext, fixed = TRUE)
  predtext <- gsub(":", "+", predtext, fixed = TRUE)
  predtext <- gsub("*", "+", predtext, fixed = TRUE)
   
  #GENERATE FORMULA #formula <- as.formula(paste(facA, "~", predtext))
   ### CHECK: are all input predictor names in data
  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
 if(any(!colnames(data) %in% predictors2)){
   stop(paste0( "'", predictors[!predictors2 %in% colnames(data)],
  "' not found in data variables ('",
  paste(colnames(data), collapse = "', '"),
  "'): check spelling of text box input."))
}

  #SUBSET DATA USING PREDICTOR COLUMN NAMES
  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2])
  model_data <- data.frame(input[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)
  
  #GENERATE TEST AND TRAIN
  set.seed(37) #Ensures same selection of data each time
  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)
  train_data <- model_data[index,] #70% of dataset
  test_data <- model_data[-index,] #30% of dataset
  predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors
  predictors_test <- predictors_test[,-1] # predictor data for test dataset
  predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-1] # Predictor variables in train dataset, creating dummy vars for categorical predictors
  predictors_train <- predictors_train[,-1] # predictor data for train dataset
  response_train <- train_data[,facA] # response data for train dataset
  response_test <- test_data[,facA] # response data for test dataset
  #cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
 
  
  #BUILD MODEL, PREDICT ### TRACE TRUE OR FALSE? false in og rf script, TRUE in ml script
  model <- randomForest::tuneRF(y=response_train, x=predictors_train, ntreeTry=500, stepFactor=2, improve=0.05, trace=FALSE, doBest=TRUE, plot=FALSE, importance=TRUE) # from ml.R script
  prediction <- predict(model, newdata = as.matrix(predictors_test)) # from ml.R script

###### 
###### [CRUNCH DONE]

  dfpred <- data.frame(fpred = prediction, fA = input[,facA])
  formula2 <- as.formula("fA ~ fpred")
  model2 <- lm(formula = formula2, data = dfpred)
    
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.rfReg <- imgName
    
    
 ### TROUBLESHOOTING:
  ##   col_dots1<-"blue"
  ##   col_line1<-"red"
  ##   plot_ci1<-TRUE
  ##   plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(formula), ")")
  ##   plot_ylab1 <- "Actual"
  ##   plot_xlab1<- "Predicted"
  
  
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
    #plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(formula), ")")
    plot_title1 <- paste0("Predicted vs Actual")
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
 
  #Set plot dimensions
    if(is.na(width)){
      w <- 10.5
    } else if(width == 0){
      w <- 7.2
    } else{
      w <- width
    }
    h <- w
  # plot(x=prediction, y=model_data[,facA], xlab=paste0("Predicted ", facA), ylab=paste0("Actual ", facA), main=model_name, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
  
  ## MAKE PLOT  
   a0 <- ggplot(data =  dfpred, aes(x = fpred, y = fA)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE) +#, formula = formula2) +
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
    print(a0)
    # plot(prediction, test_data[,1], xlab="Predicted", ylab="Actual", main=method, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
    dev.off()
      
   # STORE IN mSET
  mSetObj$analSet$rfReg$plotpred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  
 #JSON OBJECT MAKING
  build <- ggplot_build(a0)
  build_line <- build$data[[1]]
  build_points <- build$data[[2]]
  linear_plot_json <- list()
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
  linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
  linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
  linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
  linear_plot_json$points$size <- build_points[,c("size")]#[,7]
  linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
  # linear_plot_json$label <- build$data[[3]][,c("label")]
  # linear_plot_json$lines$ci <- build$data[[1]][,c("se")]
  
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
    
#'Plot random forest error rates
#'@description Plots error rate as a function of forest size 
#'@usage plot.pred.rfReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects
#'@param col_line
#'@param plot_title
#'@param plot_ylab
#'@param plot_xlab
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

rf.error.plot <- function(mSetObj=NA,
facA = "NULL",
predtext="NULL",
data="false",
  col_line="NULL", 
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
imgName, format="png", dpi=72, width=NA){ #plot.rf.err
  
# name was: plot.pred.rfError
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
## WAS THIS:
  #method <- mSetObj$analSet$rfReg$res$method
  #prediction <- mSetObj$analSet$rfReg$res$test.prediction
  #test_data <- mSetObj$analSet$rfReg$res$test_data
  #facA <- mSetObj$analSet$rfReg$res$response
  
### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

### GET FACA AND PREDTEXT

##### WITH facA and predtext options
  ##### [CRUNCH]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
  if (facA == "NULL") { 
    if("res" %in% names(mSetObj$analSet$rfReg) ){ #if there is a results made already, take that response
        facA <- mSetObj$analSet$rfReg$res$response
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

  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
  if (predtext == "NULL") {
    if("res" %in% names(mSetObj$analSet$rfReg) ){#if there is a results made already, take that predictor
        predtext <- mSetObj$analSet$rfReg$res$predictor
     } else {
    data <- input[ , colnames(input) != facA] #drop=FALSE means it will be a df
 predtext <- paste0(colnames(data)[1], ",", colnames(data)[2]) #Default is the first 2 potential predictor columns
#num.data <- dplyr::select_if(dat, is.numeric)
    #predtext <- colnames(num.data)[1] #Default is the 1st potential predictor column
 } 
    } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
  predtext <- gsub("\n", "", predtext, fixed = TRUE)
  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
  predtext <- gsub(";", "+", predtext, fixed = TRUE)
  predtext <- gsub(" ", "", predtext, fixed = TRUE)
  predtext <- gsub(":", "+", predtext, fixed = TRUE)
  predtext <- gsub("*", "+", predtext, fixed = TRUE)
   
  #GENERATE FORMULA #formula <- as.formula(paste(facA, "~", predtext))
   ### CHECK: are all input predictor names in data
  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
 if(any(!colnames(data) %in% predictors2)){
   stop(paste0( "'", predictors[!predictors2 %in% colnames(data)],
  "' not found in data variables ('",
  paste(colnames(data), collapse = "', '"),
  "'): check spelling of text box input."))
}

  #SUBSET DATA USING PREDICTOR COLUMN NAMES
  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2])
  model_data <- data.frame(input[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)
  
  #GENERATE TEST AND TRAIN
  set.seed(37) #Ensures same selection of data each time
  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)
  train_data <- model_data[index,] #70% of dataset
  test_data <- model_data[-index,] #30% of dataset
  # predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors
 # predictors_test <- predictors_test[,-1] # predictor data for test dataset
  predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-1] # Predictor variables in train dataset, creating dummy vars for categorical predictors
  predictors_train <- predictors_train[,-1] # predictor data for train dataset
  response_train <- train_data[,facA] # response data for train dataset
  #response_test <- test_data[,facA] # response data for test dataset
  #cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
 
  
  #BUILD MODEL, PREDICT ### TRACE TRUE OR FALSE? false in og rf script, TRUE in ml script
  model <- randomForest::tuneRF(y=response_train, x=predictors_train, ntreeTry=500, stepFactor=2, improve=0.05, trace=FALSE, doBest=TRUE, plot=FALSE, importance=TRUE) # from ml.R script
 # prediction <- predict(model, newdata = as.matrix(predictors_test)) # from ml.R script

###### 
###### [CRUNCH DONE]


  dferr <- data.frame(error=model$mse, trees = c(1:500))
    
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.rfReg <- imgName
    
    
 ### TROUBLESHOOTING:
  ##   col_line1<-"red"
  ##   plot_ci1<-TRUE
  ##   plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(formula), ")")
  ##   plot_ylab1 <- "Actual"
  ##   plot_xlab1<- "Predicted"
  
  

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
  
  # PLOT TITLE
  if(plot_title == " "){ 
    plot_title1 <- paste0("Random Forest Regression")
  } else {
    plot_title1 <- plot_title
  }
 
  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Error"
  } else { 
    plot_ylab1 <- plot_ylab
  }

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Trees"
  } else { 
    plot_xlab1 <- plot_xlab
  }
 
  #Set plot dimensions
    if(is.na(width)){
      w <- 10.5
    } else if(width == 0){
      w <- 7.2
    } else{
      w <- width
    }
    h <- w
  
  ## MAKE PLOT  
   a0 <- ggplot(data =  dferr, aes(x = trees, y = error)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_line(color = col_line1) +
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
    print(a0)
  # plot(model, main=method, yaxt="n");axis(2, las=2)
    dev.off()
      
   # STORE IN mSET
  mSetObj$analSet$rfReg$ploterror <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  
 #JSON OBJECT MAKING
  
  build <- ggplot_build(a0)
  build_line <- build$data[[1]]
  linear_plot_json <- list()
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
  linear_plot_json$line$coords <- build_line[,c("x","y")] #[,1:2]
  linear_plot_json$line$cols <- build_line[,grepl("col",colnames(build_line))] #[,6] #colours
  linear_plot_json$line$size <- build_line[,c("size")]#[,7]
  # linear_plot_json$label <- build$data[[3]][,c("label")]
  # linear_plot_json$lines$ci <- build$data[[1]][,c("se")]
  
  if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 }

  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName2)
  cat(json.obj)
  sink()
  
   if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }

}


#'Plot relative importance of predictors
#'@description Bar graph for relative importance of independent variables
#'@usage rf.relimpo.plot(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'@param plot_palette Set color palette from RColorBrewer paelettes (default "NULL" is Set2); (static dropdown)
#'@param plot_label Add label over bar with relimpo value (rounded 3 dig) (default "NULL" is no label); (checkbox)
#'@param plot_title Input the name of the title (default: "Relative Importance of Predictors for  (facA);, textbox)
#'@param plot_xlab Input the name to use for x-axis label (default: facB, textbox)
#'@param plot_ylab Input the name to use for y-axis label (default: facA, textbox)
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

# name was: plot.relimpo.rfReg

rf.relimpo.plot <- function(mSetObj=NA, 
facA = "NULL",
predtext="NULL",
data="false",
  plot_palette="NULL", 
  plot_label="false", 
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
imgName, format="png", dpi=72, width=NA){ #plot.rf.err
  
# name was: plot.pred.rfError
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
## WAS THIS:
  #method <- mSetObj$analSet$rfReg$res$method
  #prediction <- mSetObj$analSet$rfReg$res$test.prediction
  #test_data <- mSetObj$analSet$rfReg$res$test_data
  #facA <- mSetObj$analSet$rfReg$res$response
  
### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }

### GET FACA AND PREDTEXT

##### WITH facA and predtext options
  ##### [CRUNCH]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
  if (facA == "NULL") { 
    if("res" %in% names(mSetObj$analSet$rfReg) ){ #if there is a results made already, take that response
        facA <- mSetObj$analSet$rfReg$res$response
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

  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
  if (predtext == "NULL") {
    if("res" %in% names(mSetObj$analSet$rfReg) ){#if there is a results made already, take that predictor
        predtext <- mSetObj$analSet$rfReg$res$predictor
     } else {
    data <- input[ , colnames(input) != facA] #drop=FALSE means it will be a df
 predtext <- paste0(colnames(data)[1], ",", colnames(data)[2]) #Default is the first 2 potential predictor col   
    #num.data <- dplyr::select_if(data, is.numeric)
    #predtext <- colnames(num.data)[1] #Default is the 1st potential predictor column
 } 
    } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
  predtext <- gsub("\n", "", predtext, fixed = TRUE)
  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
  predtext <- gsub(";", "+", predtext, fixed = TRUE)
  predtext <- gsub(" ", "", predtext, fixed = TRUE)
  predtext <- gsub(":", "+", predtext, fixed = TRUE)
  predtext <- gsub("*", "+", predtext, fixed = TRUE)
   
  #GENERATE FORMULA #formula <- as.formula(paste(facA, "~", predtext))
   ### CHECK: are all input predictor names in data
  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
 if(any(!colnames(data) %in% predictors2)){
   stop(paste0( "'", predictors[!predictors2 %in% colnames(data)],
  "' not found in data variables ('",
  paste(colnames(data), collapse = "', '"),
  "'): check spelling of text box input."))
}

  #SUBSET DATA USING PREDICTOR COLUMN NAMES
  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2])
  model_data <- data.frame(input[,facA], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)
  
  #GENERATE TEST AND TRAIN
  set.seed(37) #Ensures same selection of data each time
  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)
  train_data <- model_data[index,] #70% of dataset
  #test_data <- model_data[-index,] #30% of dataset
  # predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors
 # predictors_test <- predictors_test[,-1] # predictor data for test dataset
  predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-1] # Predictor variables in train dataset, creating dummy vars for categorical predictors
  predictors_train <- predictors_train[,-1] # predictor data for train dataset
  response_train <- train_data[,facA] # response data for train dataset
  #response_test <- test_data[,facA] # response data for test dataset
  #cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
 
  
  #BUILD MODEL, PREDICT ### TRACE TRUE OR FALSE? false in og rf script, TRUE in ml script
  model <- randomForest::tuneRF(y=response_train, x=predictors_train, ntreeTry=500, stepFactor=2, improve=0.05, trace=FALSE, doBest=TRUE, plot=FALSE, importance=TRUE) # from ml.R script

###### 
###### [CRUNCH DONE]

 predictor_importance <- randomForest::importance(model, type=1) #type to sort by, 1=mean decrease in accuracy
 predictor_importance$predictors<-rownames(predictor_importance)
 predictor_importance$relaimpo<-predictor_importance[,1]

### TROUBLESHOOTING 
  ## plot_palette = "Dark2"; 
  ###  plot_title1 <- paste0("Relative Importance of Predictors for ", facA); plot_xlab1 <- "Predictors (Independent Variables)" ;plot_ylab1 <- "Proportion of R Squared" 
        

  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.rfReg <- imgName
    

  #Set plot dimensions
    if(is.na(width)){
      w <- 10.5
    } else if(width == 0){
      w <- 7.2
    } else{
      w <- width
    }
    h <- w
 
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
    plot_title1 <- paste0("Relative Importance of Predictors \nRandom Forest Regression")
  } else {
    plot_title1 <- plot_title
  }
  
  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "% Increase Mean Square Error"
  #plot_ylab1 <- expression(paste("% Increase Mean Square Error"))
  } else { 
    plot_ylab1 <- plot_ylab
  }

   
  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Predictors (Independent Variables)" 
  } else { 
    plot_xlab1 <- plot_xlab
  }
 

  #GENERATE PLOT
  a0 <- ggplot(data = predictor_importance, aes(x = predictors, y = relaimp)) +
    labs(title = plot_title1) + ylab(plot_ylab1)+ xlab(plot_xlab1) +
    geom_col(aes(fill = predictors), alpha = 0.5, show.legend = FALSE) +
    scale_y_continuous(limits = c(0,100)) +
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

  
  #GENERATE PLOT
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    print(a0)
  #  barplot(t(predictor_importance), xlab="Independent Variables (Predictors)", ylab="% Increase Mean Square Error", main="Relative Importance of Predictors \nRandom Forest Regression", yaxt="n"); axis(2, las=2)
    dev.off()
      
   # STORE IN mSET
  mSetObj$analSet$rfReg$plotrel <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  
 #JSON OBJECT MAKING
  
  build <- ggplot_build(a0)
  build_line <- build$data[[1]]
  linear_plot_json <- list()
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
  linear_plot_json$line$coords <- build_line[,c("x","y")] #[,1:2]
  linear_plot_json$line$cols <- build_line[,grepl("col",colnames(build_line))] #[,6] #colours
  linear_plot_json$line$size <- build_line[,c("size")]#[,7]
  # linear_plot_json$label <- build$data[[3]][,c("label")]
  # linear_plot_json$lines$ci <- build$data[[1]][,c("se")]
  
  if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 }

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

#'Determine number and names of numeric columns for random forest regression'
#'@description Java will use the numeric columns to enable user options for selecting dependent variable
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
