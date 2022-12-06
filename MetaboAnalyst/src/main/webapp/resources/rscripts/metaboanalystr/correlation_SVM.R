#'Perform SVM Regression'
#'@description Use SVM for regression analysis
#'@usage svm.reg.anal(mSetObj=NA, facA="NULL", predtext="NULL")
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'@param predtext Input predictor column names (java uses text box to obtain string)
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

svm.reg.anal <- function(mSetObj=NA, 
facA="NULL",
predtext="NULL"#,data="false"
) {

  #install.packages(c("e1071", "Metrics"))
  # library("assertr")
  library("e1071")
  library("Metrics")

  mSetObj <- .get.mSet(mSetObj)
    print("svm.anal: data input set")
### SET DATA (whether to use original data or not)
#  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
#  } else {
#    input <- mSetObj$dataSet$orig
#  }

 # input <- input[order(as.numeric(rownames(input)),,drop=FALSE),]
 # print(input)
  #Text should be visible to user
#  cat("One dependent variable and one or more independent variables will be tested for correlation. The dependent variable must be numeric. The independent variables can be numeric or categorical.")
  cat("For categorical independent variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the level labels to I, II and III prior to upload.")
  
  #Set dependent (response) variable name
  if (facA == "NULL") {
    for (i in seq_along(colnames(input)) ) {
      if (is.factor(input[,i])==FALSE || is.character(input[,i])==FALSE) {
        facA <- colnames(input)[i]# Default is to choose the first numeric column as response column
        break
      }
    }
  } else {
    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu (only numeric columns are available)
  }
    print("svm.anal: response var set")  

  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default predtext is second column. 
# cat("Indicate independent variables using the column names with commas in between.")
  
  #Set right side of formula with predictor variables
    data <- input[,colnames(input) != facA, drop = FALSE]

  if (predtext == "NULL") {
    #resp.col.num <- which(colnames(input) == facA); data <- input[,-resp.col.num]
   predtext <- colnames( input[, !(colnames(input) == facA), drop = FALSE] )[1] 
   # predtext <- colnames(data)[1] #Default is the 1st potential predictor column
  } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }
  
    print("svm.anal: predictor var set")
    predtext1 <- predtext

  #PREDICTORS: Curate formula right side, and extract predictors character vector
  predtext <- gsub("\n", "", predtext, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
  predtext <- gsub(",", "+", predtext, fixed=TRUE) 
  predtext <- gsub(";", "+", predtext, fixed=TRUE)
  predtext <- gsub(" ", "", predtext, fixed=TRUE)
  predtext <- gsub(":", "+", predtext, fixed=TRUE)
  predtext <- gsub("*", "+", predtext, fixed=TRUE)
  
  #GENERATE FORMULA
### 202210-25 changed 'formula' variable name to 'form' because it also exists as a function, namespace weirdness, overlap problems
  form <- as.formula(paste(facA, "~", predtext))
  #Text should be visible to user
  cat(paste0("You have created this formula for model building: ", facA, " ~ ", predtext))
  #cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
  #cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")
  
   ### CHECK: are all input predictor names in data
  predictors2 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
  #predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
  
 #  data %>% assertr::verify(assertr::has_all_names(predictors2), error_fun = justwarn)

#  if(!all(predictors2 %in% colnames(data)) ){
#   warning(paste0("THIS_is_1st_function_", "'", predictors2[!predictors2 %in% colnames(data)],
#  "' not found in data variables ('",
#  paste(colnames(data), collapse = "', '"),
#  "'): check spelling of text box input."))
#}

  pred_data <- input[,which(colnames(input) %in% predictors2), drop = FALSE]
  model_data <- data.frame(input[,facA, drop = TRUE], pred_data)
  colnames(model_data) <- c(paste0(facA), predictors2)
  
  #GENERATE TEST AND TRAIN
  set.seed(37) #Ensures same selection of data each time
  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)

  train_data <- model_data[index,, drop = FALSE] #70% of dataset
  test_data <- model_data[-index,,drop = FALSE] #30% of dataset

  predictors_train <- model.matrix(train_data[,1] ~ ., train_data)[,-1] # Train predictor variables, creating dummy variables for categorical variables
  predictors_test <- model.matrix(test_data[,1] ~ ., test_data)[,-1] # Test predictor variables, creating dummy variables for categorical variables

  response_train <- train_data[,facA, drop=TRUE] # response data for train dataset
  response_test <- test_data[,facA, drop=TRUE] # response data for test dataset
print("svm.anal: train/test set made")

  # cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
 
#BUILD MODEL, PREDICT
    mod <- e1071::tune(e1071::svm, form,  data = as.data.frame(predictors_train), ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5))))
    tunedModel <- mod$best.model
    model_name <- "SVM Regression"
  print("svm.anal: model built")
    prediction <- predict(tunedModel, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
  print("svm.anal: after predicting model 1")

  #GENERATE AND DOWNLOAD SUMMARY OF PARAMETER TESTING, WRITE TO TXT DOC 
  summ <- summary(tunedModel) 
  resid <- residuals(tunedModel)
  decision_values <- tunedModel[["decision.values"]]
  fitt <- predict(tunedModel)
  print("svm.anal: after predicting model train")
  train_rmse <- Metrics::rmse(response_train, fitt)
# svm_RMSE <- Metrics::rmse(predictors_train[,1], fitt)
  fileName <- "svm_regression_summary.txt"
  
  #Obtain test RMSE for plotting # not in the original not ml.R file function
  test_prediction <- predict(tunedModel, newdata=test_data)
  test_rmse <- Metrics::rmse(response_test, test_prediction)
print("after predicting model test")  

  #STORE REMAINING RESULTS
  mSetObj$analSet$svmReg$res <- list(summary=summ, response=facA, predictors=predictors2, predtext=predtext1, pred.data = pred_data, predicted.values=fitt, train.RMSE=train_rmse, test.prediction=test_prediction, test.RMSE=test_rmse, train.data=train_data, test.data=test_data, method=model_name, fileName=fileName)
  mSetObj$analSet$svmReg$mod <- list(model_name=model_name, model=mod, response=facA, predictors=predictors2)

    ##Store results FROM ML.R FILE:
    ##mSetObj$analSet$svmReg$res <- list(summary = summ, predicted.values = fitt, residuals = resid, decision.values = decision_values, RSME = svm_RMSE, fileName = fileName)       
    ##mSetObj$analSet$svmReg$mod <- list(model_name = model_name, model = mod, response = response_train_name, predictor = predictors_train_name)

  
#DOWNLOAD TEXT DOC: containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink(fileName) 
  cat("Formula:\n")
  print(form)
  # cat("\nReference category:\n")
  # cat(paste0(reference))
  print(summ)
  cat("Residuals:\n")
  print(resid)
  cat("\nDecision values:\n")
  print(decision_values)
  cat("\nPredicted values:\n")
  print(fitt)
  cat("\nModel RMSE:\n")
  #cat(paste0(svm_RMSE))
  sink()
 
  return(.set.mSet(mSetObj))
    
}  


#'Plot svm predicted vs actual data plot with line of best fit
#'@description Scatter plot where response variable is y and predictor variable is x
#'@usage svm.pred.plot(mSetObj, facA="NULL", predtext = "NULL", col_dots="NULL", col_line="NULL", plot_title=" ", plot_ylab=" ", plot_xlab=" ",imgName, format="png", dpi=72, width=NA)
#'@param col_dots point color
#'@param col_line line color
#'@param plot_metric one of: NULL (for no plot annotation), RMSE (for root mean squared error)
#'@param plot_title Input the name of the title (default: "SVM Regression: Predicted vs Actual"), textbox
#'@param plot_ylab Input the name of the y axis label, textbox
#'@param plot_xlab Input the name of the x axis label, textbox
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

## 202210-04 remove plot_ci, comment out data arg

svm.pred.plot <- function(mSetObj=NA, 
facA = "NULL", 
predtext ="NULL",
# data="false",

  col_dots="NULL",
  col_line="NULL", 

  plot_metric = "NULL",  # added 202212-05
  plot_label_size = "NULL", # added 202212-05

  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
imgName, format="png", dpi=72, width=NA){
  # name used to be: plot.pred.svmReg

#  library("assertr")
#  library("e1071")
  library("Metrics")
  library("ggplot2")
#  library("RJSONIO")


  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)

### SET DATA (whether to use original data or not)
#  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
#  } else {
#    input <- mSetObj$dataSet$orig
#  }
print("svm.pred: set data")


 # method <- mSetObj$analSet$svmReg$res$method
 # prediction <- mSetObj$analSet$svmReg$res$test.prediction
 # facA <- mSetObj$analSet$svmReg$res$response
## wasthis:
  #facA <- mSetObj$analSet$svmReg$mod$response
  #method <- mSetObj$analSet$svmReg$res$method
  #prediction <- mSetObj$analSet$svmReg$predicted.values
  #test_data <- mSetObj$analSet$svmReg$res$test.data
  #input <- test_data
    
### GET FACA AND PREDTEXT

##### WITH facA and predtext options
  ##### [CRUNCH]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
facA <- mSetObj$analSet$svmReg$res$response
print("svm.pred: response var set")
#  if (facA == "NULL") { 
#    if("res" %in% names(mSetObj$analSet$svmReg) ){ #if there is a results made already, take that response
#        facA <- mSetObj$analSet$svmReg$res$response
#     } else {
#    num.data <- dplyr::select_if(input, is.numeric)
#    facA <- colnames(num.data)[1] 
#    for (i in seq_along(colnames(input)) ) {
#      if (is.factor(input[,i]) == FALSE || is.character(input[,i])==FALSE) {
#        facA <- colnames(input)[i]# Default is to choose the 1st numeric column as response column
#        break
#      }
#    }
# }
#} else {
#    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu
#  }
#
  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
   data <- input[ , colnames(input) != facA, drop = FALSE] 
# predtext <- mSetObj$analSet$svmReg$res$predtext
predictors2 <- mSetObj$analSet$svmReg$res$predictors
print("svm.pred: predictor vars set")
#  if (predtext == "") {
#    if("res" %in% names(mSetObj$analSet$svmReg) ){#if there are results made already, use the predictor(s)
#        predtext <- mSetObj$analSet$svmReg$res$predtext
#     } else {
#    predtext <- colnames(data)[1] #Default is the 1st potential predictor column
#    # predtext <- paste0(predtext, ",")
# } 
#   } else {
#    predtext <- predtext #taken from text box by java, fed as string into R code
#  }
  
#CHECK PREDTEXT FOR COMMAS  
#  if( !any(grepl(",", predtext, fixed = TRUE)) ){ # if there are no commas in input predictor name(s)
#    if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ # can't be >1 other cols to use, so if there is, error
#warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
#    } }
#
#  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
#  predtext <- gsub("\n", "", predtext, fixed = TRUE)
#  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
#  predtext <- gsub(";", "+", predtext, fixed = TRUE)
#  predtext <- gsub(" ", "", predtext, fixed = TRUE)
#  predtext <- gsub(":", "+", predtext, fixed = TRUE)
#  predtext <- gsub("*", "+", predtext, fixed = TRUE)
#   
  #GENERATE FORMULA #
# form <- as.formula(paste(facA, "~", predtext))
#   ### CHECK: are all input predictor names in data
#  predictors2 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
#  #predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
# 


#   data %>% assertr::verify(assertr::has_all_names(predictors2), error_fun = justwarn)

#  if(!all(predictors2 %in% colnames(data)) ){
#   warning(paste0("THIS_is_2nd_function_", "'", predictors2[!predictors2 %in% colnames(data)],
#  "' not found in data variables ('",
#  paste(colnames(data), collapse = "', '"),
#  "'): check spelling of text box input."))
# }
#
#
  #SUBSET DATA USING PREDICTOR COLUMN NAMES
# pred_data <- mSetObj$analSet$svmReg$res$pred.data
#   pred_data <- input[,which(colnames(input) %in% predictors2), drop = FALSE]
# model_data <- data.frame(input[,facA], pred_data)
#  colnames(model_data) <- c(paste0(facA), predictors2)
#  
  #GENERATE TEST AND TRAIN
#  set.seed(37) #Ensures same selection of data each time
#  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)
#  train_data <- model_data[index,,drop=FALSE] #70% of dataset
#  test_data <- model_data[-index,,drop=FALSE] #30% of dataset
#  # PREDICTOR
#  predictors_train <- model.matrix(train_data[,1] ~ ., train_data)[,-1] # Train predictor variables, creating dummy variables for categorical variables
#  predictors_test <- model.matrix(test_data[,1] ~ ., test_data)[,-1] # Test predictor variables, creating dummy variables for categorical variables
# RESPONSE
# response_train <- train_data[,facA, drop=TRUE] # response data for train dataset
# response_test <- test_data[,facA, drop=TRUE] # response data for test dataset
 #
#BUILD MODEL, get predicted
# print("PLOT: before model making")
#    mod <- e1071::tune(e1071::svm, form,  data = as.data.frame(predictors_train), ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5))))
#print("PLOT: after model making")
#    tunedModel <- mod$best.model
#    prediction <- predict(tunedModel, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial" making
#print("PLOT: after predicting")
#

# mod <- mSetObj$analSet$svmReg$mod$model
# tunedModel <- mod$best.model
test_data <- mSetObj$analSet$svmReg$res$test.data
prediction <- mSetObj$analSet$svmReg$res$test.prediction

test_rmse <- mSetObj$analSet$svmReg$res$test.RMSE

###### 
###### [CRUNCH DONE]

  dfpred <- data.frame(fpred = prediction, fA = test_data[,facA])
  formula2 <- as.formula("fA ~ fpred")
  model2 <- lm(formula = formula2, data = dfpred)
    
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.svmReg <- imgName
    
    
 # PLOT METRIC
if(plot_metric == "NULL"){
plot_metric1 <- ""
} else  if(plot_metric == "rmse"){
plot_metric1 <- paste0("RMSE = ", test_rmse)
} else {
plot_metric1 <- ""
}

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
  
  # PLOT TITLE
  if(plot_title == " "){ 
    #plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(form), ")")
    plot_title1 <- paste0("Predicted vs Actual (Test Data)")
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
  
  line_slope <- function(x, y){
    sum((x - mean(x))*(y-mean(y))) /  sum((x - mean(x))^2)
  }

  ## MAKE PLOT  
   a0 <- ggplot(data =  dfpred, aes(x = fpred, y = fA)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = FALSE, 
                 color = col_line1, fullrange = TRUE) +#, formula = formula2) +
     geom_point(shape = 16, color = col_dots1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )

  ## if slope is negative, line goes top L to bottom R
  ##### want annotation to go top R 
  ## if slope is positive, line goes bottom L to top R
  #### want annotation to go top L 
   
## https://stackoverflow.com/questions/22488563/ggplot2-annotate-layer-position-in-r#22492191
   if(line_slope(dfpred$fpred, dfpred$fA) > 0){
   a0 <- a0 +
      annotate("text",x=min(dfpred$fpred),y=max(dfpred$fA), hjust=.2, label=plot_metric1)
   } else {
    a0 <- a0 +   annotate("text",x=max(dfpred$fpred),y=max(dfpred$fA), hjust=.2, label=plot_metric1)
   }
  print("svm.pred: PLOT: made plot")

  #GENERATE PLOT
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    print(a0)
    # plot(prediction, test_data[,1], xlab="Predicted", ylab="Actual", main=method, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
    dev.off()
      
   # STORE IN mSET
  mSetObj$analSet$svmReg$plotpred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  
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
