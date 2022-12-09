
#' Perform ANN Regression'
#' @description Use ANN for regression analysis
#' #@usage ann.reg.anal(mSetObj=NA, facA="NULL", predtext="NULL")
#' # @param mSetObj Input the name of the created mSetObj
#'  # @param facA Input the name of the response column (java uses numeric.columns() to give user options)
#'  # @param predtext Input predictor column names (java uses text box to obtain string)
#'  @param vars_notoscale Numeric variable names to not scale; all numeric variables are default scaled from 0 to 1
#'  # @author Gina Sykes\email{gsykes@ualberta.ca}
#'  # University of Alberta, Canada
#'  # License: GNU GPL (>= 2)
#'  # @export
# 
 ann.reg.anal <- function(mSetObj=NA,
                          facA="NULL",
                          predtext="",             #,data="false"
#                          resp_centrescale = "false", #subtract mean, divide sd
#                          resp_range01 = "false", # range [0-1]
#                          pred_centrescale = "false", # subtract mean, divide sd
#                          pred_range01 = "false", # range [0-1],
                          vars_nottoscale = "" # text box numeric variables
 ) {
   
 
   #install.packages(c("caret", "Metrics"))
   library("dplyr")
   library("nnet")
   library("caret")
#   library("assertr")
   library("datawizard")
   library("Metrics")

   mSetObj <- .get.mSet(mSetObj)

   ### SET DATA (whether to use original data or not)
   #  if (data == "false") {
   # input <- mSetObj$dataSet$norm #default use norm
   #  } else {
       input <- mSetObj$dataSet$orig
   #  }

   # input <- input[order(as.numeric(rownames(input)),,drop=FALSE),]
   print("ann.anal: data input set")
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

   #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default predtext is second column.
   # cat("Indicate independent variables using the column names with commas in between.")

   #Set right side of formula with predictor variables
 data <- input[,colnames(input) != facA, drop = FALSE]
   if (predtext == "") {
     #resp.col.num <- which(colnames(input) == facA); data <- input[,-resp.col.num]
     predtext <- colnames(data)[1] #Default is the 1st potential predictor column
   } else {
     predtext <- predtext #taken from text box by java, fed as string into R code
   }

   predtext1 <- predtext

   #PREDICTORS: Curate formula right side, and extract predictors character vector
   predtext <- gsub("\n", "", predtext, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
   predtext <- gsub(",", "+", predtext, fixed=TRUE)
   predtext <- gsub(";", "+", predtext, fixed=TRUE)
   predtext <- gsub(" ", "", predtext, fixed=TRUE)
   predtext <- gsub(":", "+", predtext, fixed=TRUE)
   predtext <- gsub("*", "+", predtext, fixed=TRUE)

   #GENERATE FORMULA
   form <- as.formula(paste(facA, "~", predtext))
   #Text should be visible to user
   cat(paste0("You have created this formula for model building: ", facA, " ~ ", predtext))
   #cat("The L hand side is the dependent variable. The R hand side is the independent variable(s). If there is >1 independent variable, plus signs indicate the variables are evaluated on their own; colons indicate an interaction between the variables is evaluated.")
   #cat("If the formula is not what you intended, retype independent variable(s) in the text box and/or choose another dependent variable.")

   ### CHECK: are all input predictor names in data
   predictors2 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
   #predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)

   data %>% assertr::verify(assertr::has_all_names(predictors2), error_fun = justwarn)
  
    if(!all(predictors2 %in% colnames(data)) ){
      warning(paste0("THIS_is_1st_function_", "'", 
                    paste( setdiff(predictors2, colnames(data)), collapse = "', '" ),
                     # predictors2[!predictors2 %in% colnames(data)],
                     "' not found in data variables ('",
                     paste(colnames(data), collapse = "', '"),
                     "'): check spelling of text box input."))
    }

   pred_data <- input[,which(colnames(input) %in% predictors2), drop = FALSE]
   model_data <- data.frame(input[,facA, drop = TRUE], pred_data)
   colnames(model_data) <- c(paste0(facA), predictors2)
   #GENERATE TEST AND TRAIN
   set.seed(37) #Ensures same selection of data each time
   index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)
   train_data <- model_data[index,, drop = FALSE] #70% of dataset
   test_data <- model_data[-index,,drop = FALSE] #30% of dataset
   
   pred.num.df <- dplyr::select_if(pred_data, is.numeric)
   pred.num <- colnames(pred.num.df)

   ###  RESCALING NUMERIC PREDICTOR VARIABLES
   rescale01 <- function(x) { # https://dplyr.tidyverse.org/articles/colwise.html
     rg <- range(x, na.rm = TRUE)  # scale01 <- function(x){  (x - min(x)) / (max(x) - min(x)) }
     (x - rg[1]) / (rg[2] - rg[1])
  }
   
    # Rescale numeric variables
  if (vars_nottoscale == "") { ## default to rescale all predictor numeric variables
   
      test_data <- test_data %>% datawizard::standardise(., select = pred.num)
      train_data <- train_data %>% datawizard::standardise(., select = pred.num)
    vars_noscale <- ""

     } else  {
    
   #RESCALING: Vector of variables not to rescale
   restext <- gsub("\n", "", vars_nottoscale, fixed=TRUE) #fixed=TRUE means deals with one string, vs vec of strings
   restext <- gsub(",", "+", restext, fixed=TRUE)
   restext <- gsub(";", "+", restext, fixed=TRUE)
   restext <- gsub(" ", "", restext, fixed=TRUE)
   restext <- gsub(":", "+", restext, fixed=TRUE)
   restext <- gsub("*", "+", restext, fixed=TRUE)
   vars_noscale <- unlist(strsplit(restext, "+", fixed = TRUE), use.names = FALSE)
  ### CHECK: are all input predictor names in data
 
   pred.num.df %>% assertr::verify(assertr::has_all_names(vars_noscale), error_fun = justwarn)
  
     #  if(!all(vars_noscale %in%  pred.num ) ){
     # stop(paste0("ann.anal:", "'", vars_noscale[!vars_noscale %in% pred.num],
     #                "' not found in numeric predictor variables ('",
     #                paste( pred.num, collapse = "', '"),
     #                "'): check spelling of text box input."))
      # } else {
        vars_rescale <- pred.num[!pred.num %in% vars_noscale]
      # }
      test_data <- test_data %>% datawizard::standardise(., select = vars_rescale)
      train_data <- train_data %>% datawizard::standardise(., select = vars_rescale)
     }

### PREDICTOR RESCALING
    # if(pred_centrescale == "false"){ # no z-score
    #       if(pred_range01 == "false"){ # no range scaling
    #        test_data <- test_data
    #        train_data <- train_data
    #       } else {  # range scaling
    #  test_data <- test_data %>% dplyr::mutate(across(dplyr::all_of(vars_rescale), rescale01))
    #  train_data <- train_data %>%  dplyr::mutate(across(dplyr::all_of(vars_rescale), rescale01))
    #       }
    #     } else {  # z-score scaling
    #       if(pred_range01 == "false"){ # no range scaling
    #    test_data <- test_data %>% datawizard::standardise(., select = vars_rescale)
    #    train_data <- train_data %>% datawizard::standardise(., select = vars_rescale) 
    #       } else {  # range scaling
    #     test_data <- test_data %>% 
    #    datawizard::standardise(., select = vars_rescale)%>% 
    #    dplyr::mutate(across(dplyr::all_of(vars_rescale), rescale01)) 
    #  train_data <- train_data %>% 
    #    datawizard::standardise(., select = vars_rescale)%>% 
    #    dplyr::mutate(across(dplyr::all_of(vars_rescale), rescale01)) 
    #       }
    #   }
   #### RESPONSE RESCALING
        # if(resp_centrescale == "false"){ # no z-score
        #   if(resp_range01 == "false"){ # no range scaling
        #     response_train <- response_train1
        #     response_test <- response_test1
        #   } else {  # range scaling
        #     response_train <- rescale01(response_train1)
        #     response_test <- rescale01(response_test1)
        #   }
        # } else{ # zscore scaling
        #    if(resp_range01 == "false"){ # no range scaling
        #     response_train <- scale(response_train1)
        #     response_test <- scale(response_test1)
        #   } else {  # range scaling
        #     response_train <- rescale01( scale(response_train1) )
        #     response_test <- rescale01( scale(response_test1) )
        #   }
        # }
 
  response_train <- rescale01(train_data[,facA, drop=TRUE])
  response_test <- rescale01(test_data[,facA, drop=TRUE])
   train_data[,facA] <- response_train
   test_data[,facA] <- response_test

   predictors_train <- model.matrix(train_data[,1] ~ ., train_data)[,-1] # Train predictor variables, creating dummy variables for categorical variables
   predictors_test <- model.matrix(test_data[,1] ~ ., test_data)[,-1] # Test predictor variables, creating dummy variables for categorical variables

   # cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user
   
   #### https://stats.stackexchange.com/questions/21717/how-to-train-and-validate-a-neural-network-model-in-r
  ###### https://rviews.rstudio.com/2020/07/20/shallow-neural-net-from-scratch-using-r-part-1/

   print("ann.anal: before building model")
   #BUILD MODEL, PREDICT
    set.seed(37)    #### include 'linout = 1' so it is regression
   mod  <- caret::train(predictors_train, response_train, # formula, data = train_data, 
     method = "nnet", trControl = caret::trainControl(method = "cv", number = 10),
    # preProcess = c("range"),# preProcess("center", "scale"),
     maxit = 1000, tuneGrid=expand.grid(size=c(3:10), decay=c(0.5, 0.1)), trace = F, linout = 1, returnData = TRUE) 
    tunedModel <- mod$finalModel
   model_name <- "ANN Regression"
   print("ann.anal: after building model")
   prediction <- predict(tunedModel, newdata = as.matrix(predictors_test))
   print("ann.anal: after predicting model 1")

   # STORE SOME MODEL RESULTS
   #mSetObj$analSet$annReg$meth <- model_name
   # mSetObj$analSet$annReg$pred <- prediction

   #GENERATE AND DOWNLOAD SUMMARY OF PARAMETER TESTING, WRITE TO TXT DOC
   summ <- summary(tunedModel)
   resid <- residuals(tunedModel)
   fitt <- predict(tunedModel)
   print("after predicting model train")
   train_rmse <- Metrics::rmse(response_train, fitt)
   fileName <- "ANN_regression_summary.txt"

   #Obtain test RMSE for plotting # not in the original not ml.R file function
   test_prediction <- predict(tunedModel, newdata=test_data)
   test_rmse <- Metrics::rmse(response_test, test_prediction)
   print("ann.anal: after predicting model test")

   #STORE REMAINING RESULTS
   mSetObj$analSet$annReg$res <- list(summary = summ, response = facA, predictor = predictors2, predtext = predtext1, pred.nottoscale = vars_noscale, pred.data = pred_data, predicted.values = fitt, train.RMSE=train_rmse, test.prediction = test_prediction, test.RMSE = test_rmse, train.data = train_data, test.data = test_data, method = model_name, fileName = fileName)
   mSetObj$analSet$annReg$mod <- list(model.name=model_name, model=mod, final.model = tunedModel, formula = form, response = facA, predictor = predictors2)



   #DOWNLOAD TEXT DOC: containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
   sink(fileName)
   cat("Formula:\n")
   print(form)
   # cat("\nReference category:\n")
   # cat(paste0(reference))
   print(summ)
   cat("Residuals:\n")
   print(resid)
   cat("\nPredicted values:\n")
   print(fitt)
   cat("\nModel RMSE:\n")
   #cat(paste0(train_RMSE))
   sink()

   return(.set.mSet(mSetObj))

}



#'Plot artificial neural network predicted vs actual data plot using test data
#'@description Scatter plot where actual data is y and predicted data is x
#'@usage ann.pred.plot(mSetObj, facA="NULL", predtext="", data="false", col_dots="NULL", col_line="NULL", plot_ci="NULL", plot_title=" ", plot_ylab = " ", plot_xlab = " ", imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param facA Input the name of the response column 
#'@param predtext Input predictor column names (java uses text box to obtain string)
#'@param vars_notoscale Numeric variable names to not scale; all numeric variables are default scaled from 0 to 1
#'@param plot_text_size Size of text on plot; extra small - extra large (defaults to "medium", which is 12)
#'@param col_dots point color
#'@param col_line line color
#'@param plot_title Input the name of the title (default: "ANN Regression: Predicted vs Actual"), textbox
#'@param plot_ylab Input the name of the y axis label, textbox
#'@param plot_xlab Input the name of the x axis label, textbox
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

ann.pred.plot <- function(mSetObj=NA, 
facA = "NULL", 
predtext ="",
# resp_centrescale = "false", #subtract mean, divide sd
# resp_range01 = "false", # range [0-1]
# pred_centrescale = "false", # subtract mean, divide sd
# pred_range01 = "false", # range [0-1],
vars_nottoscale = "", # text box numeric variables
  col_dots="NULL",
  col_line="NULL", 

#  plot_metric = "NULL", 
  plot_text_size = "NULL", # added 202212-08

  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
imgName, format="png", dpi=72, width=NA){

library("nnet")
library("caret")
# library("assertr")
library("ggplot2")
library("RJSONIO")

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
### SET DATA (whether to use original data or not)
#  if (data == "false") {
#    input <- mSetObj$dataSet$norm #default use norm
#  } else {
    input <- mSetObj$dataSet$orig
#  }


##### WITH facA and predtext options
  ##### [COLLAPSE]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
#  if (facA == "NULL") { 
#    if("res" %in% names(mSetObj$analSet$annReg) ){ #if there is a results made already, take that response
#        facA <- mSetObj$analSet$annReg$res$response
#     } else {
#    for (i in seq_along(colnames(input)) ) {
#      if (is.factor(input[,i]) == FALSE || is.character(input[,i]) == FALSE) {
#        facA <- colnames(input)[i]# Default is to choose the 1st numeric column as response column
#        break
#      }
#    }
# }
#} else {
#    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down menu
#  }
  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
#  if (predtext == "") {
#    if("res" %in% names(mSetObj$analSet$annReg) ){#if there is a results made already, take that predictor
#        predtext <- mSetObj$analSet$annReg$res$predtext
#     } else {
    data <- input[ , colnames(input) != facA, drop=FALSE] #means it will be a df
#    predtext <- colnames(data)[1] # paste(colnames(data), collapse = ",") #Default is 1st predictor col 
# } 
#    } else {
#    predtext <- predtext #taken from text box by java, fed as string into R code
#  }
# if(!any(grepl("\\,", predtext, fixed = TRUE)) ){ # if there are no commas in input predictor name(s)
#  if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ # can't be >1 other cols to use, so if there is, error
#warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
#} }
#
  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
#  predtext <- gsub("\n", "", predtext, fixed = TRUE)
#  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
#  predtext <- gsub(";", "+", predtext, fixed = TRUE)
#  predtext <- gsub(" ", "", predtext, fixed = TRUE)
#  predtext <- gsub(":", "+", predtext, fixed = TRUE)
#  predtext <- gsub("*", "+", predtext, fixed = TRUE)
#
#  #GENERATE FORMULA #form <- as.formula(paste(facA, "~", predtext))
#   ### CHECK: are all input predictor names in data
#  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
#  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
#  data %>% assertr::verify(assertr::has_all_names(predictors2), error_fun = justwarn)
#
# print("ann.plotpred: checked  predictors")  
#  #SUBSET DATA USING PREDICTOR COLUMN NAMES
#  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2, drop=FALSE])
#   pred.num.df <- dplyr::select_if(pred_data, is.numeric)
#   pred.num <- colnames(pred.num.df)
#  model_data <- data.frame(input[,facA], pred_data)
#  colnames(model_data) <- c(paste0(facA), predictors2)
#  
#  #GENERATE TEST AND TRAIN
#  set.seed(37) #Ensures same selection of data each time
#  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)
#  train_data <- model_data[index,,drop = FALSE] #70% of dataset
#  test_data <- model_data[-index,,drop = FALSE] #30% of dataset
#  response_train <- rescale01( train_data[,facA, drop=TRUE] ) # response data for train dataset
#  response_test <- rescale01( test_data[,facA, drop=TRUE] ) # response data for test dataset
#
#   ###  RESCALING NUMERIC PREDICTOR VARIABLES
#   rescale01 <- function(x) { # https://dplyr.tidyverse.org/articles/colwise.html
#     rg <- range(x, na.rm = TRUE)  # scale01 <- function(x){  (x - min(x)) / (max(x) - min(x)) }
#     (x - rg[1]) / (rg[2] - rg[1])
#  }
#   
#  vars_noscale <- mSetObj$analSet$annReg$res$pred.noscale
#
# # Rescale PREDICTOR numeric variables
#  if (vars_nottoscale == "") { ## default to rescale all predictor numeric variables
#     test_data <- test_data %>% datawizard::standardise()
#     train_data <- train_data %>% datawizard::standardise()
#     } else  {
#   #RESCALING: Vector of variables not to rescale
#  ### CHECK: are all input predictor names in data
#   pred.num.df %>% assertr::verify(assertr::has_all_names(vars_noscale), error_fun = justwarn)
#   vars_rescale <- pred.num[!pred.num %in% vars_noscale]
#      test_data <- test_data %>% datawizard::standardise(., select = vars_rescale)
#      train_data <- train_data %>% datawizard::standardise(., select = vars_rescale)
#     }
#
#   train_data[,facA] <- response_train
#   test_data[,facA] <- response_test
#
#   predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-c(1,2), drop = FALSE]#[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors # [,-c(1,2), drop = FALSE] removes intercept column of all 1's
#   predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-c(1,2), drop = FALSE] # Predictor variables in train dataset, creating dummy vars for categorical predictors
#
#   #cat("The train data for model building is 70% of the dataset, while the test data for model testing is# 30% of the dataset.") #Text will be visible to user.
# 
#   print("ann.plotpred: set traintest data")  
#
#   print("ann.plotpred: before building model")
#    set.seed(37) 
#   mod  <- caret::train(predictors_train, response_train, # formula = form, data = train_data, 
#     method = "nnet", trControl = caret::trainControl(method = "cv", number = 10),
#    # preProcess = c("range"),# preProcess("center", "scale"),
#     maxit = 1000, tuneGrid=expand.grid(size=c(3:10), decay=c(0.5, 0.1)),
#    trace = F, linout = 1, returnData = TRUE) 
#    tunedModel <- mod$finalModel
#    model_name <- "ANN Regression"
#   print("ann.plotpred: after building model")
#   prediction <- predict(tunedModel, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
#   print("ann.plotpred: after predicting model 1")
###### 
###### [COLLAPSE DONE]

### GET FACA AND PREDTEXT
 facA <- mSetObj$analSet$annReg$res$response
 predictors2 <- mSetObj$analSet$annReg$res$predictor
 test_prediction <- mSetObj$analSet$annReg$res$test.prediction
 test_data <- mSetObj$analSet$annReg$res$test.data
 # train_prediction <- mSetObj$analSet$annReg$res$predicted.values
 tunedModel <- mSetObj$analSet$annReg$mod$final.model
print("ann.plotpred: set data")  

   dfpred <- data.frame(fpred = test_prediction, fA = test_data[,facA])
   formula2 <- as.formula("fA ~ fpred")
   model2 <- lm(formula = formula2, data = dfpred)
    
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.annReg <- imgName
    
 ### TROUBLESHOOTING:
  ##   col_dots1<-"blue"
  ##   col_line1<-"red"
  ##   plot_ci1<-TRUE
  ##   plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(form), ")")
  ##   plot_ylab1 <- "Actual"
  ##   plot_xlab1<- "Predicted"
  ##  plot_label_size1 <- 3.88*0.9
  ##  plot_text_size1 <-  12*0.9  

# PLOT TEXT SIZE
# plot_base_size <- theme_bw()$text$size # 11
 plot_label_size <- 3.88 #GeomLabel$default_aes$size #3.88
 plot_base_size <- 12
    #SET TEXT SIZE
  plot_text_size1 <- 
				switch(
					plot_text_size,
					"NULL" = plot_base_size,
					"medium" = plot_base_size,
					"small" = 0.8*plot_base_size,
					"extrasmall" = 0.6*plot_base_size,
                                        "large" = 1.2*plot_base_size,
					"extralarge" = 1.4*plot_base_size,
					NULL
				)
 plot_label_size1 <- 
				switch(
					plot_text_size,
					"NULL" = plot_label_size,
					"medium" = plot_label_size,
					"small" = 0.9*plot_label_size,
					"extrasmall" = 0.8*plot_label_size,
                                        "large" = 1.1*plot_label_size,
					"extralarge" = 1.15*plot_label_size,
					NULL
				)

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
     

  ## MAKE PLOT  
   a0 <- ggplot(data =  dfpred, aes(x = fpred, y = fA)) +
    labs(title = plot_title1) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = FALSE, color = col_line1, fullrange = TRUE) +#, formula = formula2) +
     geom_point(shape = 16, color = col_dots1) +
      # theme_bw() +
     theme_bw(base_size = plot_text_size1) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size=plot_text_size1, colour = "black"), 
        axis.title = element_text(colour = "black"),
        # legend.title=element_text(plot_text_size1), legend.text=element_text(size=plot_text_size1), 
        plot.title = element_text(face = 'bold', hjust = 0.5)
  )
  
  #GENERATE PLOT
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    print(a0)
    dev.off()
      
   # STORE IN mSET
  mSetObj$analSet$annReg$plotpred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)

  
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
print("JSON bon jovi")
  sink(imgName2)
  cat(json.obj)
  sink()
  
   if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }
  
  
}
    

#### NEURAL INTERPRETATION DIAGRAM
##  thickness of the lines joining neurons is proportional to the magnitude of the connection weight
## line shade indicates the direction of the interaction between neurons: 
## black connections are positive (excitator) and gray connections are negative (inhibitor)


#'Plot neural network interpretation diagram linking the inputs (predictors), neurons, layers, and output
#'@description Neural network diagram depicting layers connecting the neurons to the inputs and output of the model 
#'@usage ann.nid.plot(mSetObj, facA="NULL", predtext="", vars_nottoscale="", col_input="NULL", col_other="NULL", text_size = "NULL", squish = "NULL", plot_title=" ", imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param facA Input the name of the response column. Response column will be normalized by min-max scaling so that the range of facA column has a range [0-1]
#'@param predtext Input predictor column names plus potential interactions between predictor variables (java uses text box to obtain string)
#'@param vars_nottoscale Input predictor column names not to scale (by z-score standardizing) (java uses text box to obtain string)
#'@param col_input  Colours of input (left-most) node bubbles; default lightpink, static dropdown 
#'@param col_other  Colours of non-input node bubbles; default lightblue, static dropdown 
#'@param text_size  Text size of node bubble labels; static dropdown   
#'@param squish Amount to compress the plot width-wise; default no compressing; 1 least to 4 most compressing; static dropdown
#'@param plot_title Input the name of the title (default: "ANN Regression");, textbox)
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

ann.nid.plot <- function(mSetObj=NA, 
facA = "NULL", 
predtext ="",
# resp_centrescale = "false", #subtract mean, divide sd
# resp_range01 = "false", # range [0-1]
# pred_centrescale = "false", # subtract mean, divide sd
# pred_range01 = "false", # range [0-1],
vars_nottoscale = "" ,# text box numeric variables
  col_input = "NULL",
  col_other = "NULL",
  text_size = "NULL",
  squish = "NULL",
  plot_title=" ",
imgName, format="png", dpi=72, width=NA){

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
### SET DATA (whether to use original data or not)
#  if (data == "false") {
#    input <- mSetObj$dataSet$norm #default use norm
#  } else {
    input <- mSetObj$dataSet$orig
#  }


##### WITH facA and predtext options
  ##### [CRUNCH]
  ##### 
  ##### iF VARIABLES ARE SET
  #SET RESPONSE (DEPENDENT) VARIABLE
#  if (facA == "NULL") { 
#    if("res" %in% names(mSetObj$analSet$annReg) ){ #if there is a results made already, take that response
#        facA <- mSetObj$analSet$annReg$res$response
#     } else {
#    for (i in seq_along(colnames(input)) ) {
#      if (is.factor(input[,i]) == FALSE || is.character(input[,i]) == FALSE) {
#        facA <- colnames(input)[i]# Default is to choose the 1st numeric column as response column
#        break
#      }
#    }
# }
#} else {
#    facA <- facA #User selected, java uses function numeric.columns() to provide options in drop down
#
  #SET FORMULA RIGHT SIDE WITH PREDICTORS (Default = 2nd column)
#  if (predtext == "") {
#    if("res" %in% names(mSetObj$analSet$annReg) ){#if there is a results made already, take that predictor
#        predtext <- mSetObj$analSet$annReg$res$predtext
#     } else {
    data <- input[ , colnames(input) != facA, drop=FALSE] #means it will be a df
#    predtext <- colnames(data)[1] # paste(colnames(data), collapse = ",") #Default is 1st predictor col 
# } 
#    } else {
#    predtext <- predtext #taken from text box by java, fed as string into R code
#  }
#
# if(!any(grepl("\\,", predtext, fixed = TRUE)) ){ # if there are no commas in input predictor name(s)
#  if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ # can't be >1 other cols to use, so if there is, error
#warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
#} }
#
#  #CURATE FORUMLA RIGHT SIDE, EXTRACT CHAR VEC OF PREDICTORS 
#  predtext <- gsub("\n", "", predtext, fixed = TRUE)
#  predtext <- gsub(",", "+", predtext, fixed = TRUE) 
#  predtext <- gsub(";", "+", predtext, fixed = TRUE)
#  predtext <- gsub(" ", "", predtext, fixed = TRUE)
#  predtext <- gsub(":", "+", predtext, fixed = TRUE)
#  predtext <- gsub("*", "+", predtext, fixed = TRUE)
#
#  #GENERATE FORMULA #formula <- as.formula(paste(facA, "~", predtext))
#   ### CHECK: are all input predictor names in data
#  predictors1 <- unlist(strsplit(predtext, "+", fixed = TRUE), use.names = FALSE)
#  predictors2 <- unlist(strsplit(predictors1, ":", fixed = TRUE), use.names = FALSE)
#  data %>% assertr::verify(assertr::has_all_names(predictors2), error_fun = justwarn)
#
#  print("ann.plotnid: checked  predictors")  
#  #SUBSET DATA USING PREDICTOR COLUMN NAMES
#  pred_data <- as.data.frame(input[ ,colnames(input) %in% predictors2, drop=FALSE])
#  pred.num.df <- dplyr::select_if(pred_data, is.numeric)
#  pred.num <- colnames(pred.num.df)
#  model_data <- data.frame(input[,facA], pred_data)
#  colnames(model_data) <- c(paste0(facA), predictors2)
#  
#  #GENERATE TEST AND TRAIN
#  set.seed(37) #Ensures same selection of data each time
#  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset (this will be for train)
#  train_data <- model_data[index,,drop = FALSE] #70% of dataset
#  test_data <- model_data[-index,,drop = FALSE] #30% of dataset
#  response_train <- rescale01( train_data[,facA, drop=TRUE] ) # response data for train dataset
#  response_test <- rescale01( test_data[,facA, drop=TRUE] ) # response data for test dataset
#
#   ###  RESCALING NUMERIC PREDICTOR VARIABLES
#   rescale01 <- function(x) { # https://dplyr.tidyverse.org/articles/colwise.html
#     rg <- range(x, na.rm = TRUE)  # scale01 <- function(x){  (x - min(x)) / (max(x) - min(x)) }
#     (x - rg[1]) / (rg[2] - rg[1])
#  }
#   
#  vars_noscale <- mSetObj$analSet$annReg$res$pred.noscale
#
# # Rescale PREDICTOR numeric variables
#  if (vars_nottoscale == "") { ## default to rescale all predictor numeric variables
#     test_data <- test_data %>% datawizard::standardise()
#     train_data <- train_data %>% datawizard::standardise()
#     } else  {
#   #RESCALING: Vector of variables not to rescale
#  ### CHECK: are all input predictor names in data
#   pred.num.df %>% assertr::verify(assertr::has_all_names(vars_noscale), error_fun = justwarn)
#   vars_rescale <- pred.num[!pred.num %in% vars_noscale]
#      test_data <- test_data %>% datawizard::standardise(., select = vars_rescale)
#      train_data <- train_data %>% datawizard::standardise(., select = vars_rescale)
#     }
#
#   train_data[,facA] <- response_train
#   test_data[,facA] <- response_test
#
#   predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-c(1,2), drop = FALSE]#[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors # [,-c(1,2), drop = FALSE] removes intercept column of all 1's
#   predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-c(1,2), drop = FALSE] # Predictor variables in train dataset, creating dummy vars for categorical predictors
#
#   #cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
# 
#   print("ann.plotnid: set traintest data")  
#
#   print("ann.plotnid: before building model")
#    set.seed(37) 
#   mod  <- caret::train(predictors_train, response_train, # formula = form, data = train_data, 
#     method = "nnet", trControl = caret::trainControl(method = "cv", number = 10),
#    # preProcess = c("range"),# preProcess("center", "scale"),
#     maxit = 1000, tuneGrid=expand.grid(size=c(3:10), decay=c(0.5, 0.1)),
#    trace = F, linout = 1, returnData = TRUE) 
#    tunedModel <- mod$final.Model
#    model_name <- "ANN Regression"
#   print("ann.plotpred: after building model")
#   prediction <- predict(tunedModel, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
#   print("ann.plotnid: after predicting model 1")
#
###### 
###### [CRUNCH DONE]

### GET FACA AND PREDTEXT
  facA <- mSetObj$analSet$annReg$res$response
  predictor <- mSetObj$analSet$annReg$res$predictor 
  #mod <- mSetObj$analSet$annReg$mod$model
  tunedModel <- mSetObj$analSet$annReg$mod$final.model
 print("ann.nid: tuned model:")
 print(summary(tunedModel))
print("ann.plotpred: set data")  


  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.nid.annReg <- imgName
    
 ### TROUBLESHOOTING:
    # squish1 <- "1"
    # text_size1 <- "0.4" # cex_val
    # col_input1<-"lightpink"
    # col_other1<-"lightblue" # bord_col
  ##  plot_title1 <- "ANN Regression"
  ## "blue" #0000FF
    
    #SET INPUT COLOR
  col_input1 <- 
				switch(
					col_input,
					"NULL" = "lightpink",      #FFB6C1
#					"lightpink" = "lightpink", #FFB6C1
					"lightblue" = "lightblue", #ADD8E6
				         "orchid" = "orchid",      #DA70D6
					"palegreen" = "palegreen", ##98FB98
					"grey" = "grey",
					NULL
				)
  
  #SET OTHER COLOR
   col_other1 <- 
				switch(
					col_other,
					"NULL" = "lightblue",      #ADD8E6
#					"lightblue" = "lightblue", #ADD8E6
					"lightpink" = "lightpink", #FFB6C1
  				         "orchid" = "orchid",	   #DA70D6
					"palegreen" = "palegreen", ##98FB98
					"grey" = "grey",
					NULL
				)
  
 text_size1 <-  # cex_val
				switch(
					text_size,
					"NULL" = "1",
					"1" = "0.2",
					"2" = "0.4",
					"3" = "0.6",
					"4" = "0.8",
					"5" = "1",
					"6" = "1.25",
					"7" = "1.5",
					"8" = "2",
					NULL
				)
 
 squish1 <- # pad_x = 1
				switch(
					squish,
					"NULL" = "1",
					"1" = "0.8",
					"2" = "0.6",
					"3" = "0.4",
					"4" = "0.2",
					NULL
				) 
  
  # x_names 
  # PLOT TITLE
  if(plot_title == " "){ 
    plot_title1 <- "ANN Regression"
  } else {
    plot_title1 <- plot_title
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

   # cex_val	numeric value indicating size of text labels, default 1
   # circle_col	chr string indicating color of nodes, default 'lightblue' or 2 element list with 1st element indicating color of input nodes and 2nd indicating color of remaining nodes
 
   ## MAKE PLOT
 #a0 <- ggplotify::as.ggplot(~
#NeuralNetTools::plotnet(tunedModel, 
#                         cex_val = as.numeric(text_size1),
#                         circle_col = list(col_input1, col_other1),
#                         pad_x = as.numeric(squish1)) #+ labs(title = plot_title1) 
  #)

  #GENERATE PLOT
    Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
    NeuralNetTools::plotnet(tunedModel, 
                         cex_val = as.numeric(text_size1),
                         circle_col = list(col_input1, col_other1),
                         pad_x = as.numeric(squish1))
    dev.off()
      
   # STORE IN mSET
  #mSetObj$analSet$annReg$plotnid <- list(plot = a0, title = plot_title1, xlab = "", ylab = "")
  

#  #JSON OBJECT MAKING

## create edge/node data frame
nn_coef <- nnet:::coef.nnet(tunedModel)
nn_df <- data.frame(do.call(rbind,
        strsplit(
        toupper(names(nn_coef)),
         "->" )
          ))
colnames(nn_df)<-c("n1", "n2")
nn_df$wt<-unname(nn_coef)

### Differentiate bias nodes
if("B" %in% nn_df$n1){
   nn_df$n1[nn_df$n1 %in% "B" & nn_df$n2 %in% "O"] <- "B1"
   nn_df$n1[nn_df$n1 %in% "B" & !nn_df$n2 %in% "O"] <- "B2"
}

### IDs of nodes
nn_id <- unique(c(nn_df$n1, nn_df$n2))
nn_names <- nn_id
### names of nodes
nn_names[grepl("I", nn_id )] <- tunedModel$coefnames
nn_names[grepl("O", nn_id)] <- "outcome"
### colours of nodes
nn_col <- rep(col_other1, length(nn_id))
nn_col[grepl("I", nn_id )] <- col_input1

nnet_plot_json <- list(
  main = plot_title1,
  nodes = 
    list(ID = nn_id, 
         name = nn_names, 
         text_magnification_cex = text_size1,
         node_layout_squishiness = squish1,
         cols = nn_col),
  edges = 
    list(source = nn_df$n1,
         target=nn_df$n2,
         weight_line_thickness = nn_df$wt,
         cols = ifelse(as.numeric(nn_df$wt) < 0, "grey", "black"))
)


   json.obj <- RJSONIO::toJSON(nnet_plot_json, .na='null')
  print(json.obj)
  print("JSON de replay")
   sink(imgName2)
   cat(json.obj)
   sink()
  
   if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }
  
}



#' 
#' #'Perform Machine Learning Regression'
#' #'@description Build a linear regression model for one user selected predictor variable
#' #'@usage reg.machine.anal(mSetObj=NA, method=method)
#' #'@param mSetObj Input the name of the created mSetObj
#' #'@param method Set ML regression method, default is random forest
#' #'@author Louisa Normington\email{normingt@ualberta.ca}
#' #'University of Alberta, Canada
#' #'License: GNU GPL (>= 2)
#' #'@export
#' 
#' ml.reg.anal <- function(mSetObj=NA,
#'                         method = "NULL",
#'                         data="false" 
#' ) {
#'   
#'   #install.packages(c("e1071", "randomForest"))
#'   library("e1071")
#'   library("randomForest")
#'   library("Metrics")
#'   
#'   mSetObj <- .get.mSet(mSetObj)
#'   
#'   ### SET DATA (whether to use original data or not)
#'   if (data == "false") {
#'     input <- mSetObj$dataSet$norm #default use norm
#'   } else {
#'     input <- mSetObj$dataSet$orig
#'   }
#'   
#'   #Text should be visable to user
#'   AddErrMsg("The first column will be the response variable. The remaining columns will be the predictor variables.")
#'   AddErrMsg("Response variable must be numeric for machine regression analysis. Predictor variables can be numeric or categorical.") 
#'   AddErrMsg("For categorical variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the level labels to I, II and III.")
#'   
#'   #Generate test and train data for model building
#'   set.seed(37)
#'   index <- sample(1:nrow(input), 0.7*nrow(input))
#'   train_data <- input[index,]
#'   test_data <- input[-index,]
#'   predictors_train <- model.matrix(train_data[,1] ~ ., train_data)[,-1] # Train predictor variables, creating dummy variables for categorical variables
#'   predictors_test <- model.matrix(test_data[,1] ~ ., test_data)[,-1] # Test predictor variables, creating dummy variables for categorical variables
#'   response_train_name <- colnames(input)[1] #response_train variable name
#'   predictors_train_name <- colnames(predictors_train)[-1] #response_train variable name
#'   #Text should be visable to user
#'   cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.")
#'   
#'   #Generate formula
#'   formula <- as.formula(paste(response_train_name, "~", paste(predictors_train_name, collapse = "+")))
#'   
#'   if (method == "SVM") {
#'     
#'     #Build model
#'     model <- e1071::tune(e1071::svm, formula,  data = as.data.frame(predictors_train), ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5))))
#'     tunedModel <- model$best.model
#'     model_name <- "SVM Regression"
#'     
#'     #Extract predicted values
#'     prediction <- predict(tunedModel, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
#'     
#'     #Store results for plotting
#'     mSetObj$analSet$svmReg$meth <- model_name
#'     mSetObj$analSet$svmReg$pred <- prediction
#'     mSetObj$analSet$svmReg$test <- test_data
#'     
#'     #Generate and download summary of parameter testing and write to txt document
#'     summary <- summary(tunedModel) 
#'     residuals <- residuals(tunedModel)
#'     decision_values <- tunedModel[["decision.values"]]
#'     fitted <- predict(tunedModel)
#'     svm_RMSE <- Metrics::rmse(predictors_train[,1], fitted)
#'     fileName <- "ML_regression_summary.txt"#"SVM_regression_summary.txt"
#'     
#'     #Store results
#'     mSetObj$analSet$svmReg$res <- list(summary = summary, predicted.values = fitted, residuals = residuals, decision.values = decision_values, RSME = svm_RMSE, fileName = fileName)       
#'     mSetObj$analSet$svmReg$mod <- list(model_name = model_name, model = model, response = response_train_name, predictor = predictors_train_name)
#'     
#'     #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
#'     sink(fileName) 
#'     cat("Formula:\n")
#'     print(formula)
#'     # cat("\nReference category:\n")
#'     # cat(paste0(reference))
#'     print(summary)
#'     cat("Residuals:\n")
#'     print(residuals)
#'     cat("\nDecision values:\n")
#'     print(decision_values)
#'     cat("\nPredicted values:\n")
#'     print(fitted)
#'     cat("\nRMSE:\n")
#'     cat(paste0(svm_RMSE))
#'     sink()
#'     
#'   } else { #Method is random forest
#'     
#'     #Build model
#'     model <- randomForest::tuneRF(y = train_data[,1], x = predictors_train[,-1], ntreeTry = 500, stepFactor = 2, improve = 0.05, trace = TRUE, doBest = TRUE, plot = FALSE, importance = TRUE)
#'     model_name <- "Random Forest Regression"
#'     
#'     #Extract predicted values
#'     prediction <- predict(model, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
#'     
#'     #Store results for plotting
#'     mSetObj$analSet$annReg$meth <- model_name
#'     mSetObj$analSet$annReg$pred <- prediction
#'     mSetObj$analSet$annReg$test <- test_data
#'     
#'     #Generate and download summary of parameter testing and write to txt document
#'     summary <- model 
#'     predictor_importance <- randomForest::importance(model)
#'     fitted <- predict(model)
#'     svm_RMSE <- Metrics::rmse(predictors_train[,1], fitted)
#'     fileName <- "ml_regression_summary.txt"#"random_forest_regression_summary.txt"
#'     
#'     #Store results
#'     mSetObj$analSet$annReg$res <- list(summary = summary, predicted.values = fitted, RSME = svm_RMSE, predictor.importance = predictor_importance, fileName = fileName)   
#'     mSetObj$analSet$annReg$mod <- list(model_name = model_name, model = model, response = response_train_name, predictor = predictors_train_name)
#'     
#'     #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
#'     sink(fileName) 
#'     cat("Formula:\n")
#'     print(formula)
#'     # cat("\nReference category:\n")
#'     # cat(paste0(reference))
#'     print(model)
#'     cat("\nPredicted values:\n")
#'     print(fitted)
#'     cat("\nRMSE:\n")
#'     cat(paste0(svm_RMSE, "\n"))
#'     cat("\nPredictor variable importance:\n")
#'     print(predictor_importance)
#'     sink()
#'     
#'   } 
#'   
#'   return(.set.mSet(mSetObj))
#'   
#' }  
#' 
#' #'Plot svm predicted vs actual data plot with line of best fit
#' #'@description Scatter plot with line of best fit, where response variable is y and predictor variable is x
#' #'@usage plot.pred.svmReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
#' #'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#' #'@param method Set ML regression method, default is random forest
#' #'@param imgName Input the image name
#' #'@param format Select the image format, "png" or "pdf", default is "png" 
#' #'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#' #'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#' #'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#' #'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
#' #'@author Louisa Normington\email{normingt@ualberta.ca}
#' #'University of Alberta, Canada
#' #'License: GNU GPL (>= 2)
#' #'@export
#' 
#' ml.pred.plot <- function(mSetObj=NA,
#'                          method="random forest",
#'                          facA = "NULL",
#'                          data="false",
#'                          
#'                          col_dots="NULL",
#'                          col_line="NULL", 
#'                          plot_ci="false",
#'                          plot_title=" ",
#'                          plot_ylab=" ",
#'                          plot_xlab=" ",
#'                          imgName, format="png", dpi=72, width=NA){
#'   
#'   ## used to be called: plot,pred.MLReg
#'   ## 
#'   #Extract necessary objects from mSetObj
#'   mSetObj <- .get.mSet(mSetObj)
#'   
#'   ### TROUBLESHOOTING:
#'   ##   col_dots1<-"blue"
#'   ##   col_line1<-"red"
#'   ##   plot_ci1<-TRUE
#'   ##   plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(formula), ")")
#'   ##   plot_ylab1 <- "Actual"
#'   ##   plot_xlab1<- "Predicted"
#'   
#'   
#'   #SET POINT COLOR
#'   col_dots1 <- 
#'     switch(
#'       col_dots,
#'       "NULL" = "black",
#'       "blue" = "blue",
#'       "red" = "red",
#'       "green" = "green",
#'       "grey" = "grey",
#'       NULL
#'     )
#'   #SET LINE COLOR
#'   col_line1 <- 
#'     switch(
#'       col_line,
#'       "NULL" = "black",
#'       "blue" = "blue",
#'       "red" = "red",
#'       "green" = "green",
#'       "grey" = "grey",
#'       NULL
#'     )
#'   
#'   #SET WHETHER TO ADD 95% CONF INT
#'   if (plot_ci == "false") {
#'     plot_ci1 <- FALSE # default
#'   } else {
#'     plot_ci1 <- TRUE
#'   }
#'   
#'   # PLOT TITLE
#'   if(plot_title == " "){ 
#'     plot_title1 <- paste0("Predicted vs Actual\n(", as.expression(formula), ")")
#'   } else {
#'     plot_title1 <- plot_title
#'   }
#'   
#'   ## y actual input[,facA] fA
#'   ## x prediction fpred
#'   # PLOT YAXIS
#'   if(plot_ylab == " "){
#'     plot_ylab1 <- "Actual"
#'   } else { # facA, response
#'     plot_ylab1 <- plot_ylab
#'   }
#'   
#'   # PLOT XAXIS
#'   if(plot_xlab == " "){
#'     plot_xlab1 <- "Predicted"
#'   } else { #prediction
#'     plot_xlab1 <- plot_xlab
#'   }
#'   
#'   #Set plot dimensions
#'   if(is.na(width)){
#'     w <- 10.5
#'   } else if(width == 0){
#'     w <- 7.2
#'   } else{
#'     w <- width
#'   }
#'   h <- w
#'   # plot(x=prediction, y=model_data[,facA], xlab=paste0("Predicted ", facA), ylab=paste0("Actual ", facA), main=model_name, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
#'   
#'   
#'   if (method=="SVM") {
#'     facA <- mSetObj$analSet$svmReg$mod$response
#'     method <- mSetObj$analSet$svmReg$meth
#'     prediction <- mSetObj$analSet$svmReg$predicted.values
#'     test_data <- mSetObj$analSet$svmReg$test
#'     input <- test_data
#'     
#'     dfpred <- data.frame(fpred = prediction, fA = input[,facA])
#'     formula2 <- as.formula("fA ~ fpred")
#'     model2 <- lm(formula = formula2, data = dfpred)
#'     
#'     #NAME PLOT FOR DOWNLOAD
#'     ### must put imgName2 first, re-writing imgName var in next line
#'     imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
#'                       ".json", sep="") 
#'     imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
#'     mSetObj$imgSet$plot.pred.svmReg <- imgName
#'     
#'     
#'   } else { #random forest is default
#'     
#'     facA <- mSetObj$analSet$rfReg$mod$response
#'     method <- mSetObj$analSet$rfReg$meth
#'     prediction <- mSetObj$analSet$rfReg$predicted.values
#'     test_data <- mSetObj$analSet$rfReg$test
#'     input <- test_data
#'     
#'     dfpred <- data.frame(fpred = prediction, fA = input[,facA])
#'     formula2 <- as.formula("fA ~ fpred")
#'     model2 <- lm(formula = formula2, data = dfpred)
#'     
#'     #NAME PLOT FOR DOWNLOAD
#'     ### must put imgName2 first, re-writing imgName var in next line
#'     imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
#'                       ".json", sep="") 
#'     imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
#'     mSetObj$imgSet$plot.pred.rfReg <- imgName
#'     
#'   }
#'   
#'   ## MAKE PLOT  
#'   a0 <- ggplot(data =  dfpred, aes(x = fpred, y = fA)) +
#'     labs(title = plot_title1) +
#'     ylab(plot_ylab1)+ xlab(plot_xlab1) +
#'     geom_smooth(se = plot_ci1, color = col_line1, fullrange = TRUE) +#, formula = formula2) +
#'     geom_point(shape = 16, color = col_dots1) +
#'     theme_bw() + 
#'     theme(panel.grid.major = element_blank(), 
#'           panel.grid.minor = element_blank(),
#'           axis.text = element_text(size = 12, colour = "black"), 
#'           axis.title = element_text(size = 12),
#'           # legend.title=element_text(12), legend.text=element_text(size=12), 
#'           plot.title = element_text(face = 'bold', hjust = 0.5)
#'     )
#'   
#'   #GENERATE PLOT
#'   Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
#'   print(a0)
#'   # plot(prediction, test_data[,1], xlab="Predicted", ylab="Actual", main=method, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
#'   dev.off()
#'   
#'   # STORE IN mSET
#'   if (method == "SVM") { 
#'     mSetObj$analSet$svmReg$plotpred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
#'   } else {
#'     mSetObj$analSet$rfReg$plotpred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)  
#'   } 
#'   
#'   #JSON OBJECT MAKING
#'   build <- ggplot_build(a0)
#'   build_line <- build$data[[1]]
#'   build_points <- build$data[[2]]
#'   linear_plot_json <- list()
#'   
#'   linear_plot_json$main <- plot_title1 #title
#'   linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
#'   linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
#'   linear_plot_json$points$cols <- build$data[[1]][,grepl("col",colnames(build_points))] #[,6] #colours
#'   linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
#'   linear_plot_json$points$size <- build_points[,c("size")]#[,7]
#'   linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
#'   # linear_plot_json$label <- build$data[[3]][,c("label")]
#'   # linear_plot_json$lines$ci <- build$data[[1]][,c("se")]
#'   
#'   if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
#'     ci<- build_line[,c("x","y", "ymin", "ymax")] 
#'     colnames(ci) <- c("x","y","CI_down", "CI_up")
#'     linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
#'   } else{
#'     linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
#'   }   
#'   
#'   ## BOOLEANS
#'   if(plot_ci1 == TRUE){
#'     linear_plot_json$bool_ci <- TRUE
#'   } else{
#'     linear_plot_json$bool_ci <- FALSE
#'   }
#'   
#'   
#'   linear_plot_json$model$r_sq <-
#'     summary(model2)[["r.squared"]] #Extract R^2
#'   linear_plot_json$model$r_sq_adj <-
#'     summary(model2)[["adj.r.squared"]] #Extract adjusted R^2 
#'   linear_plot_json$model$slope <-
#'     summary(model2)[["coefficients"]][2] # beta
#'   linear_plot_json$model$yint <-
#'     summary(model2)[["coefficients"]][1] # alpha
#'   
#'   json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
#'   sink(imgName2)
#'   cat(json.obj)
#'   sink()
#'   
#'   if(!.on.public.web){
#'     return(.set.mSet(mSetObj))
#'   }
#'   
#' }

