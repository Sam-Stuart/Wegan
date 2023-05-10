#'Perform Penalized Linear Regression'
 #'@description Build a penalized regression model for one user selected predictor variable
#'@usage pen.reg.anal(mSetObj=NA, facA='NULL', method="NULL") # data='false'
 #'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses Columns() to give user options)
#'@param method Set penalized regression method, default is ridge
## #@param data Boolean, whether to use original data; "false" (default) means use normalized or "true" means use original (checkbox) ## removed data param: 202209-30
 #'@author Louisa Normington\email{normingt@ualberta.ca}
 #'University of Alberta, Canada
 #'License: GNU GPL (>= 2)
 

# 'summary' to 'summ'; 'model'to 'mod'; 'formula' to 'form'; 'coef' to 'coefs' # 202210-31
# 'predictors' to 'predictor'; 'model_name' to 'model.name', add 'formula'
# pred.plot - plot_ci now has no effect, added note on why having ci on penalized predictions lead to overly optimistic estimates
 pen.reg.anal <- function(mSetObj=NA,
                          facA="NULL",
                         # method="ridge",
                         # data="false"
                         method="NULL" ##, data="false"
## ## removed data param: 202209-30
                          ){
   library("caret")
   library("glmnet")
   library("Metrics")
   library("dplyr")
   
   mSetObj <- .get.mSet(mSetObj)

   method1 <- method

##  Regularized Generalized Linear Models

# remember that: lambda is a tuning parameter that helps to control our model from over-fitting to the training data. https://uc-r.github.io/regularized_regression
# want to identify the largest lambda that falls within one standard error of the (minimum)  MSE
#  shows  how much one can constrain the coefficients while still maximizing predictive accuracy 
#  (show on a coefficients (yaxis) vs log lambda plot)

### penalized regression: Important to standardize
### {glmnet} automatically standardizes predictors for fitting by default. It also reports fitted coefficient using the original scale
### compared to least-squares, where it is not necessary to stdz the predictors or outcomes prior to fitting model
### for penalized IT IS important to stdz BECAUSE the same penalty factor lambda is applied to all coefficients (Bj) equally - to interpret Bj coeffeicients in the original units, invert the sdzn after estimation
## https://jwmi.github.io/SL/11-Penalized-regression.pdf
### TO avoid being penalized for differences in scale between variables, it a good idea to standardize each variable (subtract mean, divide by sd) before running penReg
## https://lost-stats.github.io/Machine_Learning/penalized_regresstion.html

#### VARIABLE IMPORTANCE: LASSO VS RIDGE
# 
# gtitl <- ifelse(method1 == "ridge","Top 25 influential variables", "Influential variables")
# adf <- ifelse(method1 == "ridge","df_ridg", "df_lasso")
# 
# adf1 <- coef(get(adf), s = "lambda.1se") %>%
#   broom::tidy() %>%
#   dplyr::filter(row != "(Intercept)")
# # influential variables 
# if(method1 == "ridge")
#  plot_now <- adf1 %>%  
#   dplyr::top_n(25, wt = abs(value)) %>%
#   ggplot(aes(value, reorder(row, value))) +
#     geom_point() 
# else{
#  plot_now <- plot_now <- adf1 %>%  
# ggplot(aes(value, reorder(row, value), color = value > 0)) +
#       geom_point(show.legend = FALSE) 
# }  
#   plot_now <- plot_now + ggtitle(gtit) + xlab("Coefficient") +  ylab(NULL)
# 
##### ACTUAL CODING


## removed data param: 202209-30
#  ### SET DATA (whether to use original data or not)
#  if (data=="false") { 
#    mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]

     input <- mSetObj$dataSet$norm #default use norm

  # } else {
    # input <- mSetObj$dataSet$orig
  # }
 # } else {
 #   input <- mSetObj$dataSet$orig
#warning("NOT USING STANDARDIZED DATA IN PENALIZED REGRESSION? BE CAREFUL - you may be penalized for differences in scale between variables (remember the same penalty factor lambda will be applied to all variables equally!)")
#cat("NOT USING STANDARDIZED DATA IN PENALIZED REGRESSION? BE CAREFUL - you may be penalized for differences in scale between variables (remember the same penalty factor lambda will be applied to all variables equally!)")
#  }
   
   #FILENAME FOR SUMMARY DOWNLOAD
   fileName <- "penalized_regression_summary.txt"
 
   #TEXT VISIBLE TO USER 
   # cat("One dependent variable and two or more independent variables will be tested for correlation. Only numeric variables will be used.")
  # cat("All variables other than the dependent variable will be treated as independent variables.") 
   #"The penalized regression models will constrain the contribution of the independent variables on the dependent variable, in some cases removing a variable from the model all together, so as to maximize the fit of the model."
   

  #SUBSET DATA FOR ONLY NUMERIC VARIABLES
   data <- dplyr::select_if(input, is.numeric)
   # data <- input[,sapply(input, is.numeric), drop = FALSE]
   
  #DATA CHECK
  if (ncol(data) < 3) {
      stop("Your data set only has 2 variables! Try using linear, polynomial or SVM regression.")
  }

   ### RIDGE:
   ### L2(Euclidean Norm) penalty; controlled by lambda parameter
   ### as lambda approaches infinity, the penalty becomes large and forces the coefficients approximately to zero (but not completely)
   ### GOOD IF: you feel the need to retain all features in your model yet reduce the noise from less influential variables (eg, in smaller data sets with severe multicollinearity)
   ### BAD FOR: better interpretation, many redundant features
   ## LASSO
   ## L1 penalty
   ## pushes coefficients directly to zero, performing feature selection
   ## GOOD FOR: feature selection
   ## BAD FOR: when two strongly correlated features are pushed towards zero, one may be pushed fully to zero while the other remains in the model. Furthermore, the process of one being in and one being out is not very systematic.
   ## https://bradleyboehmke.github.io/HOML/regularized-regression.html
   
   ## 
### changed facA to 1st column of numeric data instead of just generic data 202209-30
   #SET RESPONSE VARIABLE NAME
   if (facA=="NULL"){
    facA <- colnames(data)[1] #facA is the name of the response variable name. Default is first column.
   } else {
     facA <- facA #Determined using numeric.columns() function below (java will present options in drop down menu)
   }
   
   #GENERATE TEST/TRAIN DATA FOR MODEL BUILDING
  set.seed(37) #Ensures same selection of data for test and train each time
  index <- sample(1:nrow(data), 0.7*nrow(data)) #Select 70% of dataset
  train_data <- data[index,,drop = FALSE] #70% of dataset
  test_data <- data[-index,, drop = FALSE] #30% of dataset
  # resp.col.num <- which(colnames(data)==facA)
  predictors_train <- train_data[,!colnames(train_data) %in% facA, drop = FALSE]
  predictors_test <- test_data[,!colnames(train_data) %in% facA, drop = FALSE]
  response_train <- train_data[,facA, drop = TRUE] # response data for train dataset
  response_test <- test_data[,facA, drop = TRUE]
   cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
   
# object of type 'closure' is not subsettable
   # if (is.null(weights)==TRUE) { #No weights for model building
   
 ## METHOD TYPE LOOP
      lambda <- 10^seq(-3, 3, length = 100) #not used in elastic net, but used in all others
   ### ELASTIC NET

    if (method1 == "elastic net") {
       
       # TESTING DIFFERENT PARAMETERS
       params <- caret::train(x = predictors_train, y = response_train, weights = NULL, method = "glmnet", trControl = caret::trainControl("cv", number = 10), tuneLength = 5) 
       print("elast: params")
       # BUILD MODEL WITH 'BEST' PARAMETERS
      mod <- glmnet(as.matrix(predictors_train), as.matrix(response_train),
    alpha = params$bestTune$alpha, lambda = params$bestTune$lambda, weights = NULL, family = "gaussian") 
      print("elast: model built")
      # BEST PARAMETERS
       bestLambda <- params$bestTune$lambda 
       bestAlpha <- params$bestTune$alpha
      method1 <- "Elastic Net Regression"
       print(method1)
       print(bestLambda)
       ##FILENAME FOR SUMMARY DOWNLOAD 
       ##fileName <- "elastic_net_regression_summary.txt"
       #CROSS VALIDATION FOR PLOTTING
       cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
       print("elas: cv")
       
    } else if (method1 == "lasso") {
       
 ## LASSO
      # TESTING DIFFERENT PARAMETERS
      params <- caret::train(predictors_train, response_train, weights = NULL, method = "glmnet",  trControl = caret::trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 1, lambda=lambda))
      print("lasso: params")
      # BUILD MODEL WITH 'BEST' PARAMETERS
      mod <- glmnet(as.matrix(predictors_train), as.matrix(response_train),
  alpha=params$bestTune$alpha, lambda = params$bestTune$lambda,   weights = NULL, family = "gaussian")
      print("lasso: model")
      # BEST PARAMETERS
      bestAlpha <- 1
      bestLambda <- params$bestTune$lambda
      method1 <- "Lasso Regression"
      print(method1)
      print(bestLambda)
      ###FILENAME FOR SUMMARY DOWNLOAD 
      ##fileName <- "lasso_regression_summary.txt"
      #CROSS VALIDATION FOR PLOTTING
      cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
      print("lasso: cv")
    } else {
  ## RIDGE
     # TESTING DIFFERENT PARAMETERS
      params <- caret::train(predictors_train, response_train, weights = NULL, method = "glmnet", 
                      trControl = caret::trainControl("cv", number = 10),
                    tuneGrid = expand.grid(alpha = 0, lambda = lambda)) 
      print("ridge: params")
       # BUILD MODEL WITH 'BEST' PARAMETERS
      mod <- glmnet::glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda = params$bestTune$lambda, weights = NULL, family = "gaussian")
      print("ridge: model")
      # BEST PARAMETERS
      bestAlpha <- 0
      bestLambda <- params$bestTune$lambda #Extract best parameter
      method1 <- "Ridge Regression"
      print(method1)
      print(bestLambda)
      ##FILENAME FOR SUMMARY DOWNLOAD 
      ##fileName <- "ridge_regression_summary.txt" 
      #CROSS VALIDATION FOR PLOTTING
      cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
      
    }
    
  #} else {
  #  weights <- weights #Java upload weights as a vector of numeric values
  #  print(nrow(weights))
  #  print(nrow(data))
  #  if (nrow(weights) == nrow(data)) { #There must be one weight for every row in the data set
  #    if (method == "elastic net") {
  #      
  #      #Build model
  #      params <- train(x=predictors_train, y=response_train, weights=weights, method="glmnet", 
  #                      trControl=trainControl("cv", number=10), tuneLength=5) #testing variour parameters
  #      model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
  #                      weights=weights, family="gaussian") #Build model with "best" parameters
  #      bestLambda <- params$bestTune$lambda #Extract best parameters
  #     bestAlpha <- params$bestTune$alpha
  #      method <- "Elastic Net Regression"
  #      
  #      #File name for summary download
  #      fileName <- "elastic_net_regression_summary.txt"
  #      
  #      #Cross validation results for plotting
  #      cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
  #      
  #    } else if (method == "lasso") {
  #      
  #      #Build model
  #      lambda <- 10^seq(-3, 3, length = 100)
  #      params <- train(predictors_train, response_train, weights=weights, method="glmnet", 
  #                      trControl=trainControl("cv", number=10),
  #                      tuneGrid = expand.grid(alpha=1, lambda=lambda)) #testing variour parameters
  #      model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
  #                      weights=weights, family="gaussian") #Build model with "best" parameters
  #      bestAlpha <- 1
  #      bestLambda <- params$bestTune$lambda #Extract best parameter
  #      method <- "Lasso Regression"
  #      
  #      #File name for summary download
  #      fileName <- "lasso_regression_summary.txt"
  #      
  #      #Cross validation results for plotting
  #      cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
  #      
  #    } else {
  #      print("ridge")
  #      #Build model for ridge regression
  #      lambda <- 10^seq(-3, 3, length = 100)
  #      params <- train(predictors_train, response_train, weights=weights, method="glmnet", 
  #                      trControl=trainControl("cv", number=10),
  #                      tuneGrid = expand.grid(alpha=0, lambda=lambda)) #testing variour parameters
  #      print("after param")
  #      model <- glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
  #                      weights=weights, family="gaussian")#Build model with "best" parameters
  #      print("after model")
  #      bestAlpha <- 0
  #      bestLambda <- params$bestTune$lambda #Extract best parameter
  #      method <- "Ridge Regression"
  #      
  #      #File name for summary download
  #      fileName <- "ridge_regression_summary.txt" 
  #      
  #      #Cross validation results for plotting
  #      cv <- cv.glmnet(x=as.matrix(predictors_train), y=as.matrix(response_train), alpha=bestAlpha)
  #      
  #    }
  #  } else {
  #    stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
  #  }
  #}


  
  cat("The", method1, " model was optimized using alpha = ", bestAlpha, " and lambda = ", bestLambda, ".", sep="") #Text will be visible to user.

    
  #EXTRACT VALUES
  summ <- params 
  print("params summary")
  # fitted <- predict(mod, newx = as.matrix(data[,colnames(data) != facA, drop = FALSE]))
  fitt <- predict(mod, s = cv$lambda.min, newx = as.matrix(predictors_train) ) 
  print("fitted (train)")
  mod_call <- mod[["call"]]
  print("model call")
  form <- as.formula(paste(facA, "~", paste(colnames(predictors_train), collapse = "+", sep = "")))
  # coef <- as.data.frame(summary(coef(mod)))
  coefs <- as.data.frame(as.matrix(
     # summary(
      coef(mod)
             # ) 
             ))
  print("coefficients")
### uncomment
  #coef$variable <- "variable"
  #x <- 1
  #i_index <- c(1:nrow(coef))
  #for (col.num in i_index #coef[,"i"] 
  #     ) {
  #  coef$variable[x] <- colnames(data)[col.num]
  #  x <- x+1
  #}
  #coef$variable[1] <- "intercept"
  #coef <- data.frame(coef$variable, coef$x)
  #colnames(coef) <- c("Variables", "Coefficients")
### uncomment done

  fit_all <- predict(mod, s = cv$lambda.min,
                     newx = as.matrix(data[,colnames(data) != facA, drop = FALSE]))
  print("fitted on all")
  overall.rsme <- Metrics::rmse(data[,facA, drop = TRUE], fit_all)
  print("overall.rmse")

  #PREDICT ON TEST, OBTAIN RMSE
  predictors_test2 <- as.matrix(predictors_test)
  # predictors_test2 <- as.data.frame(predictors_test2)
  print("predictors_test2")
  test_prediction <- predict(mod, newx =predictors_test2  )
  print("test_prediction")
#  test_rmse <- Metrics::rmse(response_test, test_prediction)
  print("test_rmse")

   #Store results in mSetObj$analSet$penReg
  mSetObj$analSet$penReg$mod <- list(model.name = method1, model = mod, formula = form, response = facA, predictor = colnames(predictors_train), alpha = bestAlpha, lambda = bestLambda)
  mSetObj$analSet$penReg$res <- list(response = facA, predictor = colnames(predictors_train), predictors.test.data = predictors_test, predictors.train.data = predictors_train, summary = summ, coefficients = coefs, predicted.values = fitt, 
test.prediction = test_prediction, 
overall.rmse = overall.rsme, train.data = train_data, test.data = test_data, 
#test.rmse = test_rmse, 
cross.validation = cv, method = method1, fileName = fileName) 
print("stored results in mSetObj")
 
   #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
   sink(fileName) 
   cat("Formula:\n")
  print(form)
   cat("\nMethod:\n")
  cat(method1)
   cat("\n\nCall:\n")
   print(mod_call)
   cat("\nSummary:\n")
  print(summ)
   cat("\nCoefficients:\n")
  print(coefs)
   cat("\nPredicted values using trained model:\n")
  print(fitt)
  # print(test_prediction)
   cat("\nOverall RMSE:\n")
 cat(paste0(overall.rsme))
   sink()
   
   return(.set.mSet(mSetObj))

}   
    
 #'Produce predicted/actual plot for penalized regression
 #'@description Scatter plot, where actual variables are y and predicted values are x
#'@usage plot.pred.penReg(mSetObj, facA="NULL", method="NULL", col_dots="NULL", col_line="NULL", plot_ci="false", plot_title=" ", plot_ylab=" ", plot_xlab = " "imgName, format="png", dpi=72, width=NA)
 #'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
 #'@param facA Input the name of the response column (java uses Columns() to give user options)
###@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox) ## removed data param: 202209-30
 #'@param method Set penalized regression method, default is ridge
 #'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
 #'@param col_line Set color for line (default "NULL" is black); (static dropdown)
 #' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
 #'@param plot_title Input the name of the title (default: "Polynomial Regression Predicted vs Actual: (formula);, textbox)
 #'@param plot_xlab Input the name to use for x-axis label (default: facB, textbox)
 #'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
 #'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
 #'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
###@param weights Set weight values, default is NULL
 #'@author Louisa Normington\email{normingt@ualberta.ca}
 #'University of Alberta, Canada
 #'License: GNU GPL (>= 2)
 #'@export
 
 pen.pred.plot <- function(mSetObj=NA,
  facA = "NULL", 
  method = "NULL", 
  #data='false', ## removed data param: 202209-30

   col_dots="NULL",
   col_line="NULL", 
  # plot_ci="false", # plot_eq="false",  # plot_rsq="false", # plot_rsq_adj="false",
   plot_title=" ",
   plot_ylab=" ",
   plot_xlab=" ",
   imgName, format="png", dpi=72, width=NA){
   
   ## name used to be: plot.pred.penReg

 # Reporting a CI around a biased estimate will give an unrealistically optimistic indication of how close the true value of the coefficient may be to the point estimate.
# problem with ci for penalizec regression: https://stats.stackexchange.com/questions/224796/why-are-confidence-intervals-and-p-values-not-reported-as-default-for-penalized
# Penalized regression uses the bias-variance tradeoff to give us coefficient estimates with lower variance, but with bias. Reporting a CI around a biased estimate will give an unrealistically optimistic indication of how close the true value of the coefficient may be to the point estimate.
#section 6 of the vignette for the penalized R package ("L1 and L2 Penalized Regression Models" Jelle Goeman, Rosa Meijer, Nimisha Chaturvedi, Package version 0.9-47), https://cran.r-project.org/web/packages/penalized/vignettes/penalized.pdf.
# another possible option: HDCI — High Dimensional Confidence Interval Based on Lasso and Bootstrap (https://github.com/cran/HDCI)


library("glmnet")
library("dplyr")
library("Metrics")
library("ggplot2")
library("RJSONIO")
   

  #EXTRACT FROM mSetObj NECESSARY OBJECTS 
  mSetObj <- .get.mSet(mSetObj)
   
## removed data param: 202209-30
   ### SET DATA (whether to use original data or not)
#  if (data=="false") { 

     input <- mSetObj$dataSet$norm #default use norm

#  } else {
#    input <- mSetObj$dataSet$orig
#cat("NOT USING STANDARDIZED DATA IN PENALIZED REGRESSION? BE CAREFUL - you may be penalized for differences in scale between variables (remember the same penalty factor lambda will be applied to all variables equally!)")
#  }
  
print("pred: input set")
data <- dplyr::select_if(input, is.numeric)
print("pred: numeric ('data') set")

facA <- mSetObj$analSet$penReg$res$response
method1 <- mSetObj$analSet$penReg$res$method
mod <- mSetObj$analSet$penReg$mod$model
predictors_test <- mSetObj$analSet$penReg$res$predictors.test.data
test_prediction <-  mSetObj$analSet$penReg$res$test.prediction #predict(mod, newx = as.matrix(predictors_test))
test_data <- mSetObj$analSet$penReg$res$test.data
predictors_train <- mSetObj$analSet$penReg$res$predictors.train.data
form <- paste(facA, "~", paste(colnames(predictors_train), collapse = "+", sep = ""))
# form <- mSetObj$analSet$penReg$mod$formula
#  test_rmse <- <- mSetObj$analSet$penReg$res$test.rmse #Metrics::rmse(test_data[,facA, drop = TRUE], test_prediction)  

print("pred: extracted from mSet set")


  # form <- paste(facA, "~", paste(colnames(predictors_train), collapse = "+", sep = ""))
  # print("pred: formula set")
   dfpred <- data.frame(fpred = as.vector(test_prediction), fA = test_data[,facA, drop = TRUE])
 print("pred: dfpred set") 
  formula2 <- as.formula("fA~fpred")
 print("pred: formula2 set")
   model2 <- lm(formula = formula2, data = dfpred, weights = NULL)
  print("pred: model2 set")
 

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
  mSetObj$imgSet$plot.pred.penReg <- imgName
  
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

  
  # PLOT TITLE
  if(plot_title == " "){ 
    plot_title1 <- paste0(method1, "Predicted vs. Actual")
#    plot_title1 <- paste0(method1,"\n",form)
  } else {
    plot_title1 <- plot_title
  }

print("pred: title")
  print(plot_title1)

  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Actual"
  } else { # facA, response
    plot_ylab1 <- plot_ylab
  }
print("pred: ylab:")
  print(plot_ylab1)

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Predicted"
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
print("pred: xlab:")
 print(plot_xlab1)
  print("pred: plot color, labels set")
  
  
### TROUBLESHOOTING
#input <- iris
#data <- dplyr::select_if(input, is.numeric)
#facA <- colnames(input)[1]
#set.seed(37) #Ensures same selction of data for test and train each time
#index <- sample(1:nrow(data), 0.7*nrow(data)) #Select 70% of dataset
#train_data <- data[index,,drop = FALSE] #70% of dataset
#test_data <- data[-index,, drop = FALSE] #30% of dataset
#resp.col.num <- which(colnames(data)==facA)
#predictors_train <- train_data[,-resp.col.num, drop = TRUE]
#predictors_test <- test_data[,-resp.col.num, drop = TRUE]
#response_train <- train_data[,facA, drop = TRUE]
#lambda <- 10^seq(-3, 3, length = 100)
#params <- caret::train(predictors_train, response_train, weights = NULL, method = "glmnet",  trControl = caret::trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 0, lambda = lambda))
#model <- glmnet::glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda = params$bestTune$lambda, weights = NULL, family = "gaussian")
#bestAlpha <- 0
#bestLambda <- params$bestTune$lambda #Extract best parameter
#method1 <- "Ridge Regression"
#cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
#test_prediction <- predict(mod, newx = as.matrix(predictors_test))
#col_dots1 <- "blue"
#col_line1 <- "red"
#plot_title1 <- paste0(method,"\n",formula) #paste0(method, " Cross Validation Plot", "\n")
#plot_ylab1 <- "Actual"
#plot_xlab1 <- "Predicted"
#### TROUBLESHOOTING OVER

   # plot(x=test_prediction, y=test_data[,facA], xlab="Predicted", ylab="Actual", main=method, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
 

  a0 <- ggplot(data = dfpred, aes(x = fpred, y = fA)) +
#     labs(title = plot_title1) +
     labs(title = plot_title1, subtitle = form) +
      ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_smooth(se = FALSE, color = col_line1, fullrange = TRUE, method = "lm") +
      geom_point(shape = 16, color = col_dots1) +
          theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )
 

   print("pred: made plot set")
 #STORE IN mset
  mSetObj$analSet$penReg$plotpred <- list(plot= a0, title = plot_title1, 
         xlab = plot_xlab1, ylab = plot_ylab1)
 
#GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
 
   print(a0)
   # a0
   dev.off()
   
   
 # GENERATE JSON
build <- ggplot_build(a0)
linear_plot_json <- list()

build_line <- build$data[[1]] ### line is 1
# build_points <- build$data[[2]] # points is 2
linear_plot_json <- list()

linear_plot_json$main <- paste0(plot_title1, "\n", form) #title
linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
# linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
# linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
# linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
# linear_plot_json$points$size <- build_points[,c("size")]#[,7]
linear_plot_json$lines$coords <- build_line[,c("x","y")]
linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
linear_plot_json$lines$size <- build_line[,c("size")]
linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 
  
## BOOLEANS
#if(plot_ci1 == TRUE){
# linear_plot_json$bool_ci <- TRUE
# } else{
#linear_plot_json$bool_ci <- FALSE
#}

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
 print(paste("PLOT1 | facA: ", facA, " | method: ", method1, sep = ""))
 print("JSON and the argonauts")
  
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj))
  }
 }
 
 
#'Produce cross validation plot for penalized regression
#'@description
#'@usage plot.cv.penReg(mSetObj, facA="NULL",data="false", method="NULL", col_dots="NULL",col_line="NULL", plot_title=" ",plot_xlab=" ",plot_ylab=" ",imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
###@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)   ## removed data param: 202209-30
#'@param method Set penalized regression method, default is ridge
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
###@param weights Set weight values, default is NULL
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

pen.cv.plot <- function(mSetObj=NA,
  facA = "NULL",
  method = "NULL", # data = "false",  ## removed data param: 202209-30
  col_dots="NULL",
  col_line="NULL", 
  # plot_ci="false",
  
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
  imgName, format="png", dpi=72, width=NA){

   ## name was: plot.cv.penReg
library("glmnet")
library("ggplot2")
library("broom")
library("dplyr")
library("RJSONIO")
   
   #EXTRACT NECESSARY mSetObj OBJECTS 
   mSetObj <- .get.mSet(mSetObj)
  
## removed data param: 202209-30
   ### SET DATA (whether to use original data or not)
#  if (data=="false") { 
     input <- mSetObj$dataSet$norm #default use norm
#  } else {
#    input <- mSetObj$dataSet$orig
#cat("NOT USING STANDARDIZED DATA IN PENALIZED REGRESSION? BE CAREFUL - you may be penalized for differences in scale between variables (remember the same penalty factor lambda will be applied to all variables equally!)")
#  }
  

## SELECT ONLY NUMERIC DATA
  data <- dplyr::select_if(input, is.numeric)
  
## EXTRACT MODEL VARIABLES  
  mod <- mSetObj$analSet$penReg$mod$model
  method1 <- mSetObj$analSet$penReg$res$method
  predictors_test <- mSetObj$analSet$penReg$res$predictors.test.data
  test_prediction <- predict(mod, newx = as.matrix(predictors_test))
  facA <- mSetObj$analSet$penReg$res$response
  test_data <- mSetObj$analSet$penReg$res$test_data
  predictors_train <- mSetObj$analSet$penReg$res$predictors.train.data
  cv <- mSetObj$analSet$penReg$res$cross.validation
  form <- paste(facA, "~", paste(colnames(predictors_train), collapse = "+", sep = ""))
  # form <- mSetObj$analSet$penReg$mod$formula

# ### changed facA to 1st column of numeric data instead of just generic data 202209-30
#   #SET RESPONSE VARIABLE NAME
#   if (facA=="NULL"){
#      if( "res" %in% names(mSetObj$analSet$penReg) ){
#         facA <- mSetObj$analSet$penReg$res$response
#      } else {
#     facA <- colnames(data)[1] #facA is response variable name. Default is 1st column
#   } 
#     } else {
#     facA <- facA #Determined using numeric.columns() (java will present options in drop down menu)
#   }
#   
# #SET METHOD
#   if (method=="NULL"){
#      if( "res" %in% names(mSetObj$analSet$penReg) ){
#         method <- mSetObj$analSet$penReg$res$method
#      } else {
#     method <- "ridge" #Default is ridge
#   } 
#     } else {
#     method <- method # (java will present options in drop down menu)
#   }  
# 
#    #TEST AND TRAIN DATA FOR MODEL BUILDING
#   set.seed(37) #Ensures same selection of data for test and train each time
#   index <- sample(1:nrow(data), 0.7*nrow(data)) #Select 70% of dataset
#   train_data <- data[index,,drop = FALSE] #70% of dataset
#   test_data <- data[-index,, drop = FALSE] #30% of dataset
#   resp.col.num <- which(colnames(data)==facA)
#   predictors_train <- train_data[,-resp.col.num, drop = FALSE]
#   predictors_test <- test_data[,-resp.col.num, drop = FALSE]
#   response_train <- train_data[,facA, drop = TRUE] # response data for train dataset
#   # response_test <- test_data[,facA, drop = TRUE]  
# cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
# 
#     if (method == "elastic net") {
#       params <- caret::train(x = predictors_train, y = response_train, weights = NULL, method = "glmnet", 
#                       trControl = caret::trainControl("cv", number = 10), tuneLength = 5) #test params
#       model <- glmnet::glmnet(as.matrix(predictors_train), as.matrix(response_train),
#                       alpha = params$bestTune$alpha, lambda = params$bestTune$lambda, 
#                       weights = NULL, family = "gaussian") #Build model with "best" parameters
#       bestLambda <- params$bestTune$lambda #Extract best parameters
#       bestAlpha <- params$bestTune$alpha
#       method <- "Elastic Net Regression"
#       cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
#     } else if (method == "lasso") {
#       lambda <- 10^seq(-3, 3, length = 100)
#       params <- caret::train(predictors_train, response_train, weights = NULL, method = "glmnet", 
#                       trControl = caret::trainControl("cv", number = 10),
#                       tuneGrid = expand.grid(alpha = 1, lambda=lambda)) #testing various parameters
#       model <- glmnet::glmnet(as.matrix(predictors_train), as.matrix(response_train),
#                       alpha=params$bestTune$alpha, lambda = params$bestTune$lambda, 
#                       weights = NULL, family = "gaussian") #Build model with "best" parameters
#       bestAlpha <- 1
#       bestLambda <- params$bestTune$lambda #Extract best parameter
#       method <- "Lasso Regression"
#       cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
#     } else {
#       lambda <- 10^seq(-3, 3, length = 100)
#       params <- caret::train(predictors_train, response_train, weights = NULL, method = "glmnet", 
#                       trControl = caret::trainControl("cv", number = 10),
#                       tuneGrid = expand.grid(alpha = 0, lambda = lambda)) #testing variour parameters
#       model <- glmnet::glmnet(as.matrix(predictors_train), as.matrix(response_train), 
#                       alpha=params$bestTune$alpha, lambda = params$bestTune$lambda, 
#                       weights = NULL, family = "gaussian")#Build model with "best" parameters
#       bestAlpha <- 0
#       bestLambda <- params$bestTune$lambda #Extract best parameter
#       method <- "Ridge Regression"
#       cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
#     }
#
#form <- paste(facA, "~", paste(colnames(predictors_train), collapse = "+", sep = ""))
# test_prediction <- predict(mod, newx = as.matrix(predictors_test)) 
# test_rmse <- Metrics::rmse(test_data[,facA], test_prediction)
 

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
  mSetObj$imgSet$plot.cv.penReg <- imgName

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
  
  # PLOT TITLE
  if(plot_title == " "){ 
    plot_title1 <- paste0(method1, " Cross Validation Plot", "\n")
  } else {
    plot_title1 <- plot_title
  }
  
  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Mean-Squared Error (MSE)"
  } else { # facA, response
    plot_ylab1 <- plot_ylab
  }

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Log(Lambda)"
   # plot_xlab1 <- expression(paste("Log ", lambda))
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
 
  # plot(cv, yaxt="n", xlab="Log(Lambda)", main=paste0(method1, " Cross Validation Plot", "\n")); axis(2, las=2)
  # plot of MSE as a function of lambda

a0 <- ggplot(broom::tidy(cv), aes(lambda, estimate)) +
#   labs(title = plot_title1) +
   labs(title = plot_title1, subtitle = form) +
     ylab(plot_ylab1)+ xlab(plot_xlab1) +
     geom_point(shape = 16, color = col_dots1) +
  geom_line(color = col_line1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = "black", alpha = .1) +
  scale_x_log10() +
  geom_vline(xintercept = broom::glance(cv)[["lambda.min"]]) +
  geom_vline(xintercept = broom::glance(cv)[["lambda.1se"]], lty = 2) + # dotted line
  theme_bw() +
   theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )


#STORE IN mset
  mSetObj$analSet$penReg$plotcv <- list(plot= a0, title = paste0(plot_title1, "\n", form),
 xlab = plot_xlab1, ylab = plot_ylab1) 

#GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
   
  print(a0)
  # a0
  dev.off()


# GENERATE JSON
build <- ggplot_build(a0)
linear_plot_json <- list()

build_line <- build$data[[3]] ### line is 1
build_points <- build$data[[1]] # points is 1 or 2
## or 2
build_points2 <- build$data[[2]] # points is 1 or 2
build_line2 <- build$data[[4]] ### xint -3.2, linetype = 1
build_line3 <- build$data[[5]] ### xint -1.78, linetype = 2

linear_plot_json <- list()

linear_plot_json$main <- paste0(plot_title1, "\n", form) #title
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
  if(any(grepl("ymin", colnames(build_line))) && any(grepl("ymax", colnames(build_line))) ){
   ci<- build_line[,c("x","y", "ymin", "ymax")] 
   colnames(ci) <- c("x","y","CI_down", "CI_up")
   linear_plot_json$lines$ci <- ci # build$data[[1]][,c("ymin", "ymax")]
 } else{
    linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 }   
  
#### MODEL VARS FOR LINE
#  linear_plot_json$r_sq <-
#    summary(mod)[["r.squared"]] #Extract R^2
#  linear_plot_json$r_sq_adj <-
#    summary(mod)[["adj.r.squared"]] #Extract adjusted R^2 
#  linear_plot_json$slope <-
#    summary(mod)[["coefficients"]][2] # beta
#  linear_plot_json$yint <-
#    summary(mod)[["coefficients"]][1] # alpha

 
 json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
 sink(imgName2)
 cat(json.obj)
 sink()
#print(json.obj)
print(paste("PLOT2 | facA: ", facA, " | method: ", method1, sep = ""))
print("Bourne. JSON Bourne.")

if(!.on.public.web){
  return(.set.mSet(mSetObj))
    }

}
  
  













 #'Produce coefficients vs log-lambda plot for penalized regression
 #'@description Line plot,log10-lambda values on x, coefficients on the y
#'@usage pen.coef.plot(mSetObj, method="NULL", plot_title=" ", plot_ylab=" ", plot_xlab = " ,"imgName, format="png", dpi=72, width=NA)
 #'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
 #'@param facA Input the name of the response column (java uses Columns() to give user options)
###@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox) ## removed data param: 202209-30
 #'@param method Set penalized regression method, default is ridge
  #'@param plot_title Input the name of the title (default: "Polynomial Regression Predicted vs Actual: (formula);, textbox)
 #'@param plot_xlab Input the name to use for x-axis label (default: facB, textbox)
 #'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
 #'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
 #'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
###@param weights Set weight values, default is NULL
 #'@author Gina Sykes\email{gsykes@ualberta.ca}
 #'University of Alberta, Canada
 #'License: GNU GPL (>= 2)
 #'@export
 
 pen.coef.plot <- function(mSetObj=NA,
 # facA = "NULL", 
  method = "NULL", 
  #data='false', ## removed data param: 202209-30

 #  col_palette="NULL",
   plot_title=" ",
   plot_ylab=" ",
   plot_xlab=" ",
   imgName, format="png", dpi=72, width=NA){
   
   ## name used to be: plot.pred.penReg

 # Based on ggplot/data table code from: https://enhancedatascience.com/2017/07/04/machine-learning-explained-regularization/


library("glmnet")
library('data.table')
library("dplyr")
library("ggplot2")
library("RJSONIO")
   

  #EXTRACT FROM mSetObj NECESSARY OBJECTS 
  mSetObj <- .get.mSet(mSetObj)
   
## removed data param: 202209-30
   ### SET DATA (whether to use original data or not)
#  if (data=="false") { 

     input <- mSetObj$dataSet$norm #default use norm

#  } else {
#    input <- mSetObj$dataSet$orig
#cat("NOT USING STANDARDIZED DATA IN PENALIZED REGRESSION? BE CAREFUL - you may be penalized for differences in scale between variables (remember the same penalty factor lambda will be applied to all variables equally!)")
#  }
  
print("pred: input set")
data <- dplyr::select_if(input, is.numeric)
print("pred: numeric ('data') set")

facA <- mSetObj$analSet$penReg$res$response
method1 <- mSetObj$analSet$penReg$res$method
mod <- mSetObj$analSet$penReg$mod$model
predictors_test <- mSetObj$analSet$penReg$res$predictors.test.data
test_prediction <-  mSetObj$analSet$penReg$res$test.prediction #predict(mod, newx = as.matrix(predictors_test))
test_data <- mSetObj$analSet$penReg$res$test.data
predictors_train <- mSetObj$analSet$penReg$res$predictors.train.data
form <- paste(facA, "~", paste(colnames(predictors_train), collapse = "+", sep = ""))
# form <- mSetObj$analSet$penReg$mod$formula
#  test_rmse <- <- mSetObj$analSet$penReg$res$test.rmse #Metrics::rmse(test_data[,facA, drop = TRUE], test_prediction)  

print("pred: extracted from mSet set")


 

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
  mSetObj$imgSet$plot.pred.penReg <- imgName
  
## PREP DATA FOR PLOTTING
 lamb<-mod$lambda
 coeff<-as.matrix(mod$beta)
 rowName<-rownames(coeff)
 coeff<-data.table::data.table(coeff)
 coeff[,name:=rowName]
 coeff<-data.table::melt(coeff,id.vars = 'name')
 coeff[,variable:=rep(lamb, each=length(unique(name)))]
  
  # PLOT TITLE
  if(plot_title == " "){ 
    plot_title1 <- paste0("Coefficients vs. Log(Lambda) Plot")
  } else {
    plot_title1 <- plot_title
  }

  print("pred: title")
  print(plot_title1)

  # PLOT YAXIS
  if(plot_ylab == " "){
  plot_ylab1 <- "Coefficients"
  } else { # 
    plot_ylab1 <- plot_ylab
  }
  print("pred: ylab:")
  print(plot_ylab1)

  # PLOT XAXIS
  if(plot_xlab == " "){
   plot_xlab1 <- "Log Lambda"
  } else { #prediction
    plot_xlab1 <- plot_xlab
  }
  print("pred: xlab:")
  print(plot_xlab1)
  print("pred: plot color, labels set")
  
  
### TROUBLESHOOTING
#input <- iris
#data <- dplyr::select_if(input, is.numeric)
#facA <- colnames(input)[1]
#set.seed(37) #Ensures same selction of data for test and train each time
#index <- sample(1:nrow(data), 0.7*nrow(data)) #Select 70% of dataset
#train_data <- data[index,,drop = FALSE] #70% of dataset
#test_data <- data[-index,, drop = FALSE] #30% of dataset
#resp.col.num <- which(colnames(data)==facA)
#predictors_train <- train_data[,-resp.col.num, drop = TRUE]
#predictors_test <- test_data[,-resp.col.num, drop = TRUE]
#response_train <- train_data[,facA, drop = TRUE]
#lambda <- 10^seq(-3, 3, length = 100)
#params <- caret::train(predictors_train, response_train, weights = NULL, method = "glmnet",  trControl = caret::trainControl("cv", number = 10), tuneGrid = expand.grid(alpha = 0, lambda = lambda))
#model <- glmnet::glmnet(as.matrix(predictors_train), as.matrix(response_train), alpha=params$bestTune$alpha, lambda = params$bestTune$lambda, weights = NULL, family = "gaussian")
#bestAlpha <- 0
#bestLambda <- params$bestTune$lambda #Extract best parameter
#method1 <- "Ridge Regression"
#cv <- glmnet::cv.glmnet(x = as.matrix(predictors_train), y = as.matrix(response_train), alpha=bestAlpha)
#test_prediction <- predict(mod, newx = as.matrix(predictors_test))
#plot_title1 <- paste0(method) #paste0(method, " Cross Validation Plot", "\n")
#plot_ylab1 <- "Actual"
#plot_xlab1 <- "Predicted"
#### TROUBLESHOOTING OVER

   # plot(x=test_prediction, y=test_data[,facA], xlab="Predicted", ylab="Actual", main=method, yaxt="n"); axis(2, las=2); abline(a=0,b=1)
 

  a0 <- ggplot(coeff,aes(x=variable, y=value, color=name))+
   geom_line()+ scale_x_log10() +
     labs(title = plot_title1, subtitle = method1) +
      ylab(plot_ylab1)+ xlab(plot_xlab1) +

          theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 12, colour = "black"), 
        axis.title = element_text(size = 12),
        # legend.title=element_text(12), legend.text=element_text(size=12),
        plot.title = element_text(face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )
 

   print("coef: made plot set")
 #STORE IN mset
  mSetObj$analSet$penReg$plotcoef <- list(plot= a0, title = paste0(plot_title1, "\n Coefficients vs. Log(Lambda) Plot"), 
         xlab = plot_xlab1, ylab = plot_ylab1)
 
#GENERATE PLOT
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
 
   print(a0)
   # a0
   dev.off()
   
   
 # GENERATE JSON
build <- ggplot_build(a0)
linear_plot_json <- list()

build_line <- build$data[[1]] ### line is 1
# build_points <- build$data[[2]] # points is 2
linear_plot_json <- list()

linear_plot_json$main <- paste0(plot_title1, "\n", form) #title
linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
# linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
# linear_plot_json$points$cols <- build_points[,grepl("col",colnames(build_points))] #[,6] #colours
# linear_plot_json$points$shape <- build_points[,c("group")]#[,5]
# linear_plot_json$points$size <- build_points[,c("size")]#[,7]
linear_plot_json$lines$coords <- build_line[,c("x","y")]
linear_plot_json$lines$cols <- build_line[,grepl("col",colnames(build_line))]
linear_plot_json$lines$size <- build_line[,c("size")]
linear_plot_json$lines$ci <- data.frame(x = build_line[,c("x")], y = build_line[,c("y")], CI_down = 0, CI_up = 0)
 

#### MODEL VARS FOR LINE
#  linear_plot_json$r_sq <-
#    summary(model2)[["r.squared"]] #Extract R^2
#  linear_plot_json$r_sq_adj <-
#    summary(model2)[["adj.r.squared"]] #Extract adjusted R^2 
#  linear_plot_json$slope <-
#    summary(model2)[["coefficients"]][2] # beta
#  linear_plot_json$yint <-
#    summary(model2)[["coefficients"]][1] # alpha  
   
  
 json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
 sink(imgName2)
 cat(json.obj)
 sink()
 print(json.obj)
 print(paste("PLOT1 | facA: ", facA, " | method: ", method1, sep = ""))
 print("Toronto Blue JSONs")
  
  
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

pen.numeric.columns <- function(mSetObj=NA){
  
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
  
  return(name.all.numeric.cols)
  
}

penal.reg.get.results <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj)
  lin.reg.result <- c(mSetObj$analSet$penReg$mod$alpha, mSetObj$analSet$penReg$mod$lambda, mSetObj$analSet$penReg$res$test.rmse)
  return(lin.reg.result)

}
