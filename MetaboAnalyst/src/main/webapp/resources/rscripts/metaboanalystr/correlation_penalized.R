#'Perform Penalized Linear Regression'
#'@description Build a linear regression model for one user selected predictor variable
#'@param mSetObj Input the name of the created mSetObj
#'@param method Set penalized regression method, default is ridge
#'@param weights Set weight values, default is NULL
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

#library(caret)
#library(glmnet)
load_caret()
load_glmnet()

pen.reg.anal <- function(mSetObj=NA, method="ridge", weights=NULL) {
  
  mSetObj <- .get.mSet(mSetObj)

  if (class(mSetObj$dataSet$norm[,1]) == "factor"){
    family <- "multinomial"
  } else if (class(mSetObj$dataSet$norm[,1]) == "numeric"){
    family <- "gaussian"
    }

  #Generate test and train data for model building
  set.seed(37)
  index <- sample(1:nrow(mSetObj$dataSet$norm), 0.7*nrow(mSetObj$dataSet$norm))
  train_data <- mSetObj$dataSet$norm[index,]
  test_data <- mSetObj$dataSet$norm[-index,]
  predictors_train <- model.matrix(train_data[,1]~., train_data)[,-1] # Predictor variables, creating dummy vars for categorical predictors
  response_train <- train_data[,1] # response_train variable data
  response_train_name <- colnames(mSetObj$dataSet$norm)[1] #response_train variable name
  cat("The first column will be the response_train variable. The remaining columns will be the predictor variables.")
  cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.")
    
  if (is.null(weights)==TRUE) {
    
    if (method == "elastic net") {
      params <- train(
        predictors_train, response_train, weights=NULL, method="glmnet", 
        trControl=trainControl("cv", number=10), tuneLength=5)
      model <- glmnet(predictors_train, response_train, alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                      weights=NULL, family=family)
      method <- "Multivariate Elastic Net Regression"
      cat("The elastic net regression model was optimized using alpha = 0 and lambda = ", params$bestTune$lambda, ".", sep="")
      cat("Note that if alpha = 1, elastic net regression was equivalent to lasso regression.")
      
      #Extract model coefficients and write to csv file
      coef <- coef(model) 
      coeffs <- data.frame(coef@Dimnames[[1]], coef@x) #Need loop for when coeff = 0!!!!!!!!! Need loop for when family="multinomial"!!!!!!!!
      colnames(coeffs) <- c("Variable", "Coefficient")
      write.csv(coeffs, file="elastic_net_regression_coefficients.csv", row.names=FALSE)
      
      #Generate summary of parameter testing and write to txt document
      summary <- params 
      fileName_params <- "elastic_net_regression_parameter_testing.txt"
      sink(fileName_params) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
      print(summary)
      sink()
        
    } else if (method == "lasso") {
      lambda <- 10^seq(-3, 3, length = 100)
      params <- train(
        predictors_train, response_train, weights=NULL, method="glmnet", 
        trControl=trainControl("cv", number=10),
        tuneGrid = expand.grid(alpha=1, lambda=lambda))
      model <- glmnet(predictors_train, response_train, alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                      weights=NULL, family=family)
      method <- "Multivariate Lasso Regression"
      cat("The lasso regression model was optimized using alpha = 0 and lambda = ", params$bestTune$lambda, ".", sep="")
      
      #Extract model coefficients and write to csv file
      coef <- coef(model) 
      coeffs <- data.frame(coef@Dimnames[[1]], coef@x) #Need loop for when coeff = 0!!!!!!!!!Need loop for when family="multinomial"!!!!!!!!
      colnames(coeffs) <- c("Variable", "Coefficient")
      write.csv(coeffs, file="lasso_regression_coefficients.csv", row.names=FALSE)
      
      #Generate summary of parameter testing and write to txt document
      summary <- params 
      fileName_params <- "lasso_regression_parameter_testing.txt"
      sink(fileName_params) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
      print(summary)
      sink()
      
     } else {
        
        lambda <- 10^seq(-3, 3, length = 100)
        params <- train(
          predictors_train, response_train, weights=NULL, method="glmnet", 
          trControl=trainControl("cv", number=10),
          tuneGrid = expand.grid(alpha=0, lambda=lambda))
        model <- glmnet(predictors_train, response_train, alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                        weights=NULL, family=family)
        method <- "Multivariate Ridge Regression"
        cat("The ridge regression model was optimized using alpha = 0 and lambda = ", params$bestTune$lambda, ".", sep="")
        
        #Extract model coefficients and write to csv file
        coef <- coef(model) 
        coeffs <- data.frame(coef@Dimnames[[1]], coef@x)#Need loop for when coeff = 0!!!!!!!!!Need loop for when family="multinomial"!!!!!!!!
        colnames(coeffs) <- c("Variable", "Coefficient")
        write.csv(coeffs, file="ridge_regression_coefficients.csv", row.names=FALSE)
        
        #Generate summary of parameter testing and write to txt document
        summary <- params 
        fileName_params <- "ridge_regression_parameter_testing.txt"
        sink(fileName_params) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
        print(summary)
        sink()
      }
    
    } else {
        weights <- mSetObj$penReg$weights #Weights is held separate from dataSet, and can therefore only be accessed in the penalized regression analysis pathway.
        weights <- unlist(weights) #Change weights from a dataframe into a vector of numeric values
    
        if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
          if (method == "elastic net") {
            params <- train(
              predictors_train, response_train, weights=weights, method="glmnet", 
              trControl=trainControl("cv", number=10), tuneLength=5)
            model <- glmnet(predictors_train, response_train, alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                            weights=weights, family=family)
            method <- "Multivariate Elastic Net Regression"
            cat("The elastic net regression model was optimized using alpha = 0 and lambda = ", params$bestTune$lambda, ".", sep="")
            cat("Note that if alpha = 1, elastic net regression was equivalent to lasso regression.")
            
            #Extract model coefficients and write to csv file
            coef <- coef(model) 
            coeffs <- data.frame(coef@Dimnames[[1]], coef@x) #Need loop for when coeff = 0!!!!!!!!!
            colnames(coeffs) <- c("Variable", "Coefficient")
            write.csv(coeffs, file="elastic_net_regression_coefficients.csv", row.names=FALSE)
            
            #Generate summary of parameter testing and write to txt document
            summary <- params 
            fileName_params <- "elastic_net_regression_parameter_testing.txt"
            sink(fileName_params) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
            print(summary)
            sink()
            
          } else if (method == "lasso") {
            lambda <- 10^seq(-3, 3, length = 100)
            params <- train(
              predictors_train, response_train, weights=weights, method="glmnet", 
              trControl=trainControl("cv", number=10),
              tuneGrid = expand.grid(alpha=1, lambda=lambda))
            model <- glmnet(predictors_train, response_train, alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                            weights=weights, family=family)
            method <- "Multivariate Lasso Regression"
            cat("The lasso regression model was optimized using alpha = 0 and lambda = ", params$bestTune$lambda, ".", sep="")
            
            #Extract model coefficients and write to csv file
            coef <- coef(model) 
            coeffs <- data.frame(coef@Dimnames[[1]], coef@x) #Need loop for when coeff = 0!!!!!!!!!
            colnames(coeffs) <- c("Variable", "Coefficient")
            write.csv(coeffs, file="lasso_regression_coefficients.csv", row.names=FALSE)
            
            #Generate summary of parameter testing and write to txt document
            summary <- params 
            fileName_params <- "lasso_regression_parameter_testing.txt"
            sink(fileName_params) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
            print(summary)
            sink()
            
          } else {
            
            lambda <- 10^seq(-3, 3, length = 100)
            params <- train(
              predictors_train, response_train, weights=weights, method="glmnet", 
              trControl=trainControl("cv", number=10),
              tuneGrid = expand.grid(alpha=0, lambda=lambda))
            model <- glmnet(predictors_train, response_train, alpha=params$bestTune$alpha, lambda=params$bestTune$lambda, 
                            weights=weights, family=family)
            method <- "Multivariate Ridge Regression"
            cat("The ridge regression model was optimized using alpha = 0 and lambda = ", params$bestTune$lambda, ".", sep="")
            
            #Extract model coefficients and write to csv file
            coef <- coef(model) 
            coeffs <- data.frame(coef@Dimnames[[1]], coef@x)
            colnames(coeffs) <- c("Variable", "Coefficient")
            write.csv(coeffs, file="ridge_regression_coefficients.csv", row.names=FALSE)
            
            #Generate summary of parameter testing and write to txt document
            summary <- params 
            fileName_params <- "ridge_regression_parameter_testing.txt"
            sink(fileName_params) #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report
            print(summary)
            sink()
          }
        } else {
            cat("The length of the weights vector does not equal the number of rows in the data set!")
          }
      }
  
  prediction <- predict(model, newx=as.matrix(test_data)) #Need to create loop for when family="multinomial"
  
  mSetObj$analSet$penReg$mod <- model
  mSetObj$analSet$penReg$meth <- method
  mSetObj$analSet$penReg$pred <- prediction
  mSetObj$analSet$penReg$params <- params
  mSetObj$analSet$penReg$train <- train_data 
  mSetObj$analSet$penReg$test <- test_data
  
  return(.set.mSet(mSetObj))
}


#'Plot line of best fit for linear regression with one predictor variable
#'@description Scatter plot with line of best fit, where response variable is x and predictor variable is y
#'@usage Plot.linReg1(mSetObj, imgName, format="png", dpi=72, width=NA)
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

plot.pred.penReg <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  method <- mSetObj$analSet$penReg$meth
  prediction <- mSetObj$analSet$penReg$pred
  test_data <- mSetObj$analSet$penReg$test
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");

  if(is.na(width)){
  w <- 10.5
  } else if(width == 0){
   w <- 7.2
  } else{
  w <- width
  }

  mSetObj$imgSet$plot.pred.penReg <- imgName

  h <- w

  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  plot(prediction, test_data[,1], xlab="Predicted", ylab="Actual", main=method)
  abline(a=0,b=1)
  dev.off()

  return(.set.mSet(mSetObj))
}




##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################