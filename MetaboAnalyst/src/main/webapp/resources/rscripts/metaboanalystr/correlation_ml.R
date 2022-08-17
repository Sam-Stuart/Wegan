#'Perform Machine Learning Regression'
#'@description Build a linear regression model for one user selected predictor variable
#'@usage reg.machine.anal(mSetObj=NA, method=method)
#'@param mSetObj Input the name of the created mSetObj
#'@param method Set ML regression method, default is random forest
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

ml.reg.anal <- function(mSetObj=NA,
                             method="random forest",
                              data="false" 
                             ) {
  
  #install.packages(c("e1071", "randomForest"))
  library("e1071")
  library("randomForest")
  library("Metrics")
  
  mSetObj <- .get.mSet(mSetObj)

  ### SET DATA (whether to use original data or not)
  if (data == "false") {
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #Text should be visable to user
  AddErrMsg("The first column will be the response variable. The remaining columns will be the predictor variables.")
  AddErrMsg("Response variable must be numeric for machine regression analysis. Predictor variables can be numeric or categorical.") 
  AddErrMsg("For categorical variables, make sure to use characters for the levels and not numbers. For example, if you have levels 1, 2 and 3, change the level labels to I, II and III.")
  
  #Generate test and train data for model building
  set.seed(37)
  index <- sample(1:nrow(input), 0.7*nrow(input))
  train_data <- input[index,]
  test_data <- input[-index,]
  predictors_train <- model.matrix(train_data[,1] ~ ., train_data)[,-1] # Train predictor variables, creating dummy variables for categorical variables
  predictors_test <- model.matrix(test_data[,1] ~ ., test_data)[,-1] # Test predictor variables, creating dummy variables for categorical variables
  response_train_name <- colnames(input)[1] #response_train variable name
  predictors_train_name <- colnames(predictors_train)[-1] #response_train variable name
  #Text should be visable to user
  cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.")
  
  #Generate formula
  formula <- as.formula(paste(response_train_name, "~", paste(predictors_train_name, collapse = "+")))
  
  if (method == "SVM") {
    
    #Build model
    model <- e1071::tune(e1071::svm, formula,  data = as.data.frame(predictors_train), ranges = list(epsilon = seq(0,1,0.1), cost = 2^(seq(0.5,8,.5))))
    tunedModel <- model$best.model
    model_name <- "SVM Regression"
    
    #Extract predicted values
    prediction <- predict(tunedModel, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
    
    #Store results for plotting
    mSetObj$analSet$svmReg$meth <- model_name
    mSetObj$analSet$svmReg$pred <- prediction
    mSetObj$analSet$svmReg$test <- test_data
    
    #Generate and download summary of parameter testing and write to txt document
    summary <- summary(tunedModel) 
    residuals <- residuals(tunedModel)
    decision_values <- tunedModel[["decision.values"]]
    fitted <- predict(tunedModel)
    svm_RMSE <- Metrics::rmse(predictors_train[,1], fitted)
    fileName <- "ML_regression_summary.txt"#"SVM_regression_summary.txt"
    
    #Store results
    mSetObj$analSet$svmReg$res <- list(summary = summary, predicted.values = fitted, residuals = residuals, decision.values = decision_values, RSME = svm_RMSE, fileName = fileName)       
    mSetObj$analSet$svmReg$mod <- list(model_name = model_name, model = model, response = response_train_name, predictor = predictors_train_name)
  
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    # cat("\nReference category:\n")
    # cat(paste0(reference))
    print(summary)
    cat("Residuals:\n")
    print(residuals)
    cat("\nDecision values:\n")
    print(decision_values)
    cat("\nPredicted values:\n")
    print(fitted)
    cat("\nRMSE:\n")
    cat(paste0(svm_RMSE))
    sink()
    
  } else { #Method is random forest
    
    #Build model
    model <- randomForest::tuneRF(y = train_data[,1], x = predictors_train[,-1], ntreeTry = 500, stepFactor = 2, improve = 0.05, trace = TRUE, doBest = TRUE, plot = FALSE, importance = TRUE)
    model_name <- "Random Forest Regression"
    
    #Extract predicted values
    prediction <- predict(model, newdata = as.matrix(predictors_test)) #Need to create loop for when family="multinomial"
    
    #Store results for plotting
    mSetObj$analSet$rfReg$meth <- model_name
    mSetObj$analSet$rfReg$pred <- prediction
    mSetObj$analSet$rfReg$test <- test_data
    
    #Generate and download summary of parameter testing and write to txt document
    summary <- model 
    predictor_importance <- randomForest::importance(model)
    fitted <- predict(model)
    svm_RMSE <- Metrics::rmse(predictors_train[,1], fitted)
    fileName <- "ml_regression_summary.txt"#"random_forest_regression_summary.txt"
    
    #Store results
  mSetObj$analSet$rfReg$res <- list(summary = summary, predicted.values = fitted, RSME = svm_RMSE, predictor.importance = predictor_importance, fileName = fileName)   
   mSetObj$analSet$rfReg$mod <- list(model_name = model_name, model = model, response = response_train_name, predictor = predictors_train_name)
    
    #Download text document containing the results, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
    sink(fileName) 
    cat("Formula:\n")
    print(formula)
    # cat("\nReference category:\n")
    # cat(paste0(reference))
    print(model)
    cat("\nPredicted values:\n")
    print(fitted)
    cat("\nRMSE:\n")
    cat(paste0(svm_RMSE, "\n"))
    cat("\nPredictor variable importance:\n")
    print(predictor_importance)
    sink()
    
  } 
  
  return(.set.mSet(mSetObj))
  
}  

#'Plot svm predicted vs actual data plot with line of best fit
#'@description Scatter plot with line of best fit, where response variable is y and predictor variable is x
#'@usage plot.pred.svmReg(mSetObj, method=method, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param method Set ML regression method, default is random forest
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

ml.pred.plot <- function(mSetObj=NA,
                         method="random forest",
  facA = "NULL",
  data="false",
  
  col_dots="NULL",
  col_line="NULL", 
  plot_ci="false",
  plot_title=" ",
  plot_ylab=" ",
  plot_xlab=" ",
                         imgName, format="png", dpi=72, width=NA){
  
  ## used to be called: plot,pred.MLReg
  ## 
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  
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
  
  
  if (method=="SVM") {
    facA <- mSetObj$analSet$svmReg$mod$response
    method <- mSetObj$analSet$svmReg$meth
    prediction <- mSetObj$analSet$svmReg$predicted.values
    test_data <- mSetObj$analSet$svmReg$test
    input <- test_data
    
  dfpred <- data.frame(fpred = prediction, fA = input[,facA])
  formula2 <- as.formula("fA ~ fpred")
  model2 <- lm(formula = formula2, data = dfpred)
    
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.svmReg <- imgName
    
  
  } else { #random forest is default
    
    facA <- mSetObj$analSet$rfReg$mod$response
    method <- mSetObj$analSet$rfReg$meth
    prediction <- mSetObj$analSet$rfReg$predicted.values
    test_data <- mSetObj$analSet$rfReg$test
    input <- test_data
    
  dfpred <- data.frame(fpred = prediction, fA = input[,facA])
  formula2 <- as.formula("fA ~ fpred")
  model2 <- lm(formula = formula2, data = dfpred)
    
  #NAME PLOT FOR DOWNLOAD
  ### must put imgName2 first, re-writing imgName var in next line
  imgName2 <- paste(gsub( "\\_\\d+\\_", "", imgName),
 ".json", sep="") 
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.pred.rfReg <- imgName
  
  }
  
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
  if (method == "SVM") { 
  mSetObj$analSet$svmReg$plotpred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)
    } else {
  mSetObj$analSet$rfReg$plotpred <- list(plot = a0, title = plot_title1, xlab = plot_xlab1, ylab = plot_ylab1)  
  } 
    
  #JSON OBJECT MAKING
  build <- ggplot_build(a0)
  build_line <- build$data[[1]]
  build_points <- build$data[[2]]
  linear_plot_json <- list()
  
  linear_plot_json$main <- plot_title1 #title
  linear_plot_json$axis <- c(plot_xlab1, plot_ylab1) #axis titles
  linear_plot_json$points$coords <- build_points[,c("x","y")] #[,1:2]
  linear_plot_json$points$cols <- build$data[[1]][,grepl("col",colnames(build_points))] #[,6] #colours
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

