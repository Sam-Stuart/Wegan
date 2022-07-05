pcoa.meta.columns <- function(mSetObj=NA) {
  
  mSetObj <- .get.mSet(mSetObj)

print("no no no")
  
  if("pcoa" %in%  names(mSetObj$analSet) ){
  if("metaData" %in% names(mSetObj$analSet$pcoa)){
    metaData <- mSetObj$analSet$pcoa$metaData
  }
} else{
  metaData <-"No groupings"
}
  if (is.data.frame(metaData)==FALSE) {
    name.all.meta.cols <- "No groupings"
  } else {
    name.all.meta.cols <- c(colnames(metaData), "No groupings")
  }
  return(name.all.meta.cols)
  
}

pcoa.scree.dim <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj)

print("no2 no2 no2")
  
  if("pcoa" %in%  names(mSetObj$analSet) ){
  if("input" %in% names(mSetObj$analSet$pcoa)){
     num_data <- mSetObj$analSet$pcoa$input
  }
} else{
  num_data <-data.frame(one=c(1,2,3,4),two=c(5,6,7,8))
}
  dim <- ncol(num_data)
  max <- min(dim, 8)
  pcoa.scree.dim.opts <- 2:max
  return(pcoa.scree.dim.opts)
}

ord.pcoa.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)

print("no3 no3 no3")
  
  if("pcoa" %in%  names(mSetObj$analSet) ){
  if("input" %in% names(mSetObj$analSet$pcoa)){
     ord.pcoa.result <- c(mSetObj$analSet$pcoa)
  }
} else{
   ord.pcoa.result <- "the_result"
}
  return(ord.pcoa.result)
  
}

ord.pcoa <- function(mSetObj=NA, data="false", distance="NULL", binary="false", abundance="false", env_text=" ") { #4 user inputs, plus option to upload 2 supplementary data sets
  
  options(error=traceback)

  library("vegan") #For generating distance matrix
  library("dplyr") #For easy data manipulation

  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="false") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  metaData <- mSetObj$dataSet$origMeta
  envData <- mSetObj$dataSet$origEnv

  #Obtain categorical data for making dummy variables
  num_data <- select_if(input, is.numeric)

  #Transform abundance data
  if (abundance=="false") {
    abundance1 <- "absolute"
    num_data1 <- num_data #Default abundance is absolute and no change is made to data
  } else {
    abundance1 <- "relative"
    num_data1 <- decostand(num_data, method="total") #Alternative option is relative abundance, each value is divided by the column sum
  }
  
  #Combine numeric data and dummy variables #I REMOVED DUMMIES SINCE WE CANNOT USE SAME DIST MEASURES FOR BOTH NUMERIC AND CATEGORICAL VARS
  data <- num_data1
  if (ncol(data)<2) {
    #AddErrMsg("Ordination requires at least 2 variables!")
    stop("Ordination requires at least 2 variables!")
  } 
  #Set distance measure for creation of dissimilarity matrix
  if (distance=="NULL") {
    distance1 <- "bray" #Default distance
  } else {
    distance1 <- distance #USer selected from list "bray", "manhattan", "canberra", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
  } 

  #Generate dissimilarity matrix
  if (binary=="false") {
    dist <- vegdist(data, method=distance1, binary=FALSE) #Generate dissimilarity matrix
  } else {
    dist <- vegdist(data, method=distance1, binary=TRUE) #Generate dissimilarity matrix for presence/absence data
  }

  #Run PCOA 
  pcoa <- wcmdscale(dist, add="lingoes", eig=TRUE, x.ret=TRUE)

  #meta data, used to group samples using colors
  if (is.data.frame(metaData)==FALSE) { #No user uploaded meta data
    metaData1 <- "NA" #If species data had no categorical columns, meta data is NULL
    print("No grouping variables were uploaded!")  
  } else {  #User uploaded meta data
    metaData1 <- metaData #User uploaded like weights in correlation module
    if (nrow(metaData1)!=nrow(input)) {
      #AddErrMsg("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
      stop("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
    }
    for(i in 1:ncol(metaData1)) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }

  #environmental data, used to correlate with rows in main data set
  if (is.data.frame(envData)==FALSE) { #No user uplaoded environmental data
    envData1 <- "NA"
  } else {  #User uplaoded environmental data
    envData1 <- envData #User uploaded (like weights in correlation module)
    if (nrow(envData1)!=nrow(input)) {
      #AddErrMsg("Your environmental data does not have the same number of rows as your data set! Please check that you environmental data is correct.")
      stop("Your environmental data does not have the same number of rows as your data set! Please check that you environmental data is correct.")
    }
  }
  
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default env_text is second column.
  if (is.data.frame(envData1)==TRUE) { #User uplaoded environmental data
    cat("You uploaded environmental data. Identify environmental variables for PCOA using the column names with commas in between.")
  }
  
  #Set up environmental data using user selected columns
  if (is.data.frame(envData1)==TRUE) { #User uplaoded environmental data
    if (env_text==" ") { #User doesn't specify columns-- all columns used
      env_text1 <- colnames(envData1) #Default is the all env columns
      cat(paste0("You have selected these constraining variables: ", paste(env_text1, collapse=", ")))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
      
    } else { #User enters columns
      env_text1 <- env_text #taken from text box by java, fed as string into R code
      env_text1 <- gsub("\n", "", env_text1, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
      env_text1 <- gsub(",", "+", env_text1, fixed=TRUE) 
      env_text1 <- gsub(";", "+", env_text1, fixed=TRUE)
      env_text1 <- gsub(" ", "", env_text1, fixed=TRUE)
      env_text1 <- gsub(":", "+", env_text1, fixed=TRUE)
      env_text1 <- gsub("*", "+", env_text1, fixed=TRUE)
      cat(paste0("You have selected these constraining variables: ", gsub("+", ", ", env_text1, fixed=TRUE), "."))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
    }
    
    env_cols <- unlist(strsplit(env_text1, "+", fixed=TRUE)) #Extract column names from env_text1
    env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)]) #Subset environmental data set to select columns of interest
    colnames(env_data) <- env_cols #Name columns
  } else { #No environmetal data uploaded
    env_data <- "NA"
  }
  
  #Fit environmental data to ordination plots for plotting arrows
  if (is.data.frame(env_data)==FALSE) { #If environmental data not uploaded
    env.fit <- "NA"
    env.fit.char <- "NA"
    env.fit.num <- "NA"
  } else { #If environmental data uploaded
    env_data_numeric <- select_if(env_data, is.numeric)
    env_data_character <- select_if(env_data, is.character)
    env.fit <- envfit(pcoa, env_data, permutations=999, p.max=NULL)
    if (ncol(env_data_character)>0) { #If categorical variables present
      env.fit.char <- envfit(pcoa, env_data_character, permutations=999, p.max=NULL) #Fit env data to species data
    } else{
      env.fit.char <- "NA"
    }
    if (ncol(env_data_numeric)>0) { #If numeric variables present
      env.fit.num <- envfit(pcoa, env_data_numeric, permutations=999, p.max=NULL) #Fit env data to species data
    } else{
      env.fit.num <- "NA"
    }
  }

  #Extract environment scores
  if (is.data.frame(env_data)==FALSE) { #If environmental data not uploaded
    env.scores <- "NA"
  } else { #If environmental data uploaded
    if (length(env_data_numeric)>0) { 
      env.scores.num <- signif(scores(env.fit.num, display="vectors"), 5)
    } else {
      env.scores.num <- "NA"
    }
    
    if (length(env_data_character)>0) {
      env.scores.char <- signif(scores(env.fit.char, display="factors"), 5)
    } else {
      env.scores.char <- "NA"
    }
    
    if (is.matrix(env.scores.num)==TRUE) { #Numeric constraining variables
      if (is.matrix(env.scores.char)==TRUE) { #Categorical constraining variables
        env.scores <- rbind(env.scores.num, env.scores.char)
      } else {  #No categorical constraining variables
        env.scores <- env.scores.num
      }
    } else { #No numeric constraining variables
      env.scores <- env.scores.char
    }
  }
  
  #Fit variables to ordination plots for plotting arrows
  var.fit <- envfit(pcoa, data, permutations=999, p.max=NULL)
  
  #Store results in mSetObj$analSet$pcoa
  mSetObj$analSet$pcoa$pcoa <- pcoa
  mSetObj$analSet$pcoa$distance <- distance1
  mSetObj$analSet$pcoa$input <- data
  mSetObj$analSet$pcoa$eigenvalues <- pcoa$eig
  mSetObj$analSet$pcoa$var.fit <- var.fit
  mSetObj$analSet$pcoa$env.fit <- env.fit
  mSetObj$analSet$pcoa$env.fit.char <- env.fit.char
  mSetObj$analSet$pcoa$env.fit.num <- env.fit.num
  mSetObj$analSet$pcoa$metaData <- metaData1
  mSetObj$analSet$pcoa$env_data <- env_data
  
  #Output tables to save in working directory
  eigenValues_data <- cbind(pcoa$eig, pcoa$eig/sum(pcoa$eig))
  eig_rownames <- paste("PCOA ", 1:nrow(eigenValues_data), sep="")
  eigenValues_data <- cbind(eig_rownames, eigenValues_data)
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  
  write.csv(eigenValues_data, file="pcoa_scree_data.csv", row.names=FALSE)
  #write.csv(pcoa$points, file="pcoa_sample_scores.csv", row.names=row.names(input))!!!!!!!!!!!
  write.csv(var.fit$vectors$arrows, file="pcoa_2D_variable_scores.csv", row.names=TRUE)
  #write.csv(pcoa$x, file("pcoa_dissimilarity_matrix.csv", row.names=TRUE))!!!!!!!!!!!!
  if (is.data.frame(env_data)==TRUE) {
    write.csv(env.scores, file="pcoa_2D_constraining_variable_scores.csv", row.names=TRUE)
  } else {
    write.csv(NULL, file="pcoa_2D_constraining_variable_scores.csv", row.names=FALSE)
  }

  sink("variable_impact_on_pcoa.txt") 
  cat("Data columns may significantly impact PCOA\n")
  print(var.fit)
  sink() 
  
  if (is.data.frame(env_data)==TRUE) {
    sink("constraining_variables_impact_on_pcoa.txt") 
    cat("Constraining data may significantly impact PCOA\n")
    print(env.fit)
    sink() 
  }
  
  #Summary elements
  eigenValues_t <- as.data.frame(t(eigenValues_data))
  colnames(eigenValues_t) <- NULL
                
  sink("pcoa_summary.txt")
  cat("Principal Coordinate Analysis\n")
  cat("\nCall:\n")
  print(pcoa$call)
  cat(paste0("\nDistance metric:"), distance1, "\n")
  if (abundance=="absolute") {
    abundance1 <- "false"
  } else {
    abundance1 <- "true"
  }
  cat(paste0("\nPerform relative abundance transformation: "), abundance1, "\n")
  cat("\nSample Scores:\n")
  print(as.data.frame(pcoa$points))
  cat("\nVariable Scores:\n")
  print(var.fit$vectors$arrow)
  cat("\nConstraining Scores:\n")
  print(env.scores)
  cat("\nEigen values:")
  print(eigenValues_t)
  cat(paste0("\nAdditive constant calculated using Lingoes procedure for negative eigen value correction: ", pcoa$ac, "\n"))
  cat("\nLengend:\n")
  cat("Site=Samples and Species=Variables\n")
  sink()  
  
  return(.set.mSet(mSetObj))
  
}





#'Produce PCOA 2D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
#'@description Produce PCOA ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "grey"
#'@param ellipse Boolean, TRUE to add confidence ellipses, FALSE (default) to not add confidence ellipses
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param env_arrows Boolean, TRUE to produce environmental arrows, FALSE (default) to produce ordination plot without environmental arrows---only appear if env data uploaded
#'@param env_cent Boolean, TRUE to produce display factor environmental data centroids, FALSE (default) to produce ordination plot without environmental centroids
#'@param sampleNames Boolean, TRUE to display data as variable names, FALSE (default) to display data as points
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
Plot.PCOA.2D <- function(mSetObj=NA, ellipse="false", var_arrows="false", env_arrows="false", env_cent="false", sampleNames="false", color="NULL", meta_col_color="NULL", imgName, format="png", dpi=72, width=NA) { #5 check boxes, 3 drop downs
  
  options(error=traceback)

  library("vegan")
  library("viridis") 
  library("dplyr") 

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  pcoa <- mSetObj$analSet$pcoa$pcoa
  metaData <- mSetObj$analSet$pcoa$metaData
  input <- mSetObj$analSet$pcoa$input
  var.fit <- mSetObj$analSet$pcoa$var.fit
  env.fit.char <- mSetObj$analSet$pcoa$env.fit.char
  env.fit.num <- mSetObj$analSet$pcoa$env.fit.num
  env_data <- mSetObj$analSet$pcoa$env_data

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.PCOA.2D <- imgName
  
  #Produce 2D ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  ordiplot(pcoa$points, type="n", main="Principal Coordinate Analysis", yaxt="n", xlab="PCOA 1", ylab="PCOA 2", choices=c(1,2)) #Empty ordination plot 
  axis(2, las=2)
  
  #Plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE || meta_col_color=="No groupings") { #If no meta data

    #point options
    if (sampleNames!="false") { #If display data as lables
      text(pcoa$points) #Add text for samples
    } else {
      points(pcoa$points, pch=19, col="black") #Add text for samples, all black since grouping data is unavailable
    }
    
    #variable arrow options
    if (var_arrows!="false") { #If variable arrows selected
        plot(var.fit, col="darkred")        
    }
    
    #Env data options
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows!="false") { #If environment arrows selected
        num_env_data <- select_if(env_data, is.numeric)
        if (!is.null(num_env_data)){
            plot(env.fit.num, col="blue", lwd=2)
        }
      }
      
      if (env_cent!="false") { #If environment constraints selected
        fac_env_data <- select_if(env_data, is.character)
        if (!is.null(fac_env_data)){
            plot(env.fit.char, col="blue", lwd=2)
        }
      }
    }
    
  } else { #If meta data available
    
    #Set up meta data column to use for colors
    if (meta_col_color=="NULL") { 
      meta_col_color_data <- as.factor(metaData[,1]) #Default meta data column for labeling with color is the first
      meta_col_color_name <- colnames(metaData)[1]
    } else {
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User inputted meta data column for labeling with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color
    }

    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the meta data
    if (color=="NULL") {
      colors <- viridis(n)#Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1)#Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
    } 
    
    #Points options
    cols <- colors[meta_col_color_data] #Color palette applied for groupings of user's choice
    if (sampleNames!="false") { #If display data as lables
      with(metaData, text(pcoa$points, col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      with(metaData, points(pcoa$points, col=cols, pch=19, bg=cols)) 
    }
    
    #Arrow options
    if (var_arrows!="false") { #If variable arrows selected
      plot(var.fit, col="darkred", lwd=2)
    }
    
    #Ellipse option
    if (ellipse!="false") { #if ellipses selected
      with(metaData, ordiellipse(pcoa, meta_col_color_data, kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #Legend
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
    
    #Env arrows and centroids
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows!="false") { #If environment arrows selected
        num_env_data <- select_if(env_data, is.numeric)
        if (ncol(num_env_data)!=0){
            plot(env.fit.num, col="blue", lwd=2)
        } 
      }
      
      if (env_cent!="false") { #If environment constraints selected
        fac_env_data <- select_if(env_data, is.character)
        if (ncol(fac_env_data)!=0){
            plot(env.fit.char, col="blue", lwd=2)
        }
      }
    }
  }
  
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}






#'#'Generate linear regression plot
#'@description Plot line of best fit on scatter plot of linear regression model on 2 variables in data
#'@param mSetObj Input the name of the created mSetObj
#'@param facA Input the name of the response column (java uses Columns() to give user options)
#'@param facB Input the name of the predictor column (java uses Columns() to give user options)
#'@param data Boolean, whether to use original data; "false" (default) means normalized or "true" means original (checkbox)
#'
#'@param col_dots Set color for scatterplot dots (default "NULL" is black); (static dropdown)
#'@param col_line Set color for line (default "NULL" is black); (static dropdown)
###@param weights Set weight values, default is NULL
#' @param plot_ci Boolean, "false" (default), omit 95% confidence interval around line, "true" add interval around line
#'@param no_plot_eq Boolean, "false" (default) to show linear model equation on plot, "true" for plot without annotation of model equation (at top); y is 0.75*max(y) (checkbox)
#'@param no_plot_rsq Boolean, "false" (default) to show linear model rsq value on plot, "true" for plot without annotation of rsq value (at top); y is 0.75*max(y) (checkbox)
#'
#'@param plot_rsq_adj Boolean, "true" to show linear model adjusted rsq value on plot, "false" (default) for plot without annotation of adjusted rsq value (at top); y is 0.75*max(y) (checkbox)
#'
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width. 
#'@author Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
lin.reg.plot <- function(mSetObj=NA, facA="NULL", facB="NULL",
             data="false", col_dots="NULL", col_line="NULL", plot_ci="false",# weights=NULL,
  # plot_eq="true", plot_rsq="true", plot_rsq_adj="false",
  no_plot_eq="false", no_plot_rsq="false", plot_rsq_adj="false",
                  imgName, format="png", dpi=72, width=NA
  ){

  library("ggpmisc")
  library("ggplot2")
  
  mSetObj <- .get.mSet(mSetObj)
  
  # mSetObj$dataSet$norm <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),,drop=FALSE]

   ### SET DATA (whether to use original data or not)
  if (data=="false") { 
    input <- mSetObj$dataSet$norm #default use norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  ### SET VARIABLES
  #Set dependent (response) variable name
  if (facA == "NULL"){
    facA <- colnames(input)[1] #Default is 1st column.
  } else {
    facA <- facA #Determined using Columns() function below (java will present options in drop down menu)
  }
  #Set independent (predictor) variable name
  if (facB == "NULL"){
    facB <- colnames(input)[2] #Default is 2nd column.
  } else {
    facB <- facB #Determined using Columns() function below (java will present options in drop down menu)
  }
  
  # VARIABLE TYPE CHECK
  if (is.factor(mSetObj$dataSet$norm[,facA] || mSetObj$dataSet$norm[,facB])==TRUE){
    #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
    stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
  }

  #DEFINE FORMULA
  formula <- as.formula(paste0(facA, "~", facB)) 

  #GENERATE MODEL, with or without weights
  # if (is.null(weights)==TRUE) {
    model <- lm(formula=formula, data=input,#mSetObj$dataSet$norm,
                weights=NULL) #Create linear model, no weights
 #  } else {
 #    weights <- weights #Java upload weights as a vector of numeric values
 #    if (length(weights) == nrow(mSetObj$dataSet$norm)) { #There must be one weight for every row in the data set
 #      model <- lm(formula=formula, data=input, #mSetObj$dataSet$norm,
 # weights=weights) #Create linear model, with weights
 #    } else {
 #      #AddErrMsg("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
 #      stop("The length of the weights vector does not equal the number of rows in the data set! Check that the weights vector is correct.") #Error msg 
 #    }
 #  }
 
    
  ## alpha(yint) + Beta(slope) * X
  # EXTRACTING MODEL RESULTS
  # later, print()/cat() these:
  summary <- summary(model) #PRINT #Summary w coeff, resid & fit
  fitted<-fitted(model) #PRINT
  covar<-vcov(model) #PRINT
  conf.int<-confint(model, level=0.95) #PRINT
  
  fileName <- paste0("linear_regession_summary_", facA, "~", facB, ".txt") #File name for summary
  coeffs <- summary[["coefficients"]] #Extract model coefficients
  beta <- round(coeffs[2], digits = 2)
  alpha <- round(coeffs[1], digits = 2)
  equation <- paste(facA, " = ",
 paste( paste(beta, facB, sep="*"), alpha, sep=" + ") ) # equation with intercept, coefficient and predictor variable name
  r_sq <- round(summary[["r.squared"]], digits = 2) #Extract R^2
  r_sq_adj <- round(summary[["adj.r.squared"]], digits = 2) #Extract adjusted R^2 value

  # #Test residuals for normality. Error will be visible to user.
  # norm_resid <- shapiro.test(residuals) 
  # if (norm_resid$p.value < 0.05){
  #   norm_resid_text <- paste0("The residuals are normally distributed. This model is valid.") #To be used in summary, not to be displayed
  # } else {
  #   #AddErrMsg("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  #   stop("The residuals are NOT normally distributed. This model is invalid. Try other preprocessing options, or try other regression models such as SVM or random forest.")
  # }
 
   # STORE MODEL
  mSetObj$analSet$linReg1$mod <- model

  #STORE RESULTS: x/y var names, model summary, fitted values,conf intervals, covar mat, equation, rsq, rsq_adj, filename, formula
  ### change response to xlab, predictor to ylab
  mSetObj$analSet$linReg1$res <- list(response=facA, predictor=facB, summary=summary, predicted.values=fitted, 
confidence.intervals=conf.int, covariance.matrix=covar, equation=equation, r.squared.eq=paste("R-squared = ", r_sq),
 r.squared.adj.eq=paste("R-squared adjusted = ", r_sq_adj), fileName=fileName, formula=formula
) #Note where the summary is stored
  #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  
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
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$plot.linReg1 <- imgName
  
    #SET POINT COLOR
  if (col_dots == "NULL") {
      col_dots1 <- "black" # default
    } else if (col_dots == "blue") {
      col_dots1 <- "blue"
    } else if (col_dots == "red") {
      col_dots1 <- "red"
    } else if(col_dots == "green"){
      col_dots1 <- "green"
    } else if(col_dots == "grey"){
      col_dots1 <- "grey"
    }
  
  #SET LINE COLOR
  if (col_line == "NULL") {
      col_line1 <- "black" # default
    } else if (col_line == "blue") {
      col_line1 <- "blue"
    } else if (col_line == "red") {
      col_line1 <- "red"
    } else if(col_line == "green"){
      col_line1 <- "green"
    } else if(col_line == "grey"){
      col_line1 <- "grey"
    }
  
  #SET WHETHER TO ADD 95% CONF INT
  if (plot_ci == "false") {
      plot_ci1 <- FALSE # default
    } else {
      plot_ci1 <- TRUE
    }

  # #IF THERE IS A LINE COLOR, OVERRIDE DOT COLOR TO BLACK
  # if(!all.equal(col_line1, "black")){
  #   col_dots1 <- "black"
  # }
  
  #GENERATE PLOT
   Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
 
  # THEME 
  #  theme_lineplot <- theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size=12, colour="black"),    axis.title=element_text(size=12), legend.title=element_text(12), legend.text=element_text(size=12), plot.title=element_text(face='bold',hjust = 0.5)
  #   )
  
 ## NOTE THE .data ARGUMENT; 
 ##  To avoid a note from CMD check about .data, use #' @importFrom rlang .data
 ## in any roxygen code block (typically in package documentation as generated by
 ## usethis::use_package_doc()) ; https://ggplot2.tidyverse.org/articles/ggplot2-in-packages.html

a0 <- ggplot(data = input,
   # aes(x = .data[[facA]], y = .data[[facB]]) ) +
    aes_(x = as.name(facA), y = as.name(facB)) )+
   labs(title="Univariate Linear Regression Line of Best Fit") +
     ylab(facB)+ xlab(facA) +
     geom_smooth(se=plot_ci1, color=col_line1, fullrange = TRUE, method='lm') +
     geom_point(shape=16, color=col_dots1) +
     theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
axis.text = element_text(size=12, colour="black"), 
   axis.title=element_text(size=12),
 # legend.title=element_text(12), legend.text=element_text(size=12), 
plot.title=element_text(face='bold',hjust = 0.5)
  )

## {GGPMISC} ANNOTATION LOOP   
   if (plot_rsq_adj == "false"){ # default
  ### EQUATION, RSQ
   if (no_plot_eq == "false" && no_plot_rsq == "false") { #default
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label),  sep = "*\"  |  \"*")),
      eq.with.lhs =  paste0("italic(`",facB,"`)~`=`~"),
      eq.x.rhs =  paste0("~italic(`*` ~`",facA,"`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) )
  ### EQUATION
    } else if (no_plot_eq == "false" && no_plot_rsq == "true") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
    paste0(after_stat(eq.label) )),
    eq.with.lhs =  paste0("italic(`",facB,"`)~`=`~"),
    eq.x.rhs =  paste0("~italic(`*` ~`",facA,"`)"),
    rr.digits = 2, coef.digits = 2, parse = TRUE,
    label.y = 0.75 * max(input[,facB]) ) 
  ### NOTHING
    } else if (no_plot_eq == "true" && no_plot_rsq == "true") {
      a0 <- a0
  ### RSQ
    } else if (no_plot_eq == "true" && no_plot_rsq == "false") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
      paste0(after_stat(rr.label) )),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) ) 
    }
   
   } else if (plot_rsq_adj == "true"){
  ### EQUATION, RSQ, RSQ_ADJ     
   if (no_plot_eq == "false" && no_plot_rsq == "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(adj.rr.label),
      sep = "*\"  |  \"*")), size = 3,
      eq.with.lhs =  paste0("italic(`",facB,"`)~`=`~"),  eq.x.rhs =  paste0("~italic(`*` ~`",facA,"`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) ) 
  ### EQUATION, RSQ_ADJ 
    } else if (no_plot_eq == "false" && no_plot_rsq == "true") {
   a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "*\"  |  \"*")),
      eq.with.lhs =  paste0("italic(`",facB,"`)~`=`~"),
      eq.x.rhs =  paste0("~italic(`*` ~`",facA,"`)"),
      rr.digits = 2, coef.digits = 2, parse = TRUE,
      label.y = 0.75 * max(input[,facB]) ) 
  ### RSQ_ADJ 
    } else if (no_plot_eq == "true" && no_plot_rsq == "true") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = 
       paste0(after_stat(adj.rr.label) )),
       rr.digits = 2, coef.digits = 2, parse = TRUE,
       label.y = 0.75 * max(input[,facB]) ) 
  ### RSQ, RSQ_ADJ 
    } else if (no_plot_eq == "true" && no_plot_rsq == "false") {
     a0 <- a0 + ggpmisc::stat_poly_eq(aes(label = paste(after_stat(rr.label), after_stat(adj.rr.label),  sep = "*\"  |  \"*")),
       rr.digits = 2, coef.digits = 2, parse = TRUE,
       label.y = 0.75 * max(input[,facB]) ) 
    } 
     
   }

#STORE IN mset
  mSetObj$analSet$linReg1$plot <- a0

a0

### need these anymore?
 #  sink(fileName) 
 #  cat("Formula:\n")
 #  print(formula)
 #  print(summary)
 #  # print(norm_resid)
 #  # cat("Normality of residuals result:\n")
 #  # cat(paste0(norm_resid_text, "\n"))
 #  cat("\nConfidence intervals for predictor variables:")
 #  print(conf.int)
 #  cat("\nPredicted values:")
 #  print(fitted)
 #  cat("\nCovariance matrix for predictor variables:")
 #  print(covar)
 #  sink()
   
   dev.off()
 
  return(.set.mSet(mSetObj))
  
}


#'JSON object conversion for linear regression plot
#'@description Build JSON object from linear regression plot. mSetObj was used with lin.reg.plot so that 'response', 'predictor', and plot are already stored in the object
#'@param mSetObj Input the name of the created mSetObj

#'@author  Gina Sykes \email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export


lin.reg.plot.json <- function(mSetObj=NA){

  library("ggplot2")
  library("RJSONIO")
  
  mSetObj <- .get.mSet(mSetObj)
  
  imgName <- mSetObj$imgSet$plot.linReg1
  a0 <- mSetObj$analSet$linReg1$plot
  facA <- mSetObj$analSet$linReg1$res$response
  facB <- mSetObj$analSet$linReg1$res$predictor

  build <- ggplot_build(a0)
  # colnames(build$data[[1]]);  c("x", "y", "flipped_aes", "PANEL", "group", "colour", "fill",  "size", "linetype", "weight", "alpha")
  linear_plot_json <- list()
  linear_plot_json$main <- "Univariate Linear Regression Line of Best Fit" #title
  linear_plot_json$axis <- c(facA, facB) #axis titles
  linear_plot_json$points$coords <- build$data[[1]][,c("x","y")] #[,1:2]
  linear_plot_json$points$cols <- build$data[[1]][,grepl("col",colnames(build$data[[1]]))] #[,6] #colours
  linear_plot_json$points$shape <- build$data[[1]][,c("group")]#[,5]
  linear_plot_json$points$size <- build$data[[1]][,c("size")]#[,7]

  imgName <- paste(imgName, ".json", sep="")
  json.obj <- RJSONIO::toJSON(linear_plot_json, .na='null')
  sink(imgName)
  cat(json.obj)
  sink()

  if(!.on.public.web){
    return(.set.mSet(mSetObj))
    }
  
}


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
#   mSetObj$imgSet$plot.linReg1 <- imgName
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
# library("tidyselect")
  
  data <- select_if(mSetObj$dataSet$norm, is.numeric)
# data <- select(mSetObj$dataSet$norm, tidyselect::where(is.numeric)) 
  count.all.numeric.cols <- ncol(data)
  name.all.numeric.cols <- colnames(data)
  
  num.col.results <- list(
    count=count.all.numeric.cols,
    names=name.all.numeric.cols
  )
  
  return(name.all.numeric.cols)
  
}

lin.reg.get.results <- function(mSetObj=NA){

  mSetObj <- .get.mSet(mSetObj)

  lin.reg.result <- c(mSetObj$analSet$linReg1$res$equation,
mSetObj$analSet$linReg1$res$r.squared.eq,
mSetObj$analSet$linReg1$res$r.squared.adj.eq)

  return(lin.reg.result)

}
