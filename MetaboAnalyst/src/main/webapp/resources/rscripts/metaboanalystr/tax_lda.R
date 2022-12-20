
#'Perform LDA
#'@description Linear Discriminant analysis. Used to classify a response variable into two or more classes using a set of predictors. Assumptions: predictor variables have the same variance (could scale 0-1 range). statology.org/linear-discriminant-analysis-in-r/
#'@param mSetObj Input name of the created mSet Object
#'@param data Which data set to use, normalized (default) or original
#'@param facA Input the name of the response column (java uses Columns() to give user options of numeric columns)
#'@param predtext Input predictor data column names (java uses text box to obtain string)
#'@author Gina Sykes\email{gsykes@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

## USE METADATA OR NOT : PROBABLY NOT, SO REMOVE PARTS WITH METADATA 

tax.lda <- function(mSetObj=NA, 
data="false", 
facA = "NULL",
predtext="NULL" # textbox
# env_text=" ",
) { # env_text is textbox input for predictor variable names, separated by comma
  
 # options(error=traceback)

  library('MASS') # lda

  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="false") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  metaData <- mSetObj$dataSet$origMeta
  envData <- mSetObj$dataSet$origEnv

### NOTE ASSUMPTIONS FOR LDA (/QDA) : predictors are normal, equality of covariances among the predictor variables across all levels of the response variable; p must be < n

# CHECK FOR CATEGORICAL COLUMNS (1: check for metadata, if yes check for cat columns 2: if no metatadata, check in input)
 # NO METADATA
if(
 # !is.data.frame(metaData) &&
 # INPUT HAS NO CATEGORICAL 
  !any(sapply(input, is.factor) | sapply(input, is.character))  # || 
 # or HAVE METADATA but
 # is.data.frame(metaData) &&
 # NO CATEGORICAL COLUMNS IN METADATA
 # !any(sapply(metaData, is.factor) | sapply(metaData, is.character))
 ) {
AddErrMsg("No categorical variables (required as response variable); did you input your categorical variables as numbers (for example, as hot-one coded)? Alternatively, try splitting a numeric variable of interest into categories.")
}

### DEPENDENT VARIABLE: CATEGORICAL
### TAKE FROM METADATA #meta data, used to group samples using colors

   # IF NO USER UPLOADED META DATA:
   # assuming '!is.data.frame(metaData)'  will test for presence of metadata
   # needs to be incorporated into column selection function for Webserver (ie. which column names are presented on dropdown)
  if (!is.data.frame(metaData)) { #No user uploaded meta data
    # metaData1 <- "NA" #If species data had no categorical columns, meta data is NULL
    # print("No grouping variables were uploaded!")  

   # GET CATEGORICAL COLUMNS FROM INPUT DATAFRAME
   facdata <- input[,sapply(input, is.factor) | sapply(input, is.character), drop = FALSE]
   
   # IF THERE *IS* USER UPLOADED METADATA: 
  } else {  #User uploaded meta data
    metaData1 <- metaData #User uploaded like weights in correlation module

  # GET CATEGORICAL COLUMNS FROM METADATA
    facdata <- metaData[,sapply(metaData, is.factor) | sapply(metaData, is.character), drop = FALSE]

    if (nrow(metaData1)!=nrow(input)) {
      #AddErrMsg("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
      stop("Your Meta data does not have the same number of rows as your input data! Please check that you Meta Data is correct.")+
    }
    for(i in seq_len(ncol(metaData1)) ) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }


# CHOOSE DEPENDENT VARIABLE
  if (facA == "NULL") {
  facA <- colnames(facdata)[1]
  } else {
    facA <- facA #User selected, java uses function columns() to provide options in drop down menu (only categorical columns are available)
  }

# uc-r.github.io/discriminant_analysis
## PREDICTOR VARIABLES: TEXTBOX

## predtext is input into R as one string from textbox on webpage
#CHECK PREDTEXT FOR COMMAS
  # IF INPUT PREDICTOR NAMES ARE NOT " ", "", and don't contain ","
   if( !any(grepl(",", predtext, fixed = TRUE)) && predtext != "" && predtext != " " ){ 
    # if there are no commas in input predictor name(s)
   if(ncol( input[ , colnames(input) != facA, drop=FALSE] ) > 1){ 
    # can't be >1 other cols to use, so if there is, warning
    warning("Check your predictor variables; Have you separated them by a comma? Are they spelled as they are in your input data?")
  }}

  ## FORMULA SET UP
  #SET RIGHT SIDE OF FORMULA WITH PREDICTOR VARIABLES (ie not facA)
  if (predtext == "NULL") {
    predtext <- colnames( input[, !(colnames(input) == facA), drop = FALSE] )[1] #Default is 1st non-facA column
    # predtext <- paste(colnames(predData), collapse=" , ") # for testing
  } else {
    predtext <- predtext #taken from text box by java, fed as string into R code
  }

  #CURATE RIGHT SIDE OF FORMULA; EXTRACT CHARACTER VECTOR OR PREDICTORS 
   predtext <- gsub("\n", "", predtext, fixed=TRUE) 
   predtext <- gsub(" ", "", predtext, fixed=TRUE) 
   predtext <- gsub(",", "+", predtext, fixed=TRUE)  
   predtext <- gsub(";", "+", predtext, fixed=TRUE) 
   predtext <- gsub(":", "+", predtext, fixed=TRUE) 
   predtext <- gsub("*", "+", predtext, fixed=TRUE)


  #GENERATE FORMULA 
  form <- as.formula(paste0(facA, "~", predtext))

  ## PREDICTORS 
  predictors <- unlist(strsplit(predtext, "+", fixed=TRUE), use.names = FALSE) ## use.names = F speeds unlist
  pred_data <- input[, colnames(input) %in% predictors, drop = FALSE]

 #CHECK IF PREDICTORS ARE ALL FOUND IN DATAFRAME
   # input %>% assertr::verify(assertr::has_all_names(predictors), error_fun = justwarn)

   if( !all(is.element(predictors, colnames(input)) )){
     warning(paste0("tax.lda():", "'",
    paste( setdiff(predictors, colnames(input)), collapse = "', '" ),
    "' not found in data variables ('",  
    paste(colnames(input), collapse = "', '"), "'): check spelling of text box input."))
   }

#CHECK FOR LDA ASSUMPTION: number of p MUST BE < n
#as p approaches n. A simple rule of thumb is to use LDA & QDA on data sets where n>=5×p.
  # IF INPUT PREDICTOR NAMES ARE NOT " ", "", and don't contain ","
   if( length(predictors) => nrow(input) ){ 
   stop("LDA assumes that the number of predictor variables (p) is less than the sample size (n), however this is not true here. Eliminate some predictor variables to continue with LDA.")
  }

# WARN ABOUT DUMMY VARIABLE CREATION IF CAT VARS ARE IN PREDICTORS
# if(any(sapply(pred_data, is.factor) | sapply(pred_data, is.character)) ){
#  warning(paste0("tax.lda():", "'",
#    paste( colnames( pred_data[,sapply(pred_data, is.factor) | sapply(pred_data, is.character)]), collapse = "', '" ),
#    "' are categorical: they will be converted to dummy variables as numeric variables are required"))
# }

## OR IF ONLY WANT NUMERIC PREDICTOR VARIABLES

# WARN IF CATEGORICAL VARIABLES ARE IN PREDICTORS, ONLY NUMERIC ARE USED
 if(any(sapply(pred_data, is.factor) | sapply(pred_data, is.character)) ){

  warning(paste0("tax.lda():", "'",
    paste( colnames( pred_data[,sapply(pred_data, is.factor) | sapply(pred_data, is.character)]), collapse = "', '" ),
    "' are categorical: only numeric variables will be used"))
 }

pred_data <- pred_data[, sapply(pred_data, is.numeric), drop = FALSE]
predictors <- colnames(pred_data)

#GENERATE DATAFRAMES
model_data <- data.frame(input[,facA], pred_data)
colnames(model_data) <- c(paste0(facA), predictors)


print("tax.lda: model data made with predictors and facA")

  # GENERATE TEST AND TRAIN DATA FOR MODEL BUILDING
  set.seed(37) #Ensures same selection of data for test and train each time
  index <- sample(1:nrow(model_data), 0.7*nrow(model_data)) #Select 70% of dataset
  train_data <- model_data[index,,drop = FALSE] #70% of dataset
  test_data <- model_data[-index,, drop = FALSE] #30% of dataset
  
  predictors_test <- test_data[,predictors, drop = FALSE]
  
# PREDICTORS / RESPONSE (TRAIN / TEST)
  # DUMMY VARIABLES OF CATEGORICAL:
  # predictors_test <- model.matrix(test_data[,facA]~., test_data)[,-c(1,2), drop = FALSE]#[,-1] # Predictor variables in test dataset, creating dummy vars for categorical predictors # [,-c(1,2), drop = FALSE] removes intercept column of all 1's
  #  predictors_test <- predictors_test[,-1, drop = FALSE]  #Error in if (n == 0) stop("data (x) has 0 rows") : argument is of length zero 
  #  predictors_test <- predictors_test[,-1] # [,-1] removes facA from df
  # predictors_train <- model.matrix(train_data[,facA]~., train_data)[,-c(1,2), drop = FALSE] # Predictor variables in train dataset, creating dummy vars for categorical predictors
  #  predictors_train <- predictors_train[,-1,drop = FALSE]
  #  predictors_train <- predictors_train[,-1] # predictor data for train dataset
  response_train <- train_data[,facA, drop = TRUE] # response data for train dataset # vector
  response_test <- test_data[,facA, drop = TRUE] # response data for test dataset #vector
  #cat("The train data for model building is 70% of the dataset, while the test data for model testing is 30% of the dataset.") #Text will be visible to user.
 print("rf.anal: made train/test pred/resp dfs")  

  #BUILD MODE, PREDICT ##
  mod <- MASS::lda(form, data = train_data)
## statology.org/linear-discriminant-analysis-in-r/
  
## INTERPRET OUTPUT OF MODEL
# Prior probabilities of group: These represent the proportions of each response variable level in the training set. For example, 35.8% of all observations in the training set were of species virginica.
Prior probabilities of groups: # Species in iris dataset is the response var
    setosa versicolor  virginica  # levels are groups
 0.3207547  0.3207547  0.3584906 
# Group means: These display the mean values for each predictor variable for each species.
#           Sepal.Length Sepal.Width Petal.Length Petal.Width   # predictor variables
#setosa       -1.0397484   0.8131654   -1.2891006  -1.2570316   # for each level of the response var
#versicolor    0.1820921  -0.6038909    0.3403524   0.2208153
#virginica     0.9582674  -0.1919146    1.0389776   1.1229172
# Coefficients of linear discriminants: These display the linear combination of predictor variables that are used to form the decision rule of the LDA model. For example:
  #  LD1: .792*Sepal.Length + .571*Sepal.Width – 4.076*Petal.Length – 2.06*Petal.Width
# Proportion of trace: These display the percentage separation achieved by each linear discriminant function.

#Generate results
  summ <- mod 
  train_fitt <- predict(mod)
  train_prediction <- train_fitt$class
  train_ld <- train_fitt$x
  train_prob <- train_fitt$posterior
   
  test_fitt <-  predict(mod, newdata = predictors_test)
  test_prediction <- predict(mod, newdata = predictors_test)$class
  test_ld <- test_fitt$x
  test_prob <- test_fitt$posterior
  
  #train_rmse <- Metrics::rmse(response_train, train_fitt)
  fileName <- "lda_summary.txt"


# PLOT: HISTOGRAM OF CLASSES:
### stackoverflow.com/questions/69941611/how-can-i-replicate-plot-lda-with-of-r-tidymodels
### Double histogram plot (from plot.lda):
# bind_cols(Smarket_train, .fitted = predictions) %>% 
#  ggplot(aes(x=.fitted)) +
#  geom_histogram(aes(y = stat(density)),binwidth = .5) + 
#  scale_x_continuous(breaks = seq(-4, 4, by = 2))+
#  facet_grid(vars(Direction)) +
#  xlab("") + 
#  ylab("Density")

  
# PLOT: SCATTER OF LINEAR DISCRIMINANTS 
# statology.org/linear-discriminant-analysis-in-r/
# lda_plot <- cbind(train_data, train_ld)
# ggplot(lda_plot, aes(LD1, LD2)) +
#  geom_point(aes_(color = as.name(facA)))



 
## FROM PCOA BELOW  
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


#'Produce PCOA 3D ordination plot
#'@description Rotate PCOA analysis
#'@usage PlotPCOA3DScore(mSetObj=NA, imgName, format="json", inx1, inx2, inx3)
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "cividis"
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param imgName Input the image name
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

Plot.PCOA.3D <- function(mSetObj=NA, color="NULL", var_arrows="false", meta_col_color="NULL", imgName){
    
  options(error=traceback)

  library("viridis")
  library("RJSONIO")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  input <- mSetObj$analSet$pcoa$input
  pcoa <- mSetObj$analSet$pcoa$pcoa
  metaData <- mSetObj$analSet$pcoa$metaData

  #Create list to hold plot items
  pcoa3D_plot <- list()
  
  #Samples (rows)
  pcoa3D_plot$score$axis <- paste("PCOA", c(1, 2, 3), sep="")
  sampleCoords <- data.frame(t(as.matrix(pcoa$points[,1:3])))
  colnames(sampleCoords) <- NULL
  pcoa3D_plot$score$xyz <- sampleCoords
  pcoa3D_plot$score$name <- rownames(input)
  
  if (is.data.frame(metaData)==FALSE) { #If no meta data
    pcoa3D_plot$score$color <- "NA"
    pcoa3D_plot$score$point <- "NA"
  } else { #If meta data
    
    #Set up meta data column to use for colors
    if (meta_col_color=="NULL") { 
      meta_col_color_data <- as.factor(metaData[,1]) #Default meta data column for labeling with color is the first
      meta_col_color_name <- colnames(metaData)[1]
    } else {
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User imputted meta data column for labeling with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color
    }

    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the meta data
    if (is.null(color)) {
      color <- "viridis" #Default
      colors <- viridis(n) #Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1) #Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assing a grey color to each level (grDevices package- automatically installed)
    } else { 
      color <- "none"
    }
    
    #Assign colors
    if (color=="none") {
      cols <- "black"
    } else {
      cols <- colors[meta_col_color_data]
    }
    pcoa3D_plot$score$color <- col2rgb(cols)
    
  }
  
  #Variables (columns)
  if (var_arrows=="false") {
    pcoa3D_plot$scoreVar$axis <- "NA"
    pcoa3D_plot$scoreVar$xyzVar <- "NA"
    pcoa3D_plot$scoreVar$nameVar <- "NA"
    pcoa3D_plot$scoreVar$colorVar <- "NA"
  } else {
    #3D debug start
    print(var.scores3D)
    #3D debug end
    variableCoords <- data.frame(t(as.matrix(var.scores3D)))
    colnames(variableCoords) <- NULL
    pcoa3D_plot$scoreVar$axis <- paste("PCOA", c(1, 2, 3), sep="")
    pcoa3D_plot$scoreVar$xyzVar <- variableCoords
    pcoa3D_plot$scoreVar$nameVar <- colnames(input)
    if (color=="plasma") {
      pcoa3D_plot$scoreVar$colorVar <- col2rgb("black")
    } else {
      pcoa3D_plot$scoreVar$colorVar <- col2rgb("darkred")
    }
  }

  #From metaboanalyst, unclear what it does
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])))
  }else{
    cls <- as.character(mSetObj$dataSet$cls)
  }

  if(all.numeric(cls)){
    cls <- paste("Group", cls)
  }

  pcoa3D_plot$score$facA <- cls
  
  imgName <- paste(imgName, ".json", sep="")
  json.obj <- RJSONIO::toJSON(pcoa3D_plot, .na='null')
  sink(imgName)
  cat(json.obj)
  sink()
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj))
  }
}




#'Produce PCOA scree plot
#'@description Produce PCOA scree plot
#'@param mSetObj Input name of the created mSet Object
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
Plot.PCOA.scree <- function(mSetObj=NA, dim_numb="NULL", imgName, format="png", dpi=72, width=NA) {
    
  options(error=traceback)

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenvalues <- mSetObj$analSet$pcoa$eigenvalues

  if (dim_numb=="NULL") {
   dim_numb <- 2
   pcvars <- eigenvalues[1:dim_numb]/sum(eigenvalues)
   cumvars <- c(pcvars[1], pcvars[1]+pcvars[2])
  }
  if (dim_numb=="3") {
   dim_numb <- 3
   pcvars <- eigenvalues[1:dim_numb]/sum(eigenvalues)
   cumvars <- c(pcvars[1], pcvars[1]+pcvars[2], pcvars[1]+pcvars[2]+pcvars[3])
  }
  if (dim_numb=="4") {
   dim_numb <- 4
   pcvars <- eigenvalues[1:dim_numb]/sum(eigenvalues)
   cumvars <- c(pcvars[1], pcvars[1]+pcvars[2], pcvars[1]+pcvars[2]+pcvars[3], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4])
  }
  if (dim_numb=="5") {
   dim_numb <- 5
   pcvars <- eigenvalues[1:dim_numb]/sum(eigenvalues)
   cumvars <- c(pcvars[1], pcvars[1]+pcvars[2], pcvars[1]+pcvars[2]+pcvars[3], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5])
  }
  if (dim_numb=="6") {
   dim_numb <- 6
   pcvars <- eigenvalues[1:dim_numb]/sum(eigenvalues)
   cumvars <- c(pcvars[1], pcvars[1]+pcvars[2], pcvars[1]+pcvars[2]+pcvars[3], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5]+pcvars[6])
  }
  if (dim_numb=="7") {
   dim_numb <- 7
   pcvars <- eigenvalues[1:dim_numb]/sum(eigenvalues)
   cumvars <- c(pcvars[1], pcvars[1]+pcvars[2], pcvars[1]+pcvars[2]+pcvars[3], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5]+pcvars[6], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5]+pcvars[6]+pcvars[7])
  }
  if (dim_numb=="8") {
   dim_numb <- 8
   pcvars <- eigenvalues[1:dim_numb]/sum(eigenvalues)
   cumvars <- c(pcvars[1], pcvars[1]+pcvars[2], pcvars[1]+pcvars[2]+pcvars[3], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5]+pcvars[6], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5]+pcvars[6]+pcvars[7], pcvars[1]+pcvars[2]+pcvars[3]+pcvars[4]+pcvars[5]+pcvars[6]+pcvars[7]+pcvars[8])
  }

  miny<- 0
  maxy<- min(max(cumvars)+0.2, 1);
  
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
  mSetObj$imgSet$Plot.PCOA.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(mar=c(5,5,6,3));
  plot(pcvars, type='l', col='blue', main="Principal Coordinate Analysis \nScree Plot", xlab='Number of Dimensions', ylab='Variance Explained', ylim=c(miny,maxy), axes=F)
  text(pcvars, labels =paste(100*round(pcvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  points(pcvars, col='red');
  
  lines(cumvars, type='l', col='green')
  text(cumvars, labels =paste(100*round(cumvars,3),'%'), adj=c(-0.3, -0.5), srt=45, xpd=T)
  points(cumvars, col='red');
  
  abline(v=1:dim_numb, lty=3);
  axis(1, 1:length(pcvars), 1:length(pcvars));
  axis(2, las=2) #yaxis numbers upright for readability

  dev.off()
}



#'Produce PCOA stress plot
#'@description Produce PCOA stress plot (stress is the mismatch between the rank order of distances in the data, and the rank order of distances in the ordination)
#'@param mSetObj Input name of the created mSet Object
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
Plot.PCOA.stress <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
    
  options(error=traceback)

  library("vegan")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  pcoa <- mSetObj$analSet$pcoa$pcoa

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
  mSetObj$imgSet$Plot.PCOA.stress <- imgName
  
  #Stress plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) #No plotting outside of plot limits, set margins to default
  vegan::stressplot(pcoa, main="Principal Coordinate Analysis\nShepard Plot", yaxt="n")
  axis(2, las=2) #yaxis numbers upright for readability
  dev.off()
}



##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number and names of meta data columns for ordination plotting'
#'@description Java will use the meta data columns to enable user options for selecting meta data for ordination plotting
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
pcoa.meta.columns <- function(mSetObj=NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  metaData <- mSetObj$analSet$pcoa$metaData
  if (is.data.frame(metaData)==FALSE) {
    name.all.meta.cols <- "No groupings"
  } else {
    name.all.meta.cols <- c(colnames(metaData), "No groupings")
  }
  return(name.all.meta.cols)
  
}


#'Determine names of categorical columns in dataset as potential response variable options
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
  # fac.cols <- mSetObj$dataSet$norm %>% 
  #             dplyr::select_if(function(col) {is.character(col) | is.factor(col)})  
  # fac.names <- colnames(fac.cols) 
  fac.names <- colnames(
     data[,sapply(mSetObj$dataSet$norm, is.factor) | sapply(mSetObj$dataSet$norm, is.character), drop = FALSE]
     )
  return(fac.names)
}


#'Determine number of possible dimensions for scree plot
#'@description Java will use the dimension numbers to enable user options for scree plot
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
pcoa.scree.dim <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj)
  num_data <- mSetObj$analSet$pcoa$input
  dim <- ncol(num_data)
  max <- min(dim, 8)
  pcoa.scree.dim.opts <- 2:max
  return(pcoa.scree.dim.opts)
}

#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.pcoa.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.pcoa.result <- c(mSetObj$analSet$pcoa)
  return(ord.pcoa.result)
  
}