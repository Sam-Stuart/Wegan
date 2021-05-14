#'Perform RDA
#'@description Perform redundancy analysis
#'@param mSetObj Input name of the created mSet Object
#'@param abundance Set abundance transformation, default is absolute (no change), else relative (divide by column total)
#'@param metaData  Set meta data, default is categorical columns in data set
#'@param envData  Set environmental data (must be user uploaded), default is none
#'@param env_text Input environmental data column names (java uses text box to obtain string)
#'@param data Which data set to use, normalized (default) or original
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

ord.rda <- function(mSetObj=NA, abundance="NULL", metaData="NULL", envData="NULL", env_text="NULL", data="NULL") { #3 user options on all results pages, plus ability to upload up to 2 supplemental data sets
  
  library("vegan")
  library("dplyr")

  print("The use of data groupings will create more interesting plots. Load grouping data separately, where each row aligns with the rows in your data set, or include groupings as columns in your main data set.")
  print("The use of environmental data, if available, will also create more interesting plots. Load environmental data separately, where each row aligns with the rows in your data set.")
  
  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #Obtain numeric data for ordination and catgroical data for grouping data
  num_data <- select_if(input, is.numeric)
  fac_data <- select_if(input, is.factor)
  count.fac.cols <- ncol(fac_data)
  
  print("More: Should you have community species data, you may want to investigate the relative abundance (divide all values by column totals) versus absolute abundance (no change to data).")
  if (abundance=="NULL") {
    abundance1 <- "absolute"
    num_data1 <- num_data #Default abundance is absolute and no change is made to data
  } else {
    abundance1 <- "relative"
    num_data1 <- decostand(num_data, method="total") #Alternative option is relative abundance, each value is divided by the column sum
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
  if (is.data.frame(envData)==TRUE) { #User uplaoded environmental data  
    cat("You uploaded environmental data. Identify environmental variables for DCA using the column names with commas in between.")
  }
  
  #Set up environmental data using user selected columns
  if (is.data.frame(envData1)==TRUE) { #User uplaoded environmental data
    if (env_text=="NULL") { 
      env_text1 <- colnames(envData1) #Default is the all env columns
      cat(paste0("You have selected these constraining environmental variables: ", paste(env_text1, collapse=", ")))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
      
    } else {
      env_text1 <- env_text #taken from text box by java, fed as string into R code
      env_text1 <- gsub("\n", "", env_text1, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
      env_text1 <- gsub(",", "+", env_text1, fixed=TRUE) 
      env_text1 <- gsub(";", "+", env_text1, fixed=TRUE)
      env_text1 <- gsub(" ", "", env_text1, fixed=TRUE)
      env_text1 <- gsub(":", "+", env_text1, fixed=TRUE)
      env_text1 <- gsub("*", "+", env_text1, fixed=TRUE)
      cat(paste0("You have selected these constraining environmental variables: ", gsub("+", ", ", env_text1, fixed=TRUE), "."))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
    }
  
      env_cols <- unlist(strsplit(env_text1, "+", fixed=TRUE)) #Extract column names from env_text1
      env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)]) #Subset environmental data set to select columns of interest
      colnames(env_data) <- env_cols #Name columns
  } else { #No environmetal data uploaded
    env_data <- "NA"
  }
  
  #Run RDA
  if(is.data.frame(envData1)==FALSE) { #Without environmetal data
    rda <- rda(num_data1)
  } else{ #With user uploaded environmetal data
    rda <- rda(X=num_data1, Y=env_data)
  }

  #meta (grouping) data, used to group samples using colors or plotting symbols
  if (is.data.frame(metaData)==FALSE) { #No user uplaoded grouping data
    if (count.fac.cols >= 1) { #If species data had at least one categorical column, call it grouping data
      metaData1 <- as.data.frame(fac_data)
    } else {
      metaData1 <- "NA" #If species data had no categorical columns, grouping data is NULL
      #AddErrMsg("No groupings columns were detected. If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead ofgrouping data using 1, 2, and 3, use I, II and III.")
      print("No groupings columns were detected! If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
    }
  } else {  #User uploaded grouping data
    metaData1 <- metaData #User uploaded like weights in correlation module
    if (nrow(metaData1)!=nrow(input)) {
      #AddErrMsg("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
      stop("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
    }
    for(i in 1:ncol(metaData1)) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }
  print("LINE 1")
  
#Fit variables to ordination plots for plotting arrows
  var_fit <- envfit(rda, num_data1, permutations=999, p.max=NULL, type="lc")
  print("LINE 2")

  #Fit environmental data to ordination plots for plotting arrows and centroids
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded, all fit objects are the character "NA"
    env_fit_fac <- "NA"
    env_fit_num <- "NA"
    env_fit <- "NA"
  } else { #If environmental data uploaded
    env_data_numeric <- select_if(env_data, is.numeric)
    env_data_factor <- select_if(env_data, is.factor)
    env_fit <- envfit(rda, env_data, permutations=999, p.max=NULL, display="sites")
    if (ncol(env_data_factor)>0) {
      env_fit_fac <- envfit(rda, env_data_factor, permutations=999, p.max=NULL, display="sites")
    }
    if (ncol(env_data_numeric)>0) {
      env_fit_num <- envfit(rda, env_data_numeric, permutations=999, p.max=NULL, display="sites")
    }
  }
  print("LINE 3")

  #Extract row and column scores
  samp.scores <- signif(scores(rda, display="sites"), 5) #Extract scores for samples (rows), and truncate to 5 significant figures
  colnames(samp.scores) <- c("RDA1", "RDA2") #Name score columns
  var_scores <- signif(scores(rda, display="species"), 5)
  colnames(var_scores) <- c("RDA1", "RDA2")
  print("LINE 4")

  #Extract environment scores
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded
    env_scores <- "NA"
  } else { #If environmental data uploaded
    if (length(env_data_numeric)>0) { #If any numeric environmental varibles exist
      env_scores.num <- signif(scores(env_fit, display="vectors"), 5) #Obtain scores
    } else { #No numeric envrionmental columns
      env_scores.num <- "NA"
    }
    
    if (length(env_data_factor)>0) { #Repeat for categorical environmental variables
      env_scores.fac <- signif(scores(env_fit, display="factors"), 5)
    } else {
      env_scores.fac <- "NA"
    }
    
    if (is.matrix(env_scores.num)==TRUE) { #Numeric environmental variable scores turned into a matrix
      if (is.matrix(env_scores.fac)==TRUE) { #Categorical environmental variable scores turned into a matrix
        env_scores <- rbind(env_scores.num, env_scores.fac) #If both types of scores exist, combine them
      } else {  #No categorical environmental variables
        env_scores <- env_scores.num #Just scores for numeric data
      }
    } else { #No numeric environmental variables
      env_scores <- env_scores.fac #Just scores for factor data
    }
  }
  print("LINE 5")

  #Produce relevant results
  summary <- summary(rda)
  eigenvalues <- rda$CCA$eig
    
  #Store results in mSetObj$analSet$rda
  mSetObj$analSet$rda$name <- "RDA"
  mSetObj$analSet$rda$rda <- rda
  mSetObj$analSet$rda$abundance <- abundance1
  mSetObj$analSet$rda$input <- num_data1
  mSetObj$analSet$rda$metaData <- metaData1
  mSetObj$analSet$rda$env_data <- env_data
  mSetObj$analSet$rda$variable.fit <- var_fit
  mSetObj$analSet$rda$enviroment.fit <- env_fit
  mSetObj$analSet$rda$enviroment.factor.fit <- env_fit_fac
  mSetObj$analSet$rda$enviroment.numeric.fit <- env_fit_num
  mSetObj$analSet$rda$sample.scores <- samp.scores
  mSetObj$analSet$rda$var_scores <- var_scores
  mSetObj$analSet$rda$env_scores <- env_scores
  mSetObj$analSet$rda$summary <- summary
  print("PERFORMED RDA")
  mSetObj$analSet$rda$eigenvalues <- eigenvalues
  print(mSetObj$analSet$rda$eigenvalues)

  #Download relevant data
  write.csv(samp.scores, file="rda_row_scores.csv", row.names=row.names(input))
  write.csv(var_scores, file="rda_column_scores.csv", row.names=TRUE)
  if (is.data.frame(envData1)==TRUE) { #If environmental data uploaded
    write.csv(env_scores, file="rda_environment_scores.csv", row.names=TRUE)
  } 

  eigenValues_data <- cbind(eigenvalues, eigenvalues/sum(eigenvalues))
  n <- nrow(eigenValues_data)
  eigenValues_data <- as.data.frame(cbind(paste0("RDA ", 1:nrow(eigenValues_data)), eigenValues_data))
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  write.csv(eigenValues_data, file="rda_scree_data.csv", row.names=FALSE)
  
  sink("column_impact_on_rda.txt") 
  cat("Data columns may significantly impact RDA\n")
  print(var_fit)
  sink()  

  if (is.data.frame(envData1)==TRUE) { #If environmental data uploaded
    sink("environment_impact_on_rda.txt") 
    cat("Environmental data may significantly impact RDA\n")
    print(env_fit)
    sink()
  }
  
  sink("rda_summary.txt") 
  cat(paste0("Redundancy Analysis Summary\n"))
  print(summary)
  cat("\nLengend:\n")
  cat("Site=Rows and Species=Columns\n")
  sink()  
  
  return(.set.mSet(mSetObj))
  
}





#'Produce RDA 2D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows/env centroids
#'@description Produce RDA ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "grey"
#'@param ellipse Boolean, TRUE to add confidence ellipses, FALSE (default) to not add confidence ellipses
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param env_arrows Boolean, TRUE to produce environmental arrows, FALSE (default) to produce ordination plot without environmental arrows---only appear if env data uploaded
#'@param env_cent Boolean, TRUE to produce display factor environmental data centroids, FALSE (default) to produce ordination plot without environmental centroids
#'@param sampleNames Boolean, TRUE to display data as variable names, FALSE (default) to display data as points
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param point_options Boolean, TRUE to turn data into points, FALSE (default) uses row names as data points
#'@param meta_col_point Meta data column to use for plotting points, Can be user inputted where options are given to java using function meta.columns()
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
Plot.RDA.2D <- function(mSetObj=NA, color="NULL", var_arrows="NULL", env_arrows="NULL", env_cent="NULL", sampleNames="NULL", meta_col_color="NULL", point_options="NULL", meta_col_point="NULL", ellipse="NULL", imgName, format="png", dpi=72, width=NA) { #6 check boxes, 3 drop downs

  library("vegan")
  library("viridis") 
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  rda <- mSetObj$analSet$rda$rda
  metaData <- mSetObj$analSet$rda$metaData
  env_data <- mSetObj$analSet$rda$envDataPlotting
  num_data <- mSetObj$analSet$rda$num_data
  var_fit <- mSetObj$analSet$rda$variable.fit
  env_fit_fac <- mSetObj$analSet$rda$enviroment.factor.fit
  env_fit_num <- mSetObj$analSet$rda$enviroment.numeric.fit
  summary <- mSetObj$analSet$rda$summary
  
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
  mSetObj$imgSet$Plot.RDA.2D <- imgName
  
  #Extract % variance explained by axis 1 and 2 for plotting
  if (is.data.frame(env_data)==FALSE) { #No environmental data uploaded
    RDA1 <- summary$cont$importance[2,1]
    RDA2 <- summary$cont$importance[2,2]
  } else { #Yes environmental (constraing) data uploaded
    RDA1 <- summary$concont$importance[2,1]
    RDA2 <- summary$concont$importance[2,2]
  }
  
  #Produce 2D ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  
  #Set up empty ordination plot
  ordiplot(rda, type="n", xlab=paste0("RDA 1 (", signif(RDA1, 4)*100, " %)"), ylab=paste0("RDA 2 (", signif(RDA2, 4)*100, " %)"), main="Redundancy Analysis", yaxt="n")
  axis(2, las=2)
  
  #Plot with and without ellipses/sample labels/grouping options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE) { #If no grouping data uploaded
    
    #point text option
    if (sampleNames!="NULL") { #If display data as lables
      text(rda, display="sites") #Add text for samples
    } else {
      points(rda, display="sites", pch=19)
    }
    
    #Arrow options
    if (var_arrows!="NULL") { #If variable arrows selected
      plot(var_fit, col="darkred", lwd=2)
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows!="NULL") { #If environment arrows selected
        plot(env_fit_num, col="blue", lwd=2)
      }
      
      if (env_cent!="NULL") { #If environment constraints selected
        plot(env_fit_fac, col="blue", lwd=2)
      }
    }
    
  } else { #If grouping data available
    
    #Set up grouping data column to use for colors
    if (meta_col_color=="NULL") { #No column selected for grouping by color
      meta_col_color_data <- as.factor(metaData[,1]) #Default grouping data column for grouping with color is the first
      meta_col_color_name <- colnames(metaData)[1] #Extract name
    } else { #column selected for grouping by color
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User uploaded grouping data column for grouping with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color #Extract name
    }
    
    #Set up grouping data column to use for points
    if (meta_col_point=="NULL") { #No column selected for grouping by point shape
      meta_col_point_data <- as.factor(metaData[,1]) #Default grouping data column for grouping with points is the first
      meta_col_point_name <- colnames(metaData)[1] #Extract name
    } else {
      meta_col_point_data <- as.factor(metaData[,meta_col_point]) #User uploaded grouping data column for grouping with points, options given to java using function meta.columns() below
      meta_col_point_name <- meta_col_point #Extract name
    }
    
    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the grouping data column selected for grouping
    if (color=="NULL") { #Default palette 
      colors <- viridis(n)#Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1)#Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
    } else { 
      color <- "none"
    }
    
    #Assign colors to points
    if (color=="none") {
      cols <- "black" #color none means all black points
    } else {
      cols <- colors[meta_col_color_data] #Grouping points Color pallete, user's choice
    }
    
    #point options
    pch_options <- c(19, 17, 15, 18, 1, 2, 0, 5, 6, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14)
    
    if (point_options=="NULL") {
      pchs <- 19 #No points option gets solid circle
    } else {
      pchs <- pch_options[meta_col_point_data] #Otherwise point options applied for grouping of user's choice
    }
    
    if (sampleNames!="NULL") { #If display data as lables
      with(metaData, text(rda, display="sites", col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      if (point_options!="NULL") { #Engage point options
        with(metaData, points(rda, display="sites", col=cols, pch=pchs, bg=cols)) 
        with(metaData, legend("bottomright", legend=levels(meta_col_point_data), col="black", pch=unique(pchs), pt.bg="black", title=meta_col_point_name))
      } else { #No point options
        with(metaData, points(rda, display="sites", col=cols, pch=pchs, bg=cols)) 
      }
    }
    
    #arrow options
    if (var_arrows!="NULL") { #If variable arrows selected
      plot(var_fit, col="darkred", lwd=2)
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows!="NULL") { #If environment arrows selected
        plot(env_fit_num, col="blue", lwd=2)
      }
      
      if (env_cent!="NULL") { #If environment centroids selected
        plot(env_fit_fac, col="blue", lwd=2)
      }
    }
    
    #Ellipse option
    if (ellipse!="NULL") { #if ellipses selected
      with(metaData, ordiellipse(pcoa, meta_col_color_data, kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #Legend for colors
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
  }
  
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}




#'Produce RDA scree plot
#'@description Produce RDA stress plot
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
Plot.RDA.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenvalues <- mSetObj$analSet$rda$eigenvalues
  print(eigenvalues)
  print(eigenvalues/sum(eigenvalues))

  #Produce data set for plotting
  eigenValues_data <- as.data.frame(cbind(eigenvalues, eigenvalues/sum(eigenvalues))) #Eigen values and variance explained
  print(eigenValues_data)

  n <- nrow(eigenValues_data) #also used in plot code below
  eigenValues_data <- as.data.frame(cbind(1:n, eigenValues_data)) #Add dimension column
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")

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
  mSetObj$imgSet$Plot.RDA.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(x=eigenValues_data$Dimension, y=eigenValues_data$Variance_Explained, type="l", xlim=c(1, n), ylim=c(0, 1), xlab="Dimension", ylab="Proportion of Variance Explained", main="Redundancy Analysis Scree Plot", yaxt="n", xaxt="n", col="blue", lwd=2)
  points(x=eigenValues_data$Dimension, y=eigenValues_data$Variance_Explained, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
  axis(1, at=1:n)
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
meta.columns <- function(mSetObj=NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  metaData <- mSetObj$analSet$rda$metaData
  name.all.meta.cols <- colnames(metaData)

  return(name.all.meta.cols)
  
}


#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.rda.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.rda.result <- c(mSetObj$analSet$rda)
  return(ord.rda.result)
  
}