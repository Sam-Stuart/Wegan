#'Perform DCA
#'@description Perform detrended correspondence analysis
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
ord.dca <- function(mSetObj=NA, abundance="NULL", metaData="NULL", envData="NULL", env_text="NULL", data="NULL") { #3 user options on all results pages, plus ability to upload up to 2 supplemental data sets
  
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
      cat(paste0("You have selected these constraining variables: ", paste(env_text1, collapse=", "), "."))
      cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
      
    } else {
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
  
  #Run DCA
  dca <- decorana(num_data1, ira=0)
  
  #meta (grouping) data, used to group samples using colors
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
  
  #Fit variables to ordination plots for plotting arrows
  var_fit <- envfit(dca, num_data1, permutations=999, p.max=NULL, type="species")
  
  #Fit environmental data to ordination plots for plotting arrows and centroids
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded, all fit objects are the character "NA"
    env_fit_fac <- "NA"
    env_fit_num <- "NA"
    env_fit <- "NA"
  } else { #If environmental data uploaded
    env_fit <- envfit(dca, env_data, permutations=999, p.max=NULL, display="sites")
    if (ncol(select_if(env_data, is.factor))>0) {
      env_fit_fac <- envfit(dca, select_if(env_data, is.factor), permutations=999, p.max=NULL, display="sites")
    }
    if (ncol(select_if(env_data, is.numeric))>0) {
      env_fit_num <- envfit(dca, select_if(env_data, is.numeric), permutations=999, p.max=NULL, display="sites")
    }
  }
  
  #Extract row and column scores
  samp.scores <- signif(scores(dca, display="sites"), 5) #Extract scores for samples (rows), and truncate to 5 significant figures
  colnames(samp.scores) <- c("DCA1", "DCA2", "DCA3", "DCA4") #Name score columns
  var_scores <- signif(scores(dca, display="species"), 5)
  colnames(var_scores) <- c("DCA1", "DCA2", "DCA3", "DCA4")
  
  #Extract environment scores
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded
    env_scores <- "NA"
  } else { #If environmental data uploaded
    env_data_numeric <- select_if(env_data, is.numeric) #Select numeric environmental varibless
    if (length(env_data_numeric)>0) { #If any exist
      env_scores.num <- signif(scores(env_fit, display="vectors"), 5) #Obtain scores
    } else { #No numeric envrionmental columns
      env_scores.num <- "NA"
    }
    
    env_data_factor <- select_if(env_data, is.factor) #Repeat for categorical environmental variables
    if (length(env_data_factor)>0) {
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
  
  #Produce summary elements
  summary <- summary(dca)
  eigenValues <- as.data.frame(dca$evals)
  colnames(eigenValues) <- NULL
  origin <- as.data.frame(dca$origin)
  colnames(origin) <- NULL
  call <- dca$call
  
  #Store results in mSetObj$analSet$dca
  mSetObj$analSet$dca$name <- "DCA"
  mSetObj$analSet$dca$dca <- dca
  mSetObj$analSet$dca$abundance <- abundance1
  mSetObj$analSet$dca$input <- num_data1
  mSetObj$analSet$dca$metaData <- metaData1
  mSetObj$analSet$dca$env_data <- env_data
  mSetObj$analSet$dca$variable.fit <- var_fit
  mSetObj$analSet$dca$enviroment.fit <- env_fit
  mSetObj$analSet$dca$enviroment.factor.fit <- env_fit_fac
  mSetObj$analSet$dca$enviroment.numeric.fit <- env_fit_num
  mSetObj$analSet$dca$sample.scores <- samp.scores
  mSetObj$analSet$dca$var_scores <- var_scores
  mSetObj$analSet$dca$env_scores <- env_scores
  mSetObj$analSet$dca$summary <- summary
  mSetObj$analSet$dca$eigenValues <- dca$evals 
  
  #Download relevent data
  write.csv(samp.scores, file="dca_sample_scores.csv", row.names=TRUE)
  write.csv(var_scores, file="dca_variable_scores.csv", row.names=TRUE)
  if (is.data.frame(envData1)==TRUE) { #If environmental data uploaded
    write.csv(env_scores, file="dca_constraining_data_scores.csv", row.names=TRUE)
  } 
  
  eigenValues_data <- cbind(dca$evals, dca$evals/sum(dca$evals))
  eigenValues_data <- as.data.frame(cbind(paste0("DCA ",1:4), eigenValues_data))
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  write.csv(eigenValues_data, file="dca_scree_data.csv", row.names=FALSE)
  
  sink("column_impact_on_dca.txt") 
  cat("Data columns may significantly impact DCA\n")
  print(var_fit)
  sink()  
  
  if (is.data.frame(envData1)==TRUE) { #If environmental data uploaded
    sink("constraining_variables_impact_on_dca.txt") 
    cat("Constraining data may significantly impact DCA\n")
    print(env_fit)
    sink()
  }
  
  sink("dca_summary.txt") 
  cat(paste0("Detrended Correspondence Analysis Summary\n\n"))
  cat("Call:\n")
  print(call)
  cat("\nEigen values:")
  print(eigenValues)
  cat("\nOrigin values for axes:")
  print(origin)
  cat("\n")
  print(summary)
  cat("\nLengend:\n")
  cat("Site=Rows and Species=Columns\n")
  sink()  
  
  return(.set.mSet(mSetObj))
  
}





#'Produce DCA 2D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows/env centroids
#'@description Produce DCA ordination plot with user selected options
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
Plot.DCA.2D <- function(mSetObj=NA, color=NULL, ellipse="false", var_arrows="false", env_arrows="false", env_cent="false", sampleNames="false", meta_col_color="NULL", point_options="NULL", meta_col_point="NULL", imgName, format="png", dpi=72, width=NA) { #5 check boxes, 3 drop downs

  library("vegan")
  library("viridis") 
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  dca <- mSetObj$analSet$dca$dca
  metaData <- mSetObj$analSet$dca$metaData
  env_data <- mSetObj$analSet$dca$env_data
  num_data <- mSetObj$analSet$dca$num_data
  var_fit <- mSetObj$analSet$dca$variable.fit
  env_fit_fac <- mSetObj$analSet$dca$enviroment.factor.fit
  env_fit_num <- mSetObj$analSet$dca$enviroment.numeric.fit
  summary <- mSetObj$analSet$dca$summary
  
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
  mSetObj$imgSet$Plot.DCA.2D <- imgName
  
  #Extract % variance explained by axis 1 and 2 for plotting
  DCA1 <- dca$evals.decorana[1]/sum(dca$evals.decorana)
  DCA2 <- dca$evals.decorana[2]/sum(dca$evals.decorana)

  #Produce 2D ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  
  #Set up empty ordination plot
  ordiplot(dca, type="n", choices=c(1,2), xlab=paste0("DCA 1 (", signif(DCA1, 4)*100, " %)"), ylab=paste0("DCA 2 (", signif(DCA2, 4)*100, " %)"), main="Detrended Correspondence Analysis", yaxt="n")
  axis(2, las=2)
  
  #Plot with and without ellipses/sample labels/grouping options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE) { #If no grouping data uploaded
    
    #point text option
    if (sampleNames=="NULL") { #If display data as lables
      text(dca, display="sites") #Add text for samples
    } else {
      points(dca, display="sites", pch=19)
    }
    
    #Arrow options
    if (var_arrows=="true") { #If variable arrows selected
      plot(var_fit, col="darkred", lwd=2)
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows=="true") { #If environment arrows selected
        plot(env_fit_num, col="blue", lwd=2)
      }
      
      if (env_cent=="true") { #If environment constraints selected
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
    if (meta_col_point=="false") { #No column selected for grouping by point shape
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
    
    if (sampleNames=="true") { #If display data as lables
      with(metaData, text(dca, display="sites", col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      if (!is.null(point_options)) { #Engage point options
        with(metaData, points(dca, display="sites", col=cols, pch=pchs, bg=cols)) 
        with(metaData, legend("bottomright", legend=levels(meta_col_point_data), col="black", pch=unique(pchs), pt.bg="black", title=meta_col_point_name))
      } else { #No point options
        with(metaData, points(dca, display="sites", col=cols, pch=pchs, bg=cols)) 
      }
    }
    
    #Ellipse option
    if (ellipse=="true") { #if ellipses selected
      with(metaData, ordiellipse(dca, meta_col_color_data, kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #arrow options
    if (var_arrows=="true") { #If variable arrows selected
      plot(var_fit, col="darkred", lwd=2)
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (env_arrows=="true") { #If environment arrows selected
        plot(env_fit_num, col="blue", lwd=2)
      }
      
      if (env_cent=="true") { #If environment centroids selected
        plot(env_fit_fac, col="blue", lwd=2)
      }
    }
    
    #Legend
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
  }
  
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}




#'Produce DCA scree plot
#'@description Produce DCA stress plot
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
Plot.DCA.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenValues <- mSetObj$analSet$dca$eigenValues

  #Produce data set for plotting
  eigenValues_data <- cbind(eigenValues, eigenValues/sum(eigenValues))
  eigenValues_data <- as.data.frame(cbind(1:4, eigenValues_data))
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  maxVar <- max(eigenValues/sum(eigenValues))

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
  mSetObj$imgSet$Plot.DCA.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(x=eigenValues_data$Dimension, y=eigenValues_data$Variance_Explained, type="l", xlim=c(1, 4), ylim=c(0, maxVar+0.1), xlab="Dimension", ylab="Proportion of Variance Explained", main="Detrended Correspondence Analysis Scree Plot", yaxt="n", xaxt="n", col="blue", lwd=2)
  points(x=eigenValues_data$Dimension, y=eigenValues_data$Variance_Explained, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
  axis(1, at=1:4)
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
  
  metaData <- mSetObj$analSet$dca$metaData
  count.all.meta.cols <- ncol(metaData)
  name.all.meta.cols <- colnames(metaData)
  
  meta.col.results <- list(
    count=count.all.meta.cols,
    names=name.all.meta.cols
  )
  
  return(meta.col.results)
  
}


#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.dca.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.dca.result <- c(mSetObj$analSet$dca)
  return(ord.dca.result)
  
}