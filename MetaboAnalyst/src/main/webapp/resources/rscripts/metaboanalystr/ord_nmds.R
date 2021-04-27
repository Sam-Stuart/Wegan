#'Perform NMDS analysis
#'@description Perform NMDS analysis
#'@param mSetObj Input name of the created mSet Object
#'@param distance Input distance as one of "bray" (default), "manhattan", "canberra", "euclidean", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
#'@param abundance Set abundance transformation, default is absolute (no change), else relative
#'@param metaData  Set meta data, default is categorical columns in data set
#'@param envData  Set environmental data (must be user uploaded), default is none
#'@param env_text Input environmental data column names (java uses text box to obtain string)
#'@param data Which data set to use, normalized (default) or original
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.NMDS <- function(mSetObj=NA, distance="NULL", abundance="NULL", data="NULL", metaData="NULL", envData="NULL", env_text="NULL") { #3 drop downs, one text box
  
  library("vegan")
  library("dplyr")
  
  print("The use of data groupings will create more interesting plots. Load grouping data separately, where each row aligns with the rows in your data set, or include groupings as columns in your data set.")
  print("The use of environmental data, if available, will also create more interesting plots. Load environmental data separately, where each row aligns with the rows in your data set.")
  
  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #Obtain numeric data for ordination and categorical data for grouping data
  num_data <- select_if(input, is.numeric) #Numeric data only for NMDS
  fac_data <- select_if(input, is.factor) #Any categorical data will be used for grouping
  count.fac.cols <- ncol(fac_data)
  
  print("Should you have community species data, you may want to investigate the relative abundance (divide all values by column totals) versus absolute abundance (no change to data).")
  if (abundance=="NULL") {
    abundance <- "absolute" #Default abundance is absolute and no change is made to data
    num_data1 <- num_data
  } else {
    abundance <- "relative"
    num_data1 <- decostand(num_data, method="total") #Alternative option is relative abundance, each value is divided by the column sum
  }

  if (distance=="NULL") {
    distance1 <- "bray" #Default distance
  } else {
    distance1 <- distance #USer selected from list "euclidean", "manhattan", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
  } 
  
  #Run NMDS for 1D through 5D
  nmds1D <- metaMDS(num_data1, maxit=999, trymax=50, dist=distance1, k=1, trace=0)
  nmds2D <- metaMDS(num_data1, maxit=999, trymax=50, dist=distance1, k=2, trace=0)
  nmds3D <- metaMDS(num_data1, maxit=999, trymax=50, dist=distance1, k=3, trace=0)
  nmds4D <- metaMDS(num_data1, maxit=999, trymax=50, dist=distance1, k=4, trace=0)
  nmds5D <- metaMDS(num_data1, maxit=999, trymax=50, dist=distance1, k=5, trace=0)

  #meta data, used to group samples using colors or plotting symbols
  if (is.data.frame(metaData)==FALSE) { #No user uplaoded meta data
    if (count.fac.cols >= 1) { #If species data had at least one categorical column, call it meta data
      metaData1 <- as.data.frame(fac_data)
    } else {
      metaData1 <- "NA" #If species data had no categorical columns, meta data is NA
      #AddErrMsg("No groupings columns were detected. If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead ofgrouping data using 1, 2, and 3, use I, II and III.")
      print("No groupings columns were detected! If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
    }
  } else {  #User uplaoded meta data
    metaData1 <- metaData #Data integrated like weights in correlation module
    if (nrow(metaData1)!=nrow(input)) {
      #AddErrMsg("Your meta data does not have the same number of rows as your numerical data! Please check that you meta data is correct.")
      stop("Your grouping data does not have the same number of rows as your main data set! Please check that you meta data is correct.")
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
      #AddErrMsg("Your meta data does not have the same number of rows as your numerical data! Please check that you meta data is correct.")
      stop("Your environmental data does not have the same number of rows as your numerical data! Please check that you environmental data is correct.")
    }
  }
  
  #Text box instructions for selecting environmental variables. Text box should be interactive, meaning any change in text alters the result in real time. Default env_text is second column.
  if (is.data.frame(envData1)==TRUE) {
    cat("You uploaded environmental data. Identify environmental variables for NMDS using the column names with commas in between.")
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
    env_cols <- unlist(strsplit(env_text1, "+", fixed=TRUE))
    env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)])
    colnames(env_data) <- env_cols
  } else {
    env_data <- "NA"
  }
  
  #Fit variables to ordination plots for plotting arrows
  var.fit2D <- envfit(nmds2D, num_data1, permutations=999, p.max=NULL)

  #Fit environmental data to ordination plots for plotting arrows
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded
    env.fit2D <- "NA"
    env.fit2D.fac <- "NA"
    env.fit2D.num <- "NA"
  } else { #If environmental data uploaded
    env_data_numeric <- select_if(env_data, is.numeric)
    env_data_factor <- select_if(env_data, is.factor)
    env.fit2D <- envfit(nmds2D, env_data, permutations=999, p.max=NULL)
    if (ncol(env_data_factor)>0) {
      env.fit2D.fac <- envfit(nmds2D, env_data_factor, permutations=999, p.max=NULL)
    }
    if (ncol(env_data_numeric)>0) {
      env.fit2D.num <- envfit(nmds2D, env_data_numeric, permutations=999, p.max=NULL)
    }
  }

  #Extract scores
  samp.scores2D <- signif(scores(nmds2D, display="sites"), 5)
  samp.scores3D <- signif(scores(nmds3D, display="sites"), 5)
  var.scores2D <- signif(scores(nmds2D, display="species"), 5)
  var.scores3D <- signif(scores(nmds3D, display="species"), 5)
  
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded
    env.scores2D <- "NA"
  } else { #If environmental data uploaded
    env.scores2D <- signif(scores(env.fit2D, display="vectors"), 5) #Calculate variable scores
  }
  
  #Extract environment scores
  if (is.data.frame(envData1)==FALSE) { #If environmental data not uploaded
    env.scores2D <- "NA"
  } else { #If environmental data uploaded
    if (length(env_data_numeric)>0) {
      env.scores2D.num <- signif(scores(env.fit2D, display="vectors"), 5)
    } else {
      env.scores2D.num <- "NA"
    }
    
    if (length(env_data_factor)>0) {
      env.scores2D.fac <- signif(scores(env.fit2D, display="factors"), 5)
    } else {
      env.scores2D.fac <- "NA"
    }
    
    if (is.matrix(env.scores2D.num)==TRUE) { #Numeric constraining variables
      if (is.matrix(env.scores2D.fac)==TRUE) { #Categorical constraining variables
        env.scores2D <- rbind(env.scores2D.num, env.scores2D.fac)
      } else {  #No categorical constraining variables
        env.scores2D <- env.scores2D.num
      }
    } else { #No numeric constraining variables
      env.scores2D <- env.scores2D.fac
    }
  }
  
  #Store results in mSetObj$analSet$nmds
  mSetObj$analSet$nmds$name <- "NMDS"
  mSetObj$analSet$nmds$nmds1D <- nmds1D
  mSetObj$analSet$nmds$nmds2D <- nmds2D
  mSetObj$analSet$nmds$nmds3D <- nmds3D
  mSetObj$analSet$nmds$nmds4D <- nmds4D
  mSetObj$analSet$nmds$nmds5D <- nmds5D
  mSetObj$analSet$nmds$distance <- distance1 
  mSetObj$analSet$nmds$abundance <- abundance
  mSetObj$analSet$nmds$input <- num_data1
  mSetObj$analSet$nmds$metaData <- metaData1
  mSetObj$analSet$nmds$env_data <- env_data
  mSetObj$analSet$nmds$var.fit2D <- var.fit2D
  mSetObj$analSet$nmds$env.fit2D <- env.fit2D
  mSetObj$analSet$nmds$env.fit2D.fac <- env.fit2D.fac
  mSetObj$analSet$nmds$env.fit2D.num <- env.fit2D.num
  mSetObj$analSet$nmds$sample.scores3D <- samp.scores3D
  mSetObj$analSet$nmds$var.scores3D <- var.scores3D
  
  #Export tables and text docs into working directory
  write.csv(samp.scores2D, file="nmds_2D_row_scores.csv", row.names=TRUE)
  write.csv(samp.scores3D, file="nmds_3D_row_scores.csv", row.names=TRUE)
  write.csv(var.scores2D, file="nmds_2D_column_scores.csv", row.names=TRUE)
  write.csv(var.scores3D, file="nmds_3D_column_scores.csv", row.names=TRUE)
  if (is.data.frame(envData1)==TRUE) {
    write.csv(env.scores2D, file="nmds_2D_environment_scores.csv", row.names=TRUE)
  }
  
  stress_data <- c(nmds1D[["stress"]], nmds2D[["stress"]], nmds3D[["stress"]], nmds4D[["stress"]], nmds5D[["stress"]])
  scree_data <- data.frame(paste("NMDS ", 1:5), stress_data)
  colnames(scree_data) <- c("Dimension", "Stress")
  write.csv(scree_data, file="nmds_scree_data.csv", row.names=FALSE)
  
  sink("column_impact_on_nmds_2D.txt") 
  cat("Data columns may significantly impact NMDS\n")
  cat("\nNMDS dimension=2\n\n")
  print(var.fit2D)
  sink()  
  
  sink("environment_impact_on_nmds_2D.txt") 
  cat("Environmental factors may significantly impact NMDS\n")
  cat("\nNMDS dimension=2\n\n")
  print(env.fit2D)
  sink()  
  
  sink("nmds_summary.txt")
  cat("Non-Metric Multidimensional Analysis\n")
  if (abundance=="absolute") {
    abundance1 <- "false"
  } else {
    abundance1 <- "true"
  }
  cat(paste0("\n\nPerform relative abundance transformation: "), abundance1, "\n\n")
  cat("NMDS 1D")
  print(nmds1D)
  cat("NMDS 2D")
  print(nmds2D)
  cat("NMDS 3D")
  print(nmds3D)
  cat("NMDS 4D")
  print(nmds4D)
  cat("NMDS 5D")
  print(nmds5D)
  cat("\nLengend:\n")
  cat("Site=Rows and Species=Columns\n")
  sink() 
  
  return(.set.mSet(mSetObj))
  
}



#'Produce NMDS 2D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
#'@description Produce NMDS ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "cividis"
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
Plot.NMDS.2D <- function(mSetObj=NA, ellipse=NULL, var_arrows=NULL, env_arrows=NULL, env_cent=NULL, sampleNames=NULL, point_options=NULL, color=NULL, meta_col_color=NULL, meta_col_point=NULL, imgName, format="png", dpi=72, width=NA) {

  library("vegan")
  library("viridis") 
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  nmds2D <- mSetObj$analSet$nmds$nmds2D
  metaData <- mSetObj$analSet$nmds$metaData
  env_data <- mSetObj$analSet$nmds$env_data
  input <- mSetObj$analSet$nmds$input
  var.fit2D <- mSetObj$analSet$nmds$var.fit2D
  env.fit2D.fac <- mSetObj$analSet$nmds$env.fit2D.fac
  env.fit2D.num <- mSetObj$analSet$nmds$env.fit2D.num
  distance <- mSetObj$analSet$nmds$distance
  
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
  mSetObj$imgSet$Plot.NMDS.2D <- imgName
  
  #Produce 2D ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  if (distance=="euclidean") {
    ordiplot(nmds2D, type="n", xlab="MDS 1", ylab="MDS 2", main="Multidimensional Scaling", yaxt="n") #Empty ordination plot
  } else {
    ordiplot(nmds2D, type="n", xlab="NMDS 1", ylab="NMDS 2", main="Non-metric Multidimensional Scaling", yaxt="n") #Empty ordination plot
  } 
  axis(2, las=2)
  
  #Plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE) { #If no meta data

    #point options
    if (is.null(sampleNames)) { #default
      points(nmds2D, display="sites", pch=19) #Use points for samples
    } else {
      text(nmds2D, display="sites") #Use text for samples
    }
    
    #arrow options
    if (!is.null(var_arrows)) { #If variable arrows selected
      plot(var.fit2D, col="darkred")
    }
    
    if (is.data.frame(env_data)==TRUE) { #If environment data uploaded
      if (!is.null(env_arrows)) { #If environment arrows selected
        plot(env.fit2D.num, col="blue", lwd=2)
      }
      
      if (!is.null(env_cent)) { #If environment constraints selected
        plot(env.fit2D.fac, col="blue", lwd=2)
      }
    }
  } else { #If meta data available
    
    #Set up meta data column to use for colors
    if (is.null(meta_col_color)) { 
      meta_col_color_data <- as.factor(metaData[,1]) #Default meta data column for labeling with color is the first
      meta_col_color_name <- colnames(metaData)[1]
    } else {
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User imputted meta data column for labeling with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color
    }
    
    #Set up meta data column to use for points
    if (is.null(meta_col_point)) { 
      meta_col_point_data <- as.factor(metaData[,1]) #Default meta data column for labeling with points is the first
      meta_col_point_name <- colnames(metaData)[1]
    } else {
      meta_col_point_data <- as.factor(metaData[,meta_col_point]) #User imputted meta data column for labeling with points, options given to java using function meta.columns() below
      meta_col_point_name <- meta_col_point #User defined
    }
    
    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the meta data
    if (is.null(color)) { #Default
      colors <- viridis(n) #Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1) #Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
    } else { 
      color <- "none"
    }
    
    #Assign colors to points
    if (color=="none") {
      cols <- "black" #color none means black points
    } else {
      cols <- colors[meta_col_color_data] #Color pallete applied for groupings of user's choice
    }
    
    #point options
    pch_options <- c(19, 17, 15, 18, 1, 2, 0, 5, 6, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14)
    
    if (is.null(point_options)) {
      pchs <- 19 #No points option gets solid circle
    } else {
      pchs <- pch_options[meta_col_point_data] #Otherwise point options applied for grouping of user's choice
    }
     
    if (!is.null(sampleNames)) { #If display data as lables
      with(metaData, text(nmds2D, display="sites", col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      if (!is.null(point_options)) { #Engage point options
        with(metaData, points(nmds2D, display="sites", col=cols, pch=pchs, bg=cols)) 
        with(metaData, legend("bottomright", legend=levels(meta_col_point_data), col="black", pch=unique(pchs), pt.bg="black", title=meta_col_point_name))
      } else { #No point options
        with(metaData, points(nmds2D, display="sites", col=cols, pch=pchs, bg=cols)) 
      }
    }
    
    #arrow options
    if (!is.null(var_arrows)) { #If variable arrows selected
      plot(var.fit2D, col="darkred", lwd=2)
    }
    
    if (is.data.frame(envData)==TRUE) { #If environment data uploaded
      if (!is.null(env_arrows)) { #If environment arrows selected
        plot(env.fit2D.num, col="blue", lwd=2)
      }

      if (!is.null(env_cent)) { #If environment arrows selected
        plot(env.fit2D.fac, col="blue", lwd=2)
      }
    }
    
    #Ellipse options
    if (!is.null(ellipse)) { #if ellipses selected
      with(metaData, ordiellipse(nmds2D, meta_col_color_data, display="sites", kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #Legend
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
  }
  
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}




#'Produce NMDS 3D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
#'@description Produce NMDS ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "cividis"
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
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
Plot.NMDS.3D <- function(mSetObj=NA, color=NULL, var_arrows=NULL, meta_col_color=NULL, imgName, format="png", dpi=72, width=NA) {
  
  library("viridis")
  library("RJSONIO")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  input <- mSetObj$analSet$nmds$input
  nmds3D <- mSetObj$analSet$nmds$nmds3D
  metaData <- mSetObj$analSet$nmds$meta
  samp.scores3D <- mSetObj$analSet$nmds$sample.scores3D
  var.scores3D <- mSetObj$analSet$nmds$var.scores3D
  distance <- mSetObj$analSet$nmds$distance

  #Create list to hold plot items
  nmds3D_plot <- list()
  
  #Samples (rows)
  if (distance=="euclidean") {
    nmds3D_plot$score$axis <- paste("MDS", c(1, 2, 3), sep="")
  } else {
    nmds3D_plot$score$axis <- paste("NMDS", c(1, 2, 3), sep="")
  } 
  
  sampleCoords <- data.frame(t(as.matrix(samp.scores3D)))
  colnames(sampleCoords) <- NULL
  nmds3D_plot$score$xyz <- sampleCoords
  nmds3D_plot$score$name <- rownames(input)
  
  if (is.data.frame(metaData)==FALSE) { #If no meta data
    nmds3D_plot$score$color <- "NA"
    nmds3D_plot$score$point <- "NA"
  } else { #If meta data
    
    #Set up meta data column to use for colors
    if (is.null(meta_col_color)) { 
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
    nmds3D_plot$score$color <- col2rgb(cols)

  }
  
  #Variables (columns)
  variableCoords <- data.frame(t(as.matrix(var.scores3D)))
  colnames(variableCoords) <- NULL
  if (distance=="euclidean") {
    nmds3D_plot$scoreVar$axis <- paste("MDS", c(1, 2, 3), sep="")
  } else {
    nmds3D_plot$scoreVar$axis <- paste("NMDS", c(1, 2, 3), sep="")
  } 
  nmds3D_plot$scoreVar$xyzVar <- variableCoords
  nmds3D_plot$scoreVar$nameVar <- colnames(input)
  nmds3D_plot$scoreVar$colorVar <- col2rgb("darkred")
  
  #From metaboanalyst, unclear what it does
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])))
  }else{
    cls <- as.character(mSetObj$dataSet$cls)
  }

  if(all.numeric(cls)){
    cls <- paste("Group", cls)
  }

  nmds3D_plot$score$facA <- cls
  
  imgName=paste(imgName, ".", format, sep="")
  json.obj <- RJSONIO::toJSON(nmds3D_plot, .na='null')
  sink(imgName)
  cat(json.obj)
  sink()
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj))
  }
}



#'Produce NMDS stress plot
#'@description Produce NMDS stress plot (stress is the mismatch between the rank order of distances in the data, and the rank order of distances in the ordination)
#'@param mSetObj Input name of the created mSet Object
#'@param k The dimension of interest
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
Plot.NMDS.stress <- function(mSetObj=NA, k=NULL, imgName, format="png", dpi=72, width=NA) {
  
  library("vegan")

  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  distance <- mSetObj$analSet$nmds$distance


  if (is.null(k)) {
    k <- 1
    nmds <- mSetObj$analSet$nmds$nmds1D
  } else if (k=="2") {
    nmds <- mSetObj$analSet$nmds$nmds2D
  } else if (k=="3") {
    nmds <- mSetObj$analSet$nmds$nmds3D
  } else if (k=="4") {
    nmds <- mSetObj$analSet$nmds$nmds4D
  } else if (k=="5") {
    nmds <- mSetObj$analSet$nmds$nmds5D
  } 
  
  stress <- nmds$stress
  stress_eq <- paste0("Stress Statistic=", signif(stress,2))

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
  mSetObj$imgSet$Plot.NMDS.stress <- imgName
  
  #Stress plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 6.1, 2.1)) 
  if (distance=="euclidean") {
    stressplot(nmds, main=paste0("Multidimensional Scaling\nDimension ", k,"\nShepard Stress Plot\n\n"), yaxt="n")
  } else {
    stressplot(nmds, main=paste0("Non-metric Multidimensional Scaling\nDimension ", k,"\nShepard Stress Plot\n\n"), yaxt="n")
  } 
  
  axis(2, las=2)
  mtext(stress_eq, side=3)
  dev.off()
}



#'Produce NMDS scree plot
#'@description Produce NMDS stress plot
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
Plot.NMDS.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  nmds1D <- mSetObj$analSet$nmds$nmds1D
  nmds2D <- mSetObj$analSet$nmds$nmds2D
  nmds3D <- mSetObj$analSet$nmds$nmds3D
  nmds4D <- mSetObj$analSet$nmds$nmds4D
  nmds5D <- mSetObj$analSet$nmds$nmds5D
  input <- mSetObj$analSet$nmds$input
  distance <- mSetObj$analSet$nmds$distance

  stressMax <- nmds1D[["stress"]]
  stress_data <- c(nmds1D[["stress"]], nmds2D[["stress"]], nmds3D[["stress"]], nmds4D[["stress"]], nmds5D[["stress"]])
  scree_data <- data.frame(1:5, stress_data)
  colnames(scree_data) <- c("Dimension", "Stress")

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
  mSetObj$imgSet$Plot.NMDS.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  if (distance=="euclidean") {
    plot(x=scree_data$Dimension, y=scree_data$Stress, type="l", xlim=c(1, 5), ylim=c(0, stressMax+0.02), xlab="Number of Dimensions", ylab="Stress", main="Multidimensional Scaling Scree Plot", yaxt="n", col="blue", lwd=2)
  } else {
    plot(x=scree_data$Dimension, y=scree_data$Stress, type="l", xlim=c(1, 5), ylim=c(0, stressMax+0.02), xlab="Number of Dimensions", ylab="Stress", main="Non-metric Multidimensional Scaling Scree Plot", yaxt="n", col="blue", lwd=2)
  } 
  points(x=scree_data$Dimension, y=scree_data$Stress, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
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

  metaData <- mSetObj$analSet$nmds$metaData
  meta.col.names <- colnames(metaData)

  return(meta.col.names)
  
}


#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.nmds.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.nmds.result <- c(mSetObj$analSet$nmds)
  return(ord.nmds.result)
  
}