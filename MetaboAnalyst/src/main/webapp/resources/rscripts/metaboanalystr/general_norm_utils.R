#'Clean the data matrix
#'@description Function used in higher functinos to clean data matrix
#'@param ndata Input the data to be cleaned
#'@export
#'
CleanDataMatrix <- function(ndata){
  # make sure no costant columns crop up
  varCol <- apply(data.frame(ndata), 2, var, na.rm=T); # getting an error of dim(X) must have a positive length, fixed by data.frame 
  constCol <- (varCol == 0 | is.na(varCol));
  return(ndata[,!constCol, drop=FALSE]); # got an error of incorrect number of dimensions, added drop=FALSE to avoid vector conversion
}


#'BestNormalize
#'@description This function performs column-wise normalization using all methods and selects the "best" (ie highest SW p-value).
#'@usage Normalization(mSetObj, rowNorm)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Louisa Normington \email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'@export
#'
BestNormalize <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);

  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr
  }else{
    data<-mSetObj$dataSet$prenorm
  }
    
  numData <- select_if(data, is.numeric)
  colNamesNum <- colnames(numData)
  rowNamesNum <- rownames(numData)

  charData <- select_if(data, is.character)
  colNamesChar <- colnames(charData)

  #QuantileNorm
  numDataQuantileNorm <- QuantileNormalize(numData);
  colnames(numDataQuantileNorm) <- colNamesNum;
  rownames(numDataQuantileNorm) <- rowNamesNum;

  #ProbNormGroup
  grp.inx <- cls == ref;
  grp.ref.smpl <- numData[,grp.inx]; #I CHANGED THIS TO COLS
  numDataGroupPQN <- apply(numData, 2, ProbNorm, grp.ref.smpl);
  colnames(numDataGroupPQN) <- colNamesNum;
  rownames(numDataGroupPQN) <- rowNamesNum;

  #ProbNormSample
  samp.ref.smpl <- numData[ref,];
  numDataSamplePQN <- apply(numData, 1, ProbNorm, samp.ref.smpl); #I CHANGED THIS TO ROWS
  colnames(numDataSamplePQN) <- colNamesNum;
  rownames(numDataSamplePQN) <- rowNamesNum;

  #SumNorm
  numDataSumNorm<-apply(numData, 2, SumNorm);
  colnames(numDataSumNorm) <- colNamesNum;
  rownames(numDataSumNorm) <- rowNamesNum;

  #MedianNorm
  numDataMedianNorm<-apply(numData, 2, MedianNorm);
  colnames(numDataMedianNorm) <- colNamesNum;
  rownames(numDataMedianNorm) <- rowNamesNum;

  #BoxNorm
  if(sum(as.numeric(numData<0)) > 0){
    numDataBoxNorm<-apply(numData, 2, YeoNorm);
  } else {
    numDataBoxNorm<-apply(numData, 2, BoxNorm);
  }
  colnames(numDataBoxNorm) <- colNamesNum;
  rownames(numDataBoxNorm) <- rowNamesNum;

  numDataNormList <- list(numDataQuantileNorm, numDataGroupPQN, numDataSamplePQN, numDataSumNorm, numDataMedianNorm, numDataBoxNorm)
  normNames <- c("QuantileNorm", "CompNorm", "SamplePQN", "SumNorm", "MedianNorm", "BoxNorm")

  SWList <- list()
  for (i in 1:length(numDataNormList)) {
    SWList[[normNames[i]]] <- apply(numDataNormList[[i]], 2, shapiroWilk) #FINISH WRITING SW FUNCTION!!!!
  }    
    Next: count the number of p-values above 0.05. The highest count wins.
    If there is a tie, sum the p-values and the highest number wins.
    Whoever wins, record i.
  normBest <- numDataNormList[[i]]
  numDataNorm <- normNames[i]

  data_1 <- cbind(numDataNorm, charData)
  colNames <- c(colNamesNum, colNamesChar)
  colnames(data_1) <- colNames
  rownames(data_1) <- rownames(data)

  mSetObj$dataSet$norm <- data_1;
  mSetObj$dataSet$best.method <- normBest;
  return(.set.mSet(mSetObj));
}


#'Normalization
#'@description This function performs row-wise normalization, transformation, and 
#'scaling of your metabolomic data. 
#'@usage Normalization(mSetObj, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param rowNorm Select the option for row-wise normalization, "QuantileNorm" for Quantile Normalization, 
#'"ProbNormT" for Probabilistic Quotient Normalization without using a reference sample,
#'"ProbNormF" for Probabilistic Quotient Normalization based on a reference sample, 
#'"CompNorm" for Normalization by a reference variable,
#'"SumNorm" for Normalization to constant sum, 
#'"MedianNorm" for Normalization to sample median, and 
#'"SpecNorm" for Normalization by a sample-specific factor.
#'@param transNorm Select option to transform the data, "LogNorm" for Log Normalization,
#'and "CrNorm" for Cubic Root Transformation. 
#'@param scaleNorm Select option for scaling the data, "MeanCenter" for Mean Centering,
#'"AutoNorm" for Autoscaling, "ParetoNorm" for Pareto Scaling, amd "RangeNorm" for Range Scaling.
#'@param ref Input the name of the reference sample or the reference variable, use " " around the name.  
#'@param ratio This option is only for biomarker analysis.
#'@param ratioNum Relevant only for biomarker analysis.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong
#'McGill University, Canada
#'@export
#'
Normalization <- function(mSetObj=NA, rowNorm, transNorm, scaleNorm, ref=NULL, ratio=FALSE, ratioNum=20){
  
  mSetObj <- .get.mSet(mSetObj);
  load_dplyr()

  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  
  numData <- select_if(data, is.numeric)
  charData <- select_if(data, is.character)

  if(!is.null(mSetObj$dataSet[["origMeta"]])){
    meta <-mSetObj$dataSet$origMeta
  }
  if(!is.null(mSetObj$dataSet[["origEnv"]])){
    env <-mSetObj$dataSet$origEnv
  }

  if(is.null(mSetObj$dataSet[["prenorm.cls"]])){ # can be so for regression 
    mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$proc.cls;
  }
  
  cls <- mSetObj$dataSet$prenorm.cls;
  
  # note, setup time factor
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    if(exists('prenorm.facA', where = mSetObj$dataSet)){
      nfacA <- mSetObj$dataSet$prenorm.facA;
      nfacB <- mSetObj$dataSet$prenorm.facB;
    }else{
      nfacA <- mSetObj$dataSet$facA;
      nfacB <- mSetObj$dataSet$facB;
    }
    
    mSetObj$dataSet$facA <- nfacA;
    mSetObj$dataSet$facB <- nfacB;
    if(mSetObj$dataSet$design.type =="time" | mSetObj$dataSet$design.type =="time0"){
      # determine time factor and should order first by subject then by each time points
      if(tolower(mSetObj$dataSet$facA.lbl) == "time"){ 
        time.fac <- nfacA;
        exp.fac <- nfacB;
      }else{
        time.fac <- nfacB;
        exp.fac <- nfacA;
      }
      mSetObj$dataSet$time.fac <- time.fac;
      mSetObj$dataSet$exp.fac <- exp.fac;
    }
  }
  
  colNames <- colnames(numData);
  rowNames <- rownames(numData);
  
  # row-wise normalization #CHANGED TO COLUMN-WISE FOR WEGAN!!!!
  if(rowNorm=="QuantileNorm"){
    numData<-QuantileNormalize(numData);
    # this can introduce constant variables if a variable is 
    # at the same rank across all samples (replaced by its average across all)
    #varCol <- apply(numData, 2, var, na.rm=T); #I REMOVED THIS
    #constCol <- (varCol == 0 | is.na(varCol));
    #constNum <- sum(constCol, na.rm=T);
    #if(constNum > 0){
    #  print(paste("After quantile normalization", constNum, "variables with a constant value were found and deleted."));
    #  numData <- numData[,!constCol];
    #  colNames <- colnames(numData);
    #  rowNames <- rownames(numData);
    #}
    rownm<-"Quantile Normalization";
  }else if(rowNorm=="GroupPQN"){
    grp.inx <- cls == ref;
    ref.smpl <- apply(numData[grp.inx, ], 1, mean);
    numData<-t(apply(numData, 2, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference variable";
  }else if(rowNorm=="SamplePQN"){
    ref.smpl <- numData[ref,];
    numData<-t(apply(numData, 2, ProbNorm, ref.smpl));
    rownm<-"Probabilistic Quotient Normalization by a reference sample";
  }else if(rowNorm=="CompNorm"){
    numData<-t(apply(numData, 2, CompNorm, ref));
    rownm<-"Normalization by a reference variable";
  }else if(rowNorm=="SumNorm"){
    numData<-t(apply(numData, 2, SumNorm));
    rownm<-"Normalization to constant sum";
  }else if(rowNorm=="MedianNorm"){
    numData<-t(apply(numData, 2, MedianNorm));
    rownm<-"Normalization to sample median";
  }else if(rowNorm=="SpecNorm"){
    if(!exists("norm.vec")){
      norm.vec <- rep(1,nrow(numData)); # default all same weight vec to prevent error
      print("No sample specific information were given, all set to 1.0");
    }
    rownm<-"Normalization by sample-specific factor";
    numData<-numData/norm.vec;
  }else{
    # nothing to do
    rownm<-"N/A";
  }
  
  # use apply will lose dimesion info (i.e. row names and colnames)
  rownames(numData)<-rowNames;
  colnames(numData)<-colNames;
  
  # note: row-normed data is based on biological knowledge, since the previous
  # replacing zero/missing values by half of the min positive (a constant) 
  # now may become different due to different norm factor, which is artificial
  # variance and should be corrected again
  # stopped, this step cause troubles
  # minConc<-round(min(numData)/2, 5);
  # numData[dataSet$fill.inx]<-minConc;
  
  # if the reference by variable, the variable column should be removed, since it is all 1
  if(rowNorm=="CompNorm" && !is.null(ref)){
    inx<-match(ref, colnames(numData));
    numData<-numData[,-inx];
    colNames <- colNames[-inx];
  }
  
  # record row-normed data for fold change analysis (b/c not applicable for mean-centered data)
  #mSetObj$dataSet$row.norm <- as.data.frame(CleanData(numData, T, T)); #I REMOVED THIS

  # this is for biomarker analysis only (for compound concentration data)
  if(ratio){
    min.val <- min(abs(numData[numData!=0]))/2;
    norm.data <- log2((numData + sqrt(numData^2 + min.val))/2);
    transnm<-"Log Normalization";
    ratio.mat <- CalculatePairwiseDiff(norm.data);
    
    fstats <- Get.Fstat(ratio.mat, cls);
    hit.inx <- rank(-fstats) < ratioNum;  # get top n
    
    ratio.mat <- ratio.mat[, hit.inx];
    
    numData <- cbind(norm.data, ratio.mat);
    
    colNames <- colnames(numData);
    rowNames <- rownames(numData);
    mSetObj$dataSet$procr <- numData;
    mSetObj$dataSet$use.ratio <- TRUE;
    mSetObj$dataSet$proc.ratio <- numData;

  }else{
    mSetObj$dataSet$use.ratio <- FALSE;
    # transformation
    if(transNorm=='LogNorm'){
      min.val <- min(abs(numData[numData!=0]))/10;
      numData<-apply(numData, 2, LogNorm, min.val);
      transnm<-"Log Normalization";
    }else if(transNorm=='CrNorm'){
      norm.data <- abs(numData)^(1/3);
      norm.data[numData<0] <- - norm.data[numData<0];
      numData <- norm.data;
      transnm<-"Cubic Root Transformation";
    }else{
      transnm<-"N/A";
    }
  }
  
  # scaling
  if(scaleNorm=='MeanCenter'){
    numData<-apply(numData, 2, MeanCenter);
    scalenm<-"Mean Centering";
  }else if(scaleNorm=='AutoNorm'){
    numData<-apply(numData, 2, AutoNorm);
    scalenm<-"Autoscaling";
  }else if(scaleNorm=='ParetoNorm'){
    numData<-apply(numData, 2, ParetoNorm);
    scalenm<-"Pareto Scaling";
  }else if(scaleNorm=='RangeNorm'){
    numData<-apply(numData, 2, RangeNorm);
    scalenm<-"Range Scaling";
  }else{
    scalenm<-"N/A";
  }
  
  # note after using "apply" function, all the attribute lost, need to add back
  rownames(numData)<-rowNames;
  colnames(numData)<-colNames;
  
  # need to do some sanity check, for log there may be Inf values introduced
  #numData <- CleanData(numData, T, F, F); #I REMOVED THIS FOR NOW
  #meta <- meta[rownames(numData),]
  #env <- env[rownames(numData),]
  #charData <- charData[rownames(numData),]

  if(ratio){
    mSetObj$dataSet$ratio <- CleanData(ratio.mat, T, F)
  }
  
  dataNorm <- cbind(data, charData)

  mSetObj$dataSet$norm <- as.data.frame(dataNorm);
  mSetObj$dataSet$cls <- cls;
  
  mSetObj$dataSet$rownorm.method <- rownm;
  mSetObj$dataSet$trans.method <- transnm;
  mSetObj$dataSet$scale.method <- scalenm;
  mSetObj$dataSet$combined.method <- FALSE;
  mSetObj$dataSet$norm.all <- NULL; # this is only for biomarker ROC analysis
  return(.set.mSet(mSetObj));
}


#'Row-wise Normalization
#'@description Row-wise norm methods, when x is a row.
#'Normalize by a sum of each sample, assume constant sum (1000).
# Return: normalized data.
#'Options for normalize by sum median, reference sample,
#'reference reference (compound), or quantile normalization
#'@param x Input data to normalize
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'@export
#'
SumNorm<-function(x){
  1000*x/sum(x, na.rm=T);
}

# normalize by median
MedianNorm<-function(x){
  x/median(x, na.rm=T);
}

# normalize by a reference sample (probability quotient normalization)
# ref should be the name of the reference sample
ProbNorm<-function(x, ref.smpl){
  x/median(as.numeric(x/ref.smpl), na.rm=T)
}

# normalize by a reference reference (i.e. creatinine)
# ref should be the name of the cmpd
CompNorm<-function(x, ref){
  1000*x/x[ref];
}

# perform quantile normalization on the raw data (can be log transformed later by user)
# https://stat.ethz.ch/pipermail/bioconductor/2005-April/008348.html
QuantileNormalize <- function(data){
  return(t(preprocessCore::normalize.quantiles(t(data), copy=FALSE)));
}

#'Column-wise Normalization
#'@description Column-wise norm methods, when x is a column
#'Options for log, zero mean and unit variance, and
#'several zero mean and variance/SE 
#'@param x Input data
#'@param min.val Input minimum value
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada

# generalize log, tolerant to 0 and negative values
LogNorm<-function(x, min.val){
  log2((x + sqrt(x^2 + min.val^2))/2)
}

# normalize to zero mean and unit variance
AutoNorm<-function(x){
  (x - mean(x))/sd(x, na.rm=T);
}

# normalize to zero mean but variance/SE
ParetoNorm<-function(x){
  (x - mean(x))/sqrt(sd(x, na.rm=T));
}

# normalize to zero mean but variance/SE
MeanCenter<-function(x){
  x - mean(x);
}

# normalize to zero mean but variance/SE
RangeNorm<-function(x){
  if(max(x) == min(x)){
    x;
  }else{
    (x - mean(x))/(max(x)-min(x));
  }
}

#'Two plot summary plot: Variable View of before and after normalization
#'@description For each plot, the top is a box plot, bottom is a density plot
#'@usage PlotNormSummary(mSetObj, imgName, format, dpi, width)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export
#'
PlotNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  mSetObj <- .get.mSet(mSetObj);
print("1")
print(mSetObj$dataSet$procr)

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10.5; h <- 12.5;
  }else if(width==0){
    w = 7.2
    h = 9.5
  }else if(width>0){
    w = width
    h = width*1.25
    # w <- 7.2; h <- 9;
  }
  
  mSetObj$imgSet$norm <- imgName
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,2,2,2,3,4,4,4), 4, 2, byrow = FALSE))
  
  # since there may be too many compounds, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(ncol(mSetObj$dataSet$procr), sub.num=50);
  namesVec <- colnames(mSetObj$dataSet$procr[,pre.inx]);
  
  # only get common ones
  nm.inx <- namesVec %in% colnames(mSetObj$dataSet$norm)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, colnames(mSetObj$dataSet$norm));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(mSetObj$dataSet$procr[, pre.inx], na.rm=T);
  rangex.norm <- range(mSetObj$dataSet$norm[, norm.inx], na.rm=T);
  
  x.label<-GetAbundanceLabel(mSetObj$dataSet$type);
  y.label<-GetVariableLabel(mSetObj$dataSet$type);
  
  # fig 1
  op<-par(mar=c(4,7,4,0), xaxt="s");
  plot(density(apply(mSetObj$dataSet$procr, 2, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
  mtext("Density", 2, 5);
  mtext("Before Normalization",3, 1)
  
  # fig 2
  op<-par(mar=c(7,7,0,0), xaxt="s");
  boxplot(mSetObj$dataSet$procr[,pre.inx], names= namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T);
  mtext(x.label, 1, 5);
  
  # fig 3
  op<-par(mar=c(4,7,4,2), xaxt="s");
  plot(density(apply(mSetObj$dataSet$norm, 2, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
  mtext("After Normalization",3, 1);
  
  # fig 4
  op<-par(mar=c(7,7,0,2), xaxt="s");
  boxplot(mSetObj$dataSet$norm[,norm.inx], names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", horizontal=T);
  mtext(paste("Normalized",x.label),1, 5);
  
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'Two plot summary plot: Sample View of before and after normalization
#'@description For each plot, the top is a density plot and the bottom is a box plot.
#'@usage PlotSampleNormSummary(mSetObj=NA, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", of "pdf". 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.   
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export

PlotSampleNormSummary <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10.5; h <- 12.5;
  }else if(width == 0){
    w <- 7.2;h <- 9.5;
  }else if(width>0){
    w = width
    h = width*1.25
    # w <- 7.2; h <- 9;
  }
  
  mSetObj$imgSet$summary_norm <-imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  layout(matrix(c(1,1,1,2,3,3,3,4), 4, 2, byrow = FALSE))
  
  # since there may be too many samples, only plot a subsets (50) in box plot
  # but density plot will use all the data
  
  pre.inx<-GetRandomSubsetIndex(nrow(mSetObj$dataSet$procr), sub.num=50);
  namesVec <- rownames(mSetObj$dataSet$procr[pre.inx,]);
  
  # only get common ones
  nm.inx <- namesVec %in% rownames(mSetObj$dataSet$norm)
  namesVec <- namesVec[nm.inx];
  pre.inx <- pre.inx[nm.inx];
  
  norm.inx<-match(namesVec, rownames(mSetObj$dataSet$norm));
  namesVec <- substr(namesVec, 1, 12); # use abbreviated name
  
  rangex.pre <- range(mSetObj$dataSet$procr[pre.inx,], na.rm=T);
  rangex.norm <- range(mSetObj$dataSet$norm[norm.inx,], na.rm=T);
  
  x.label<-GetAbundanceLabel(mSetObj$dataSet$type);
  y.label<-"Samples";
  
  # fig 1
  op<-par(mar=c(5.75,8,4,0), xaxt="s");
  boxplot(t(mSetObj$dataSet$procr[pre.inx, ]), names= namesVec, ylim=rangex.pre, las = 2, col="lightgreen", horizontal=T);
  mtext("Before Normalization", 3,1)
  
  # fig 2
  op<-par(mar=c(6.5,7,0,0), xaxt="s");
  plot(density(apply(mSetObj$dataSet$procr, 1, mean, na.rm=TRUE)), col='darkblue', las =2, lwd=2, main="", xlab="", ylab="");
  mtext(x.label, 1, 4);
  mtext("Density", 2, 5);
  
  # fig 3
  
  op<-par(mar=c(5.75,8,4,2), xaxt="s");
  boxplot(t(mSetObj$dataSet$norm[norm.inx,]), names=namesVec, ylim=rangex.norm, las = 2, col="lightgreen", ylab="", horizontal=T);
  mtext("After Normalization", 3, 1);
  
  # fig 4
  op<-par(mar=c(6.5,7,0,2), xaxt="s");
  plot(density(apply(mSetObj$dataSet$norm, 1, mean, na.rm=TRUE)), col='darkblue', las=2, lwd =2, main="", xlab="", ylab="");
  mtext(paste("Normalized",x.label),1, 4)
  
  dev.off();
  return(.set.mSet(mSetObj));
}


#'Box-Cox normalization
#'@description performs Box-Cox (data must be 0 or positive values)
#'@usage BoxNorm(x)
#'@param x is the column being normalized 
#'@author Louisa Normington \email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'@export
BoxNorm <- function(data, x) {
  library(EnvStats)
  boxcox(x, lambda=seq(-6, 6, 0.1))
}


#'Yeo-Johnson normalization
#'@description performs Yeo-Johnson normalization (data can have negative values)
#'@usage YeoNorm(x)
#'@param x is the column being normalized 
#'@author Louisa Normington \email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'@export
YeoNorm <- function(x) {
  library(VGAM)
  yeo.johnson(x, lambda=seq(-6,6,0.1))
}


#'Shapiro Wilk test for normality
#'@usage shapiroWilk(x)
#'@param x is the column being normalized 
#'@author Louisa Normington \email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'@export
shapiroWilk <- function(x) {
  shapiro <- shapiro.test(x) #Perform the test
  #extract the p_value!!!!!!!!!!!!!
  return(p)
}




##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################


#'Remove a group from the data 
#'@description This function removes a user-specified group from the data set.
#'This must be performed following data processing and filtering. If the data was normalized prior to removal,
#'you must re-normalize the data. 
#'@usage UpdateGroupItems(mSetObj=NA, grp.nm.vec)  
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param grp.nm.vec Input the name of the group you would like to remove from the data set in quotation marks 
#'(ex: "Disease B") The name must be identical to a class label. 
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export
#'
UpdateGroupItems <- function(mSetObj=NA, grp.nm.vec){
  
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$filt)){
    data <- mSetObj$dataSet$procr;
    cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$proc.facA;
      facB <- mSetObj$dataSet$proc.facB;
    }
  }else{
    data <- mSetObj$dataSet$filt;
    cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$filt.facA;
      facB <- mSetObj$dataSet$filt.facB;
    }
  }
  
  hit.inx <- cls %in% grp.nm.vec;
  mSetObj$dataSet$prenorm <- CleanDataMatrix(data[!hit.inx,,drop=FALSE]);

  mSetObj$dataSet$prenorm.cls <- droplevels(factor(cls[!hit.inx])); 
  
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    mSetObj$dataSet$prenorm.facA <- droplevels(factor(facA[!hit.inx]));
    mSetObj$dataSet$prenorm.facB <- droplevels(factor(facB[!hit.inx]));
  }
  
  AddMsg("Successfully updated the group items!");
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(length(levels(mSetObj$dataSet$prenorm.cls)));
  }else{
    return(.set.mSet(mSetObj));
  }
}

#'Remove samples from user's data
#'@description This function removes samples from the data set. This must be performed following data processing and filtering.
#'If the data was normalized prior to removal, you must re-normalize the data.  
#'@usage UpdateSampleItems(mSetObj=NA, smpl.nm.vec)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param smpl.nm.vec Input the name of the sample to remove from the data in quotation marks. The name must be identical to the 
#'sample names found in the data set.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export
#'
UpdateSampleItems <- function(mSetObj=NA, smpl.nm.vec){
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$filt)){
    data <- mSetObj$dataSet$procr;
    cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$proc.facA;
      facB <- mSetObj$dataSet$proc.facB;
    }
  }else{
    data <- mSetObj$dataSet$filt;
    cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$filt.facA;
      facB <- mSetObj$dataSet$filt.facB;
    }
  }
  
  hit.inx <- rownames(data) %in% smpl.nm.vec;
  mSetObj$dataSet$prenorm <- CleanDataMatrix(data[!hit.inx,,drop=FALSE]);

  mSetObj$dataSet$prenorm.cls <- as.factor(as.character(cls[!hit.inx]));
  if(substring(mSetObj$dataSet$format,4,5)=="ts"){
    mSetObj$dataSet$prenorm.facA <- as.factor(as.character(facA[!hit.inx]));
    mSetObj$dataSet$prenorm.facB <- as.factor(as.character(facB[!hit.inx]));
  }
  
  AddMsg("Successfully updated the sample items!");
  
  if(.on.public.web){
    .set.mSet(mSetObj);
    return(length(levels(mSetObj$dataSet$prenorm.cls)));
  }else{
    return(.set.mSet(mSetObj)); 
  }
}

#' Remove variable items
#' @description This function removes user-selected variables from the data set. 
#' This must be performed following data processing and filtering.
#' If the data was normalized prior to removal, you must re-normalize the data.  
#' @usage UpdateFeatureItems(mSetObj=NA, variable.nm.vec)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#' @param variable.nm.vec Input the name of the variable to remove from the data in quotation marks. 
#' The name must be identical to the variable names found in the data set.  
#'@author Jeff Xia \email{jeff.xia@mcgill.ca}, Jasmine Chong 
#'McGill University, Canada
#'@export
#'
UpdateFeatureItems <- function(mSetObj=NA, variable.nm.vec){
  
  mSetObj <- .get.mSet(mSetObj);
  if(is.null(mSetObj$dataSet$filt)){
    data <- mSetObj$dataSet$procr;
    cls <- mSetObj$dataSet$proc.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$proc.facA;
      facB <- mSetObj$dataSet$proc.facB;
    }
  }else{
    data <- mSetObj$dataSet$filt;
    cls <- mSetObj$dataSet$filt.cls;
    if(substring(mSetObj$dataSet$format,4,5)=="ts"){
      facA <- mSetObj$dataSet$filt.facA;
      facB <- mSetObj$dataSet$filt.facB;
    }
  }
  
  hit.inx <- colnames(data) %in% variable.nm.vec;
  mSetObj$dataSet$prenorm <- CleanDataMatrix(data[,!hit.inx,drop=FALSE]);

  mSetObj$dataSet$prenorm.cls <- cls; # this is the same
  
  AddMsg("Successfully updated the variable items!");
  return(.set.mSet(mSetObj));
}


InitPrenormData <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    if(is.null(mSetObj$dataSet[["prenorm"]])){
        if(is.null(mSetObj$dataSet[["filt"]])){
            mSetObj$dataSet$prenorm <- mSetObj$dataSet$procr;
            mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$proc.cls;
            if(substring(mSetObj$dataSet$format,4,5) == "ts"){
                mSetObj$dataSet$prenorm.facA <- mSetObj$dataSet$proc.facA;
                mSetObj$dataSet$prenorm.facB <- mSetObj$dataSet$proc.facB;
            }
        }else{
            mSetObj$dataSet$prenorm <- mSetObj$dataSet$filt;
            mSetObj$dataSet$prenorm.cls <- mSetObj$dataSet$filt.cls;
            if(substring(mSetObj$dataSet$format,4,5)=="ts"){
                mSetObj$dataSet$prenorm.facA <- mSetObj$dataSet$filt.facA;
                mSetObj$dataSet$prenorm.facB <- mSetObj$dataSet$filt.facB;
            }
        }
        .set.mSet(mSetObj)
    }
}

# get the dropdown list for sample normalization view
GetPrenormSmplNms <-function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$dataSet$prenorm));
}

GetPrenormFeatureNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$dataSet$prenorm));
}

GetPrenormClsNms <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(levels(mSetObj$dataSet$prenorm.cls));
}

########## Utility Functions ###############
GetRandomSubsetIndex<-function(total, sub.num = 50){
  if(total < sub.num){
    1:total;
  }else{
    sample(1:total, sub.num);
  }
}