#' #'Plot Dendrogram 
#' #'@description Dendogram
#' #'@param mSetObj Input name of the created mSet Object
#' #'@param imgName Input a name for the plot
#' #'@param format Select the image format, "png", or "pdf".
#' #'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#' #'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#' #'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#' #'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#' #'@param smplDist Method to calculate sample distance
#' #'@param clstDist Method to calculate clustering distance 
#' #'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#' #'McGill University, Canada
#' #'License: GNU GPL (>= 2)
#' #'@export
#' #'
#' PlotHCTree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, smplDist, clstDist){
#'   
#'   mSetObj <- .get.mSet(mSetObj);
#'   # set up data set
#'   hc.dat <- as.matrix(mSetObj$dataSet$norm);
#'   colnames(hc.dat) <- substr(colnames(hc.dat), 1, 18) # some names are too long
#'   # set up distance matrix
#'   if(smplDist == 'euclidean'){
#'     dist.mat <- dist(hc.dat, method = smplDist);
#'   }else{
#'     dist.mat <- dist(1-cor(t(hc.dat), method = smplDist));
#'   }
#'   
#'   # record the paramters
#'   mSetObj$analSet$tree <- list(dist.par=smplDist, clust.par=clstDist);
#'   # build the tree
#'   hc_tree <- hclust(dist.mat, method=clstDist);
#'   
#'   # plot the tree
#'   imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
#'   if(is.na(width)){
#'     w <- minH <- 630;
#'     myH <- nrow(hc.dat)*10 + 150;
#'     if(myH < minH){
#'       myH <- minH;
#'     }   
#'     w <- round(w/72,2);
#'     h <- round(myH/72,2);
#'   }else if(width == 0){
#'     w <- h <- 7.2;
#'   }else{
#'     w <- h <- 7.2;
#'   }
#'   
#'   mSetObj$imgSet$tree <- imgName;
#'   
#'   Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
#'   par(cex=0.8, mar=c(4,2,2,8));
#'   if(mSetObj$dataSet$cls.type == "disc"){
#'     clusDendro <- as.dendrogram(hc_tree);
#'     cols <- GetColorSchema(mSetObj);
#'     names(cols) <- rownames(hc.dat);
#'     labelColors <- cols[hc_tree$order];
#'     colLab <- function(n){
#'       if(is.leaf(n)) {
#'         a <- attributes(n)
#'         labCol <- labelColors[a$label];
#'         attr(n, "nodePar") <- 
#'           if(is.list(a$nodePar)) c(a$nodePar, lab.col = labCol,pch=NA) else
#'             list(lab.col = labCol,pch=NA)
#'       }
#'       n
#'     }
#'     clusDendro <- dendrapply(clusDendro, colLab)
#'     plot(clusDendro,horiz=T,axes=T);
#'     par(cex=1);
#'     
#'     if(mSetObj$dataSet$type.cls.lbl=="integer"){
#'       legend.nm <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
#'     }else{
#'       legend.nm <- as.character(mSetObj$dataSet$cls);
#'     }
#'     
#'     legend("topleft", legend = unique(legend.nm), pch=15, col=unique(cols), bty = "n");
#'     
#'   }else{
#'     plot(as.dendrogram(hc_tree), hang=-1, main=paste("Cluster with", clstDist, "method"), xlab=NULL, sub=NULL, horiz=TRUE);
#'   }
#'   dev.off();
#'   
#'   return(.set.mSet(mSetObj));
#' }

#'Plot Dendrogram 
#'@description Dendogram
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param data Which data set to use, normalized (default) or original
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param smplDist Method to calculate sample distance, 'euclidean' (default), 'spearman', 'pearson'.
#'@param clstDist Method to calculate clustering distance, 'ward' (default), 'average', 'complete', 'single'.
#'@param rotate Default is horizontal, checkbox option for vertical.
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

#### Trouble Shooting assignments ####
# dataset <- iris;
# smplDist <- 'euclidean'
# clstDist <- 'complete'
# imgName <- 'test'
# rotate <- 'true'
# branch_labels <- 'false'
# plot_palette <- "plasma"
# plot_title <- "This is the Title"
# plot_legtitle <- "legend_title"
# colorbar_name <- 'Species' # column names to pick from
#######################################

# Get column names from user dataset
dendro.columns <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$analSet$dendro$data
  name.all.cols <- c(colnames(metaData), "No groupings")
  return(name.all.cols)
}


PlotHCTree <- function(mSetObj=NA,
                       data = "false",
                       imgName, 
                       format="png", 
                       dpi=72, 
                       width=NA, 
                       smplDist="NULL", 
                       clstDist="NULL",
                       rotate='false',
                       branch_labels='false',
                       plot_palette="NULL",
                       plot_legtitle=" ",
                       plot_title=" ",
                       colorbar_name="NULL"){
  
  library('ggplot2')
  library('dplyr')
  library('ggdendro')
  library("viridis")
  
  # Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="false") {
    dataset <- mSetObj$dataSet$norm
  } else {
    dataset <- mSetObj$dataSet$orig
  };
  
  #colnames(dataset) <- substr(colnames(dataset), 1, 18)  # some names are too long
  
  # set up distance matrix
  input <- select_if(dataset, is.numeric)
  if (smplDist == 'euclidean' || smplDist == "NULL"){  # default is 'euclidean'
    smplDist <- 'euclidean'
    dist.mat <- dist(input, method = smplDist);
  }else {
    dist.mat <- dist(1-cor(t(input), method = smplDist));
  }
  
  if (clstDist == "NULL"){  # default is 'ward'
    clstDist <- 'ward'
  }
  
  # record the paramters
  mSetObj$analSet$tree <- list(dist.par=smplDist, clust.par=clstDist);
  
  # build the tree
  hc <- hclust(dist.mat, method=clstDist);
  dendro <- as.dendrogram(hc)
  ddata <- dendro %>% dendro_data(type="rectangle")
  
  # Set up df for color bar option and join df by index values so categorical value aligned properly
  facA_df <- dataset %>% mutate(id=row_number(), var = as.factor(dataset[,colorbar_name])) %>% select(c(var, id));
  ddata$labels <- ddata$labels %>% mutate(id=hc$order) %>% left_join(facA_df, by='id')
  
  p <- ggplot() + 
    # Plot dendrogram
    geom_segment(data = ddata$segments, aes(x = x, y = y, xend = xend, yend = yend)) +
    # Add title
    ggtitle(plot_title) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_blank(),
          axis.ticks =  element_blank(),
          axis.line = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank(),
          legend.position = c("top"))
  # Add text labels 
  if (branch_labels == 'true') {
    scale_y_continuous(expand = expansion(mult = c(.2, .05)))
    if (rotate == 'true'){
      p <- p + geom_text(data = ddata$labels,
                         aes(x = x, y = -(max(ddata$segments$yend)*0.05), label = label),
                         color = "black",
                         vjust = 'middle',
                         hjust = 'right',
                         angle = 0,
                         show.legend = FALSE) +
        coord_flip()
    }
    else{
      p <- p + geom_text(data = ddata$labels,
                         aes(x = x, y = -(max(ddata$segments$yend)*0.05), label = label),
                         color = "black",
                         vjust = 'middle',
                         hjust = 'right',
                         angle = 90,
                         show.legend = FALSE)
    }
    
  }
  # Add colorbar and  corresponding legend
  if (colorbar_name != "NULL"){
    p <- p + geom_point(data = ddata$labels, 
                        aes(x = x, y = y, color = var), 
                        shape = 15,
                        size=2.5,
                        show.legend = TRUE) + 
      theme(legend.background = element_blank(),
            legend.key = element_blank(),
            legend.position = c("top")) + 
      guides(color=guide_legend(title=plot_legtitle)) +
      scale_color_viridis(discrete=TRUE, option=plot_palette)
  }
  
  
  
  
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
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  print(p)
  dev.off()
  
  # Make JSON
  
  #### Making Dendrogram with Cairo
  # Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  # par(cex=0.8, mar=c(4,2,2,8));
  # if(mSetObj$dataSet$cls.type == "disc"){
  #   clusDendro <- as.dendrogram(hc_tree);
  #   #cols <- GetColorSchema(mSetObj);
  #   cols <- as.numeric(mSetObj$dataSet$cls)+1
  #   names(cols) <- rownames(input);
  #   labelColors <- cols[hc_tree$order];
  #   colLab <- function(n){
  #     if(is.leaf(n)) {
  #       a <- attributes(n)
  #       labCol <- labelColors[a$label];
  #       attr(n, "nodePar") <- 
  #         if(is.list(a$nodePar)) c(a$nodePar, lab.col = labCol,pch=NA) else
  #           list(lab.col = labCol,pch=NA)
  #     }
  #     n
  #   }
  #   clusDendro <- dendrapply(clusDendro, colLab)
  #   plot(clusDendro,horiz=T,axes=T);
  #   par(cex=1);
  #   
  #   if(mSetObj$dataSet$type.cls.lbl=="integer"){
  #     legend.nm <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])));
  #   }else{
  #     legend.nm <- as.character(mSetObj$dataSet$cls);
  #   }
  #   
  #   legend("topleft", legend = unique(legend.nm), pch=15, col=unique(cols), bty = "n");
  #   
  # }else{
  #   plot(as.dendrogram(hc_tree), hang=-1, main=paste("Cluster with", clstDist, "method"), xlab=NULL, sub=NULL, horiz=TRUE);
  # }
  # dev.off();
  
  return(.set.mSet(mSetObj));
}

#'SOM analysis
#'@description SOM analysis
#'@param mSetObj Input name of the created mSet Object
#'@param x.dim Input X dimension for SOM analysis
#'@param y.dim Input Y dimension for SOM analysis
#'@param initMethod Input the method 
#'@param neigb Default is set to 'gaussian'
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
SOM.Anal <- function(mSetObj=NA, x.dim, y.dim, initMethod, neigb = 'gaussian'){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$som <- som::som(as.matrix(mSetObj$dataSet$norm), xdim=x.dim, ydim=y.dim, init=initMethod, neigh=neigb);
  return(.set.mSet(mSetObj));
}


#'SOM Plot
#'@description Plot SOM map for  less than 20 clusters
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSOM <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  xdim<-mSetObj$analSet$som$xdim;
  ydim<-mSetObj$analSet$som$ydim;
  total<-xdim*ydim;
  if(total>20) { return();}
  
  ylabel<-GetAbundanceLabel(mSetObj$dataSet$type);
  clust<-mSetObj$analSet$som$visual;
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*8/9;
  
  mSetObj$imgSet$som <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow = GetXYCluster(total), mar=c(5,4,2,2));
  for (i in 0:(xdim-1)) {
    xTrue<-clust$x == i;
    for (j in 0:(ydim-1)) {
      yTrue<-clust$y == j;
      sel.inx<-xTrue & yTrue; # selected row
      if(sum(sel.inx)>0){ # some cluster may not contain any member
        matplot(t(mSetObj$dataSet$norm[sel.inx, ]), type="l", col='grey', axes=F, ylab=ylabel,
                main=paste("Cluster(", i, ",", j,")", ", n=", sum(sel.inx), sep=""))
        lines(apply(mSetObj$dataSet$norm[sel.inx, ], 2, median), type="l", col='blue', lwd=1);
      }else{ # plot a dummy 
        plot(t(mSetObj$dataSet$norm[1, ]), type="n", axes=F, ylab=ylabel,
             main=paste("Cluster(", i, ",", j,")",", n=", sum(sel.inx),sep=""))
      }
      axis(2);
      axis(1, 1:ncol(mSetObj$dataSet$norm), substr(colnames(mSetObj$dataSet$norm), 1, 7), las=2);
    }
  }
  dev.off();
  
  return(.set.mSet(mSetObj));
}

#'K-means analysis
#'@description Perform K-means analysis
#'@param mSetObj Input name of the created mSet Object
#'@param clust.num Numeric, input the number of clusters for K-means analysis
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
Kmeans.Anal <- function(mSetObj=NA, clust.num){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$analSet$kmeans <- kmeans(mSetObj$dataSet$norm, clust.num, nstart=100);
  return(.set.mSet(mSetObj));
}

#'Plot K-means analysis
#'@description Plot K-means analysis
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PlotKmeans <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  clust.num <- max(mSetObj$analSet$kmeans$cluster);
  
  if(clust.num>20) return();
  # calculate arrangement of panel
  ylabel<- GetAbundanceLabel(mSetObj$dataSet$type);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 9;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w*8/9;
  
  mSetObj$imgSet$kmeans <- imgName;
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  par(mfrow = GetXYCluster(clust.num), mar=c(5,4,2,2));
  for (loop in 1:clust.num) {
    matplot(t(mSetObj$dataSet$norm[mSetObj$analSet$kmeans$cluster==loop,]), type="l", col='grey', ylab=ylabel, axes=F,
            main=paste("Cluster ",loop, ", n=", mSetObj$analSet$kmeans$size[loop], sep=""))
    lines(apply(mSetObj$dataSet$norm[mSetObj$analSet$kmeans$cluster==loop,], 2, median), type="l", col='blue', lwd=1);
    axis(2);
    axis(1, 1:ncol(mSetObj$dataSet$norm), substr(colnames(mSetObj$dataSet$norm), 1, 7), las=2);
  }
  dev.off();
  return(.set.mSet(mSetObj));
}

#'Create Sub Heat Map Plot
#'@description Plot a sub heatmap based on results from t-tests/ANOVA, VIP or randomforest
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width.  
#'@param dataOpt Set data options
#'@param scaleOpt Set the image scale
#'@param smplDist Input the sample distance method
#'@param clstDist Input the clustering distance method
#'@param palette Input color palette choice
#'@param method.nm Input the method for sub-heat map
#'@param top.num Input the top number
#'@param viewOpt Set heatmap options, default is set to "detail"
#'@param rowV Default is set to T
#'@param colV Default is set to T
#'@param border Indicate whether or not to show cell-borders, default is set to T
#'@param grp.ave Logical, default is set to F
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotSubHeatMap <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, dataOpt, scaleOpt, 
                           smplDist, clstDist, palette, method.nm, top.num, viewOpt, rowV=T, colV=T, border=T, grp.ave=F){
  
  mSetObj <- .get.mSet(mSetObj);
  var.nms = colnames(mSetObj$dataSet$norm);
  if(top.num < length(var.nms)){
    if(method.nm == 'tanova'){
      if(GetGroupNumber(mSetObj) == 2){
        if(is.null(mSetObj$analSet$tt)){
          Ttests.Anal(mSetObj);
          mSetObj <- .get.mSet(mSetObj);
        }
        var.nms <- names(sort(mSetObj$analSet$tt$p.value))[1:top.num];
      }else{
        if(is.null(mSetObj$analSet$aov)){
          ANOVA.Anal(mSetObj);
          mSetObj <- .get.mSet(mSetObj);
        }
        var.nms <- names(sort(mSetObj$analSet$aov$p.value))[1:top.num];
      }
    }else if(method.nm == 'cor'){
      if(is.null(mSetObj$analSet$cor.res)){
        Match.Pattern(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      
      # re-order for pretty view
      cor.res <- mSetObj$analSet$cor.res;
      
      ord.inx<-order(cor.res[,3]);
      cor.res <- cor.res[ord.inx, ];
      
      ord.inx<-order(cor.res[,1]);
      cor.res <- cor.res[ord.inx, ];
      
      var.nms <- rownames(cor.res)[1:top.num];
    }else if(method.nm == 'vip'){
      if(is.null(mSetObj$analSet$plsda)){
        PLSR.Anal(mSetObj);
        PLSDA.CV(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      vip.vars <- mSetObj$analSet$plsda$vip.mat[,1];# use the first component
      var.nms <- names(rev(sort(vip.vars)))[1:top.num];
    }else if(method.nm == 'rf'){
      if(is.null(analSet$rf)){
        RF.Anal(mSetObj);
        mSetObj <- .get.mSet(mSetObj);
      }
      var.nms <- GetRFSigRowNames()[1:top.num];
    }
  }
  var.inx <- match(var.nms, colnames(mSetObj$dataSet$norm));
  PlotHeatMap(mSetObj, imgName, format, dpi, width, dataOpt, scaleOpt, smplDist, clstDist, palette, viewOpt, rowV, colV, var.inx, border, grp.ave);
}

#'Create Heat Map Plot
#'@description Plot a heatmap based on results from t-tests/ANOVA, VIP or randomforest
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input a name for the plot
#'@param format Select the image format, "png", or "pdf".
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths, the first, width = NULL, is 10.5.
#'The second default is width = 0, where the width is 7.2. Otherwise users can input their own width. 
#'@param dataOpt Set data options
#'@param scaleOpt Set the image scale
#'@param smplDist Input the sample distance method
#'@param clstDist Input the clustering distance method
#'@param palette Input color palette choice
#'@param viewOpt Set heatmap options, default is set to "detail"
#'@param rowV Default is set to T
#'@param colV Default is set to T
#'@param var.inx Default is set to NA
#'@param border Indicate whether or not to show cell-borders, default is set to T
#'@param grp.ave Logical, default is set to F
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'
PlotHeatMap <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, dataOpt, scaleOpt, smplDist, 
                        clstDist, palette, viewOpt="detail", rowV=T, colV=T, var.inx=NA, border=T, grp.ave=F){
  
  mSetObj <- .get.mSet(mSetObj);
  
  # record the paramters
  mSetObj$analSet$htmap <- list(dist.par=smplDist, clust.par=clstDist);
  
  # set up data set
  if(dataOpt=="norm"){
    my.data <- mSetObj$dataSet$norm;
  }else{
    my.data <- mSetObj$dataSet$procr;
  }
  
  if(is.na(var.inx)){
    hc.dat<-as.matrix(my.data);
  }else{
    hc.dat<-as.matrix(my.data[,var.inx]);
  }
  
  colnames(hc.dat) <- substr(colnames(hc.dat),1,18) # some names are too long
  
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    hc.cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
  }else{
    hc.cls <- mSetObj$dataSet$cls;
  }
  
  if(grp.ave){ # only use group average
    lvs <- levels(hc.cls);
    my.mns <- matrix(ncol=ncol(hc.dat),nrow=length(lvs));
    for(i in 1:length(lvs)){
      inx <-hc.cls == lvs[i];
      my.mns[i,]<- apply(hc.dat[inx, ], 2, mean);
    }
    rownames(my.mns) <- lvs;
    colnames(my.mns) <- colnames(hc.dat);
    hc.dat <- my.mns;
    hc.cls <- as.factor(lvs);
  }
  
  # set up colors for heatmap
  if(palette=="gbr"){
    colors <- colorRampPalette(c("green", "black", "red"), space="rgb")(256);
  }else if(palette == "heat"){
    colors <- heat.colors(256);
  }else if(palette == "topo"){
    colors <- topo.colors(256);
  }else if(palette == "gray"){
    colors <- colorRampPalette(c("grey90", "grey10"), space="rgb")(256);
  }else{
    colors <- rev(colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(256));
  }
  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  
  if(is.na(width)){
    minW <- 630;
    myW <- nrow(hc.dat)*18 + 150;
    
    if(myW < minW){
      myW <- minW;
    }   
    w <- round(myW/72,2);
  }else if(width == 0){
    w <- 7.2;
  }else{
    w <- 7.2;
  }
  
  mSetObj$imgSet$heatmap <- imgName;
  
  myH <- ncol(hc.dat)*18 + 150;
  h <- round(myH/72,2);
  
  if(viewOpt == "overview"){
    if(is.na(width)){
      if(w > 9){
        w <- 9;
      }
    }else if(width == 0){
      if(w > 7.2){
        w <- 7.2;
      }
      
    }else{
      w <- 7.2;
    }
    if(h > w){
      h <- w;
    }
    
    mSetObj$imgSet$heatmap <- imgName;
  }
  
  # make the width smaller fro group average
  if(grp.ave){
    w <- nrow(hc.dat)*25 + 300;
    w <- round(w/72,2);
  }
  
  if(border){
    border.col<-"grey60";
  }else{
    border.col <- NA;
  }
  if(format=="pdf"){
    pdf(file = imgName, width=w, height=h, bg="white", onefile=FALSE);
  }else{
    Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  }
  if(mSetObj$dataSet$cls.type == "disc"){
    annotation <- data.frame(class = hc.cls);
    rownames(annotation) <- rownames(hc.dat); 
    
    # set up color schema for samples
    if(palette == "gray"){
      cols <- GetColorSchema(mSetObj, T);
      uniq.cols <- unique(cols);
    }else{
      cols <- GetColorSchema(mSetObj);
      uniq.cols <- unique(cols);
    }
    
    if(mSetObj$dataSet$type.cls.lbl=="integer"){
      cls <- as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls]);
    }else{
      cls <- mSetObj$dataSet$cls;
    }
    
    names(uniq.cols) <- unique(as.character(sort(cls)));
    ann_colors <- list(class= uniq.cols);
    
    pheatmap::pheatmap(t(hc.dat), 
             annotation=annotation, 
             fontsize=8, fontsize_row=8, 
             clustering_distance_rows = smplDist,
             clustering_distance_cols = smplDist,
             clustering_method = clstDist, 
             border_color = border.col,
             cluster_rows = colV, 
             cluster_cols = rowV,
             scale = scaleOpt, 
             color = colors,
             annotation_colors = ann_colors);
  }else{
    heatmap(hc.dat, Rowv = rowTree, Colv=colTree, col = colors, scale="column");
  }
  dev.off();
  return(.set.mSet(mSetObj));
}

##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'SOM analysis
#'@description Get members for given cluster index, return a character string
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param i Index of X
#'@param j Index of Y
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetSOMClusterMembers <- function(mSetObj=NA, i, j){
  mSetObj <- .get.mSet(mSetObj);
  clust <- mSetObj$analSet$som$visual;
  xTrue <- clust$x == i;
  yTrue <- clust$y == j;
  hit.inx <- xTrue & yTrue;
  
  all.cols <- GetColorSchema(mSetObj);
  paste("<font color=\"", all.cols[hit.inx], "\">", rownames(mSetObj$dataSet$norm)[hit.inx], "</font>",collapse =", ");
}

#'SOM analysis
#'@description Get members for given cluster index, return a character string
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
GetAllSOMClusterMembers <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  clust <- mSetObj$analSet$som$visual;
  xdim <- mSetObj$analSet$som$xdim;
  ydim <- mSetObj$analSet$som$ydim;
  
  clust.df = data.frame();
  rowNameVec = c();
  i = 0;
  while(i < xdim){
    j = 0;
    while(j < ydim){
      xTrue<-clust$x == i;
      yTrue<-clust$y == j;
      if(i==0 & j==0){ # bug in R, the first one need to be different
        clust.df <- rbind(paste(rownames(mSetObj$dataSet$norm)[xTrue & yTrue], collapse = " "));
        rowNameVec <- c(paste("Cluster(", i, ",", j,")"));
      }else{
        clust.df <- rbind(clust.df, paste(rownames(mSetObj$dataSet$norm)[xTrue & yTrue], collapse=" "));
        rowNameVec <- c(rowNameVec, paste("Cluster(", i, ",", j,")"));
      }
      j = j+1;
    }
    i = i+1;
  }
  row.names(clust.df) <- rowNameVec;
  colnames(clust.df) <- "Samples in each cluster";
  print(xtable::xtable(clust.df, align="l|p{8cm}", caption="Clustering result using SOM"),caption.placement="top", size="\\scriptsize");
}


# inx has to be 1 or 2
GetClassLabel<-function(mSetObj=NA, inx){
  mSetObj <- .get.mSet(mSetObj);
  levels(mSetObj$dataSet$cls)[inx]
}


#'K-means analysis - cluster
#'@description Get the cluster members for given index
#'add HTML color to the names based on its group membership
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param i Input the cluster index
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
GetKMClusterMembers <- function(mSetObj=NA, i){
  mSetObj <- .get.mSet(mSetObj);
  all.cols <- GetColorSchema(mSetObj);
  hit.inx <- mSetObj$analSet$kmeans$cluster== i;
  
  paste("<font color=\"", all.cols[hit.inx], "\">", rownames(mSetObj$dataSet$norm)[hit.inx], "</font>",collapse =", ");
  # paste(all.cols[hit.inx], rownames(dataSet$norm)[hit.inx], collapse =", ");
}

#'K-means analysis - cluster
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetAllKMClusterMembers <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  clust.df = data.frame();
  rowNameVec = c();
  i = 1;
  clust.num<-max(mSetObj$analSet$kmeans$cluster);
  while(i<=clust.num){
    if(i==1){
      clust.df <- rbind(paste(rownames(mSetObj$dataSet$norm)[mSetObj$analSet$kmeans$cluster== i], collapse = " "));
    }else{
      clust.df <- rbind(clust.df,paste(rownames(mSetObj$dataSet$norm)[mSetObj$analSet$kmeans$cluster== i], collapse = " "));
    }
    rowNameVec <- c(rowNameVec, paste("Cluster(", i, ")"));
    i = i+1;
  }
  row.names(clust.df) <- rowNameVec;
  colnames(clust.df) <-"Samples in each cluster";
  print(xtable::xtable(clust.df, align="l|p{8cm}", caption="Clustering result using K-means"), caption.placement="top", size="\\scriptsize");
}

########## Utility Functions ##############
#'Determine row/column number for plotting
#'@description Determine the number of rows and columns for a given total
#'number of plots (used by Kmeans and SOM plots)
#'@param total Input the total
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'
GetXYCluster<-function(total){
  if(total>16){
    ncol<-4;
    nrow<-5;
  }else if(total>12){
    ncol<-4;
    nrow<-4;
  }else if(total>9){
    ncol<-3;
    nrow<-4;
  }else if(total>6){
    ncol<-3;
    nrow<-3;
  }else if(total>4){
    ncol<-2;
    nrow<-3;
  }else{
    ncol<-1;
    nrow<-total;
  }
  c(nrow, ncol);
}

#'Visualization ggmap - cluster
#'@description Plot spatial map using ggmap package based on users' provided data points
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param source Input the name of map source, options are stamen (default) and google
#'@param maptype Input the type of map, options are "terrain" (default),"terrain-background", "satellite", "roadmap", "hybrid", "toner","watercolor", "terrain-labels", "terrain-lines", "toner-2010",
#                "toner-2011", "toner-background", "toner-hybrid", "toner-labels","toner-lines", "toner-lite"
#'@param zoom Input the map zoom, an integer from 3 (continent) to 21 (building), default is 3
#'@param rangA 
#'@param ele Return elevation for each spatial data point
#'@param datum Return the longtitude and latitude of data points in to WGS84
#'@param proj Boolean variable, if "false", spatial data is in longitude and latitude; if "true", spatial data is in UTM
#'@param CRS_txt Input the CRS code to convert data collected in the particular UTM zone
#'@param CRS_option Boolean variable, if "false", spatial data is in longitude and latitude; if "true", spatial data is collected in Alberta 10TM zone
#'@param point.size Input the size of data point shown on map
#'@param path.size Input the length of path shown on map
#'@param point Return the variable name which shown as point on map
#'@param polygon Return the variable name which shown as polygon on map
#'@param path Return the variable name which shown as path on map
#'@param border.col Return the color of the border of polygons shown on map
#'@param color.point Return the color of the points shown on map
#'@param uni.point Boolean variable, if "false", the color of the points shown on map gets darker while the value of the point increasing; 
#                  if "true", the color of the point is fixed
#'@author Shiyang Zhao{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
ColorCol <- function(mSetObj = NA) {
  #load_plyr()
  #load_dplyr()
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data<- mSetObj$dataSet$procr;
  }else{
    data<-mSetObj$dataSet$prenorm
  }
  
  columnsData <- c("NULL",colnames(data))
  
  return(columnsData)
  
}


VarCol <- function(mSetObj = NA) {
  #load_plyr()
  #load_dplyr()
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data1<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data1<- mSetObj$dataSet$procr;
  }else{
    data1<-mSetObj$dataSet$prenorm
  }
  
  varData1 <- c("NULL",colnames(data1))
  
  return(varData1)
  
}


LineCol <- function(mSetObj = NA) {
  #load_plyr()
  #load_dplyr()
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$dataSet[["procr"]])){
    data3<-mSetObj$dataSet$preproc
  }else if(is.null(mSetObj$dataSet[["prenorm"]])){
    data3<- mSetObj$dataSet$procr;
  }else{
    data3<-mSetObj$dataSet$prenorm
  }
  
  lineData3 <- c("NULL",colnames(data3))
  
  return(lineData3)
  
}


Raster_data <- function(mSetObj = NA, data = "false", proj = "NULL", CRS_txt = "", CRS_option = "NULL", datum = "NULL", zoom = "", 
                        maptype = "NULL", source = "NULL", rangeA = "", ele = "false", border.col = "NULL", color.point = "NULL", 
                        point.size = "", path.size = "", point = "NULL", polygon = "NULL", path = "NULL", uni.point = "false", imgName, format = "png", dpi = 72, width = NA) {

  library("sp")  
  library("ggmap")
  library("googleway")
  library("rgdal")
    
  mSetObj <- .get.mSet(mSetObj)
  
  
  # set default dataset as 'dune' for now
  #Extract input from mSetObj
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  print(head(input))  
  
  if (proj == "NULL" ){
    colnames(input)[1:2] <- c("Latitude", "Longitude")
  } else if (proj == "UTM") {
    colnames(input)[1:2] <- c("Northing", "Easting")
  }  
  print(proj)
  
  if (CRS_txt == ""){
    CRS_txt1 = ""
  } else {
    CRS_txt1 <- CRS(CRS_txt)
  }
  print(CRS_txt)


  # CRS: coordinate reference system
  if (CRS_option == "NULL"){
    print(CRS_option)
    data.tm <- input #input contains coordinates and is recorded in a datum that user wants
    print(head(data.tm))
  } else if (CRS_option == "10TM") { #only take northing and easting in utm. e,g,: nothing and eating in Alberta 10tm -- +init = espg:3402
    #input_10tm <- input[input$Zone == "10TM", ]
    utm.spatial.data.10tm <- sp::SpatialPointsDataFrame(coords = cbind(input$Easting, input$Northing), data = input, proj4string = CRS_txt1) 
    latlong.tm10 <- sp::spTransform(x = utm.spatial.data, CRS("+init = espg:4326"))       
    data.tm <- cbind(latlong.tm10@coords, input[-c(1,2)])
    #data.tm <- input
  } else if (CRS_option == "Manual") {
    utm.spatial.data <- sp::SpatialPointsDataFrame(coords = cbind(input$Easting, input$Northing), data = input, proj4string = CRS(CRS_txt)) 
    latlong.tm <- sp::spTransform(x = utm.spatial.data, CRS("+init = espg:4326"))       
    data.tm <- cbind(latlong.tm@coords, input[-c(1,2)])
  }
  print(CRS_option)
  
  
  # IFgeographic coordinate system has to be WGS84 (epsg:4326 in Canada) to fit Stamen and Google map (NAD83:espg:4269)
  if (datum == "NULL"){
    input1 <- data.tm
  } else if (datum == "NAD83") {
    utm.spatial.data.83 <- sp::SpatialPointsDataFrame(coords = cbind(input$Latitude, input$Longitude), data = input, proj4string = CRS("+init=espg:4269")) 
    latlong.tm83 <- sp::spTransform(x = utm.spatial.data.83, CRS("+init = espg:4326"))
    input1 <- cbind(latlong.tm83@coords, input[-c(1,2)])
  }
  print(datum)
  
  if (zoom == "") {
    zoom1 <- 3
  } else {
    zoom1 <- as.numeric(zoom)
  }
  print(zoom1)
  # if the map area is too large, use smaller zoom number, e.g.: 5
  
  if (maptype == "NULL") {
    maptype1 <- "terrain"
  } else {
    maptype1 <- maptype
  }
  print(maptype1)
  
  if(source == "NULL") {
    source1 <- "stamen"
  } else if (source == "google") {
    source1 <- "google"
  } 
  print(source1)
  
  if (rangeA == "") {
    range1 <- 0.1
  } else {
    range1 <- as.numeric(rangeA)
  }
  print(range1)
  
  if (ele == "false") {
    coord.data <- input[c(1:2)]
    elevation <- "NA"
    ele1 <- as.data.frame(cbind(coord.data, elevation))
    write.csv(ele1, "Elevation.csv")
  } else {
    coord.data <- input1[c(1:2)]
    ele1 <- google_elevation(df_locations = coord.data, simplify = TRUE)
    write.csv(ele1, "Elevation.csv")
  }
  
  bbox1 <- make_bbox(lon = input$Long, lat = input$Lat, f = range1)
  print(bbox1)
  
  #if (source1 == "stamen") {
    map <- get_map(location = bbox1, maptype = maptype1, zoom = zoom1, source = source1)
  #} else if (source1 == "google") {
  #  map <- get_googlemap(location = bbox1, maptype = maptype1, zoom = zoom1)
  #}
  
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
  mSetObj$imgSet$ggmap <- imgName
  print(imgName)
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  if(border.col=="NULL") { 
    border.col1 = "skyblue" #default fill palette is grayscale
  } else if (border.col == "green") {
    border.col1 = "green"
  } else if (border.col == "turquoise") {
    border.col1 = "turquoise4"
  } else if (border.col == "steelblue") {
    border.col1 = "lightsteelblue4"
  } else if (border.col1 == "peach") {
    border.col1 = "tan2"  
  } else if (border.col1 == "wheat") {
    border.col1 <- "wheat3" 
  }
  print(border.col)
  print(border.col1)
  
  if(color.point == "NULL") { 
    color.point1 = "blue" #default fill palette is grayscale
  } else if (color.point == "green") { #manual user entry. Selection of this option causes text box to appear
    color.point1 = "palegreen"
  } else if (color.point == "turquoise") {
    color.point1 = "turquoise"
  } else if (color.point == "steelblue") {
    color.point1 = "lightsteelblue1"
  } else if (color.point == "peach") {
    color.point1 = "peachpuff"
  } else if (color.point == "wheat") {
    color.point1 = "wheat1"
  } 
  print(color.point)
  print(color.point1)
  
  if (point.size == ""){
    point.size1 = 2
  } else {
    point.size1 <- as.numeric(point.size)
  }
  
  if (path.size == ""){
    path.size1 = 2
  } else {
    path.size1 <- as.numeric(path.size)
  }
  
  if (point == "NULL") {
    point.idx = NA
  } else {
    for (i in 1:length(colnames(input))) {
      if (point == colnames(input)[i]) {
        point.idx <- i
      }
    }
  }
  
  if (polygon == "NULL") {
    polygon.idx = NA
  } else {
    for (j in 1:length(colnames(input))) {
      if (polygon == colnames(input)[j]) {
        polygon.idx <- j
      }
    }
  }
  
  if (path == "NULL") {
    path.idx = NA
  } else {
    for (k in 1:length(colnames(input))) {
      if (path == colnames(input)[k]) {
        path.idx <- k
      }
    }
  }
  
  if (point == "NULL") {
    if (path == "NULL") {
      if (polygon == "NULL") {
        gg <- ggmap(map) +
          labs(x = "longitude", y = "latitude") 
        print("point = Null; path = NULL; polygon = NULL")
      } else {
        gg <- ggmap(map) +
          geom_polygon(data = input1, mapping = aes(x = Longitude, y = Latitude, group = input[,polygon.idx]), fill = NA, color = border.col1) +
          labs(x = "longitude", y = "latitude") 
        print("point = Null; path = NULL; polygon = polygon.idx")
      }
    } else {
      if (polygon == "NULL") {
        gg <- ggmap(map) +
          geom_path(data = input1, aes(x = Longitude, y = Latitude, color = input[,path.idx]), size = path.size1, lineend = "round") + 
          labs(x = "longitude", y = "latitude") 
        print("point = Null; path = path.idx; polygon = NULL")
      } else {
        gg <- ggmap(map) +
          geom_polygon(data = input1, mapping = aes(x = Longitude, y = Latitude, group = input[,polygon.idx]), fill = NA, color = border.col1) +
          geom_path(data = input1, aes(x = Longitude, y = Latitude, color = input[,path.idx]), size = path.size1, lineend = "round") + 
          labs(x = "longitude", y = "latitude") 
        print("point = Null; path = path.idx; polygon = polygon.idx")
      }
    }
  } else {
    if (path == "NULL") {
      if (polygon == "NULL") {
        if (uni.point == "false") {
          gg <- ggmap(map) +
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x = Longitude, y = Latitude, color = input[,point.idx]), size = point.size1, data = input1)  
          print("uni.point = false; point = point.idx; path = NULL; polygon = NULL")
        } else {
          gg <- ggmap(map) +
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x = Longitude, y = Latitude), color = color.point1, size = point.size1, data = input1)   
          print("uni.point = true; point = color.point1; path = NULL; polygon = NULL")
          print(color.point1)
        }
      } else {
       if (uni.point == "false") {
          gg <- ggmap(map) +
            geom_polygon(data = input1, mapping = aes(x = Longitude, y = Latitude, group = input[,polygon.idx]), fill = NA, color = border.col1) +
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x= Longitude, y = Latitude, color = input[,point.idx]), size = point.size1, data = input1) 
          print("uni.point = false; point = point.idx; path = NULL; polygon = polygon.idx")
        } else {
          gg <- ggmap(map) +
            geom_polygon(data = input1, mapping = aes(x = Longitude, y = Latitude, group = input[,polygon.idx]), fill = NA, color = border.col1) +
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x= Longitude, y = Latitude), color = color.point1, size = point.size1, data = input1) 
          print("uni.point = true; point = color.point1; path = NULL; polygon = polygon.idx")
          print(color.point1)
        }
      }
    } else {
      if (polygon == "NULL") {
        if (uni.point == "false") {
          gg <- ggmap(map) +
            geom_path(data = input1, aes(x = Longitude, y = Latitude, color = input[,path.idx]), size = path.size1, lineend = "round") + 
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x= Longitude, y = Latitude, color = input[,point.idx]), size = point.size1, data = input1) +
            theme(legend.title = element_blank())
          print("uni.point = false; point = point.idx; path = path.idx; polygon = NULL")
        } else {
          gg <- ggmap(map) +
            geom_path(data = input1, aes(x = Longitude, y = Latitude, color = input[,path.idx]), size = path.size1, lineend = "round") + 
            geom_point(mapping = aes(x= Longitude, y = Latitude), color = color.point1, size = point.size1, data = input1) +
            labs(x = "longitude", y = "latitude") 
          print("uni.point = true; point = color.point1; path = path.idx; polygon = NULL")
          print(color.point1)
        }
      } else {
        if (uni.point == "false") {
          gg <- ggmap(map) +
            geom_polygon(data = input1, mapping = aes(x = Longitude, y = Latitude, group = input[,polygon.idx]), fill = NA, color = border.col1) +
            geom_path(data = input1, aes(x = Longitude, y = Latitude, color = input[,path.idx]), size = path.size1, lineend = "round") + 
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x= Longitude, y = Latitude, color = input[,point.idx]), size = point.size1, data = input1) 
          print("uni.point = false; point = point.idx; path = path.idx; polygon = polygon.idx")
          
        } else {
          gg <- ggmap(map) +
            geom_polygon(data = input1, mapping = aes(x = Longitude, y = Latitude, group = input[,polygon.idx]), fill = NA) +
            geom_path(data = input1, aes(x = Longitude, y = Latitude, color = input[,path.idx]), size = path.size1, lineend = "round") + 
            labs(x = "longitude", y = "latitude") +
            geom_point(mapping = aes(x = Longitude, y = Latitude), color = color.point1, size = point.size1, data = input1)
          print("uni.point = true; point = color.point1; path = path.idx; polygon = polygon.idx")
          print(color.point1)
          
        }
      }  
    }
  }
  
  print(gg + theme(legend.position='none')) 
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}
