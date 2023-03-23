library(vegan)
mSetObj=list()
data("dune")
data("dune.taxon")
mSetObj$dataSet$taxon=dune.taxon
mSetObj$dataSet$norm=dune
color="NULL"; main="NULL"; xlab="NULL"; ylab="NULL"; rank="NULL"; data="NULL"; facA="NULL"

##########
taxonORA <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$dataSet$taxon
  phyperResults <- list()
  for (j in 1:ncol(data)) {
    results <- c()
    for (i in 1:nrow(data)) {
      x <- sum(data[,j]==data[i,j])-1 # Name counts within the taxon level - 1
      m <- sum(data==data[i,j]) # Name counts within all taxon data (in that level as it is a distinct variable)
      n <- length(data[!is.na(data)])-m # All name counts within all taxon data - m 
      k <- length(data[,j][!is.na(data[,j])]) # All name counts in taxon level
      res <- phyper(x,m,n,k, lower.tail=FALSE, log.p=FALSE) # Apply hypergeometric test to obtain p-value associated with significant enrichment of organism within the taxon level
      results <- c(results, res)
      phyperResults[[colnames(data)[j]]] <- results
    }
  }
  phyperResults <- as.data.frame(do.call(rbind, phyperResults))
  phyperResults <- t(phyperResults)
  phyperResultsAdj <- apply(phyperResults, 2, adjustP)
  rownames(phyperResultsAdj) <- rownames(data)
  colnames(phyperResultsAdj) <- colnames(data)
  write.csv(phyperResultsAdj, file="taxonORA_adjpvalues.csv")
  
  mSetObj$analSet$taxonORA$taxonORApvalues <- phyperResultsAdj
  return(.set.mSet(mSetObj))
}
  

###########
taxonORAheat <- function (mSetObj=NA, color="NULL", textColor="NULL", main="NULL", xlab="NULL", ylab="NULL", imgName, format="png", dpi=72, width=NA) {
  library(viridis)
  library(gplots)
  mSetObj <- .get.mSet(mSetObj)
  taxon_data <- mSetObj$dataSet$taxon
  phyperResultsAdj <- as.data.frame(mSetObj$analSet$taxonORA$taxonORApvalues)
  heatmap_matrix <- as.matrix(phyperResultsAdj)
  colnames(heatmap_matrix) <- colnames(phyperResultsAdj)
  rownames(heatmap_matrix) <- rownames(phyperResultsAdj)
  
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
  mSetObj$imgSet$taxonORA <- imgName
  
  if (color=="NULL") { #Default
    colors <- viridis #Assign a color to each level using the viridis pallete (viridis package)
  } else if (color=="plasma") {
    colors <- plasma #Assign a color to each level using the plasma pallete (viridis package)
  } else if (color=="grey") {
    colors <- grey.colors(start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
  } else if (color=="heat") {
    colors <- heat.colors #Assign a heat color to each level (grDevices package)
  } 
  
  if (textColor=="NULL") { #Default
    textColor <- "cyan" #Assign a color to each level using the viridis pallete (viridis package)
  } else if (color=="red") {
    textColor <- "red" #Assign a color to each level using the plasma pallete (viridis package)
  } else if (color=="black") {
    textColor <- "black" #Assign a grey color to each level (grDevices package- automatically installed)
  } else if (color=="white") {
    textColor <- "white" #Assign a heat color to each level (grDevices package)
  } 
  
  if (main=="NULL") {
    main="Taxon Data Over-Representation Analysis"
  } else {
    main=main
  }  
  
  if (xlab=="NULL") {
    xlab="Taxonomic Rank"
  } else {
    xlab=xlab
  }  
  
  if (ylab=="NULL") {
    ylab="Species"
  } else {
    ylab=ylab
  }
  
  #Produce heatmap
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  heatmap.2(heatmap_matrix, col=colors, key.title="none", keysize=1, key.ylab="none", key.xlab="absolute abundance", density.info="none", Rowv=NA, Colv=NA, 
            cexCol=.8, cexRow=.8, trace="none", main=main, xlab=xlab, ylab=ylab, cellnote=taxon_data, notecol=textColor) 
  dev.off()
}



##########
commORA <- function(mSetObj=NA, data="NULL"){ 
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    data <- mSetObj$dataSet$norm
  } else {
    data <- mSetObj$dataSet$orig
  }
    
  phyperResults <- list()
  for (j in 1:nrow(data)) {
    results <- c()
    for (i in 1:ncol(data)) {
      x <- data[j, i]-1 # Species i counts in the sample - 1
      m <- sum(data[, i]) # Species i counts in all data
      n <- sum(data)-m # All species counts in all data - m 
      k <- sum(data[j,]) # All species counts in sample
      res <- phyper(x,m,n,k, lower.tail=FALSE, log.p=FALSE) # Apply hypergeometric test to obtain p-value associated with significant enrichment of organism within the taxon level
      results <- c(results, res)
      phyperResults[[rownames(data)[j]]] <- results
    }
  }
  phyperResults <- as.data.frame(do.call(rbind, phyperResults))
  phyperResultsAdj <- apply(phyperResults, 2, adjustP)
  rownames(phyperResultsAdj) <- rownames(data)
  colnames(phyperResultsAdj) <- colnames(data)
  write.csv(phyperResultsAdj, file="communityORA_adjpvalues.csv")
  
  mSetObj$analSet$commORA$commORApvalues <- as.data.frame(phyperResultsAdj)
  mSetObj$analSet$commORA$counts <- data
  return(.set.mSet(mSetObj))
}



###########
commORAplot <- function (mSetObj=NA, facA="NULL", color="NULL", imgName, format="png", dpi=72, width=NA) {
  library(viridis)
  library(ggplot2)
  mSetObj <- .get.mSet(mSetObj)
  phyperResultsAdj <- mSetObj$analSet$commORA$commORApvalues
  data <- mSetObj$analSet$commORA$counts
  
  if (facA=="NULL") {
    facA <- colnames(data)[1]
  } else {
    facA <- facA
  }
  
  counts <- cbind(rownames(data), data[facA], phyperResultsAdj[facA])
  colnames(counts) <- c("Sample", facA, "p.value")  
  
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
  mSetObj$imgSet$commORA <- imgName

  if (is.null(color)) { #Default
    colors <- viridis #Use viridis pallete (yellow, green, blue)
  } else if (color=="plasma") {
    colors <- plasma #Use plasma pallete (yellow, orange, red, purple)
  } else if (color=="grey") {
    colors <- grey.colors(n, start=0.1, end=0.8) #Use shades of grey
  } else if (color=="heat") {
    colors <- heat.colors #Use heat pallette (red, orange, yellow)
  } 
  
  #Produce heatmap
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  
  
  
  ggplot(counts, aes(y=facAname, x=Sample, fill=p.value)) +
    geom_bar(stat="identity") +
    scale_fill_gradient(low="blue", high="yellow") +
    theme_classic()
  
  dev.off()
}



####
#Venn diagram for up to 5 columns
venn <- function(mSetObj=NA, col.text="NULL", color="NULL", perc="NULL", imgName, format="png", dpi=72, width=NA){
  library(VennDiagram)
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$dataSet$venn
  
  if (is.null(col.text)) {
    data <- as.data.frame(data[,1])
  } else if (length(col.text)>=1 && length(col.text)<=5) {
    data <- as.data.frame(data[,col.text])
  } else if (length(col.text)>5) {
    data <- data[,col.text]
    data <- data[1:5] #Limit to first 5 columns
  }
  
  vennData <- list()
  for (i in 1:ncol(data)) {
    vennData[[i]] <- data[,i] #Turn df into list of columns
  }
  names(vennData) <- colnames(data)
  
  #Color options
  if (is.null(color)) { #Default
    colors <- viridis #Assign a color to each level using the viridis pallete (viridis package)
  } else if (color=="plasma") {
    colors <- plasma #Assign a color to each level using the plasma pallete (viridis package)
  } else if (color=="grey") {
    colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
  } else if (color=="heat") {
    colors <- heat.colors #Assign a grey color to each level (grDevices package- automatically installed)
  } else if (color=="rainbow") {
    colors <- rainbow #Assign a grey color to each level (grDevices package- automatically installed)
  } 
  
  if (n==1) {
    fill.col <- c(alpha(colors[1],0.3))
  } else if (n==2) {
    fill.col <- c(alpha(colors[1],0.3), alpha(colors[2],0.3))
  } else if (n==3) {
    fill.col <- c(alpha(colors[1],0.3), alpha(colors[2],0.3), alpha(colors[3],0.3))
  } else if (n==4) {
    fill.col <- c(alpha(colors[1],0.3), alpha(colors[2],0.3), alpha(colors[3],0.3), alpha(colors[4],0.3))
  } else if (n==5) {
    fill.col <- c(alpha(colors[1],0.3), alpha(colors[2],0.3), alpha(colors[3],0.3), alpha(colors[4],0.3), alpha(colors[5],0.3))
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
  mSetObj$imgSet$venn <- imgName
  
  #Produce Venn diagram
  if (is.null(perc)) {
    venn.diagram(vennData, force.unique=TRUE, filename=imgName, dpi=dpi, units="in", height=h, width=w, imagetype=format, category.names=names(vennData), lwd=1, col=colors, fill=fill.col, print.mode="raw", cat.just=rep(list(c(0.5, 0.5)), length(vennData)))
  } else {
    venn.diagram(vennData, force.unique=TRUE, filename=imgName, dpi=dpi, units="in", height=h, width=w, imagetype=format, category.names=names(vennData), lwd=1, col=colors, fill=fill.col, print.mode=c("raw", "percent"), cat.just=rep(list(c(0.5, 0.5)), length(vennData)))
  }
  
  mSetObj$analSet$venn$vennList <- vennData
  return(.set.mSet(mSetObj))
}



####
vennElements <- function(mSetObj=NA) {
  library(VennDiagram)
  mSetObj <- .get.mSet(mSetObj)
  vennData <- mSetObj$analSet$venn$vennList
  overlap <- calculate.overlap(vennData)
  for (i in 1:length(overlap)) {
    overlap[[i]] <- unique(overlap[[i]])
  }

  if (length(overlap)==31) {
    #names(overlap) <- c("cell_12345", ..., "cell_1", "cell_2", "cell_3", "cell_4", "cell_5")
  } else if (length(overlap)==15) {
    names(overlap) <- c("cell_1234", "cell_123", "cell_124", "cell_134", "cell_234", "cell_12", "cell_13", "cell_14", "cell_23", "cell_24", "cell_34", "cell_1", "cell_2", "cell_3", "cell_4")
  } else if (length(overlap)==7) {
    names(overlap) <- c("cell_123", "cell_12", "cell_13", "cell_23", "cell_1", "cell_2", "cell_3")
  } else if (length(overlap)==3) {
    names(overlap) <- c("cell_12", "cell_1", "cell_2")
  } else {
    names(overlap) <- c("cell_1")
  }
  
  overlap_df <- as.data.frame(do.call(cbind, overlap))
  for (j in 1:ncol(overlap_df)) {
    overlap_df[,j][duplicated(overlap_df[,j])] <- NA
  }
  write.csv(overlap_df, file="venn_cell_elements.csv", row.names=FALSE)
} 



#Plot stacked bar graph where colors indicate relative abundance or raw counts
taxAbunCompPlot <- function(mSetObj=NA, data="NULL", color="NULL", level="NULL", relative="NULL") {
  library(microbiome)
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
    
  dataRelative <- apply(data, 2, x/sum(x))
  
  plot.composition.relAbun <- plot_composition(dataRelative, sample.sort = level) +
    theme(legend.position = "bottom") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("Relative abundance") + 
    theme(legend.title = element_text(size = 18))
  colors!!!!!!!!
  quality!!!!!!!
  legend title!!!!!
}




#Plot heatmap where colors indicate relative abundance or raw counts
taxAbunHeatmap <- function(mSetObj=NA, data="NULL", color="NULL", relative=FALSE, main="NULL", xlab="NULL", ylab="NULL", imgName, format="png", dpi=72, width=NA) {
  library(gplots)
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    data <- mSetObj$dataSet$norm
  } else {
    data <- mSetObj$dataSet$orig
  }

  #Truncate labels to fit heatmap
  rownames(data) <- substr(rownames(data), start = 1, stop = 8) 
  colnames(data) <- substr(colnames(data), start = 1, stop = 8)
  
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
  mSetObj$imgSet$taxAbunHeatmap <- imgName

  if (is.null(color)) { #Default
    colors <- viridis #Assign a color to each level using the viridis pallete (viridis package)
  } else if (color=="plasma") {
    colors <- plasma #Assign a color to each level using the plasma pallete (viridis package)
  } else if (color=="grey") {
    colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
  } else if (color=="heat") {
    colors <- heat.colors #Assign a grey color to each level (grDevices package- automatically installed)
  } else if (color=="rainbow") {
    colors <- rainbow #Assign a grey color to each level (grDevices package- automatically installed)
  } 
  
  if (is.null(xlab)) {
      xlab="Sample"
    } else {
      xlab=xlab
  }
  if (is.null(ylab)) {
      ylab="Species"
    } else {
      ylab=ylab
  }
    
  #Produce heatmap
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")

  if (relative) {    
    dataRelative <- apply(data, 2, function(x) x/sum(x))
    heatmap_matrix <-as.matrix(t(dataRelative))
    rownames(heatmap_matrix) <- colnames(dataRelative)
    colnames(heatmap_matrix) <- rownames(dataRelative) 
    if (is.null(main)) {
      main="Relative Abundance"
    } else {
      main=main
    }
    heatmap.2(heatmap_matrix, col=colors, key.title="none", keysize=1, key.ylab="none", key.xlab="relative abundance", density.info="none", Rowv=NA, Colv=NA, 
              cexCol=.8, cexRow=.8, trace="none", main=main, xlab=xlab, ylab=ylab)  
 } else {
    heatmap_matrix <-as.matrix(t(data))
    rownames(heatmap_matrix) <- colnames(data)
    colnames(heatmap_matrix) <- rownames(data) 
    if (is.null(main)) {
      main="Absolute Abundance"
    } else {
      main=main
    }
    heatmap.2(heatmap_matrix, col=colors, key.title="none", keysize=1, key.ylab="none", key.xlab="absolute abundance", density.info="none", Rowv=NA, Colv=NA, 
              cexCol=.8, cexRow=.8, trace="none", main=main, xlab=xlab, ylab=ylab)    
  }

  dev.off()
}



#####
adjustP <- function (x) {
  x <- p.adjust(x, method="BH")
}

