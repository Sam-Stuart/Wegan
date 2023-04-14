library(WGCNA)

options(stringsAsFactors = FALSE)

# Input female liver data 
femData <- utils::read.csv(file = "./rawData-WGCNA/LiverFemale3600.csv")   

# Input female liver data 
maleData <- utils::read.csv("./rawData-WGCNA/LiverMale3600.csv")

makeDualSet_exprdata <- function(df1, df2, nSets = 2, mode = "list"){
    multiExpr <- vector(mode = mode, length = nSets)
    
    df_combined <- list(df1, df2)

    for(i in 1:nSets) {
        multiExpr[[i]] <- list(data = as.data.frame(t(df_combined[[i]][-c(1:8)]))) 
        names(multiExpr[[i]]$data) <- df_combined[[i]][["substanceBXH", exact = FALSE]] 
        rownames(multiExpr[[i]]$data) <- names(df_combined[[i]])[-c(1:8)]
    }

    return(multiExpr)
}

# Apply the above function to the sample data 
multiExpr <- makeDualSet_exprdata(femData, maleData, nSets = 2)
exprSize <- WGCNA::checkSets(multiExpr)
exprSize

# Create labels for two sets 
setLabels <- c("Female liver", "Male liver") 
shortLabels <- c("Female", "Male")

# Remove samples with too many missingness 
gsg <- WGCNA::goodSamplesGenesMS(multiExpr, verbose = 3)

clean.highMissing <- function (multi.expr, set.lab, expr.size, verbose = 3) {
    gsg <- WGCNA::goodSamplesGenesMS(multi.expr, verbose = verbose)
    
    if (gsg$allOK) {
        message("All genes and samples have passed the cuts") 
    } else {
        if (sum(!gsg$goodGenes) > 0)
            dynamicTreeCut::printFlush(paste("Removing genes:", 
                paste(names(multi.expr[[1]]$data)[!gsg$goodGenes], collapse = ", ")))
        for (i in 1:length(expr.size$nSets)) {
            if (sum(!gsg$goodSamples[[i]]) > 0)
                dynamicTreeCut::printFlush(paste("In set", setLabels[i], "removing samples",
            paste(rownames(multi.expr[[i]]$data)[!gsg$goodSamples[[i]]], collapse = ", ")))
            # Remove offending genes and samples 
            multi.expr[[i]]$data <- multi.expr[[i]]$data[gsg$goodSamples[[i]], gsg$goodGenes]
        }
        exprSize.rm <- WGCNA::checkSets(multi.expr) 
    }
    return(multi.expr)
}

multiExpr2 <- clean.highMissing(multi.expr = multiExpr, 
                                set.lab = setLabels, 
                                expr.size = exprSize)

# Cluster samples on Euclidean distance 
make.clusterTrees <- function(object, method = "average") {
    data.list <- lapply(object, "[[", 1)
    dist.list <- lapply(data.list, dist)
    hc.list <- lapply(dist.list, function(x) stats::hclust(x, method = method)) 
    
    return(hc.list)
}

# Visualize cluster with dendrograms 
trees <- make.clusterTrees(multiExpr2)    

# Create and export dendrogram plots 
make.dendrogram <- function(cluster.object, 
                            set.lab = setLabels, 
                            files, 
                            widths = 12, 
                            heights = 12) {
    pdf(file = files, width = widths, height = heights) 
    par(mfrow = c(2,1))
    par(mar = c(0, 4, 2, 0))
    for(i in 1:length(set.lab)) {
        plot(cluster.object[[i]], main = "", xlab = "", sub = "", cex = 0.6)  
        title(main = paste("Sample clustering all genes in:", set.lab[i]))
    }
    dev.off()
} 

# Apply the self-written function on sample data 
make.dendrogram(cluster.object = trees,
                set.lab = setLabels,
                file = "./output-WGCNA/sampleDendrogram.pdf")


# Choose the "base" cut height for the female data set 
baseHeight <- 16

make.dendrogramCut <- function(cluster.object, 
                               base.height, 
                               expr.size, 
                               set.lab = setLabels, 
                               files, 
                               widths = 12, 
                               heights = 12){
    
    cutHeights <- c(base.height, 
                    base.height*expr.size$nSamples[2] / expr.size$nSamples[1]) 
    
    pdf(file = files, width = widths, height = heights) 
    par(mfrow = c(2,1))
    par(mar = c(0, 4, 2, 0))
    for (i in 1:length(set.lab)){
        plot(cluster.object[[i]], 
             main = paste("Sample clustering on all genes in", set.lab[i]),
             xlab = "", sub = "", cex = 0.7)
        abline(h = cutHeights[i], col = "royalblue")
    }
    dev.off()
}

# args(make.dendrogramCut)
make.dendrogramCut(cluster.object = trees,
                   base.height = baseHeight,
                   expr.size = exprSize,
                   files = "./output-WGCNA/sampleDendrogramCut.pdf") 

# Write a function to remove outlier 
remove.outlier <- function(multi.expr, 
                           expr.size, 
                           cluster.object, 
                           base.height){
    
    cut.height <- c(base.height, 
                    base.height*expr.size$nSamples[2] / expr.size$nSamples[1]) 
    
    for (i in 1:length(cluster.object)){
        labels <- WGCNA::cutreeStatic(cluster.object[[i]], cutHeight = cut.height[i])
        keep <- (labels == 1)
        multi.expr[[i]]$data = multi.expr[[i]]$data[keep, ]
    }
    
    return(multi.expr)
}

# Apply the above function to the sample data 
# debug(remove.outlier)
multiExpr.rm <- remove.outlier(multi.expr = multiExpr2, 
                               expr.size = exprSize, 
                               cluster.object = trees,
                               base.height = baseHeight) 
# undebug(remove.outlier)
exprSize.rm <- checkSets(multiExpr.rm)
exprSize.rm


# Load clinical trait data 
traitData <- read.csv("./rawData-WGCNA/ClinicalTraits.csv")

# Remove variables we don't need 
allTraits <- traitData[, -c(31, 16)]
allTraits <- allTraits[, c(2, 11:36)]

# Form a multi-set structure that will hold the clinical traits 
nSets <- 2

Traits <- vector(mode = "list", length = nSets) 
for (set in 1:nSets) {
    setSamples <- rownames(multiExpr[[set]]$data)
    traitRows <- match(setSamples, allTraits$Mice)
    Traits[[set]] <- list(data = allTraits[traitRows, -1])
    rownames(Traits[[set]]$data) <- allTraits[traitRows, 1] 
}
WGCNA::collectGarbage()

nGenes <- exprSize$nGenes
nSamples <- exprSize$nSamples 

# Save relevant data for subsequent analysis 
save(multiExpr.rm, Traits, nGenes, nSamples, setLabels, shortLabels, exprSize, 
     file = "./output-WGCNA/consensusDataInput.RData")





