# This R file demonstrates interfacing network analysis with other data such 
# as functional annotation and gene ontology 

library(WGCNA) 
options(stringsAsFactors = FALSE)


# Install organism-specific package, 'org.Mm.eg.db' from Bioconductor 
# if (!require("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("org.Mm.eg.db")

library(org.Mm.eg.db)


# Load input data 
load("./output-WGCNA/consensusDataInput.RData")
load("./femaleLiver-networkConstruct-auto.RData") 

datExpr <- multiExpr.rm[[1]]$data # female liver expression data 

nGenes <- ncol(datExpr)
nSamples <- nrow(datExpr)

datTraits <- Traits[[1]]$data
datTraits <- datTraits[rownames(datTraits) %in% rownames(datExpr), ] 

# To facilitate a biological interpretation, we would like to know the gene 
# ontologies of the genes in the modules, whether they are significantly 
# enriched in certain functional categories etc. 


#===============================================================================

# Output gene lists for use with online software and services 

#===============================================================================

# Write out the LocusLinkID (entrez) codes for the brown module into a file, for
# instance 

# Read in the probe annotation 
annot <- read.csv(file = "./rawData-WGCNA/GeneAnnotation.csv") 

# Match probes in the data set to the probe IDs in the annotation file 
probes <- names(datExpr) 
probes2annot <- match(probes, annot$substanceBXH) 

# Get the corresponding Locus Link IDs 
allLLIDs <- annot$LocusLinkID[probes2annot] 

# Choose interesting modules and export IDs 
intModules <- c("brown", "red", "salmon") 
for (module in intModules) {
    modGenes <- (moduleColors == module)
    modLLIDs <- allLLIDs[modGenes] 
    fileName <- paste("LocusLinkIDs-", module, ".txt", sep = "")
    write.table(as.data.frame(modLLIDs), 
                file = fileName,
                row.names = FALSE,
                col.names = FALSE)
}

fileName <- paste("LocusLinkIDs-all.txt", sep ="")
write.table(as.data.frame(allLLIDs), 
            file = fileName,
            row.names = FALSE, 
            col.names = FALSE)


#===============================================================================

# Enrichment analysis directly within R 

#===============================================================================

GOenr <- WGCNA::GOenrichmentAnalysis(moduleColors, 
                                     allLLIDs, 
                                     organism = "mouse",
                                     nBestP = 10)

tab <- GOenr$bestPTerms[[4]]$enrichment 

write.table(tab, 
            file = "GOEnrichmentTable.csv", 
            sep = ",",
            quote = TRUE,
            row.names = FALSE)

# Display the result directly on screen 
keepCols <- c(1, 2, 5, 6, 7, 12, 13) 
screenTab <- tab[, keepCols] 

numCols <- c(3, 4) 
screenTab[, numCols] <- signif(apply(screenTab[, numCols],
                                     2, as.numeric), 2) 
screenTab[, 7] <- substring(screenTab[, 7], 1, 40)

colnames(screenTab) <- c("module", "size", "p-val", "Bonf", "nInTerm",
                         "ont", "term name")
rownames(screenTab) <- NULL 

options(width = 95)

screenTab 








