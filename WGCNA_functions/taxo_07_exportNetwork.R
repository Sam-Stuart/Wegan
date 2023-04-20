# This R function exports a gene network to external visualization 
# software (VisANT and Cytoscape) using WGCNA package

export.networkToExternal <- function(mSetObj = NULL,
                                     custom_norm = NULL,
                                     power = 6,
                                     module = "all", # or particular names if known 
                                     nHubgenes = 30, # subset hub genes 
                                     software = "VisANT", # or "Cytoscape"
                                     file # output file name 
                                     ) {
    
    library(WGCNA)
    
    mSetObj <- .get.mSet(mSetObj) 
    
    # Define gene expression data frame 
    datExpr <- mSetObj$dataSet$exprObj[[1]]$data 
    
    # Define module colors 
    net <- WGCNA::blockwiseModules(datExpr,
                                   power = power, # Default value
                                   TOMType = "unsigned",
                                   minModuleSize = 30,
                                   reassignThreshold = 0,
                                   mergeCutHeight = 0.25,
                                   numericLabels = TRUE,
                                   pamRespectsDendro = FALSE,
                                   saveTOMs = TRUE,
                                   saveTOMFileBase = "femaleMouseTOM",
                                   verbose = 3)
    
    moduleColors <- WGCNA::labels2colors(net$colors) 
    
    # Calculate topological overlap 
    TOM <- WGCNA::TOMsimilarityFromExpr(datExpr, power = 6) 
    
    # Extract annotation data from the mSet object
    if (is.null(mSetObj$dataSet$annotation)) {
        # A message pops up without affecting execution 
        message("Despite gene annotations absent, the execution continues.")
        
        if (module == "all") {
            for (mod in moduleColors) {
                # Select module probes 
                module <- mod
                # Select module genes (probes)
                probes <- names(datExpr) 
                inModule <- (moduleColors == module) # parentheses 
                modProbes <- probes[inModule] 
                # Select the corresponding topocial overlap 
                modTOM <- TOM[inModule, inModule]
                dimnames(modTOM) <- list(modProbes, modProbes) 
                # Subset hub genes 
                IMConn <- WGCNA::softConnectivity(datExpr[, modProbes])
                top <- (rank(-IMConn) <= nHubgenes) 
                
                if (software == "VisANT") {
                    # Export the network into an edge list file VisANT can read 
                    networkPlotFile <- WGCNA::exportNetworkToVisANT(
                        modTOM[top, top],
                        file = paste("VisANTInput-", 
                                     module, 
                                     ".txt",
                                     sep = ""),
                        weighted = TRUE,
                        threshold = 0)
                } else if (software == "Cytoscape") {
                    # Export the network into a file Cytoscape can read 
                    networkPlotFile <- WGCNA::exportNetworkToCytoscape(
                        modTOM[top, top], # select modules and hub genes 
                        edgeFile = paste("CytoscapeInput-edges-", 
                                         module,
                                         ".txt", 
                                         sep = ""),
                        nodeFile = paste("CytoscapeInput-nodes-", 
                                         module,
                                         ".txt", 
                                         sep = ""),
                        weighted = TRUE,
                        threshold = 0.02,
                        nodeNames = modProbes,
                        nodeAttr = moduleColors[inModule])
                }
                
            }
            save(networkPlotFile, file = file)
        } else {
            for (mod in module) {
                # Select module probes 
                module <- mod
                # Select module genes (probes)
                probes <- names(datExpr) 
                inModule <- (moduleColors == module) # parentheses 
                modProbes <- probes[inModule] 
                # Select the corresponding topocial overlap 
                modTOM <- TOM[inModule, inModule]
                dimnames(modTOM) <- list(modProbes, modProbes) 
                # Subset hub genes 
                IMConn <- WGCNA::softConnectivity(datExpr[, modProbes])
                top <- (rank(-IMConn) <= nHubgenes) 
                
                if (software == "VisANT") {
                    # Export the network into an edge list file VisANT can read 
                    networkPlotFile <- WGCNA::exportNetworkToVisANT(
                        modTOM[top, top],
                        file = paste("VisANTInput-", 
                                     module, 
                                     ".txt",
                                     sep = ""),
                        weighted = TRUE,
                        threshold = 0)
                } else if (software == "Cytoscape") {
                    # Export the network into a file Cytoscape can read 
                    networkPlotFile <- WGCNA::exportNetworkToCytoscape(
                        modTOM[top, top], # select modules and hub genes 
                        edgeFile = paste("CytoscapeInput-edges-", 
                                         module,
                                         ".txt", 
                                         sep = ""),
                        nodeFile = paste("CytoscapeInput-nodes-", 
                                         module,
                                         ".txt", 
                                         sep = ""),
                        weighted = TRUE,
                        threshold = 0.02,
                        nodeNames = modProbes,
                        nodeAttr = moduleColors[inModule])
                }
            }
            save(networkPlotFile, file = file)
        }
        
    }
    
    # Read in the annotation file 
    annot <- mSetObj$dataSet$annotation 
    
    if (module == "all") {
        for (mod in moduleColors) {
            # Select module probes 
            module <- mod
            # Select module genes (probes)
            probes <- names(datExpr) 
            inModule <- (moduleColors == module) # parentheses 
            modProbes <- probes[inModule] 
            # Select the corresponding topocial overlap 
            modTOM <- TOM[inModule, inModule]
            dimnames(modTOM) <- list(modProbes, modProbes) 
            # Subset hub genes 
            IMConn <- WGCNA::softConnectivity(datExpr[, modProbes])
            top <- (rank(-IMConn) <= nHubgenes) 
            
            modGenes <- annot$gene_symbol[match(modProbes,
                                                annot$substanceBXH)]
            
            if (software == "VisANT") {
                # Export the network into an edge list file VisANT can read 
                networkPlotFile <- WGCNA::exportNetworkToVisANT(
                    modTOM[top, top],
                    file = paste("VisANTInput-", 
                                 module, 
                                 ".txt",
                                 sep = ""),
                    probeToGene = data.frame(annot$substanceBXH,
                                             annot$gene_symbol),
                    weighted = TRUE,
                    threshold = 0)
            } else if (software == "Cytoscape") {
                # Export the network into a file Cytoscape can read 
                networkPlotFile <- WGCNA::exportNetworkToCytoscape(
                    modTOM[top, top], # select modules and hub genes 
                    edgeFile = paste("CytoscapeInput-edges-", 
                                     module,
                                     ".txt", 
                                     sep = ""),
                    nodeFile = paste("CytoscapeInput-nodes-", 
                                     module,
                                     ".txt", 
                                     sep = ""),
                    weighted = TRUE,
                    threshold = 0.02,
                    nodeNames = modProbes,
                    altNodeNames = modGenes, # the variable is missing ... 
                    nodeAttr = moduleColors[inModule])
            }
            
        }
        save(networkPlotFile, file = file)
    } else {
        for (mod in module) {
            # Select module probes 
            module <- mod
            # Select module genes (probes)
            probes <- names(datExpr) 
            inModule <- (moduleColors == module) # parentheses 
            modProbes <- probes[inModule] 
            # Select the corresponding topocial overlap 
            modTOM <- TOM[inModule, inModule]
            dimnames(modTOM) <- list(modProbes, modProbes) 
            # Subset hub genes 
            IMConn <- WGCNA::softConnectivity(datExpr[, modProbes])
            top <- (rank(-IMConn) <= nHubgenes) 
            
            if (software == "VisANT") {
                # Export the network into an edge list file VisANT can read 
                networkPlotFile <- WGCNA::exportNetworkToVisANT(
                    modTOM[top, top],
                    file = paste("VisANTInput-", 
                                 module, 
                                 ".txt",
                                 sep = ""),
                    weighted = TRUE,
                    threshold = 0,
                    probeToGene = data.frame(annot$substanceBXH, 
                                             annot$gene_symbol))
            } else if (software == "Cytoscape") {
                # Export the network into a file Cytoscape can read 
                networkPlotFile <- WGCNA::exportNetworkToCytoscape(
                    modTOM[top, top], # select modules and hub genes 
                    edgeFile = paste("CytoscapeInput-edges-", 
                                     module,
                                     ".txt", 
                                     sep = ""),
                    nodeFile = paste("CytoscapeInput-nodes-", 
                                     module,
                                     ".txt", 
                                     sep = ""),
                    weighted = TRUE,
                    threshold = 0.02,
                    nodeNames = modProbes,
                    altNodeNames = modGenes,
                    nodeAttr = moduleColors[inModule])
            }
        }
        save(networkPlotFile, file = file)
    }
    
}



#===============================================================================

# Validate the above function  

#===============================================================================

load('./mSet_example.RData')   
load("./clinicalTrait_example.RData")
load("./net_example.RData")
source("./Func_WGCNA/dataInputClean_fun.R")

# Read in gene annotation file 
annot <- read.csv("./rawData-WGCNA/GeneAnnotation.csv") 


.on.public.web <- FALSE  

mSetObj <- make.exprSet(mSetObj = mSetObj_example)

mSetObj$dataSet$annotation <- annot 


# Test the function 

args(export.networkToExternal)

export.networkToExternal(mSetObj = mSetObj,
                         file = "./networkExport.txt",
                         module = "brown")








