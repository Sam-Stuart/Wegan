# This script was written by Louisa C. Normington of LCN Bioinformatics, LLC for Palleon Pharmaceuticals. 

# This script implements differential expression analysis using R version 3.6.1 and the DESeq2 v1.26.0 package.
# See this webpage for information on how to switch R versions if needed:
# https://support.rstudio.com/hc/en-us/articles/200486138-Changing-R-versions-for-RStudio-desktop 

setwd("C:/Users/Louisa/Desktop/LCN_Bioinformatics/Palleon/RNA-Seq_Analysis_Melanoma/Palleon_Melanoma_DESeq2") # Must use forward slash for Windows OS.

install.packages("BiocManager")
BiocManager::install("DESeq2")
library ("DESeq2")  # for count normalization, differential expression analysis, dispersion plot and MA-plot


# ~~~~~~~~~~First round of DE analysis will be with the count data produced by featureCounts from the Rsubread R-package~~~~~~~~~~

# Import and inspect count data: 
# Raw count data produced by Canopy Biosciences LLC. The first 7 columns contain gene identifiers and descriptions. Sample columns are ordered by 
# tissue type (19 melanoma samples, 4 normal samples), with the cancer samples listed first, followed by the normal samples.
countdata_Melanoma_featureCounts <- read.csv("featureCounts_counts.csv", header=TRUE) # This counts file was derived from "all.gene_counts.xsl.txt" in the directory "RNA-Seq Melanoma.zip\RNA-Seq Melanoma\Results\P_AP_1349_052119"
countdata_Melanoma_featureCounts_protein <- countdata_Melanoma_featureCounts[countdata_Melanoma_featureCounts$gene_biotype=="protein_coding",] # Select rows that are for protein coding sequences only. 
countdata_Melanoma_featureCounts_protein <- countdata_Melanoma_featureCounts_protein[!duplicated(countdata_Melanoma_featureCounts_protein),] # Remove any duplicate rows. 
dim(countdata_Melanoma_featureCounts_protein) # Dimensions of the data frame
colnames(countdata_Melanoma_featureCounts_protein) # Check order of samples
colSums(countdata_Melanoma_featureCounts_protein[,-c(1:7)]) # Read depth
summary(countdata_Melanoma_featureCounts_protein) # Statistical summary of count data

# Filter count data by keeping genes that have greater than 10 counts for at least one sample:
countdata_Melanoma_featureCounts_filtered <- countdata_Melanoma_featureCounts_protein[apply(countdata_Melanoma_featureCounts_protein[,-c(1:7)], 1, function(countdata_Melanoma_featureCounts_protein) length(countdata_Melanoma_featureCounts_protein[countdata_Melanoma_featureCounts_protein>=10])>0),]  
dim(countdata_Melanoma_featureCounts_filtered) 
summary(countdata_Melanoma_featureCounts_filtered)
countmatrix_Melanoma_featureCounts_filtered <- data.matrix(countdata_Melanoma_featureCounts_filtered) # Convert data frame into a matrix.

# Load the sample information (i.e. meta-data):
# This file contains the experimental design. Note that the cancer samples (yes melanoma) are listed first, followed by the normal samples (no melanoma).
# This order of groups is required to obtain the proper logFC values.
sample_info_Melanoma <- read.csv('meta_DESeq2.csv', header = TRUE) 

# Generate the DESeqDataSet object from the count matrix, the meta data (ie melanoma or control assignment), and the desired linear model design:
# The ddsMat object contains the input data and the information about the procedures implemented on the data.
ddsMat_Melanoma_featureCounts <- DESeqDataSetFromMatrix(countData=countmatrix_Melanoma_featureCounts_filtered[,-c(1:7)], colData=sample_info_Melanoma, design = ~ 0 + melanoma) 
ddsMat_Melanoma_featureCounts$melanoma <- relevel(ddsMat_Melanoma_featureCounts$melanoma, ref="no") # Set the 4 no melanoma samples as the control group to be compared with the 19 samples in the melanoma group.

# Estimate size factors (normalization factors) required for differential expression analysis:
# This function estimates the size factors using the "median ratio method" described by Equation 5 in Anders and Huber (2010).
# The size factors are used to normalize the data both within and between the samples.
norm_ddsMat_Melanoma_featureCounts <- estimateSizeFactors(ddsMat_Melanoma_featureCounts)

# Perform differential expression analysis:
# This single command performs differential expression analysis which includes estimating gene-wise dispersions and determining the mean-dispersion relationship, 
# both using the Cox-Reid adjusted likelihood and fitting the negative binomial general linear model. It also performs the Wald Test (the "stat" statistic in the output file), which 
# can be used for Gene Set Enrichment Analysis (GSEA). 
DESeq_Melanoma_featureCounts <- DESeq(norm_ddsMat_Melanoma_featureCounts, test="Wald")

# Generate the dispersion plot for inspection:
# The black points are the dispersion estimates for each gene as obtained by considering the information from each gene separately. The red trend line shows the dispersions' 
# dependence on the mean. Each gene's dispersion estimate is shrunk towards the red line to obtain the final estimates (blue points) that are then used in the hypothesis test.
tiff(filename = "Palleon_Melanoma_DESeq2_featureCounts_Disperison_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
plotDispEsts(DESeq_Melanoma_featureCounts, main="Melanoma tissue versus normal tissue \nDispersion plot in DESeq2 with featureCounts data")
dev.off()

# Extract and export all results:
# The output includes the following statistics: "baseMean" (average of the normalized count values, dividing by size factors, taken over all samples), "logFC" (log2FC), 
# "logfcSE" (log2FC standard error), "waldStat" (Wald statistic), "pValue", "pValue.adjust" (adjusted using the Benjamini-Hochberg method).
res_Melanoma_featureCounts1 <- results(DESeq_Melanoma_featureCounts) # The false discovery rate was adjusted using the Benjamini-Hochberg procedure.
res_Melanoma_featureCounts2 <- cbind(countdata_Melanoma_featureCounts_filtered[,1:7], res_Melanoma_featureCounts1) 
colnames(res_Melanoma_featureCounts2) <- c("ensembl_gene_id", "entrezgene", "external_gene_name", "gene_biotype", "external_gene_source", "transcript_count", "description", "baseMean", "logFC", "logfcSE", "waldStat", "pValue", "pValue.adjust")
res_Melanoma_featureCounts3 <- res_Melanoma_featureCounts2[!duplicated(res_Melanoma_featureCounts2),] # Remove any duplicate rows. 
write.csv(res_Melanoma_featureCounts3[order(res_Melanoma_featureCounts3$pValue.adjust),], file="Palleon_Melanoma_DESeq2_featureCounts_DE_Results.csv", row.names=FALSE)

# Produce an MA plot as a visualization of the DE results:
# MA-plots visualize gene level differences in expression. Each point in the plot represents a gene. The function plotMA shows the 
# log2FC over the mean of normalized counts across all samples. Points are colored red if the adjusted p-value is less than 0.1. 
# Points which fall out of the window are plotted as open triangles pointing either up or down.
tiff(filename = "Palleon_Melanoma_DESeq2_featureCounts_MA_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
DESeq2::plotMA(res_Melanoma_featureCounts1, ylim=c(-20,20), main="Melanoma tissue versus normal tissue \nMA plot in DESeq2 with featureCounts data", xlab="mean of normalized counts", ylab="log2FC", cex=.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5) 
dev.off()

# Produce a volcano plot as a visualization of the DE results:
# A volcano plot was generated using the Benjamini-Hochberg FDR adjustment of p-values. Volcano plots show both fold change and p-value. 
# The log2 fold change is plotted on the x-axis and the negative log10 p-value is plotted on the y-axis.
Volcano1 <- res_Melanoma_featureCounts3[,c(1,9,13)]
tiff(filename = "Palleon_Melanoma_DESeq2_featureCounts_Volcano_Plot.tiff", res=1200, width=9, height=5.75, units="in")
plot(Volcano1[,2], -log10(Volcano1[,3]), pch=20, col="black", cex=0.8, xlim=c(-15,15), ylim=c(0,60), xlab="log2(fold change)", ylab="-log10(adjusted p-value)", main="Melanoma tissue versus normal tissue \nVolcano plot in DESeq2 with featureCounts data")
sigGenes1 <- which(Volcano1[,3]<0.05)
notSigGenes1 <- which(Volcano1[,3]>=0.05)
sigDEGenes1 <- which(Volcano1[,3]<0.05 & abs(Volcano1[,2])>=1)
points(Volcano1[,2][sigGenes1], -log10(Volcano1[,3])[sigGenes1], pch=20, col="red")
points(Volcano1[,2][notSigGenes1], -log10(Volcano1[,3])[notSigGenes1], pch=20, col="black")
points(Volcano1[,2][sigDEGenes1], -log10(Volcano1[,3])[sigDEGenes1], pch=20, col="darkorange")
legend("topright", xjust=1, yjust=1, legend=c("adj p-value<0.05", "adj p-value<0.05 and |logFC|>=1", "not significant"), pch=20, col=c("red","darkorange","black"))
dev.off()


# ~~~~~~~~~~~~~~Second round of DE analysis will be with the count data produced by Sailfish from the Rsubread R-package~~~~~~~~~~~~

# Import and inspect count data: 
# Raw count data produced by Canopy Biosciences LLC. The first 7 columns contain gene identifiers and descriptions. Sample columns are ordered by 
# tissue type (19 melanoma samples, 4 normal samples), with the cancer samples listed first, followed by the normal samples.
countdata_Melanoma_Sailfish <- read.csv("Sailfish_counts.csv", header=TRUE) # This counts file was derived from"all.transcript_counts.xsl.txt" in the directory "RNA-Seq Melanoma.zip\RNA-Seq Melanoma\Results\P_AP_1349_052119"
countdata_Melanoma_Sailfish_protein <- countdata_Melanoma_Sailfish[countdata_Melanoma_Sailfish$transcript_biotype=="protein_coding",] # Select rows that are for protein coding sequences only. 
countdata_Melanoma_Sailfish_protein <- countdata_Melanoma_Sailfish_protein[!duplicated(countdata_Melanoma_Sailfish_protein),] # Remove any duplicate rows. 
dim(countdata_Melanoma_Sailfish_protein) 
colnames(countdata_Melanoma_Sailfish_protein) 
colSums(countdata_Melanoma_Sailfish_protein[,-c(1:7)])
summary(countdata_Melanoma_Sailfish_protein) 

# Round abundance values (counts) and filter expression data using a count cut-off: 
# Note that Sailfish produced non-integer counts and DESeq2 requires integers. After rounding, count data is filtered by keeping genes that have 
# greater than 10 counts for at least one sample.
countdata_Melanoma_Sailfish_round <- data.frame(countdata_Melanoma_Sailfish_protein[,1:7], ceiling(countdata_Melanoma_Sailfish_protein[,-c(1:7)])) # Round counts to upper integer.
countdata_Melanoma_Sailfish_filtered <- countdata_Melanoma_Sailfish_round[apply(countdata_Melanoma_Sailfish_round[,-c(1:7)], 1, function(countdata_Melanoma_Sailfish_round) length(countdata_Melanoma_Sailfish_round[countdata_Melanoma_Sailfish_round>=10])>0),]  
dim(countdata_Melanoma_Sailfish_filtered)
summary(countdata_Melanoma_Sailfish_filtered)
countmatrix_Melanoma_Sailfish_filtered <- data.matrix(countdata_Melanoma_Sailfish_filtered) # Convert data frame into a matrix.

# Load the sample information (i.e. meta-data):
# sample_info_Melanoma <- read.csv('meta_DESeq2.csv', header = TRUE) 

# Generate the DESeqDataSet object:
ddsMat_Melanoma_Sailfish <- DESeqDataSetFromMatrix(countData=countmatrix_Melanoma_Sailfish_filtered[,-c(1:7)], colData=sample_info_Melanoma, design = ~ 0 + melanoma) 
ddsMat_Melanoma_Sailfish$melanoma <- relevel(ddsMat_Melanoma_Sailfish$melanoma, ref="no")

# Estimate size factors (normalization factors) required for differential expression analysis:
norm_ddsMat_Melanoma_Sailfish <- estimateSizeFactors(ddsMat_Melanoma_Sailfish)

# Perform differential expression analysis:
DESeq_Melanoma_Sailfish <- DESeq(norm_ddsMat_Melanoma_Sailfish, test="Wald")

# Generate the dispersion plot for inspection:
tiff(filename = "Palleon_Melanoma_DESeq2_Sailfish_Disperison_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
plotDispEsts(DESeq_Melanoma_Sailfish, main="Melanoma tissue versus normal tissue \nDispersion plot in DESeq2 with Sailfish data")
dev.off()

# Extract and export all results:
res_Melanoma_Sailfish1 <- results(DESeq_Melanoma_Sailfish) # The false discovery rate was adjusted using the Benjamini-Hochberg procedure.
res_Melanoma_Sailfish2 <- cbind(countdata_Melanoma_Sailfish_filtered[,1:7], res_Melanoma_Sailfish1) 
colnames(res_Melanoma_Sailfish2) <- c("ensembl_transcript_id", "entrezgene", "ensembl_transcript_id.1", "transcript_biotype", "ensembl_gene_id", "external_gene_name", "description", "baseMean", "logFC", "logfcSE", "waldStat", "pValue", "pValue.adjust")
res_Melanoma_Sailfish3 <- res_Melanoma_Sailfish2[!duplicated(res_Melanoma_Sailfish2),]
write.csv(res_Melanoma_Sailfish3[order(res_Melanoma_Sailfish3$pValue.adjust),], file="Palleon_Melanoma_DESeq2_Sailfish_DE_Results.csv", row.names=FALSE)

# Produce an MA plot as a visualization of the DE results:
tiff(filename = "Palleon_Melanoma_DESeq2_Sailfish_MA_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
DESeq2::plotMA(res_Melanoma_Sailfish1, ylim=c(-20,20), main="Melanoma tissue versus normal tissue \nMA plot in DESeq2 with Sailfish data", xlab="mean of normalized counts", ylab="log2FC", cex=.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5) 
dev.off()

# Produce a volcano plot as a visualization of the DE results:
Volcano2 <- res_Melanoma_Sailfish3[,c(1,9,13)]
tiff(filename = "Palleon_Melanoma_DESeq2_Sailfish_Volcano_Plot.tiff", res=1200, width=9, height=5.75, units="in")
plot(Volcano2[,2], -log10(Volcano2[,3]), pch=20, col="black", cex=0.8, xlim=c(-15,15), ylim=c(0,60), xlab="log2(fold change)", ylab="-log10(adjusted p-value)", main="Melanoma tissue versus normal tissue \nVolcano plot in DESeq2 with Sailfish data")
sigGenes1 <- which(Volcano2[,3]<0.05)
notSigGenes1 <- which(Volcano2[,3]>=0.05)
sigDEGenes1 <- which(Volcano2[,3]<0.05 & abs(Volcano2[,2])>=1)
points(Volcano2[,2][sigGenes1], -log10(Volcano2[,3])[sigGenes1], pch=20, col="red")
points(Volcano2[,2][notSigGenes1], -log10(Volcano2[,3])[notSigGenes1], pch=20, col="black")
points(Volcano2[,2][sigDEGenes1], -log10(Volcano2[,3])[sigDEGenes1], pch=20, col="darkorange")
legend("topright", xjust=1, yjust=1, legend=c("adj p-value<0.05", "adj p-value<0.05 and |logFC|>=1", "not significant"), pch=20, col=c("red","darkorange","black"))
dev.off()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Genes of interest~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gene lists were given to me by Jill Prendergast

Melanoma_DESeq2_Sailfish_DE_Results <- read.csv(file="Palleon_Melanoma_DESeq2_Sailfish_DE_Results.csv", header=TRUE)

Melanoma_Genes_and_Pathways <- read.csv(file="Palleon_Melanoma_Genes_and_Pathways.csv", header=TRUE)
colnames(Melanoma_Genes_and_Pathways)[1] <- "external_gene_name"

Other_Human_Glycan_Related_Genes <- read.csv(file="Palleon_Other_Human_Glycan_Related_Genes.csv", header=TRUE)
colnames(Other_Human_Glycan_Related_Genes)[1] <- "external_gene_name"

Human_Glycoside_Hydrolases <- read.csv(file="Palleon_Human_Glycoside_Hydrolases.csv", header=TRUE)
colnames(Human_Glycoside_Hydrolases)[1] <- "external_gene_name"

Human_Glycosyltransferases <- read.csv(file="Palleon_Human_Glycosyltransferases.csv", header=TRUE)
colnames(Human_Glycosyltransferases)[1] <- "external_gene_name"

Melanoma_Genes_and_Pathways_DE_results <- merge(Melanoma_Genes_and_Pathways, Melanoma_DESeq2_Sailfish_DE_Results, by="external_gene_name", all.x=TRUE)
write.csv(Melanoma_Genes_and_Pathways_DE_results, file="Palleon_Melanoma_Genes_and_Pathways_DE_results.csv", row.names=FALSE)

Other_Human_Glycan_Related_Genes_DE_results <- merge(Other_Human_Glycan_Related_Genes, Melanoma_DESeq2_Sailfish_DE_Results, by="external_gene_name", all.x=TRUE)
write.csv(Other_Human_Glycan_Related_Genes_DE_results, file="Palleon_Other_Human_Glycan_Related_Genes_DE_results.csv", row.names=FALSE)

Human_Glycoside_Hydrolases_DE_results <- merge(Human_Glycoside_Hydrolases, Melanoma_DESeq2_Sailfish_DE_Results, by="external_gene_name", all.x=TRUE)
write.csv(Human_Glycoside_Hydrolases_DE_results, file="Palleon_Human_Glycoside_Hydrolases_DE_results.csv", row.names=FALSE)

Human_Glycosyltransferases_DE_results <- merge(Human_Glycosyltransferases, Melanoma_DESeq2_Sailfish_DE_Results, by="external_gene_name", all.x=TRUE)
write.csv(Human_Glycosyltransferases_DE_results, file="Palleon_Human_Glycosyltransferases_DE_results.csv", row.names=FALSE)


save.image(file="Palleon_Melanoma_DESeq2.RDATA")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Citations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Anders, S. and Huber, W. (2010). Differential expression analysis for sequence count data. Genome Biology, 11, 106.

# Benjamini Y. and Hochberg Y. (1995). Controlling the False Discovery Rate: A Practical and Powerful Approach to Multiple 
# Testing, Journal of the Royal Statistical Society, Series B (Methodological), 57(1), 289-300. 

# Love, M.I., Huber, W., Anders, S. (2014) Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2. 
# Genome Biology, 15, 550.
