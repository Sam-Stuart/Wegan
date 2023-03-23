# This script was written by Louisa C. Normington of LCN Bioinformatics, LLC for Palleon Pharmaceuticals. 

# This script was done in R v3.6.1 using packages limma v3.42.1 and edgeR v3.28.0. 
# See this webpage for information on how to switch R versions if needed:
# https://support.rstudio.com/hc/en-us/articles/200486138-Changing-R-versions-for-RStudio-desktop 

setwd("C:/Users/Louisa/Desktop/LCN_Bioinformatics/Palleon/RNA-Seq_Analysis_Melanoma/Palleon_Melanoma_edgeR") # Must use forward slash for Windows OS.

install.packages("BiocManager")
BiocManager::install(c("limma", "edgeR"))
library("limma") # for dispersion plot (plotBCV) and MA-plot (plotsmear)
library("edgeR") # for count normalization and differential expression analysis


# ~~~~~~~~~~~~~~First round of DE analysis will be with the count data produced by featureCounts from the Rsubread R-package.~~~~~~~~~~~~~~~~~~~~~~~~

# Import and inspect count data: 
# Raw count data produced by Canopy Biosciences LLC. The first 7 columns contain gene identifiers and descriptions. Sample columns are ordered by tissue 
# type (19 melanoma samples, 4 normal samples), with the cancer samples listed first, followed by the normal samples.
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

# Create DGEList object:
# The DGEList object contains RNA-seq count data with the associated treatment labels. 
group1 <- factor(c("melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "normal", "normal", "normal", "normal"))
d1 <- DGEList(counts=countdata_Melanoma_featureCounts_filtered[,-c(1:7)], group=group1) 
d1$samples # Opportunity to check that the correct groups were assigned to the samples. 

# Estimate normalization factors across samples of the same group:
# eadgR uses by default the trimmed mean of M-values (TMM) method (Robinson and Oshlak, 2010).
dnorm1 <- calcNormFactors(d1, method="TMM")

# Estimate common and trended dispersions over all genes, and the tagwise dispersion using the Cox-Reid adjusted likelihood (McCarthy et al. 2012):
# Dispersion estimation is the critical determinant of read count bias and gene length bias.
ddisp1 <- estimateCommonDisp(dnorm1)
ddisp1 <- estimateTrendedDisp(ddisp1)
ddisp1 <- estimateTagwiseDisp(ddisp1)

# Generate the dispersion plot for inspection:
# This figure gives a visual representation for the dispersion estimate process used by edgeR. It shows the estimated common dispersion, the trended dispersion and the tagwise
# (gene-wise) dispersions. 
tiff(filename = "Palleon_Melanoma_edgeR_featureCounts_Disperison_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
plotBCV(ddisp1, cex.axis=1.5, cex.lab=1.5, main="Melanoma tissue versus normal tissue \nDispersion plot in edgeR with featureCounts data")
dev.off()

# Construct the experimental design matrix:
# We will provide an experimental design matrix to ensure that differential expression is calculated using the normal group as the control.
# This technique gives us more control, and will allow transferability of this script to more complicated experimental designs where more than 
# two groups are to be compared for differential expression. 
design1 <- model.matrix(~0+group1, data=dnorm1$samples)
colnames(design1) <- levels(dnorm1$samples$group)

# Fit a GLM to each gene:
# Fit a negative binomial generalized log-linear model to the normalized read counts for each gene using the estimated tag-wise dispersion.
fit1 <- glmFit(ddisp1, design1)
colnames(fit1) # This prints the order of the contrast coefficients for the glm. Specific differential expression comparisons require the user to choose the correct coefficients when using the contrast argument below.

# Assess Differential expression:
# Count data with overdispersion can be modeled using standard generalized linear models. To compare expression levels of specific groups individually or in 
# combination, we use the 'contrast' argument of the 'glmLRT' function. The 'contrast' argument allows the user to specify the comparison. The order of treatment groups is determined 
# by the glmFit function on line 65. This determines the order of the DE contrasts in the glm. All values must add to zero for statistical methods in order to test the NULL hypothesis, 
# no differential expression will occur. The control group(s) is given the negative value. Fractions (in decimal form) can be used to compare more than two treatment groups.
DE_test1 <- glmLRT(fit1, contrast=c(1,-1)) 
DE_test1_decideTestsDGE <- decideTestsDGE(DE_test1, adjust.method="BH", p.value=0.05)
summary(DE_test1_decideTestsDGE) # Sanity check for the chosen coefficients. 
FDR1 <- p.adjust(DE_test1$table$PValue, method="BH") # The false discovery rate is adjusted using the Benjamini-Hochberg procedure (Benjamini & Hochberg, 1995). 
Result1 <- DE_test1$table
Result1["PValue.adjust"] <- FDR1
Result1 <- cbind(countdata_Melanoma_featureCounts_filtered[,1:7], Result1)
Result1_dup <- Result1[!duplicated(Result1),] # Remove any duplicate rows. 
colnames(Result1_dup)[1:7] <- c("ensembl_gene_id", "entrezgene", "external_gene_name", "gene_biotype", "external_gene_source", "transcript_count", "description")
write.csv(Result1_dup[order(Result1_dup$PValue.adjust),], file="Palleon_Melanoma_edgeR_featureCounts_DE_Results.csv", row.names=FALSE)

# Produce an MA plot as a visualization of the DE results:
# MA-plots visualize gene level differences in expression. Each point represents a differentially expressed gene. The average expression 
# of each gene is given in a log2 scale and is based on the counts of a specific gene per million counts of all genes. 
# Red genes are significantly differentially expressed based on the FDR of <0.05. 
DE_names1 <- rownames(Result1_dup[which(Result1_dup$PValue.adjust<0.05),])
tiff(filename = "Palleon_Melanoma_edgeR_featureCounts_MA_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
plotSmear(DE_test1, de.tags=DE_names1, main="Melanoma tissue versus normal tissue \nMA plot in edgeR with featureCounts data", xlab="average log2CPM", ylab="log2FC", cex=.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5) 
dev.off()

# Produce a volcano plot as a visualization of the DE results:
# A volcano plot was generated using the Benjamini-Hochberg FDR adjustment of p-values. Volcano plots show both fold change and p-value. 
# The log2 fold change is plotted on the x-axis and the negative log10 p-value is plotted on the y-axis.
Volcano1 <- Result1_dup[,c(1,8,12)]
tiff(filename = "Palleon_Melanoma_edgeR_featureCounts_Volcano_Plot.tiff", res=1200, width=9, height=5.75, units="in")
plot(Volcano1[,2], -log10(Volcano1[,3]), pch=20, col="black", cex=0.8, xlim=c(-15,15), ylim=c(0,60), xlab="log2(fold change)", ylab="-log10(adjusted p-value)", main="Melanoma tissue versus normal tissue \nVolcano plot in edgeR with featureCounts data")
sigGenes1 <- which(Volcano1[,3]<0.05)
notSigGenes1 <- which(Volcano1[,3]>=0.05)
sigDEGenes1 <- which(Volcano1[,3]<0.05 & abs(Volcano1[,2])>=1)
points(Volcano1[,2][sigGenes1], -log10(Volcano1[,3])[sigGenes1], pch=20, col="red")
points(Volcano1[,2][notSigGenes1], -log10(Volcano1[,3])[notSigGenes1], pch=20, col="black")
points(Volcano1[,2][sigDEGenes1], -log10(Volcano1[,3])[sigDEGenes1], pch=20, col="darkorange")
legend("topright", xjust=1, yjust=1, legend=c("adj p-value<0.05", "adj p-value<0.05 and |logFC|>=1", "not significant"), pch=20, col=c("red","darkorange","black"))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Second round of DE analysis will be with the count data produced by Sailfish (no changes to script)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Import and inspect count data: 
# Raw count data produced by Canopy Biosciences LLC. The first 7 columns contain gene identifiers and descriptions. Sample columns are ordered by tissue 
# type (19 melanoma samples, 4 normal samples), with the cancer samples listed first, followed by the normal samples.
countdata_Melanoma_Sailfish <- read.csv("Sailfish_counts.csv", header=TRUE) # This counts file was derived from"all.transcript_counts.xsl.txt" in the directory "RNA-Seq Melanoma.zip\RNA-Seq Melanoma\Results\P_AP_1349_052119"
countdata_Melanoma_Sailfish_protein <- countdata_Melanoma_Sailfish[countdata_Melanoma_Sailfish$transcript_biotype=="protein_coding",] # Select rows that are for protein coding sequences only. 
countdata_Melanoma_Sailfish_protein <- countdata_Melanoma_Sailfish_protein[!duplicated(countdata_Melanoma_Sailfish_protein),] # Remove any duplicate rows. 
dim(countdata_Melanoma_Sailfish_protein) 
colnames(countdata_Melanoma_Sailfish_protein) 
colSums(countdata_Melanoma_Sailfish_protein[,-c(1:7)])
summary(countdata_Melanoma_Sailfish_protein) 

# Filter count data by removing genes that have less than or equal to 10 counts for each sample:
countdata_Melanoma_Sailfish_filtered <- countdata_Melanoma_Sailfish_protein[apply(countdata_Melanoma_Sailfish_protein[,-c(1:7)], 1, function(countdata_Melanoma_Sailfish_protein) length(countdata_Melanoma_Sailfish_protein[countdata_Melanoma_Sailfish_protein>=10])>0),]  
dim(countdata_Melanoma_Sailfish_filtered)
summary(countdata_Melanoma_Sailfish_filtered)

# Create DGEList object:
group2 <- factor(c("melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "melanoma", "normal", "normal", "normal", "normal"))
d2 <- DGEList(counts=countdata_Melanoma_Sailfish_filtered[,-c(1:7)], group=group2) 
d2$samples # Opportunity to check that the correct groups were assigned to the samples. 

# Estimate normalization factors using the TMM:
dnorm2 <- calcNormFactors(d2, method="TMM")

# Estimate common and trended dispersions over all genes, and the tagwise dispersion using the Cox-Reid adjusted likelihood:
ddisp2 <- estimateCommonDisp(dnorm2)
ddisp2 <- estimateTrendedDisp(ddisp2)
ddisp2 <- estimateTagwiseDisp(ddisp2)

# Generate the dispersion plot for inspection:
tiff(filename = "Palleon_Melanoma_edgeR_Sailfish_Disperison_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
plotBCV(ddisp2, cex.axis=1.5, cex.lab=1.5, main="Melanoma tissue versus normal tissue \nDispersion plot in edgeR with Sailfish data")
dev.off()

# Construct the experimental design matrix:
design2 <- model.matrix(~0+group2, data=dnorm2$samples)
colnames(design2) <- levels(dnorm2$samples$group)

# Fit a GLM to each gene:
fit2 <- glmFit(ddisp2, design2)
colnames(fit2) # This prints the order of the contrast coefficients for the glm. 

# Assess Differential expression:
DE_test2 <- glmLRT(fit2, contrast=c(1,-1)) 
DE_test2_decideTestsDGE <- decideTestsDGE(DE_test2, adjust.method="BH", p.value=0.05)
summary(DE_test2_decideTestsDGE) # Sanity check for the chosen coefficients. 
FDR2 <- p.adjust(DE_test2$table$PValue, method="BH") 
Result2 <- DE_test2$table
Result2["PValue.adjust"] <- FDR2
Result2 <- cbind(countdata_Melanoma_Sailfish_filtered[,1:7], Result2)
Result2_dup <- Result2[!duplicated(Result2),] # Remove any duplicate rows. 
colnames(Result2_dup)[1:7] <- c("ensembl_transcript_id", "entrezgene", "ensembl_transcript_id.1", "transcript_biotype", "ensembl_gene_id", "external_gene_name", "description")
write.csv(Result2_dup[order(Result2_dup$PValue.adjust),], file="Palleon_Melanoma_edgeR_Sailfish_DE_Results.csv", row.names=FALSE)

# Produce an MA plot as a visualization of the DE results:
DE_names2 <- rownames(Result2_dup[which(Result2_dup$PValue.adjust<0.05),])
tiff(filename = "Palleon_Melanoma_edgeR_Sailfish_MA_Plot.tiff", res=1200, width=9, height=5.75, units="in")
par(mar=c(6, 6, 6, 6))
plotSmear(DE_test2, de.tags=DE_names2, main="Melanoma tissue versus normal tissue \nMA plot in edgeR with Sailfish data", xlab="average log2CPM", ylab="log2FC", cex=.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5) 
dev.off()

# Produce a volcano plot as a visualization of the DE results:
Volcano2 <- Result2_dup[,c(1,8,12)]
tiff(filename = "Palleon_Melanoma_edgeR_Sailfish_Volcano_Plot.tiff", res=1200, width=9, height=5.75, units="in")
plot(Volcano2[,2], -log10(Volcano2[,3]), pch=20, col="black", cex=0.8, xlim=c(-15,15), ylim=c(0,60), xlab="log2(fold change)", ylab="-log10(adjusted p-value)", main="Melanoma tissue versus normal tissue \nVolcano plot in edgeR with Sailfish data")
sigGenes1 <- which(Volcano2[,3]<0.05)
notSigGenes1 <- which(Volcano2[,3]>=0.05)
sigDEGenes1 <- which(Volcano2[,3]<0.05 & abs(Volcano2[,2])>=1)
points(Volcano2[,2][sigGenes1], -log10(Volcano2[,3])[sigGenes1], pch=20, col="red")
points(Volcano2[,2][notSigGenes1], -log10(Volcano2[,3])[notSigGenes1], pch=20, col="black")
points(Volcano2[,2][sigDEGenes1], -log10(Volcano2[,3])[sigDEGenes1], pch=20, col="darkorange")
legend("topright", xjust=1, yjust=1, legend=c("adj p-value<0.05", "adj p-value<0.05 and |logFC|>=1", "not significant"), pch=20, col=c("red","darkorange","black"))
dev.off()

save.image(file="Palleon_Melanoma_edgeR.RDATA")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Citations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Benjamini Y. and Hochberg Y. (1995). Controlling the False Discovery Rate: A Practical and Powerful Approach to Multiple 
# Testing, Journal of the Royal Statistical Society, Series B (Methodological), Vol.57, No. 1: 289-300. 

# McCarthy, D. J., Chen, Y., & Smyth, G. K. (2012). Differential expression analysis of multifactor RNA-Seq experiments 
# with respect to biological variation. Nucleic Acids Research, 40(10), 4288-4297. 

# Robinson, M. D., & Oshlack, A. (2010). A scaling normalization method for differential expression analysis of RNA-seq data. 
# Genome Biology, 11(3).
