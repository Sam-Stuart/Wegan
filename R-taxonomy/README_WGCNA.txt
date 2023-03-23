WGCNA for gene expression network analysis of lodgepole pine libraries. Script developed and executed by Louisa Normington Feb 5, 2020.

This script was run using R v3.6.1, WGCNA v1.68, DESeq2 v1.24.0 and gplots v3.0.3.


Input files:
"edgeR_counts_1cpm.csv", Counts produced by the featureCounts function from the Rsubread v1.24.2 R-package. Counts were filtered using the edgeR v3.20.9 function cpm (counts per million). This removed genes with low abundance values which may have reflected statistcal noise. Only genes which had >1 read per million mapped reads in at least 4 libraries were kept. 

"edgeR_LowN_2P_FungusVsWound_DE_results.csv", "edgeR_LowN_2X_FungusVsWound_DE_results.csv", "edgeR_HighN_2P_FungusVsWound_DE_results.csv", "edgeR_High N_2X_FungusVsWound_DE_results.csv" edgeR diffferntial expression results. See "README_edgeR.txt" for more information.

"All_DEG_categories_WGCNA.csv", Significantly differentially expressed genes that fell into each module were assigned categories and associated key words. 

"WGCNA_module_darkmagenta_heatmap_data_filtered_1E-5.csv", "WGCNA_module_orange_heatmap_data_filtered_1E-13.csv", "WGCNA_module_darkcyan_heatmap_data_filtered_1E-7.csv", "WGCNA_module_darkslateblue_heatmap_data_filtered_1E-6.csv", Spreadsheets contain GeneID, category/keyword, DE log2FC and adjp-value data. Duplicate GeneIDs were removed prior to import by choosing the GeneID with the lowest adjusted p-value for all DE comparisons. Note all genes were significantly diffferentially expressed in at least one Fungus vs Wound comparison (adj p-value<0.01 and |log2FC|>=2).

"WGCNA_module_darkcyan_categories_2P_sig_DEGs_for_agg.csv", "WGCNA_module_darkcyan_categories_2X_sig_DEGs_for_agg.csv", "WGCNA_module_darkcyan_categories_2X_sig_DEGs_for_agg.csv", "WGCNA_module_darkmagenta_categories_2P_sig_DEGs_for_agg.csv", Spreadsheets contain keyword, DE log2FC for low N and DE log2FC for low N. Genes included were significantly DE in at leaste one comparison (LowN_2P_FungusVsWound, HighN_2P_FungusVsWound, LowN_2X_FungusVsWound or HighN_2X_FungusVsWound). They also had duplicate GeneIDs removed by choosing the GeneID with the lowest adjusted p-value.

"lodgeAntn_v2.csv", Lodgepole pine master transcriptome annotation for annotating hub genes.


Script details:  
1. Input and inspect expression data: 64412 genes and 32 samples. 
 
2. Transform expression data by adding 1 and using the DESeq2 function varianceStabilizingTransformation, as was suggested by the creators of WGCNA here: https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/faq.html.

3. Check quality of genes: The function goodSampleGenes checks for missing entries, entries with weights below a threshold, and zero-variance genes, and returns a list of samples and genes that pass criteria on maximum number of missing or low weight values. All samples and genes pass the quality test.

4. Hierarchical clustering of samples using the average linakge method to detect outlier samples using the function hclust: Sample .17.06.2X is an outlier. It will not be included in the network analysis.

5. Pick a soft-threshold power (exponent) using the pickSoftThreshold and biweight midcorrelation argument: The soft-threshold is a value used to power the correlation of the genes to that threshold. The assumption is that raising the correlation to a higher power will reduce statistical noise. We will use two different techniques to choose the soft-threshold power: the Scale-free topology fit and the Mean connectivity. We will choose a soft-threshold as a trade-off between these two techniques. Scale-free networks of genes are robust in that they correlate genes in a manner that greatly exceeds the average correlation. We want highly correlated genes to be connected and fall into the same modules. We will choose a soft-thresholding power that will ensure that highly connected genes are connected in a manner that greatly exceeds the mean connectivity. Though 7 is a good choice based on mean connectivity, 15 is our soft-threshold power for network construction as we are assuming a scale-free topology. 

6. Generate TOM and identify expression modules using the function blockwiseModules: Pair-wise correlation (connectivity) values between genes are calculated using the biweight midcorrelation function with a 10% maximum outlier detection parameter. The Pearson correlation value is employed for any genes that have a median absolute deviation of zero, for which the biweight midcorrelation function fails. Correlation values are then weighted by raising them to the soft-threshold power of 15 in order to acheive a scale-free topology. The topological overlay map is then constructed to calculate the interconnectedness between correlated genes proportional to the number of neighbors that a pair of genes share in common. A signed network is being employed to differentiate between positively correlated genes that are strongly connected, and negatively correlated genes that are not strongly connected. Module membership is calculated by correlating the gene and its associated module eigengene using the biweight midcorrelation function with average sensitivity (deepsplit=2) and p-value ratio of 1E-8 for reassigning genes to a closer module. Parameters: maxBlockSize=70000, minBlockSize=0, corType="bicor", maxPOutliers=0.10, pearsonFallback="individual", power=15, networkType="signed", TOMType="signed", reassignThreshold=1E-8, minModuleSize=50, deepsplit=2, numericLabels=TRUE, verbose=4. 

Note on parameter optimization: Try different combinations and save figures under different file names (I like to use the parameter numbers in the file names to set them apart). If this optimization does not satisfy you, you may need to go further down in your analysis before making a decision. Try different minModuleSize parameters to determine cut-off (20 to 50; looking at table(moduleColors_neurons), we want a smaller number of small modules), maxPOutliers 0.05 or 0.1 (look at module dendrogram and choose the figure that looks less messy. If both figures look good, check the ME dendrogram. The more variation in heights, the better.), and different mergeCutHeight parameters (Look at module dendrogram, ME heatmap and ME dendrogram to determine cut-off. We want good separation between modules without over merging. Note that grey=no assigned module). Start with no color sequence (ie choose defaults) for module colors by excluding the argument "colorSeq". Once you settle on your parameters, you can adjust the color palette.

7. The Topological Overlay Map was generated on the Compute Canada server "Cedar" using the script "WGCNA_TOMs.slm". 256 GB were needed to create a TOM with up to 70,000 genes in about 2.5 hours. See this webpage for more information on WGCNA memory constraints: https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/Tutorials/FemaleLiver-02-networkConstr-auto.pdf

8. Convert the module labels to colors for plotting using the labels2colors function: The grey color is reserved for unassigned genes. Color palette chosen for color-blindness. See the cell phone app "SimDaltonism" when choosing a color palette.

9. Extract eigengenes from the blockwiseModules results: Module eigengenes represent the expression profile of their respective modules. They can be thought of as the first principal component of the module. 

10. Plot the interconnectedness dendrogram and the module colors underneath: Densely interconnected genes will be clustered into modules and displayed in an interconnectedness dendrogram, where genes with the highest intramodular connectivity were located at the tip of each module branch. Note that modules are networks in and of themselves. Intramodular connectivity is calculated by taking the sum of the pairwise biweight midcorrelation values (edge weights) of the gene to its module counterparts.

11. Plot the eigengene heatmap matrix using the function plotEigengeneNetworks: Modules are related pair-wise. Similar modules (red side of correlation spectrum) and dissimilar modules (blue side of correlation spectrum) are showcased. 

12. Plot the eigengene dendrogram using the function plotEigengeneNetworks: Modules are clustered by eigengene correlation. Highly crrelated eigengenes indicate more similar expresion patterns. 

13. Given the "messiness" of the module dendrogram figure, the modules were then merged until until larger unbroken modules were obtained using the function mergeCloseModules: The cutHeight argument 0.65 was chosen, which indicates a module eigene correlation cut off of 1-0.65=0.35 (ie modules with a correlation greater than 0.35 were merged). 

14. Plot the interconnectedness dendrogram with the module colors AND merged colors underneath using the function plotDendroAndColors: This offers visualization of the module merge process.

15. Add DE log2FC panels below the module colors panel for all FungusVsWound comparisons: log2FC values were produced by edgeR v3.20.9. See "README_edgeR.txt" for details. Note that the data were alphabetized by GeneID prior to being imported to ensure that all GeneIDs properly aligned in the resulting figure.

16. Extract the merged module eigengenes and remake the eigengene heatmap and eigengene dendrogram.

17. Created "for" loop to generate gene lists in .txt format for each module color.

18. Assigned categories, associated key words and DE results (including annotations) to significantly differentially expressed genes (detected by edgeR v3.20.9) that fell into each module: Darkcyan, darkmagenta, darkslateblue and orange had categorized genes. These four modules were used for data mining purposes. 

19. Created module heatmaps: log2FC data of significantly DE categorized genes that fell into the four largest modules were used to generate heatmaps. The dark slate blue genes included in the heatmap had an adjusted p-value cut-off of 1E-6 in at least one DE contrast (LowN_2P_FungusVsWound, HighN_2P_FungusVsWound, LowN_2X_FungusVsWound or HighN_2X_FungusVsWound). The dark magenta genes included in the heatmap had an adjusted p-value cut-off of 1E-5 in at least one DE contrast. The orange genes included in the heatmap had an adjusted p-value cut-off of 1E-13 in at least one DE contrast.The dark cyan genes included in the heatmap had an adjusted p-value cut-off of 1E-7 in at least one DE contrast.

20. Prepared module expression data for comparisons between high N and low N: A comparison of the gene expression patterns of defense-related genes induced by inoculation with G. clavigera was made between the low N trees (0.3 mM NH4NO3, fertilization) and the high N trees (10 mM NH4NO3 fertilization). Both tissue types, secondary phloem (2P) and secondary xylem (2X), were explored within the four largest modules. Median log2FC values for genes significantly DE in at least one category (adjusted P<0.01 and |logFC|>=2) were calculated for plotting in Excel. Imported files contained genes significantly DE in at least one comparison (LowN_2P_FungusVsWound, HighN_2P_FungusVsWound, LowN_2X_FungusVsWound or HighN_2X_FungusVsWound).They also had duplicate GeneIDs removed by choosing the GeneID with the lowest adjusted p-value.

21. Detected hub genes: Hub genes are highly connected to other genes in their module and are assumed to be involved in gene regulation. Therefore we looked for genes with high intramodular connectivity (top 10% most connected genes) plus significant module membership (>0.80). Connectivity was calculated using the function "intramodularConnectivity.fromExpr" and module membership was calculated with the function "signedKME".

22. Export nodes and edges for network visualization using the "exportNetworkToCytoscape" function. See "README_Cytoscape.txt" for more information. 


Output files: 
"WGCNA_network_genes.csv", This file will be used to determine significant differences in MapMan bincodes within modules. See "README_Enrichment_Analysis.txt" for more information. This file will also be used for creating edge files for Cytoscape.

"WGCNA_mergedColors.csv", All module colors produced with WGCNA. This file will also be used for creating edge files for Cytoscape.

10 tab separated files called "WGCNA_module_*COLOR*.txt", where *COLOR* indicates the module color to which the genes belong. 

"WGCNA_module_*COLOR*_categories.csv", 10 module gene lists with categories added.

"WGCNA_module_*COLOR*_categories_DE_results.csv", 10 module gene lists with categories and edgeR DE results added.

"WGCNA_module_category_counts.csv", Tabulation of cetegories that fell into each module (specifically Carbon-based, Nitrogen-based and total).

"WGCNA_*COLOR*_2*_sig_DEGs_agg.csv", Rows for duplicated keywords were collapsed using the aggregate function by calculating the median log2FC value for each keyword.

"WGCNA_*COLOR*_hub_genes.csv", Hub genes were used for network visualization with Cytoscape.


Figures:
" WGCNA clustering for the detection of outliers.tiff"- see step 4
" WGCNA scale-free topology as a function of the soft-thresholding power.tiff"- see step 5
" WGCNA mean connectivity as a function of the soft-thresholding power.tiff"- see step 5
" WGCNA module dendrogram.tiff"- see step 10
" WGCNA eigengene correlation heatmap.tiff"- see step 11
" WGCNA eigengene correlation dendrogram.tiff"- see step 12
" WGCNA module dendrogram with merged colors.tiff- see step 14
" WGCNA module dendrogram merged colors with DE data.tiff"- see step 15
" WGCNA eigengene correlation heatmap with merged colors.tiff" - see step 16
" WGCNA eigengene correlation dendrogram with merged colors.tiff"- see step 16
" WGCNA *COLOR* heatmap.tiff"- see step 19