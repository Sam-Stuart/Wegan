/*
 * ApplicationBean1.java
 *
 * Created on Oct 21, 2008, 9:37:17 AM
 */
package metaboanalyst.controllers;

import metaboanalyst.models.User;
import metaboanalyst.utils.DataUtils;
import java.io.File;
import java.io.Serializable;
//import java.util.Arrays;
//import java.util.List;
import javax.annotation.PostConstruct;
import javax.faces.bean.ApplicationScoped;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import metaboanalyst.rwrappers.RCenter;

@ManagedBean(name = "applicationBean1")
@ApplicationScoped
public class ApplicationBean1 implements Serializable {

    private final String domain_url;
    private final String rootContext = "/MetaboAnalyst";
    private final String resourcePath = "/resources"; //to store the real file paths
    private final String realPath;

    /*
    All relative paths below are below /resources
     */
    private static final String usr_home = "/users/";
    public static final String rscript_path = "/rscripts";
    private static final String integ_cmpd_data = "/data/integ_cmpds.txt";
    private static final String integ_gene_data = "/data/integ_genes.txt";
    private static final String network_kos_data = "/data/network_kos_example.txt";
    private static final String network_cpd_name_data = "/data/network_cpd_name_example.txt";
    private static final String test_conc_cow_data = "/data/cow_diet.csv";
    private static final String test_nmrbin = "/data/nmr_bins.csv";
    private static final String test_lcmsspec = "/data/lcms_netcdf.zip";
    private static final String test_nmrpeak = "/data/nmr_peaks.zip";
    private static final String test_concpair = "/data/time_series.csv";
    private static final String test_mstable = "/data/lcms_table.csv";
    private static final String test_mspeak2 = "/data/lcms_peaks_2col.zip";
    private static final String test_mspeak3 = "/data/lcms_3col_peaks.zip";
    private static final String test_gcmsspec = "/data/gcms_netcdf.zip";
    private static final String test_discr_data = "/data/human_cachexia.csv";
    private static final String test_cont_data = "/data/cachexia_continuous.csv";
    private static final String test_ts_data = "/data/cress_time.csv";
    private static final String test_ts1_data = "/data/time_series_only.csv";
    private static final String test_mz_all = "/data/mummichog_mzs.txt";
    private static final String test_currency_data = "/data/currency.txt";
    private static final String test_pos_adduct_data = "/data/pos_add_list.txt";
    private static final String test_neg_adduct_data = "/data/neg_add_list.txt";

    
    //WEGAN TEST DATA PATHS ----------------------------------------------------
    
//    private static final String test_amf = "/data/WeganTestAMF.csv";
//    private static final String test_dune = "/data/dune.txt";
//    private static final String test_BCI = "/data/BCI.txt";
//    private static final String test_varespec = "/data/varespec.txt";
//    private static final String test_linear = "/data/linear_test_data4.txt"; // linear plot test data 
//   
//    
//    private static final String test_iris = "/data/iris.txt";
////    private static final String test_linear = "/data/linear.txt";
//    private static final String test_dune_weights = "/data/dune_weights.txt";
//    private static final String test_iris = "/data/iris.txt";
    
    
    //**************************************************************************
    
    
    
    
    

    private static final String qc_cmp_data = "/data/qc_compare.csv";
    private static final String path_lib = "/libs/smp_path.csv";
    private static final String mset_dir = "/libs/msets";
    private static final String cmpd_conc_data = "/libs/cmpd_conc.csv";
    private static final String kegg_dir = "/libs/kegg";
    private static final String test_powerdata = "/data/human_cachexia.csv";
    private static final String test_rocdata = "/data/plasma_nmr.csv";
    private static final String test_rocnewdata = "/data/plasma_nmr_new.csv";
    private static final String bg_image = "/images/background.png";
        //WEGAN TEST DATA PATHS ----------------------------------------------------
    
    private static final String test_amf = "/data/WeganTestAMF.csv";
    private static final String test_dune = "/data/dune.csv";
    private static final String test_duneMeta = "/data/dune_meta.csv";
    private static final String test_duneEnv = "/data/dune_env.csv";
    private static final String test_duneTaxon = "/data/dune_taxon.csv";
    private static final String test_BCI = "/data/BCI.txt";
    private static final String test_varespec = "/data/varespec.csv";
    private static final String test_linear = "/data/linear_test_data.csv"; // linear plot test data 
    private static final String test_iris = "/data/iris.csv";
    private static final String test_irisOrd = "/data/iris_ord.csv";
    private static final String test_irisMeta = "/data/iris_meta.csv";
    private static final String test_dune_weights = "/data/dune_weights.txt";
    private static final String test_pitlatrine = "/data/SPE_pitlatrine.csv";
    private static final String test_pitlatrineEnv = "/data/ENV_pitlatrine.csv";
    private static final String test_WolvesElk = "/data/Wolves_Elk.csv";
    private static final String test_pitlatrineMeta = "/data/META_pitlatrine.csv";
    private static final String test_missing = "/data/missing_data_test.csv";

  
    //**************************************************************************

    private final SelectItem[] csvOpts;
    private final SelectItem[] cmpdIDOpts;
    private final SelectItem[] cmpdIDOpts1;
    private final SelectItem[] zipOpts;
    private final SelectItem[] csvFormatOpts;

    private final SelectItem[] metaFormatOpts;
    private final SelectItem[] taxFormatOpts;
    private final SelectItem[] envFormatOpts;
    private final SelectItem[] dataNamesOpts;
    private final SelectItem[] metaNamesOpts;
    private final SelectItem[] taxNamesOpts;
    private final SelectItem[] envNamesOpts;

    private final SelectItem[] rocFormatOpts;
    private final SelectItem[] tsFormatOpts;
    private final SelectItem[] testDataOpts;
    private final SelectItem[] caDataOpts;
    private final SelectItem[] qeaTestDataOpts;
    private final SelectItem[] tsDataOpts;
    private final SelectItem[] designOpts;
    private final SelectItem[] clsOpts;
    private final SelectItem[] biofluidOpts;
    private final SelectItem[] pathIDOpts;
    private final SelectItem[] dataNormOpts;
    private final SelectItem[] probNormOpts;
    private final SelectItem[] transNormOpts;
    private final SelectItem[] scaleNormOpts;
    private final SelectItem[] ordStressDimensionOpts;
    private final SelectItem[] corLinColorDotsOpts;    
    private final SelectItem[] corLinColorLineOpts; 
    private final SelectItem[] corModType;
    private final SelectItem[] corLegPosOpts;  
    private final SelectItem[] corColorPalettePredictors;    
    private final SelectItem[] corColorPaletteNonPredictors;   
    private final SelectItem[] corLinTextSizeOpts;   
    private final SelectItem[] corPlotNarrowWidthOpts; 
    private final SelectItem[] corColorPaletteOpts;
    private final SelectItem[] corBrewerPaletteOpts;
    private final SelectItem[] corPlotMetricOpts;
    private final SelectItem[] corPlotLabSize; 
    private final SelectItem[] corSizeTitle;
    private final SelectItem[] corSizeXlab;
    private final SelectItem[] corSizeYlab;
    private final SelectItem[] corSizeXtick;
    private final SelectItem[] corSizeYtick;
    private final SelectItem[] ordColorPaletteOpts;
    private final SelectItem[] pcaPairsColorPaletteOpts;
    private final SelectItem[] boxPltColorPaletteOpts;
    private final SelectItem[] pairAnalOpts;
    private final SelectItem[] equalVarOpts;
    private final SelectItem[] posthocOpts;
    private final SelectItem[] distMeasureOpts;
    private final SelectItem[] orgOpts;
    private final SelectItem[] geneIDOpts;
    private final SelectItem[] colorContrastOpts;
    private final SelectItem[] ptnOpts;
    private final SelectItem[] loadPlotOpts;
    private final SelectItem[] plsCVOpts;
    private final SelectItem[] plsClsOpts;
    private final SelectItem[] plsImpOpts;
    private final SelectItem[] clustDistOpts;
    private final SelectItem[] clustMethodOpts;
    private final SelectItem[] pathLibOpts;
    private final SelectItem[] massLibOpts;
    private final SelectItem[] refLibOpts;
    private final SelectItem[] vegdistMeasureOpts;
    private final SelectItem[] dendroPalletteOpts;
    private final SelectItem[] hmSmplColorOpts;
    private final SelectItem[] corrMethodsOpts;
    private final SelectItem[] ciaDataSetOpts;
    private final SelectItem[] ciaTypeOpts;
    private final SelectItem[] plottingDataOpts;
    private final SelectItem[] betadisperDataOpts;
//    private final SelectItem[] columnOptsFill = null;
   
    
    //Wegan variables begin here (currently ot used, regulat TestDataOpt is used
    
    private final SelectItem[] NMDSTestDataOpts;
    
    
    
    
    
    
    
    
    private final String testSSP = "L-Isolecine	0.34\nFumaric acid	0.47\nAcetone	0.58\nSuccinic acid	9.4\n1-Methylhistidine	9.6\n"
            + "L-Asparagine	19.62\n3-Methylhistidine	9.7\nL-Threonine	93.19\nCreatine	720\ncis-Aconitic acid	14.39\n"
            + "L-Tryptophan	35.78\nL-Carnitine	16.01\nL-Serine	17.32\nL-Tyrosine	67.51\nL-Alanine	219.02\n"
            + "L-Fucose	20.37\nD-Glucose	23.92\nPyroglutamic acid	26.38\nFormic acid	26.72\nIndoxyl sulfate	34.21\n"
            + "Dimethylamine	38.28\nEthanolamine	39.29\nGlycolic acid	41.39\nL-Glutamine	52.99\nL-Histidine	55.95\n"
            + "Trigonelline	57.4\n3-Aminoisobutanoic acid	89.76\nTaurine	116\nGlycine	123.52\nTrimethylamine N-oxide	128.04\n"
            + "Citric acid	225.31\nHippuric acid	278.53";

    private final String testORA = "Acetoacetic acid\nBeta-Alanine\nCreatine\nDimethylglycine\nFumaric acid\nGlycine\nHomocysteine\nL-Cysteine\n"
            + "L-Isolucine\nL-Phenylalanine\nL-Serine\nL-Threonine\nL-Tyrosine\nL-Valine\nPhenylpyruvic acid\nPropionic acid\nPyruvic acid\nSarcosine";

    
   
    public ApplicationBean1() {

        domain_url = ((HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRe‌​quest()).getRequestURL().toString();

        ServletContext context = (ServletContext) FacesContext.getCurrentInstance().getExternalContext().getContext();
        this.realPath = context.getRealPath(resourcePath);

        csvOpts = new SelectItem[3];
        csvOpts[0] = new SelectItem("main", "Main");
        csvOpts[1] = new SelectItem("meta", "Grouping");
        csvOpts[2] = new SelectItem("env", "Constraining");

        zipOpts = new SelectItem[3];
        zipOpts[0] = new SelectItem("nmrpeak", "NMR peak list");
        zipOpts[1] = new SelectItem("mspeak", "MS peak list");
        zipOpts[2] = new SelectItem("msspec", "MS spectra");

        cmpdIDOpts = new SelectItem[7];
        cmpdIDOpts[0] = new SelectItem("name", "Compound names");
        cmpdIDOpts[1] = new SelectItem("hmdb", "HMDB ID");
        cmpdIDOpts[2] = new SelectItem("kegg", "KEGG ID");
        cmpdIDOpts[3] = new SelectItem("pubchem", "PubChem CID");
        cmpdIDOpts[4] = new SelectItem("chebi", "ChEBI ID");
        cmpdIDOpts[5] = new SelectItem("metlin", "METLIN");
        cmpdIDOpts[6] = new SelectItem("hmdb_kegg", "HMDB and KEGG ID");



        csvFormatOpts = new SelectItem[2];
        csvFormatOpts[0] = new SelectItem("rowu", "Samples in rows, variables in columns");
        csvFormatOpts[1] = new SelectItem("colu", "Samples in columns, variables in rows");
//        csvFormatOpts[2] = new SelectItem("rowp", "Samples in rows (paired)");
//        csvFormatOpts[3] = new SelectItem("colp", "Samples in columns (paired)");

        metaFormatOpts = new SelectItem[2];
        metaFormatOpts[0] = new SelectItem("rowu", "Samples in rows, variables in columns");
        metaFormatOpts[1] = new SelectItem("colu", "Samples in columns, variables in rows");
//        metaFormatOpts[2] = new SelectItem("rowp", "Samples in rows (paired)");
//        metaFormatOpts[3] = new SelectItem("colp", "Samples in columns (paired)");
        
        taxFormatOpts = new SelectItem[2];
        taxFormatOpts[0] = new SelectItem("rowu", "Taxonomy data in row");
        taxFormatOpts[1] = new SelectItem("colu", "Taxonomy data in column");
        
        envFormatOpts = new SelectItem[2];
        envFormatOpts[0] = new SelectItem("rowu", "Samples in rows, variables in columns");
        envFormatOpts[1] = new SelectItem("colu", "Samples in columns, variables in rows");
//        envFormatOpts[2] = new SelectItem("rowp", "Samples in rows (paired)");
//        envFormatOpts[3] = new SelectItem("colp", "Samples in columns (paired)");
        
        dataNamesOpts = new SelectItem[4];
        dataNamesOpts[0] = new SelectItem("colOnly", "Column labels only");
        dataNamesOpts[1] = new SelectItem("rowOnly", "Row labels only");
        dataNamesOpts[2] = new SelectItem("bothNames", "Column and row labels");
        dataNamesOpts[3] = new SelectItem("noNames", "No labels");
        
        metaNamesOpts = new SelectItem[4];
        metaNamesOpts[0] = new SelectItem("colOnly", "Column labels only");
        metaNamesOpts[1] = new SelectItem("rowOnly", "Row labels only");
        metaNamesOpts[2] = new SelectItem("bothNames", "Both");
        metaNamesOpts[3] = new SelectItem("noNames", "Neither");
        
        taxNamesOpts = new SelectItem[3];
        taxNamesOpts[0] = new SelectItem("colOnly", "Column labels");
        taxNamesOpts[1] = new SelectItem("rowOnly", "Row labels");
        taxNamesOpts[2] = new SelectItem("noNames", "No labels");
        
        envNamesOpts = new SelectItem[4];
        envNamesOpts[0] = new SelectItem("colOnly", "Column labels only");
        envNamesOpts[1] = new SelectItem("rowOnly", "Row labels only");
        envNamesOpts[2] = new SelectItem("bothNames", "Both");
        envNamesOpts[3] = new SelectItem("noNames", "Neither");
        
        tsFormatOpts = new SelectItem[2];
        tsFormatOpts[0] = new SelectItem("rowts", "Samples in rows");
        tsFormatOpts[1] = new SelectItem("colts", "Samples in columns");

        rocFormatOpts = new SelectItem[2];
        rocFormatOpts[0] = new SelectItem("rowu", "Samples in rows");
        rocFormatOpts[1] = new SelectItem("colu", "Samples in columns");

        //Changed for WEGAN
        testDataOpts = new SelectItem[8];
        testDataOpts[0] = new SelectItem("Dune", "Dune");
        testDataOpts[1] = new SelectItem("Iris", "Iris");
        testDataOpts[2] = new SelectItem("BCI", "BCI");
        testDataOpts[3] = new SelectItem("Varespec", "Varespec");
        testDataOpts[4] = new SelectItem("Linear", "Linear");  
        testDataOpts[5] = new SelectItem("Pitlatrine", "Pitlatrine");
        testDataOpts[6] = new SelectItem("WolvesElk", "WolvesElk");
        testDataOpts[7] = new SelectItem("Missing", "Missing");
//        testDataOpts[4] = new SelectItem("nmrspecbin", "NMR spectral bins");
//        testDataOpts[5] = new SelectItem("nmrpeaklist", "NMR peak lists");
//        testDataOpts[6] = new SelectItem("concpair", "Concentrations (paired)");
//        testDataOpts[7] = new SelectItem("mspkint", "MS peak intensities");
//        testDataOpts[8] = new SelectItem("mspklist", "MS peak list");
//        testDataOpts[9] = new SelectItem("lcmsspec", "LC-MS spectra");
//        testDataOpts[10] = new SelectItem("gcmsspec", "GC-MS spectra");


        
        caDataOpts = new SelectItem[2];
        caDataOpts[0] = new SelectItem("Dune", "Dune");
        caDataOpts[1] = new SelectItem("BCI", "BCI");
        
        //WEGAN TEST FUNCTION
        NMDSTestDataOpts = new SelectItem[1];
        NMDSTestDataOpts[0] = new SelectItem("Dune");

        betadisperDataOpts = new SelectItem[2];
        betadisperDataOpts[0] = new SelectItem("org","Original Data set");
        betadisperDataOpts[1] = new SelectItem("norm","Normalized Data set");

                
        vegdistMeasureOpts = new SelectItem[14];
        vegdistMeasureOpts[0] = new SelectItem("NULL", "Bray-Curtis");
        vegdistMeasureOpts[1] = new SelectItem("binomial", "Binomial");
        vegdistMeasureOpts[2] = new SelectItem("canberra", "Canberra");
        vegdistMeasureOpts[3] = new SelectItem("cao", "Cao");
        vegdistMeasureOpts[4] = new SelectItem("chao", "Chao");
        vegdistMeasureOpts[5] = new SelectItem("euclidean", "Euclidean");
        vegdistMeasureOpts[6] = new SelectItem("gower", "Gower");
        vegdistMeasureOpts[7] = new SelectItem("horn", "Horn-Morisita");
        vegdistMeasureOpts[8] = new SelectItem("jaccard", "Jaccard");
        vegdistMeasureOpts[9] = new SelectItem("kulczynski", "Kulczynski");
        vegdistMeasureOpts[10] = new SelectItem("mahalanobis", "Mahalanobis");
        vegdistMeasureOpts[11] = new SelectItem("manhattan", "Manhattan");
        vegdistMeasureOpts[12] = new SelectItem("mountford", "Mountford");
        vegdistMeasureOpts[13] = new SelectItem("raup" , "Raup-Crick");
        
        dendroPalletteOpts = new SelectItem[3];
        dendroPalletteOpts[0] = new SelectItem("NULL", "Viridis");
        dendroPalletteOpts[1] = new SelectItem("plasma", "Plasma");
        dendroPalletteOpts[2] = new SelectItem("grey", "Grayscale");
        //dendroPalletteOpts[3] = new SelectItem("none", "No Color");
        
        hmSmplColorOpts = new SelectItem[3];
        hmSmplColorOpts[0] = new SelectItem("NULL", "Viridis");
        hmSmplColorOpts[1] = new SelectItem("plasma", "Plasma");
        hmSmplColorOpts[2] = new SelectItem("grey", "Grayscale");
        //hmSmplColorOpts[3] = new SelectItem("none", "No Color")
        
        ciaTypeOpts = new SelectItem[2];
        ciaTypeOpts[0] = new SelectItem("NULL", "Numeric Data Types");
        ciaTypeOpts[1] = new SelectItem("categorical", "Categorical Data Types");
                
        ciaDataSetOpts = new SelectItem[2];
        ciaDataSetOpts[0] = new SelectItem("NULL", "Main Data Set");
        ciaDataSetOpts[1] = new SelectItem("env", "Constraining Data Set");
                
        corLinColorDotsOpts = new SelectItem[5];
        corLinColorDotsOpts[0] = new SelectItem("NULL", "Black");
        corLinColorDotsOpts[1] = new SelectItem("blue", "Blue");
        corLinColorDotsOpts[2] = new SelectItem("red", "Red");
        corLinColorDotsOpts[3] = new SelectItem("green", "Green");
        corLinColorDotsOpts[4] = new SelectItem("grey", "Grey");

        corLinColorLineOpts = new SelectItem[5];
        corLinColorLineOpts[0] = new SelectItem("NULL", "Black");
        corLinColorLineOpts[1] = new SelectItem("blue", "Blue");
        corLinColorLineOpts[2] = new SelectItem("red", "Red");
        corLinColorLineOpts[3] = new SelectItem("green", "Green");
        corLinColorLineOpts[4] = new SelectItem("grey", "Grey");

//        lightpink = hashFFB6C1, lightblue = hashADD8E6, orchid = hashDA70D6, palegreen = hash98FB98
        corColorPalettePredictors = new SelectItem[5];
        corColorPalettePredictors[0] = new SelectItem("NULL", "Pink");
        corColorPalettePredictors[1] = new SelectItem("lightblue", "Blue");
        corColorPalettePredictors[2] = new SelectItem("orchid", "Purple");
        corColorPalettePredictors[3] = new SelectItem("palegreen", "Green");
        corColorPalettePredictors[4] = new SelectItem("grey", "Grey");        

//        lightpink = hashFFB6C1, lightblue = hashADD8E6, orchid = hashDA70D6, palegreen = hash98FB98
        corColorPaletteNonPredictors = new SelectItem[5];
        corColorPaletteNonPredictors[0] = new SelectItem("NULL", "Blue");
        corColorPaletteNonPredictors[1] = new SelectItem("lightpink", "Pink");
        corColorPaletteNonPredictors[2] = new SelectItem("orchid", "Purple");
        corColorPaletteNonPredictors[3] = new SelectItem("palegreen", "Green");
        corColorPaletteNonPredictors[4] = new SelectItem("grey", "Grey");  
        
        corLinTextSizeOpts = new SelectItem[9];
        corLinTextSizeOpts[0] = new SelectItem("NULL", "1");
        corLinTextSizeOpts[1] = new SelectItem("1", "1");
        corLinTextSizeOpts[2] = new SelectItem("2", "2");
        corLinTextSizeOpts[3] = new SelectItem("3", "3");
        corLinTextSizeOpts[4] = new SelectItem("4", "4");
        corLinTextSizeOpts[5] = new SelectItem("5", "5");
        corLinTextSizeOpts[5] = new SelectItem("6", "6");
        corLinTextSizeOpts[5] = new SelectItem("7", "7");
        corLinTextSizeOpts[5] = new SelectItem("8", "8"); 
 
        corPlotNarrowWidthOpts = new SelectItem[5];
        corPlotNarrowWidthOpts[0] = new SelectItem("NULL", "1");
        corPlotNarrowWidthOpts[1] = new SelectItem("1", "1");
        corPlotNarrowWidthOpts[2] = new SelectItem("2", "2");
        corPlotNarrowWidthOpts[3] = new SelectItem("3", "3");
        corPlotNarrowWidthOpts[4] = new SelectItem("4", "4");
        
        corModType = new SelectItem[4];
        corModType[0] = new SelectItem("NULL", "Select");
        corModType[1] = new SelectItem("binomial", "Binomial");
        corModType[2] = new SelectItem("multinomial", "Multinomial");
        corModType[3] = new SelectItem("ordinal", "Ordinal");
        
        corLegPosOpts = new SelectItem[4];
        corLegPosOpts[0] = new SelectItem("NULL", "Bottom");
        corLegPosOpts[1] = new SelectItem("top", "Top");
        corLegPosOpts[2] = new SelectItem("right", "Right");
        corLegPosOpts[3] = new SelectItem("left", "Left");
        
        corColorPaletteOpts = new SelectItem[8];
        corColorPaletteOpts[0] = new SelectItem("NULL", "Blambus");
        corColorPaletteOpts[1] = new SelectItem("metro", "Metro");
        corColorPaletteOpts[2] = new SelectItem("hero", "Hero");
        corColorPaletteOpts[3] = new SelectItem("ipsum", "Ipsum");
        corColorPaletteOpts[4] = new SelectItem("circus", "Circus");
        corColorPaletteOpts[5] = new SelectItem("viridis", "Virdis");
        corColorPaletteOpts[6] = new SelectItem("breakfast.club", "Breakfast Club");
        corColorPaletteOpts[7] = new SelectItem("aqua", "Aqua");
        
        corBrewerPaletteOpts = new SelectItem[5];
        corBrewerPaletteOpts[0] = new SelectItem("NULL", "Dark2");
        corBrewerPaletteOpts[1] = new SelectItem("Set2", "Set2");
        corBrewerPaletteOpts[2] = new SelectItem("RdYlBu", "Red Yellow Blue");
        corBrewerPaletteOpts[3] = new SelectItem("PiYG", "Pink Yellow Green");
        corBrewerPaletteOpts[4] = new SelectItem("BrBG", "Brown Blue Green");
        
        corPlotLabSize = new SelectItem[5];
        corPlotLabSize[0] = new SelectItem("NULL", "Medium");
        corPlotLabSize[1] = new SelectItem("sm", "Small");
        corPlotLabSize[2] = new SelectItem("med", "Medium");
        corPlotLabSize[3] = new SelectItem("lg", "Large");
        corPlotLabSize[4] = new SelectItem("lgs", "Larger");
        
        corSizeTitle = new SelectItem[6];
        corSizeTitle[0] = new SelectItem("NULL", "Medium");
        corSizeTitle[1] = new SelectItem("extrasmall", "Extra Small");        
        corSizeTitle[2] = new SelectItem("small", "Small");
        corSizeTitle[3] = new SelectItem("medium", "Medium");
        corSizeTitle[4] = new SelectItem("large", "Large");
        corSizeTitle[5] = new SelectItem("extralarge", "Extra Large");
        
        corSizeXlab = new SelectItem[6];
        corSizeXlab[0] = new SelectItem("NULL", "Medium");
        corSizeXlab[1] = new SelectItem("extrasmall", "Extra Small");        
        corSizeXlab[2] = new SelectItem("small", "Small");
        corSizeXlab[3] = new SelectItem("medium", "Medium");
        corSizeXlab[4] = new SelectItem("large", "Large");
        corSizeXlab[5] = new SelectItem("extralarge", "Extra Large");
        
        corSizeYlab = new SelectItem[6];
        corSizeYlab[0] = new SelectItem("NULL", "Medium");
        corSizeYlab[1] = new SelectItem("extrasmall", "Extra Small");        
        corSizeYlab[2] = new SelectItem("small", "Small");
        corSizeYlab[3] = new SelectItem("medium", "Medium");
        corSizeYlab[4] = new SelectItem("large", "Large");
        corSizeYlab[5] = new SelectItem("extralarge", "Extra Large");
        
        corSizeXtick = new SelectItem[6];
        corSizeXtick[0] = new SelectItem("NULL", "Medium");
        corSizeXtick[1] = new SelectItem("extrasmall", "Extra Small");        
        corSizeXtick[2] = new SelectItem("small", "Small");
        corSizeXtick[3] = new SelectItem("medium", "Medium");
        corSizeXtick[4] = new SelectItem("large", "Large");
        corSizeXtick[5] = new SelectItem("extralarge", "Extra Large");
        
        corSizeYtick = new SelectItem[6];
        corSizeYtick[0] = new SelectItem("NULL", "Medium");
        corSizeYtick[1] = new SelectItem("extrasmall", "Extra Small");        
        corSizeYtick[2] = new SelectItem("small", "Small");
        corSizeYtick[3] = new SelectItem("medium", "Medium");
        corSizeYtick[4] = new SelectItem("large", "Large");
        corSizeYtick[5] = new SelectItem("extralarge", "Extra Large");

        corPlotMetricOpts = new SelectItem[2];
        corPlotMetricOpts[0] = new SelectItem("NULL", "None");
        corPlotMetricOpts[1] = new SelectItem("rmse", "RMSE");
        
        ordColorPaletteOpts = new SelectItem[4];
        ordColorPaletteOpts[0] = new SelectItem("NULL", "Viridis");
        ordColorPaletteOpts[1] = new SelectItem("plasma", "Plasma");
        ordColorPaletteOpts[2] = new SelectItem("grey", "Grayscale");
        ordColorPaletteOpts[3] = new SelectItem("none", "No Color");
        
        pcaPairsColorPaletteOpts = new SelectItem[5];
        pcaPairsColorPaletteOpts[0] = new SelectItem("NULL", "Viridis");
        pcaPairsColorPaletteOpts[1] = new SelectItem("plasma", "Plasma");
        pcaPairsColorPaletteOpts[2] = new SelectItem("grey", "Grayscale");
        pcaPairsColorPaletteOpts[3] = new SelectItem("blue", "Blue");
        pcaPairsColorPaletteOpts[4] = new SelectItem("none", "No Color");
                
        boxPltColorPaletteOpts = new SelectItem[5];
        boxPltColorPaletteOpts[0] = new SelectItem("v", "Viridis");
        boxPltColorPaletteOpts[1] = new SelectItem("p", "Plasma");
        boxPltColorPaletteOpts[2] = new SelectItem("g", "Grayscale");
        boxPltColorPaletteOpts[3] = new SelectItem("r", "Rainbow");
        boxPltColorPaletteOpts[4] = new SelectItem("b", "Light Blue");
        
        ordStressDimensionOpts = new SelectItem[5];
        ordStressDimensionOpts[0] = new SelectItem("NULL", "1");
        ordStressDimensionOpts[1] = new SelectItem("2", "2");
        ordStressDimensionOpts[2] = new SelectItem("3", "3");
        ordStressDimensionOpts[3] = new SelectItem("4", "4");
        ordStressDimensionOpts[4] = new SelectItem("5", "5");    

        plottingDataOpts = new SelectItem[2];
        plottingDataOpts[0] = new SelectItem("Dune");
        plottingDataOpts[1] = new SelectItem("Linear");

        cmpdIDOpts1 = new SelectItem[4];
        cmpdIDOpts1[0] = new SelectItem("na", "-- Please specify");
        cmpdIDOpts1[1] = new SelectItem("name", "Compound names");
        cmpdIDOpts1[2] = new SelectItem("hmdb", "HMDB ID");
        cmpdIDOpts1[3] = new SelectItem("kegg", "KEGG ID");

        pathIDOpts = new SelectItem[4];
        pathIDOpts[0] = new SelectItem("na", "-- Please specify");
        pathIDOpts[1] = new SelectItem("name", "Compound Name");
        pathIDOpts[2] = new SelectItem("hmdb", "HMDB ID");
        pathIDOpts[3] = new SelectItem("kegg", "KEGG ID");

        clsOpts = new SelectItem[2];
        clsOpts[0] = new SelectItem("disc", "Discrete (Classification)");
        clsOpts[1] = new SelectItem("cont", "Continuous (Regression)");

        qeaTestDataOpts = new SelectItem[2];
        qeaTestDataOpts[0] = new SelectItem("msetDis", "Data 1");
        qeaTestDataOpts[1] = new SelectItem("conReq", "Data 2");

        tsDataOpts = new SelectItem[2];
        tsDataOpts[0] = new SelectItem("time2", "Time-series + one experimental factor");
        tsDataOpts[1] = new SelectItem("time1", "Time-series only");

        biofluidOpts = new SelectItem[10];
        biofluidOpts[0] = new SelectItem("urine", "Urine (umol/mmol_creatinine)");
        biofluidOpts[1] = new SelectItem("blood", "Blood (umol)");
        biofluidOpts[2] = new SelectItem("csf", "CSF (umol)");
        biofluidOpts[3] = new SelectItem("saliva", "Saliva (uM)");
        biofluidOpts[4] = new SelectItem("amniotic fluid", "Amniotic fluid (uM)");
        biofluidOpts[5] = new SelectItem("bile", "Bile (uM)");
        biofluidOpts[6] = new SelectItem("breast milk", "Breast Milk (uM)");
        biofluidOpts[7] = new SelectItem("cellular cytoplasm", "Cytoplasm (uM)");
        biofluidOpts[8] = new SelectItem("feces1", "Feces (uM)");
        biofluidOpts[9] = new SelectItem("feces", "Feces (nmol/g)");

        designOpts = new SelectItem[3];
        designOpts[0] = new SelectItem("time0", "Time-series only");
        designOpts[1] = new SelectItem("time", "Time-series + one experimental factor");
        designOpts[2] = new SelectItem("g2", "Two-factor independent samples");

        dataNormOpts = new SelectItem[8];
        dataNormOpts[0] = new SelectItem("NULL", "None");
//        dataNormOpts[1] = new SelectItem("SpecNorm", "Sample-specific normalization");
        dataNormOpts[1] = new SelectItem("SumNorm", "Normalization by sum");
        dataNormOpts[2] = new SelectItem("MedianNorm", "Normalization by median");
//        dataNormOpts[4] = new SelectItem("SamplePQN", "Specify a reference sample");
//        dataNormOpts[5] = new SelectItem("GroupPQN", "Create a pooled average sample from group");
//        dataNormOpts[6] = new SelectItem("CompNorm", "Normalization by reference feature");
        dataNormOpts[3] = new SelectItem("QuantileNorm", "Quantile normalization");
        dataNormOpts[4] = new SelectItem("BoxNorm", "Box-Cox normalization");
        dataNormOpts[5] = new SelectItem("LogNorm", "Log transformation");
        dataNormOpts[6] = new SelectItem("CrNorm", "Cube root transformation");
        dataNormOpts[7] = new SelectItem("SqNorm", "Square root transformation");

        probNormOpts = new SelectItem[2];
        probNormOpts[0] = new SelectItem("F", "Specify a reference sample");
        probNormOpts[1] = new SelectItem("T", "Create a pooled average sample from group");

        transNormOpts = new SelectItem[4];
        transNormOpts[0] = new SelectItem("NULL", "None");
        transNormOpts[1] = new SelectItem("LogNorm", "Log transformation");
        transNormOpts[2] = new SelectItem("CrNorm", "Cube root transformation");
        transNormOpts[3] = new SelectItem("SqNorm", "Square root transformation");

        scaleNormOpts = new SelectItem[5];
        scaleNormOpts[0] = new SelectItem("NULL", "None");
        scaleNormOpts[1] = new SelectItem("MeanCenter", "Mean centering");
        scaleNormOpts[2] = new SelectItem("AutoNorm", "Auto scaling");
        scaleNormOpts[3] = new SelectItem("ParetoNorm", "Pareto scaling");
        scaleNormOpts[4] = new SelectItem("RangeNorm", "Range scaling");

        pairAnalOpts = new SelectItem[2];
        pairAnalOpts[0] = new SelectItem("FALSE", "Unpaired");
        pairAnalOpts[1] = new SelectItem("TRUE", "Paired");

        equalVarOpts = new SelectItem[2];
        equalVarOpts[0] = new SelectItem("TRUE", "Equal");
        equalVarOpts[1] = new SelectItem("FALSE", "Unequal");

        posthocOpts = new SelectItem[2];
        posthocOpts[0] = new SelectItem("fisher", "Fisher\'s LSD");
        posthocOpts[1] = new SelectItem("tukey", "Tukey\'s HSD");

        distMeasureOpts = new SelectItem[3];
        distMeasureOpts[0] = new SelectItem("pearson", "Pearson R correlation");
        distMeasureOpts[1] = new SelectItem("spearman", "Spearman rank correlation");
        distMeasureOpts[2] = new SelectItem("kendall", "Kendall rank correlation");

        corrMethodsOpts = new SelectItem[3];
        corrMethodsOpts[0] = new SelectItem("ridge", "Ridge");
        corrMethodsOpts[1] = new SelectItem("elastic net", "Elastic Net");
        corrMethodsOpts[2] = new SelectItem("lasso", "Lasso");
        
        orgOpts = new SelectItem[4];
        orgOpts[0] = new SelectItem("vegdistMeasureOpts", "----Not specified----");
        orgOpts[1] = new SelectItem("hsa", "Homo sapiens (human)");
        orgOpts[2] = new SelectItem("mmu", "Mus musculus (mouse)");
        orgOpts[3] = new SelectItem("rno", "Rattus norvegicus (rat)");

        geneIDOpts = new SelectItem[6];
        geneIDOpts[0] = new SelectItem("NA", "--- Not Specified ---");
        geneIDOpts[1] = new SelectItem("entrez", "Entrez ID");
        geneIDOpts[2] = new SelectItem("refseq", "RefSeq ID");
        geneIDOpts[3] = new SelectItem("genbank", "Genbank Accession Number");
        geneIDOpts[4] = new SelectItem("embl", "ENSEMBL Gene Accession Number");
        geneIDOpts[5] = new SelectItem("symbol", "Official Gene Symbol");

        colorContrastOpts = new SelectItem[5];
        colorContrastOpts[0] = new SelectItem("bwm", "Default");
        colorContrastOpts[1] = new SelectItem("gbr", "Red / Green");
        colorContrastOpts[2] = new SelectItem("heat", "Heat Color");
        colorContrastOpts[3] = new SelectItem("topo", "Topo Color");
        colorContrastOpts[4] = new SelectItem("gray", "Gray Scale");

        ptnOpts = new SelectItem[3];
        ptnOpts[0] = new SelectItem("featptn", "a feature of interest");
        ptnOpts[1] = new SelectItem("preptn", "a predefined profile");
        ptnOpts[2] = new SelectItem("usrptn", "a custom profile");

        loadPlotOpts = new SelectItem[2];
        loadPlotOpts[0] = new SelectItem("scatter", "Scatter Plot");
        loadPlotOpts[1] = new SelectItem("bar", "Bar Plot");

        plsCVOpts = new SelectItem[2];
        plsCVOpts[0] = new SelectItem("T", "10-fold CV");
        plsCVOpts[1] = new SelectItem("L", "LOOCV");

        plsClsOpts = new SelectItem[3];
        plsClsOpts[0] = new SelectItem("Q2", "Q2");
        plsClsOpts[1] = new SelectItem("Accuracy", "Accuracy");
        plsClsOpts[2] = new SelectItem("R2", "R2");

        plsImpOpts = new SelectItem[2];
        plsImpOpts[0] = new SelectItem("vip", "VIP score");
        plsImpOpts[1] = new SelectItem("coef", "Coefficient score");

        clustMethodOpts = new SelectItem[4];
        clustMethodOpts[0] = new SelectItem("ward.D", "Ward");
        clustMethodOpts[1] = new SelectItem("average", "Average");
        clustMethodOpts[2] = new SelectItem("complete", "Complete");
        clustMethodOpts[3] = new SelectItem("single", "Single");

        clustDistOpts = new SelectItem[3];
        clustDistOpts[0] = new SelectItem("euclidean", "Euclidean");
        clustDistOpts[1] = new SelectItem("spearman", "Spearman");
        clustDistOpts[2] = new SelectItem("pearson", "Pearson");

        pathLibOpts = new SelectItem[23];
        pathLibOpts[0] = new SelectItem("hsa", "Homo sapiens (human) [KEGG:80]");
        pathLibOpts[1] = new SelectItem("hsa-smpdb", "Homo sapiens (human) [SMPDB]"); //SMPDB list
        pathLibOpts[2] = new SelectItem("mmu", "Mus musculus (mouse) [KEGG:82]");
        pathLibOpts[3] = new SelectItem("mmu-smpdb", "Mus musculus (mouse) [SMPDB]");
        pathLibOpts[4] = new SelectItem("rno", "Rattus norvegicus (rat) [81]");
        pathLibOpts[5] = new SelectItem("bta", "Bos taurus (cow) [81]");
        pathLibOpts[6] = new SelectItem("gga", "Gallus gallus (chicken) [78]");
        pathLibOpts[7] = new SelectItem("dre", "Danio rerio (zebrafish) [81]");
        pathLibOpts[8] = new SelectItem("dme", "Drosophila melanogaster (fruit fly) [79]");
        pathLibOpts[9] = new SelectItem("cel", "Caenorhabditis elegans (nematode) [78]");
        pathLibOpts[10] = new SelectItem("sce", "Saccharomyces cerevisiae (yeast) [65]");
        pathLibOpts[11] = new SelectItem("osa", "Oryza sativa japonica (Japanese rice) [83]");
        pathLibOpts[12] = new SelectItem("ath", "Arabidopsis thaliana (thale cress) [87]");
        pathLibOpts[13] = new SelectItem("smm", "Schistosoma mansoni [69]");
        pathLibOpts[14] = new SelectItem("pfa", "Plasmodium falciparum 3D7 (Malaria) [47]");
        pathLibOpts[15] = new SelectItem("tbr", "Trypanosoma brucei [54]");
        pathLibOpts[16] = new SelectItem("eco", "Escherichia coli K-12 MG1655 [87]");
        pathLibOpts[17] = new SelectItem("bsu", "Bacillus subtilis [80]");
        pathLibOpts[18] = new SelectItem("ppu", "Pseudomonas putida KT2440 [89]");
        pathLibOpts[19] = new SelectItem("sau", "Staphylococcus aureus N315 (MRSA/VSSA) [73]");
        pathLibOpts[20] = new SelectItem("tma", "Thermotoga maritima [57]");
        pathLibOpts[21] = new SelectItem("syf", "Synechococcus elongatus PCC7942 [75]");
        pathLibOpts[22] = new SelectItem("mlo", "Mesorhizobium loti [86]");

        massLibOpts = new SelectItem[27];
        massLibOpts[0] = new SelectItem("hsa_mfn", "Homo sapiens (human) [Recon2]");
        massLibOpts[1] = new SelectItem("hsa_biocyc", "Homo sapiens (human) [BioCyc]");
        massLibOpts[2] = new SelectItem("hsa_kegg", "Homo sapiens (human) [KEGG]");
        massLibOpts[3] = new SelectItem("mmu_biocyc", "Mus musculus (mouse) [BioCyc]");
        massLibOpts[4] = new SelectItem("mmu_kegg", "Mus musculus (mouse) [KEGG]");
        massLibOpts[5] = new SelectItem("rno_kegg", "Rattus norvegicus (rat) [KEGG]");
        massLibOpts[6] = new SelectItem("bta_kegg", "Bos taurus (cow) [KEGG]");
        massLibOpts[7] = new SelectItem("gga_kegg", "Gallus gallus (chicken) [KEGG]");
        massLibOpts[8] = new SelectItem("dre_kegg", "Danio rerio (zebrafish) [KEGG]");
        massLibOpts[9] = new SelectItem("dre_mtf", "Danio rerio (zebrafish) [MTF]");
        massLibOpts[10] = new SelectItem("dme_kegg", "Drosophila melanogaster (fruit fly) [KEGG]");
        massLibOpts[11] = new SelectItem("dme_biocyc", "Drosophila melanogaster (fruit fly) [BioCyc]");
        massLibOpts[12] = new SelectItem("cel_kegg", "Caenorhabditis elegans (nematode) [KEGG]");
        massLibOpts[13] = new SelectItem("sce_kegg", "Saccharomyces cerevisiae (yeast) [KEGG]");
        massLibOpts[14] = new SelectItem("sce_biocyc", "Saccharomyces cerevisiae (yeast) [BioCyc]");
        massLibOpts[15] = new SelectItem("osa_kegg", "Oryza sativa japonica (Japanese rice) [KEGG]");
        massLibOpts[16] = new SelectItem("ath_kegg", "Arabidopsis thaliana (thale cress) [KEGG]");
        massLibOpts[17] = new SelectItem("smm_kegg", "Schistosoma mansoni [KEGG]");
        massLibOpts[18] = new SelectItem("pfa_kegg", "Plasmodium falciparum 3D7 (Malaria) [KEGG]");
        massLibOpts[19] = new SelectItem("tbr_kegg", "Trypanosoma brucei [KEGG]");
        massLibOpts[20] = new SelectItem("eco_kegg", "Escherichia coli K_12 MG1655 [KEGG]");
        massLibOpts[21] = new SelectItem("bsu_kegg", "Bacillus subtilis [KEGG]");
        massLibOpts[22] = new SelectItem("ppu_kegg", "Pseudomonas putida KT2440 [KEGG]");
        massLibOpts[23] = new SelectItem("sau_kegg", "Staphylococcus aureus N315 (MRSA/VSSA) [KEGG]");
        massLibOpts[24] = new SelectItem("tma_kegg", "Thermotoga maritima [KEGG]");
        massLibOpts[25] = new SelectItem("syf_kegg", "Synechococcus elongatus PCC7942 [KEGG]");
        massLibOpts[26] = new SelectItem("mlo_kegg", "Mesorhizobium loti [KEGG]");

        refLibOpts = new SelectItem[2];
        refLibOpts[0] = new SelectItem("all", "Use all compounds in the selected pathways");
        refLibOpts[1] = new SelectItem("self", "Upload a reference metabolome based on your technical platform");

    }

    //The compiling will be created upon application start up
    @PostConstruct
    public void compileRScript() {
        RCenter.compileRScripts(realPath + usr_home, realPath + rscript_path + "/_script_loader.R");
    }

    private int userCount = 0;

    // start distributing for over 10 users after deploy, 
    // to save time for dev server to compile R code
    // even to main, odd to dev
    public String getNaviURL() {
        String URL;
        if (isOnPublicServer() & !domain_url.contains("old")) {
            //  if (userCount < 10 || userCount % 2 == 0) { 
            URL = "http://www.metaboanalyst.ca/faces/ModuleView.xhtml";
            //  } else {
            //      URL = "http://old.metaboanalyst.ca/faces/ModuleView.xhtml";
            //  }
        } else {
            URL = domain_url + "/faces/ModuleView.xhtml";
        }
        userCount++;
        return URL;
    }

    // create a tempature user accoutn if user log in as a guest will not remember, only session only
    public User createTempUser() {
        try {
            String realUsrPath = realPath + usr_home;
            //try to clean the user folder to remove old files (more than 1 day)
            DataUtils.deleteFilesOlderThanNdays(realUsrPath);
            //first create a random user names
            User user = new User();
            String guestName = File.createTempFile("guest", "tmp").getName();
            String guestDir = realUsrPath + File.separator + guestName;
            File guestFolder = new File(guestDir);
            while (guestFolder.exists()) {
                guestName = File.createTempFile("guest", "tmp").getName();
                guestDir = realUsrPath + File.separator + guestName;
                guestFolder = new File(realUsrPath + File.separator + guestName);
            }
            guestFolder.mkdir();
            user.setName(guestName);
            user.setRelativeDir("/resources/users/" + guestName);
            user.setHomeDir(guestDir);
            return user;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public boolean isOnPublicServer() {
        return domain_url.contains("metaboanalyst.ca");
    }

    public String getRootContext() {
        return rootContext;
    }

    public String getDomainURL() {
        return domain_url;
    }

    public String getRscriptsPath() {
        return realPath + rscript_path;
    }

    public String getRscriptLoaderPath() {
        return realPath + rscript_path + "/_script_loader.R";
    }

    //WEGAN GET TEST DATA PATHS ------------------------------------------------
    public String getTestamf() {
        return realPath + test_amf;
    }
    
    public String getTestDuneMeta() {
        return realPath + test_duneMeta;
    }
    
    public String getTestDuneEnv() {
        return realPath + test_duneEnv;
    }
    
    public String getTestDune() {
        return realPath + test_dune;
    }
    
    public String getTestDuneTaxon() {
        return realPath + test_duneTaxon;
    }
            
    public String getTestPitlatrine() {
        return realPath + test_pitlatrine;
    }
    
    public String getTestPitlatrineEnv() {
        return realPath + test_pitlatrineEnv;
    }
    
    public String getTestPitlatrineMeta() {
        return realPath + test_pitlatrineMeta;
    }
    
    public String getTestMissing() {
        return realPath + test_missing;
    }

    public String getTestWolvesElk() {
        return realPath + test_WolvesElk;
    }
            
    public String getTestIris() {
        return realPath + test_iris;
    }

    public String getTestIrisMeta() {
        return realPath + test_irisMeta;
    }
    
    public String getTestIrisOrd() {
        return realPath + test_irisOrd;
    }            
            
    public String getTestWeightDune() {
        return realPath + test_dune_weights;
    }
        
    public String getTestBCI(){
        return realPath + test_BCI;
    }
    
    public String getTestVarespec(){
        return realPath + test_varespec;
    }
    
    public String getTestLinear(){
        return realPath + test_linear;
    }
    
    //--------------------------------------------------------------------------
    
    
    public String getTestAllPeakPath() {
        return realPath + test_mz_all;
    }

    public String getTestConcHsaPath() {
        return realPath + test_discr_data;
    }

    public String getTestPowerPath() {
        return realPath + test_powerdata;
    }

    public String getTestRocPath() {
        return realPath + test_rocdata;
    }

    public String getTestRocNewPath() {
        return realPath + test_rocnewdata;
    }

    public String getBgImgPath() {
        return realPath + bg_image;
    }

    public String getPosAdductPath() {
        return realPath + test_pos_adduct_data;
    }

    public String getNegAdductPath() {
        return realPath + test_neg_adduct_data;
    }

    public String getCurrPath() {
        return realPath + test_currency_data;
    }

    public String getIntegCmpdPath() {
        return realPath + integ_cmpd_data;
    }

    public String getIntegGenePath() {
        return realPath + integ_gene_data;
    }

    public String getIntegKosPath() {
        return realPath + network_kos_data;
    }

    public String getIntegNamesPath() {
        return realPath + network_cpd_name_data;
    }

    public String getTestConcCowPath() {
        return realPath + test_conc_cow_data;
    }

    public String getTestMummichog() {
        return realPath + test_mz_all;
    }

    public String getQcCmpPath() {
        return realPath + qc_cmp_data;
    }

    public String getTimeOnlyPath() {
        return realPath + test_ts1_data;
    }

    public String getTestNMRbinPath() {
        return realPath + test_nmrbin;
    }

    public String getTestLCMSspecPath() {
        return realPath + test_lcmsspec;
    }

    public String getTestGCMSspecPath() {
        return realPath + test_gcmsspec;
    }

    public String getTestConcPairPath() {
        return realPath + test_concpair;
    }

    public String getTestMSTable() {
        return realPath + test_mstable;
    }

    public String getTestTimeSeriesData() {
        return realPath + test_ts_data;
    }

    public SelectItem[] getQeaTestDataOpts() {
        return qeaTestDataOpts;
    }

    public SelectItem[] getTsDataOpts() {
        return tsDataOpts;
    }

    public String getTestNMRpeakPath() {
        return realPath + test_nmrpeak;
    }

    public String getTestMSpeakPath2Col() {
        return realPath + test_mspeak2;
    }

    public String getTestMSpeakPath3Col() {
        return realPath + test_mspeak3;
    }

    public SelectItem[] getPairAnalOpts() {
        return pairAnalOpts;
    }

    public SelectItem[] getEqualVarOpts() {
        return equalVarOpts;
    }

    public String getContDataPath() {
        return realPath + test_cont_data;
    }

    public String getDisreteDataPath() {
        return realPath + test_discr_data;
    }

    public String getPathway_libPath() {
        return realPath + path_lib;
    }

    public String getMset_path() {
        return realPath + mset_dir;
    }

    public String getCmpdConcPath() {
        return realPath + cmpd_conc_data;
    }

    public String getKegg_path() {
        return realPath + kegg_dir;
    }

    public SelectItem[] getCsvOpts() {
        return csvOpts;
    }

    public SelectItem[] getZipOpts() {
        return zipOpts;
    }

    public SelectItem[] getCsvFormatOpts() {
        return csvFormatOpts;
    }
    
    public SelectItem[] getMetaFormatOpts() {
        return metaFormatOpts;
    }
        
    public SelectItem[] getTaxFormatOpts() {
        return taxFormatOpts;
    }
        
    public SelectItem[] getEnvFormatOpts() {
        return envFormatOpts;
    }
    
    public SelectItem[] getDataNamesOpts() {
        return dataNamesOpts;
    }
    
    public SelectItem[] getMetaNamesOpts() {
        return metaNamesOpts;
    }    
    
    public SelectItem[] getTaxNamesOpts() {
        return taxNamesOpts;
    } 
        
    public SelectItem[] getEnvNamesOpts() {
        return envNamesOpts;
    }
    
    public SelectItem[] getRocFormatOpts() {
        return rocFormatOpts;
    }

    public SelectItem[] getTsFormatOpts() {
        return tsFormatOpts;
    }

    public SelectItem[] getCmpdIDOpts() {
        return cmpdIDOpts;
    }

    public SelectItem[] getIntegOrgOpts() {
        return orgOpts;
    }

    public SelectItem[] getGeneIDOpts() {
        return geneIDOpts;
    }

    public SelectItem[] getTestDataOpts() {
        return testDataOpts;
    }
    
    public SelectItem[] getPlottingDataOpts() {
        return plottingDataOpts;
    }
    //Public Wegan functions NOT USED ATM
    //********----------------------------------------------------
    
        public SelectItem[] getNMDSTestDataOpts() {
        return testDataOpts;
    }
    
    
    //********----------------------------------------------------
    public SelectItem[] getCmpdIDOpts1() {
        return cmpdIDOpts1;
    }

    public SelectItem[] getClsOpts() {
        return clsOpts;
    }

    public SelectItem[] getBiofluidOpts() {
        return biofluidOpts;
    }

    public SelectItem[] getPathIDOpts() {
        return pathIDOpts;
    }

    public SelectItem[] getDesignOpts() {
        return designOpts;
    }

    public SelectItem[] getLoadPlotOpts() {
        return loadPlotOpts;
    }

    public String getTestORA() {
        return testORA;
    }

    public String getTestSSP() {
        return testSSP;
    }

    public SelectItem[] getDataNormOpts() {
        return dataNormOpts;
    }

    public SelectItem[] getTransNormOpts() {
        return transNormOpts;
    }

    public SelectItem[] getScaleNormOpts() {
        return scaleNormOpts;
    }
    
    public SelectItem[] getOrdStressDimensionOpts() {
        return ordStressDimensionOpts;
    }

//    public SelectItem[] getColumnOptsFill() {
//        return columnOptsFill;
//    } 
    
     public SelectItem[] getCorPlotNarrowWidthOpts() {
        return corPlotNarrowWidthOpts;
    } 
    
    public SelectItem[] getCorLinTextSizeOpts() {
        return corLinTextSizeOpts;
    }      
    
    public SelectItem[] getCorColorPaletteNonPredictors() {
        return corColorPaletteNonPredictors;
    }  
    
    public SelectItem[] getCorColorPalettePredictors() {
        return corColorPalettePredictors;
    }  
    
    public SelectItem[] getCorLinColorDotsOpts() {
        return corLinColorDotsOpts;
    }
    
    public SelectItem[] getCorLinColorLineOpts() {
        return corLinColorLineOpts;
    }
    
    public SelectItem[] getCorLegPosOpts() {
        return corLegPosOpts;
    }
    
    public SelectItem[] getCorColorPaletteOpts(){
        return corColorPaletteOpts;
    }
    
    public SelectItem[] getCorBrewerPaletteOpts(){
        return corBrewerPaletteOpts;
    }
    
    public SelectItem[] getCorModType() {
        return corModType;
    }
    
    public SelectItem[] corPlotMetricOpts() {
        return corPlotMetricOpts;
    }
    
    public SelectItem[] corPlotLabSize() {
        return corPlotLabSize;
    }
    
    public SelectItem[] corSizeTitle() {
        return corSizeTitle;
    }
    
    public SelectItem[] corSizeXlab() {
        return corSizeXlab;
    }

    public SelectItem[] corSizeYlab() {
        return corSizeYlab;
    }
    
    public SelectItem[] corSizeXtick() {
        return corSizeXtick;
    }
    
    public SelectItem[] corSizeYtick() {
        return corSizeYtick;
    }
    
    public SelectItem[] getOrdColorPaletteOpts() {
        return ordColorPaletteOpts;
    }
    
    public SelectItem[] getBoxPltColorPaletteOpts() {
        return boxPltColorPaletteOpts;
    }
    
            
    public SelectItem[] getPcaPairsColorPaletteOpts() {
        return pcaPairsColorPaletteOpts;
    }
      
            
    public SelectItem[] getProbNormOpts() {
        return probNormOpts;
    }

    public SelectItem[] getPosthocOpts() {
        return posthocOpts;
    }

    public SelectItem[] getDistMeasureOpts() {
        return distMeasureOpts;
    }

    public SelectItem[] getCorrMethodsOpts() {
        return corrMethodsOpts;
    }
    
    public SelectItem[] getVegdistMeasureOpts() {
        return vegdistMeasureOpts;
    }
    
    public SelectItem[] getDendroPalletteOpts() {
        return dendroPalletteOpts;
    }
    
    public SelectItem[] getHmSmplColorOpts() {
        return hmSmplColorOpts;
    }

    
    public SelectItem[] getBetadisperDataOpts() {
        return betadisperDataOpts;
    }
      

    public SelectItem[] getCiaTypeOpts() {
        return ciaTypeOpts;
    }
        
    public SelectItem[] getCiaDataSetOpts() {
        return ciaDataSetOpts;
    }
            
    public SelectItem[] getColorContrastOpts() {
        return colorContrastOpts;
    }

    public SelectItem[] getPtnOpts() {
        return ptnOpts;
    }

    public SelectItem[] getPlsCVOpts() {
        return plsCVOpts;
    }

    public SelectItem[] getPlsClsOpts() {
        return plsClsOpts;
    }

    public SelectItem[] getPlsImpOpts() {
        return plsImpOpts;
    }

    public SelectItem[] getClustDistOpts() {
        return clustDistOpts;
    }

    public SelectItem[] getClustMethodOpts() {
        return clustMethodOpts;
    }

    public SelectItem[] getPathLibOpts() {
        return pathLibOpts;
    }

    public SelectItem[] getMassLibOpts() {
        return massLibOpts;
    }

    public SelectItem[] getRefLibOpts() {
        return refLibOpts;
    }

    public String getDataSet1Path() {
        return realPath + "/data/data1.csv";
    }

    public String getDataSet2Path() {
        return realPath + "/data/data2.csv";
    }

    public String getDataSet3Path() {
        return realPath + "/data/data3.csv";
    }

    public String getDataSet4Path() {
        return realPath + "/data/data4.csv";
    }
}