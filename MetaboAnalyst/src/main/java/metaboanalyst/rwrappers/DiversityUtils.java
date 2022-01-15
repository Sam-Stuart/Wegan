/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import java.util.logging.Level;
import java.util.logging.Logger;
import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;


public class DiversityUtils {
        
//    public static boolean CreateAlpha(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "ord.cca(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            RC.voidEval(rCommand);
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }
//     
//    public static void PlotAlpha(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "Plot.CCA.biplot(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("div_alpha", rCommand);
//            RC.voidEval(rCommand);
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        }
//    }
//
//    public static boolean CreateBeta(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "ord.cca(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            RC.voidEval(rCommand);
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }
//     
//    public static void PlotBeta(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "Plot.CCA.biplot(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("div_alpha", rCommand);
//            RC.voidEval(rCommand);
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        }
//    }
//    public static boolean CreateGamma(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "ord.cca(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            RC.voidEval(rCommand);
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }
//     
//    public static void PlotGamma(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "Plot.CCA.biplot(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("div_alpha", rCommand);
//            RC.voidEval(rCommand);
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        }
//    }
//    public static boolean CreateSpecies(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "ord.cca(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            RC.voidEval(rCommand);
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }
//     
//    public static void PlotSpecies(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "Plot.CCA.biplot(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("div_alpha", rCommand);
//            RC.voidEval(rCommand);
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        }
//    }
//    
//    
//    
//    public static boolean CreateTestTutorial(SessionBean1 sb) {
//        try {
//            RConnection RC = sb.getRConnection(); //Start R connection
//            String rCommand = "lin.reg.anal.one(NA)"; // Creates R command
//            RCenter.recordRCommand(RC, rCommand); // records r command
//            RC.voidEval(rCommand); // tells you want your r script returns  
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }
//    
//    public static void PlotTestTutorial(SessionBean1 sb, String imgName, String format, int dpi) {
//        try {
//            RConnection RC = sb.getRConnection();
////            String rCommand = "plot.linReg1(NA)";
//            String rCommand = "plot.linReg1(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("div_alpha", rCommand);
//            RC.voidEval(rCommand);
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        }
//    }
    
    
    public static boolean CreateIndicesDiv(SessionBean1 sb, boolean data, String groupColName) {
        try {
            System.out.print("R RAREFACTION");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "div_index(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + groupColName                                                
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }	
    
    public static void PlotAlphaDiversity(SessionBean1 sb, String color, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "AlphaPlot(NA" 
                                                 + ", \""  + color
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Alpha_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotBetaDiversity(SessionBean1 sb, String colorb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "BetaPlot(NA" 
                                                 + ", \""  + colorb
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Beta_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    public static boolean CreateRarefactionDiv(SessionBean1 sb, boolean data, String type, String sample, boolean se) {
        try {
            System.out.print("R RAREFACTION");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "Rarefaction_div(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + type
                                                 + "\", \"" + sample
                                                 + "\", \""  + se                                            
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }	//mSet<-Rarefaction_div(mSet, "NULL, ", "false", "NULL" , "false")
    
//    public static boolean PlotRarefactionCurveDiversity(SessionBean1 sb, String step, String color, String imgName, String format, int dpi, String width) {
//        try {
//            System.out.print("R RAREFACTION");
//            RConnection RC = sb.getRConnection(); //Start R connection
//            String rCommand = "RarefactionCurve(NA" 
//                                                 + ", \"" + step
//                                                 + "\", \""  + color
//                                                 + "\", \"" + imgName 
//                                                 + "\", \"" + format 
//                                                 + "\", " + dpi 
//                                                 + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand); // records r command
//            RC.voidEval(rCommand); // tells you want your r script returns  
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }	
    
    public static void PlotRarefactionCurveDiversity(SessionBean1 sb, String step, String color, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "RarefactionCurve(NA" 
                                                 + ", \"" + step
                                                 + "\", \""  + color
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Rarefaction_Curve_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotRarefactionPlotDiversity(SessionBean1 sb, String colorb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "RarefactionPlot(NA" 
                                                 + ", \""  + colorb 
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Rarefaction_Linear_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static boolean CreateAbundFisherDistDiv(SessionBean1 sb, boolean data, String communityFisher) {
        try {
            System.out.print("R Abundancedist");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "AbundanceFisherModel(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + communityFisher                                         
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static boolean CreateAbundPresDistDiv(SessionBean1 sb, boolean data, String communityPres, boolean tiesplit, String truncate) {
        try {
            System.out.print("R Abundancedist");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "AbundancePrestonModel(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + communityPres
                                                 + "\", \"" + tiesplit
                                                 + "\", \"" + truncate                                           
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static boolean CreateAbundRankDistDiv(SessionBean1 sb, boolean data, String communityRank) {
        try {
            System.out.print("R Abundancedist");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "AbundanceRankModel(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + communityRank                                        
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static void PlotAbundFisherPlotDiv(SessionBean1 sb, String bar_color, String line_color_addFit, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "AbundanceFisherPlot(NA" 
                                                 + ", \""  + bar_color 
                                                 + "\", \"" + line_color_addFit
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Abundance_Fisher_Dist_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotAbundPrestPlotDiv(SessionBean1 sb, String bar_color, String line_color_addPoi, String line_color_addMax, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "AbundancePrestPlot(NA" 
                                                 + ", \""  + bar_color 
                                                 + "\", \"" + line_color_addPoi
                                                 + "\", \"" + line_color_addMax
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Abundance_Prest_Dist_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotAbundRankPlotDiv(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "AbundanceRankPlot(NA" 
                                                 + ", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Abundance_Rank_Dist_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    public static boolean CreateAccumModelDiv(SessionBean1 sb, boolean data, String permutations, boolean conditioned, String gamma,
                                              String models, String object, String interval) {
        try {
            System.out.print("R AccumulationModel");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "AccumulationModel(NA" 
                                                 + ", \"" + data
                                                 + "\", \"" + permutations
                                                 + "\", \"" + conditioned
                                                 + "\", \"" + gamma
                                                 + "\", \"" + models
                                                 + "\", \"" + object
                                                 + "\", \"" + interval
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    
    public static void PlotAccumCurveDiv(SessionBean1 sb, String type, String color, String ci_color, String ci_type, String box_color,
                            String line_color, String pred_color, String pch, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "AccumCurve(NA" 
                                                 + ", \""  + type 
                                                 + "\", \"" + color
                                                 + "\", \"" + ci_color
                                                 + "\", \"" + ci_type
                                                 + "\", \"" + box_color
                                                 + "\", \"" + line_color
                                                 + "\", \"" + pred_color
                                                 + "\", \"" + pch
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Species_Accumulation_Model", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    public static boolean CreateTaxoDiv(SessionBean1 sb, boolean data, String dis, boolean match_force, boolean varstep, String aggme, boolean check) {
        try {
            System.out.print("Taxo");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "Taxonomic_div(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + dis
                                                 + "\", \"" + match_force
                                                 + "\", \"" + varstep
                                                 + "\", \"" + aggme
                                                 + "\", \"" + check
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
        
    
    public static void PlotTaxaTree(SessionBean1 sb, String color, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "taxa_tree(NA"
                                                 + ", \"" + color
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Taxa_Tree_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    
    public static void PlotTaxonScatter(SessionBean1 sb, String plotD, String colorc, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "taxon_scatter(NA"
                                                 + ", \"" + plotD
                                                 + "\", \"" + colorc
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Taxa_Scatter_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }       
    
    
    public static void PlotTaxonHeatmap(SessionBean1 sb, String colord, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "taxon_heatmap(NA"
                                                 + ", \"" + colord
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Taxa_Heatmap_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }       

        public static boolean CreateFdDiv(SessionBean1 sb, boolean data, String w_text, String corr, boolean w_abun, boolean stand_x, String m,  boolean stand_FRic, boolean print_pco, boolean messages, String asym_bin, String ord) {
        try {
            System.out.print("FD");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "functionalDiv(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + w_text
                                                 + "\", \"" + corr
                                                 + "\", \"" + w_abun
                                                 + "\", \"" + stand_x
                                                 + "\", \"" + m
//                                                 + "\", \"" + calc_FDiv
//                                                 + "\", \"" + calc_FRic
                                                 + "\", \"" + stand_FRic
//                                                 + "\", \"" + calc_CWM
                                                 + "\", \"" + print_pco
                                                 + "\", \"" + messages
                                                 + "\", \"" + asym_bin
                                                 + "\", \"" + ord
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static void PlotFdTree(SessionBean1 sb, String color, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FD_cluster_plot(NA"
                                                 + ", \"" + color
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("Cluster_Plot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
           
    
    public static boolean CreateUnseenDiv(SessionBean1 sb, boolean data, String poolColName, boolean smallsample, String index, String permutations, String minsize,  String parallel) {
        try {
            System.out.print("UD");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "sp_pool(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + poolColName
                                                 + "\", \"" + smallsample
                                                 + "\", \"" + index
                                                 + "\", \"" + permutations
                                                 + "\", \"" + minsize
                                                 + "\", \"" + parallel
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    
    public static void PlotPoolBoxplot(SessionBean1 sb, String plotdata, String box_color, String border_col, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "pool_boxplot(NA"
                                                 + ", \"" + plotdata
                                                 + "\", \"" + box_color
                                                 + "\", \"" + border_col
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("boxplot_richness", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    
    public static void PlotUnseenCurve(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "RichEstCurve(NA" 
                                                 + ", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("plot_matrices", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    public static void CreateSpatialvis(SessionBean1 sb, boolean data, String source, String maptype, String zoom, String varColName, 
            String rangeA, String colorColName, boolean ele, String lineB, boolean polygon, boolean path, String imgName, String format, int dpi, String width) {
        try {
            System.out.print("UD");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "Raster_data(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + source
                                                 + "\", \"" + maptype
                                                 + "\", \"" + zoom
                                                 + "\", \"" + varColName
                                                 + "\", \"" + rangeA
                                                 + "\", \"" + colorColName
                                                 + "\", \"" + ele
                                                 + "\", \"" + lineB
                                                 + "\", \"" + polygon
                                                 + "\", \"" + path
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ggmap", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
//    public static void PlotSpatialmap(SessionBean1 sb, boolean polygon, boolean path, String imgName, String format, int dpi, String width) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "Raster_map(NA"
//                                                 + "\", \"" + polygon
//                                                 + "\", \"" + path
//                                                 + "\", \"" + imgName 
//                                                 + "\", \"" + format 
//                                                 + "\", " + dpi 
//                                                 + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("ggmap", rCommand);
//            RC.voidEval(rCommand);
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        }
//    }
    
    
    public static String[] IndiceColumn(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "IndiceCol(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(DiversityUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    
    public static String[] poolColumn(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PoolCol(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(DiversityUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
  
    public static String[] varColumn(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "VarCol(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(DiversityUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] colorColumn(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ColorCol(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(DiversityUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    

    public static String[] lineColumn(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "LineCol(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(DiversityUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    /////// ------------ Diversity helper functions --------------- //////////////

    public static void PlotRarefactionCurveDiversity(SessionBean1 sb, String currentImage, String png, int i) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
//    public static void PlotRarefactionPlotDiversity(SessionBean1 sb, String currentImage, String png, int i) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }
    
//    public static void PlotPlotAbundFisherPlotDiv(SessionBean1 sb, String currentImage, String png, int i) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }
//    
//    public static void PlotAbundPrestPlotDiv(SessionBean1 sb, String currentImage, String png, int i) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }
//    
//    public static void PlotAccumCurveDiv(SessionBean1 sb, String currentImage, String png, int i) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }
        
}