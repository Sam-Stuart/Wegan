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
        
    public static boolean CreateAlpha(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cca(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
     
    public static void PlotAlpha(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.CCA.biplot(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("div_alpha", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static boolean CreateBeta(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cca(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
     
    public static void PlotBeta(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.CCA.biplot(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("div_alpha", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static boolean CreateGamma(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cca(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
     
    public static void PlotGamma(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.CCA.biplot(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("div_alpha", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static boolean CreateSpecies(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cca(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
     
    public static void PlotSpecies(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.CCA.biplot(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("div_alpha", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    
    public static boolean CreateTestTutorial(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "lin.reg.anal.one(NA)"; // Creates R command
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static void PlotTestTutorial(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
//            String rCommand = "plot.linReg1(NA)";
            String rCommand = "plot.linReg1(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("div_alpha", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    public static boolean CreateRarefactionDiv(SessionBean1 sb, boolean data, String type, String sample, boolean se, String margin) {
        try {
            System.out.print("R RAREFACTION");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "Rarefaction_div(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + type
                                                 + "\", \"" + sample
                                                 + "\", \""  + se
                                                 + "\", \""  + margin                                                 
                                                 + "\")";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }	//mSet<-Rarefaction_div(mSet, "NULL, ", "false", "NULL" , "false")
    
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

    public static boolean CreateAbundDistDiv(SessionBean1 sb, boolean data, String community, boolean tiesplit, String truncate) {
        try {
            System.out.print("R Abundancedist");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "AbundanceModel(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + community
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
    
    public static boolean PlotTaxaTree(SessionBean1 sb, String color, String imgName, String format, int dpi, String width) {
        try {
            System.out.print("Taxo");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "taxa_tree(NA"
                                                 + ", \"" + color
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static boolean PlotTaxonScatter(SessionBean1 sb, String colorc, String imgName, String format, int dpi, String width) {
        try {
            System.out.print("Taxo");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "taxon_scatter(NA"
                                                 + ", \"" + colorc
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static boolean PlotTaxonHeatmap(SessionBean1 sb, String colord, String imgName, String format, int dpi, String width) {
        try {
            System.out.print("Taxo");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "taxon_heatmap(NA"
                                                 + ", \"" + colord
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand); // records r command
            RC.voidEval(rCommand); // tells you want your r script returns  
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
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