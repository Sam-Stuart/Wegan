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
    
    
        public static boolean CreateRarefactionDiv(SessionBean1 sb, Boolean data, String type, String sample, Boolean se, String margin) {
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
    public static void PlotRarefactionCurveDiversity(SessionBean1 sb, String step, String color, String color_text, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "RarefactionCurve(NA" 
                                                 + ", \"" + step 
                                                 + "\", \""  + color 
                                                 + "\", \"" + color_text
                                                 + "\", \"" + imgName 
                                                 + "\", \"" + format 
                                                 + "\", " + dpi 
                                                 + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("div_Rarefact_curve", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    /////// ------------ Diversity helper functions --------------- //////////////

    public static void PlotRarefactionCurveDiversity(SessionBean1 sb, String currentImage, String png, int i) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
        
}