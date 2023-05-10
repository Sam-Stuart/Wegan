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


public class TaxonomyUtils {


// make.exprSet <- function(mSetObj = NA, 
//                          custom_norm = "false", # Check box 
//                          nSets = 1) # Static drop-down 
   
// plot.geneNetwork <- function(mSetObj = NULL, 
//                              custom_norm = NULL, 
//                              power = 6, # Static drop-down
//                              imgName,   
//                              format = "png", 
//                              dpi = 72, 
//                              width = NULL, 
//                              method = "average", # Static drop-down 
//                              nSelect = 400) # Static drop-down 
    
    public static void MakeWGCNAData(
            SessionBean1 sb, 
            Boolean custom_norm, 
            int nSets) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "make.exprSet(NA"
                                        + ", \"" + custom_norm 
                                        + "\", \"" + nSets
                                        + "\")";
            RCenter.recordRCommand(RC, rCommand);

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotWGCNAGeneNetwork(
            SessionBean1 sb, 
            Boolean custom_norm, 
            int power, 
            String imgName, 
            String format, 
            int dpi, 
            String method, 
            int nSelect,
            int width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.geneNetwork(NA"                                            
                                        + ", \"" + custom_norm 
                                        + "\", \"" + power 
                                        + "\", \"" + imgName 
                                        + "\", \"" + format
                                        + "\", \"" + dpi
                                        + "\", \"" + method
                                        + "\", \"" + nSelect 
                                        + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_nmds_2D", // What is plot name?
                              rCommand);

            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

  
}

