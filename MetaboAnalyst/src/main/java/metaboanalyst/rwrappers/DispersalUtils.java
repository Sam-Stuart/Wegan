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
/**
 *
 * @author Leif Wilm
 */
public class DispersalUtils {

    public static void InitBGD(SessionBean1 sb) {
        System.out.print(sb);
        try {
            String rCommand = "bgdispersalWegan(NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
      
    public static void PlotBGD(SessionBean1 sb, String imageName, String format, int dpi, int pcNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotBGD(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + ")";
            System.out.print(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("bgd", rCommand);
            System.out.print(imageName);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    
//    ####### Beals Smoothing functions 
            
    public static void CreateBeals(SessionBean1 sb, String species, String ref, int type, String incld) {
        try {
            String rCommand = "bealsWegan(NA" + ", \"" + species + "\", \"" + ref + "\", \"" + type + "\",\"" + incld + "\")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotBeals(SessionBean1 sb, String imageName, String format, int dpi, int pcNum, String species) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotBeals(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + " ,\""+ species + "\")";
            
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("beals", rCommand);
            
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static void LoadDplyr(SessionBean1 sb) {
        try {
            String rCommand = "load_dplyr()";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        try {
            String rCommand = "load_vegan()";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
   // ### Beta Dispersal Functions #####
    public static boolean CreateBetaDisper(SessionBean1 sb, String groups, String labels, String dataOpts) {
        try {
            String rCommand = "betadisperWegan(NA" + ", \"" + groups + "\", \"" + labels + "\", \"" + dataOpts + "\")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static void PlotBetaDisper(SessionBean1 sb, String imageName, String format, int dpi, int pcNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotBetaDisper(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + ")";
            System.out.print(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("betadisper", rCommand);
            System.out.print(imageName);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static String[] GetDataColumns(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "disp.reg.columns(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(DispersalUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
        

}
