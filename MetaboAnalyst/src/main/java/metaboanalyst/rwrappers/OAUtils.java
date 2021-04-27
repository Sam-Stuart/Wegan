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


public class OAUtils {

    public static void CreateNMDSOrdination(SessionBean1 sb, String distance, Boolean abundance, Boolean data, String metaData, String envData, String env_text) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.NMDS(NA" + ", \"" + distance + "\", \"" + abundance + "\", \"" + data + "\", metaData=NULL, envData=NULL, env_text=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotNMDS2DOrdination(SessionBean1 sb, Boolean ellipse, Boolean var_arrows, Boolean env_arrows, Boolean env_cent, Boolean sampleNames, Boolean point_options, String color, String meta_col_color, String meta_col_point, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.NMDS.2D(NA" + ", \"" + ellipse + "\", \"" + var_arrows + "\", \"" + env_arrows + "\", \"" + env_cent + "\", \"" + sampleNames + "\", \"" + point_options + "\", \"" + color + "\", meta_col_color=NULL, meta_col_point=NULL" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_nmds_2D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotNMDS3DOrdination(SessionBean1 sb, String color, String var_arrows, String meta_col_color, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.NMDS.3D(NA, color=NULL, var_arrows=NULL, meta_col_color=NULL" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_nmds_3D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotNMDSscreeOrdination(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.NMDS.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_nmds_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotNMDSstressOrdination(SessionBean1 sb, String k, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.NMDS.stress(NA, k=NULL" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_nmds_stress", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static String[] GetOrdinationNMDSResults(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.nmds.get.results(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(OAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    
    public static void CreatePCOAOrdination(SessionBean1 sb, String distance, Boolean abundance, Boolean data, Boolean binary, String metaData, String envData, String env_text) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.pcoa(NA" + ", \"" + distance + "\", \"" + abundance + "\", \"" + data + "\", \"" + binary + "\", metaData=NULL, envData=NULL, env_text=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotPCOA2DOrdination(SessionBean1 sb, Boolean ellipse, Boolean var_arrows, Boolean env_arrows, Boolean env_cent, Boolean sampleNames, Boolean point_options, String color, String meta_col_color, String meta_col_point, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.PCoA.2D(NA" + ", \"" + ellipse + "\", \"" + var_arrows + "\", \"" + env_arrows + "\", \"" + env_cent + "\", \"" + sampleNames + "\", \"" + point_options + "\", \"" + color + "\", meta_col_color=NULL, meta_col_point=NULL" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_pcoa_2D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotPCOA3DOrdination(SessionBean1 sb, String color, String var_arrows, String meta_col_color, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.PCoA.3D(NA, color=NULL, var_arrows=NULL, meta_col_color=NULL" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_pcoa_3D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotPCOAscreeOrdination(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.PCoA.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_pcoa_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPCOAstressOrdination(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.PCoA.stress(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_pcoa_stress", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static String[] GetOrdinationPCOAResults(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.pcoa.get.results(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(OAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    


    public static boolean CreateCIAOrdination(SessionBean1 sb, Boolean data, String type, String metaData, String envData, String env_text) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cia(NA" + ", \"" + data + "\", \"" + type + "\", metaData=NULL, envData=NULL, env_text=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
        
    public static void PlotCIAscreeOrdination(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.cia.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_cia_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotCIAloadingOrdination(SessionBean1 sb, String dataSet, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.cia.loading(NA" + ", \"" + dataSet + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_cia_loading", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotCIAscatterOrdination(SessionBean1 sb, String meta_group, String meta_colname, String color, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.cia.scatter(NA + meta_group + meta_colname" + ", \"" + color + "\", \""  + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_cia_scatter", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String[] GetOrdinationCIAResults(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cia.get.results(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(OAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
     
}
