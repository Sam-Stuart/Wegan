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

    public static void CreateNMDSOrdination(SessionBean1 sb, Boolean data, String distance, String abundance, String env_text) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.NMDS(NA" + ", \"" + data + "\"distance=NULL, abundance=NULL, env_text=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotNMDS2DOrdination(SessionBean1 sb, Boolean ellipse, Boolean var_arrows, Boolean env_arrows, Boolean env_cent, Boolean sampleNames, Boolean point_options, String color, String meta_col_color, String meta_col_point, String imgName, String format, int dpi) {
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
        
    public static void PlotNMDS3DOrdination(SessionBean1 sb, String color, String var_arrows, String meta_col_color, String imgName, String format, int dpi) {
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
    
    public static void PlotNMDSscreeOrdination(SessionBean1 sb, String imgName, String format, int dpi) {
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

    public static void PlotNMDSstressOrdination(SessionBean1 sb, String k, String imgName, String format, int dpi) {
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
        
    public static void PlotPCOA2DOrdination(SessionBean1 sb, Boolean ellipse, Boolean var_arrows, Boolean env_arrows, Boolean env_cent, Boolean sampleNames, Boolean point_options, String color, String meta_col_color, String meta_col_point, String imgName, String format, int dpi) {
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
        
    public static void PlotPCOA3DOrdination(SessionBean1 sb, String color, String var_arrows, String meta_col_color, String imgName, String format, int dpi) {
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
    
    public static void PlotPCOAscreeOrdination(SessionBean1 sb, String imgName, String format, int dpi) {
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

    public static void PlotPCOAstressOrdination(SessionBean1 sb, String imgName, String format, int dpi) {
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

    


    public static void CreateCIAOrdination(SessionBean1 sb, String type, String env_text, Boolean data) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cia(NA" + ", \"" + type + "\", \"" + env_text + "\", data=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }


    public static void PlotCIAloadingOrdination(SessionBean1 sb, String dataSet, String imgName, String format, int dpi) {
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
    
    public static void PlotCIAscatterOrdination(SessionBean1 sb, Boolean meta_group, String meta_colname, String color, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.cia.scatter(NA, meta_group=NULL" + ", \"" + meta_colname + "\", \"" + color + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_cia_scatter", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotCIAscreeOrdination(SessionBean1 sb, String imgName, String format, int dpi) {
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

    public static void CreateRDA(SessionBean1 sb, Boolean abundance, String envText, Boolean data) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.rda(NA" 
                                        + ", \"" + abundance 
                                        + "\", \"" + envText 
                                        + "\" , \"" + data 
                                        + "\")";
//            String rCommand = "ord.rda(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    
    public static void PlotRDA2D(SessionBean1 sb, String color, Boolean varArrows, Boolean envArrows, Boolean envCent, Boolean sampleNames, String metaColColor, Boolean pointOptions, String metaColPoint, Boolean ellipse, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.RDA.2D(NA" 
                                            + ", \"" + color 
                                            + "\", \"" + varArrows
                                            + "\", \"" + envArrows
                                            + "\", \"" + envCent
                                            + "\", \"" + sampleNames
                                            + "\", \"" + metaColColor
                                            + "\", \"" + pointOptions
                                            + "\", \"" + metaColPoint
                                            + "\", \"" + ellipse
                                            + "\", \"" + imgName 
                                            + "\", \"" + format  
                                            + "\", " + dpi 
                                            + ", width=NA)";
//            String rCommand = "Plot.RDA.2D(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_rda_2D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotRDAScree(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.RDA.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";;
//            String rCommand = "Plot.RDA.2D(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_rda_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void CreateBray(SessionBean1 sb, Boolean abundance, String distance, Boolean data, Boolean binary) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.bray(NA" 
                                            + ", \"" + abundance 
                                            + "\", \"" + distance
                                            + "\", \"" + data
                                            + "\", \"" + binary
                                            + "\")";
//            String rCommand = "ord.rda(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotBray2D(SessionBean1 sb, String color, Boolean ellipse, Boolean varArrows, Boolean sampleNames, String metaColColor, String pointOptions, String metaColPoint, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.bray.2D(NA" 
                                            + ", \"" + color 
                                            + "\", \"" + ellipse
                                            + "\", \"" + varArrows
                                            + "\", \"" + sampleNames
                                            + "\", \"" + metaColColor
                                            + "\", \"" + pointOptions
                                            + "\", \"" + metaColPoint
                                            + "\", \"" + imgName 
                                            + "\", \"" + format  
                                            + "\", " + dpi 
                                            + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_bray_2D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static void PlotBray3D(SessionBean1 sb, String imgName, String format) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.bray.3D(NA, NULL, NULL, NULL" + ", \"" + imgName + "\", \"" + format + "\")";
//            String rCommand = "Plot.RDA.2D(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("bray_score3d", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotBrayScree(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.bray.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            String rCommand = "Plot.RDA.2D(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_bray_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static boolean CreateCCA(SessionBean1 sb, Boolean abundance, Boolean data, String env_text) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.cca(NA, NA"
                                        + ", \"" + abundance 
                                        + "\", \"" + data
                                        + "\", \"" + env_text + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
     
    public static void PlotCCA(SessionBean1 sb, String color, Boolean ellipse, Boolean varArrows, Boolean envArrows, Boolean envCent, Boolean sampleNames, String metaColColor, String pointOptions, String metaColPoint, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.CCA.biplot(NA" 
                                            + ", \"" + color 
                                            + "\", \"" + ellipse
                                            + "\", \"" + varArrows
                                            + "\", \"" + envArrows
                                            + "\", \"" + envCent
                                            + "\", \"" + sampleNames
                                            + "\", \"" + metaColColor
                                            + "\", \"" + pointOptions
                                            + "\", \"" + metaColPoint
                                            + "\", \"" + imgName 
                                            + "\", \"" + format  
                                            + "\", " + dpi 
                                            + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("ord_bray_2D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }  
    
    public static void PlotCcaScree(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.cca.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            String rCommand = "Plot.RDA.2D(NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_cca_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static boolean CreateCA(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.ca(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }

    public static void PlotCA2D(SessionBean1 sb, String color, Boolean varArrows, Boolean pointOptions, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.ca.2D(NA" 
                                            + ", \"" + color 
                                            + "\", \"" + varArrows
                                            + "\", \"" + pointOptions
                                            + "\", \"" + imgName 
                                            + "\", \"" + format  
                                            + "\", " + dpi 
                                            + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_ca_2D", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotCAScree(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.ca.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_ca_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static boolean CreateDCA(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ord.dca(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }

    public static void PlotDCA2D(SessionBean1 sb, String color, Boolean ellipse, Boolean varArrows, Boolean envArrows, Boolean sampleNames, Boolean envCent, String metaColColor, String pointOptions, String metaColPoint, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.DCA.2D(NA" 
                                            + ", \"" + color 
                                            + "\", \"" + ellipse
                                            + "\", \"" + varArrows
                                            + "\", \"" + envArrows
                                            + "\", \"" + envCent
                                            + "\", \"" + sampleNames
                                            + "\", \"" + metaColColor
                                            + "\", \"" + pointOptions
                                            + "\", \"" + metaColPoint
                                            + "\", \"" + imgName 
                                            + "\", \"" + format  
                                            + "\", " + dpi 
                                            + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_dca_2d", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotDCAScree(SessionBean1 sb, String imgName, String format, int dpi, String width) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.DCA.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_dca_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    /////// ------------ Ordination helper functions --------------- //////////////
    
    public static String[] ciaGetMetaColumns(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "cia.meta.columns(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(OAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
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
        
}