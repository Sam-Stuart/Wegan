/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import java.util.logging.Level;
import java.util.logging.Logger;
import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
import org.rosuda.REngine.REXPMismatchException;


/**
 *
 * @author Jeff
 */
public class Clustering {

    //private static Object Logger;

    public static void PlotClustTree(SessionBean1 sb, String imgName, String format, int dpi, String smplDist, String clstDist) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotHCTree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + smplDist + "\", \"" + clstDist + "\")";
            sb.addGraphicsCMD("tree", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String dataOpt, String scaleOpt, String smplDist, String clstDist, String colors, String viewOpt, String rowV, String colV, String drawBorder, String grpAve) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotHeatMap(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + dataOpt+ "\", \""+ scaleOpt+ "\", \""+ smplDist + "\", \"" + clstDist + "\",\"" + colors + "\", \""  + viewOpt + "\", " +rowV + ", " + colV + ", NA, " + drawBorder + ", " + grpAve +")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("heatmap", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSubHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String dataOpt, String scaleOpt, String smplDist,
            String clstDist, String colors, String method, int num, String viewOpt, String rowV, String colV, String drawBorder, String grpAve) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSubHeatMap(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + dataOpt + "\", \""+ scaleOpt + "\", \"" + smplDist + "\", \"" + clstDist + "\",\"" + colors + "\", \"" + method + "\", " + num + ", \"" + viewOpt +"\", " + rowV + ", " + colV + ", " + drawBorder + ", " + grpAve + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("heatmap", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotKmeans(SessionBean1 sb, String imgName, String format, int dpi, int clustNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Kmeans.Anal(NA" + ", " + clustNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);

            String rCommand2 = "PlotKmeans(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand2);
            sb.addGraphicsCMD("km", rCommand2);
            RC.voidEval(rCommand2);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int GetKMClusterNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("length(mSet$analSet$kmeans$size)").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String GetKMClusterMembers(SessionBean1 sb, int clNo) {
        try {
            String rCommand = "GetKMClusterMembers(NA" + ", " + clNo + ")";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String getClassLabel(SessionBean1 sb, int inx) {
        try {
            String rCommand = "GetClassLabel(NA" + ", " + inx + ")";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void PlotSOM(SessionBean1 sb, String imgName, String format, int dpi, int clx, int cly, String init, String nb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SOM.Anal(NA" + ", " + clx + "," + cly + ",\"" + init + "\"," + "\"" + nb + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);

            String rCommand2 = "PlotSOM(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand2);
            sb.addGraphicsCMD("som", rCommand2);
            RC.voidEval(rCommand2);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int GetSOMXdimension(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("mSet$analSet$som$xdim").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int GetSOMYdimension(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("mSet$analSet$som$ydim").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String GetSOMClusterMembers(SessionBean1 sb, int xord, int yord) {
        try {
            String rCommand = "GetSOMClusterMembers(NA" + ", " + xord + "," + yord + ")";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
    
    public static void CreateSpatialvis(SessionBean1 sb, boolean data, boolean datum, boolean proj, String crs_txt, String crs_option, String source, String maptype, String zoom, String varColName, 
            String rangeA, String colorColName, boolean ele, String lineB, boolean polygon, boolean path, String imgName, String format, int dpi, String width) {
        try {
            System.out.print("UD");
            RConnection RC = sb.getRConnection(); //Start R connection
            String rCommand = "Raster_data(NA"
                                                 + ", \"" + data
                                                 + "\", \"" + datum
                                                 + "\", \"" + proj
                                                 + "\", \"" + crs_txt
                                                 + "\", \"" + crs_option
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
    
    public static String[] varColumn(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "VarCol(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(Clustering.class.getName()).log(Level.SEVERE, null, ex);
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
            Logger.getLogger(Clustering.class.getName()).log(Level.SEVERE, null, ex);
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
            Logger.getLogger(Clustering.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    
}
