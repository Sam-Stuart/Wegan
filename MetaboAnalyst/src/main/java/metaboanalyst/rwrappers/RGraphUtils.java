/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Jeff
 */
public class RGraphUtils {

    public static String plotMetPAPath(SessionBean1 sb, String pathName, int width, int height) {
        try {
            RConnection RC = sb.getRConnection();
               sb.setCurrentPathName(pathName);
            String rCommand = "PlotMetPath(NA" + ", \"" + pathName + "\", " + width + ", " + height + ")";
            RCenter.recordRCommand(RC, rCommand);
            String res = RC.eval(rCommand).asString();
            return res;
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String plotKEGGPath(SessionBean1 sb, String pathName, String format, String width, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotKEGGPath(NA" + ", \"" + pathName + "\", \"" + format + "\", " + width + ", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            String name = RC.eval(rCommand).asString();
            return name;
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static void plotPathSummary(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPathSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("path_view", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static boolean drawMetPAGraph(RConnection RC, String imgName, double width, double height) {
        try {
            String rCommand = "RerenderMetPAGraph(NA" + ", \"" + imgName + "\"," + width + ", " + height + ")";
            RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            return (res == 1);
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static String getOverviewImgMap(RConnection RC) {
        try {
            String rCommand = "GetCircleInfo(NA)";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String getCurrentImg(RConnection RC) {
        try {
            return RC.eval("GetCurrentImg(NA)").asString();
        } catch (Exception rse) {
            rse.printStackTrace();
            return null;
        }
    }

    public static String getConvertFullPath(RConnection RC) {
        try {
            return RC.eval("GetConvertFullPath()").asString();
        } catch (Exception rse) {
            rse.printStackTrace();
            return null;
        }
    }
}
