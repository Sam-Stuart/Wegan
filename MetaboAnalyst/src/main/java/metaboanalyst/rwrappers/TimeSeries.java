/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author Jeff
 */
public class TimeSeries {

    public static void initIPCA(RConnection RC, String fileNm) {
        try {
            String rCommand = "iPCA.Anal(NA" + ", \"" + fileNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void prepareLiveGraphics(RConnection RC, String urlPath, int inx) {
        try {
            String rCommand = "SetLiveGraphics(\"" + urlPath + "\", " + inx + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotHeatMap2(SessionBean1 sb, String imgName, String format, int dpi, String smplDist, String clstDist, String colors, String viewOpt, int sortInx, String useSigFeature, String drawBorder) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotHeatMap2(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + smplDist + "\",\"" + clstDist + "\",\"" + colors + "\",\"" + viewOpt + "\", F, " + sortInx + ", " + useSigFeature + ", " + drawBorder + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("heatmap2", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int InitANOVA2(RConnection RC, double thresh, String pCorType, String type, int aovOpt, int useInteraction) {
        try {
            String rCommand = "ANOVA2.Anal(NA, " + thresh + ", \"" + pCorType + "\", \"" + type + "\", " + aovOpt + ", " + useInteraction +")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void PlotAOV2(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotANOVA2(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("aov2", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String GetAov2SigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetAov2SigFileName(NA)";
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String GetAscaSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetAscaSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //threshx is fc, threshy is pvalue
    public static double[][] GetAov2SigMat(RConnection RC) {
        try {
            String rCommand = "GetAov2SigMat(NA)";
            double[][] cmpds = RC.eval(rCommand).asDoubleMatrix();
            return cmpds;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetAov2SigRowNames(RConnection RC) {
        try {
            String rCommand = "GetAov2SigRowNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetAov2SigColNames(RConnection RC) {
        try {
            String rCommand = "GetAov2SigColNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void performASCA(RConnection RC, int a, int b, int x, int res) {
        try {
            String rCommand = "Perform.ASCA(NA" + ", " + a + ", " + b + ", " + x + ", " + res + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void performASCAVarSelection(RConnection RC, double speThresh, double lvThresh) {
        try {
            String rCommand = "CalculateImpVarCutoff(NA" + ", " + speThresh + ", " + lvThresh + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void performASCAPermutation(RConnection RC, int permNum) {
        try {
            String rCommand = "Perform.ASCA.permute(NA" + ", " + permNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAPermSummary(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotASCA.Permutation(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_perm", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAscree(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotModelScree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAModels(SessionBean1 sb, String imgName, String format, int dpi, String type, String colorBW) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotASCAModel(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\"," + colorBW + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_f" + type, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAInteraction(SessionBean1 sb, String imgName, String format, int dpi, String colorBW) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotInteraction(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + "," + colorBW + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_fab", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotASCAImpVar(SessionBean1 sb, String imgName, String format, int dpi, String type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotAscaImpVar(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("asca_imp" + type, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //threshx is fc, threshy is pvalue
    public static double[][] GetAscaSigMat(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetAscaSigMat(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetAscaSigRowNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetAscaSigRowNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetAscaSigColNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetAscaSigColNames(\"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int performMB(RConnection RC, int topPerc) {
        try {
            String rCommand = "performMB(NA" + ", " + topPerc + ")";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String plotMBProfile(SessionBean1 sb, String cmpdName, String format, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotMBTimeProfile(NA" + ", \"" + cmpdName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("mb", rCommand);
            String imgName = RC.eval(rCommand).asString();
            return imgName;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //threshx is fc, threshy is pvalue
    public static double[][] getMBSigMat(RConnection RC) {
        try {
            String rCommand = "GetMBSigMat(NA)";
            double[][] cmpds = RC.eval(rCommand).asDoubleMatrix();
            return cmpds;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMBSigRowNames(RConnection RC) {
        try {
            String rCommand = "GetMBSigRowNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMBSigColNames(RConnection RC) {
        try {
            String rCommand = "GetMBSigColNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
}
