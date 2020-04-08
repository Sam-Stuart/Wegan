/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author Jeff
 */
public class SigVarSelect {

    public static void InitSAM(SessionBean1 sb, String method, String paired, String equalVar) {
        try {
            //Note: paried and equalVar mimic T and F, should not quote
            String rCommand = "SAM.Anal(NA" + ", \"" + method + "\", " + paired + ", " + equalVar + ")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSAM_FDR(SessionBean1 sb, double delta, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotSAM.FDR(NA" + ", " + delta + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("sam_view", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //plot SAM image and set up sig.cmpd object so that SAM member page can use
    public static void PlotSAM_Cmpd(SessionBean1 sb, String imgName, String format, int dpi, double delta) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetSAMSigMat(NA" + ", " + delta + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);

            String rCommand2 = "PlotSAM.Cmpd(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            sb.addGraphicsCMD("sam_imp", rCommand2);
            RCenter.recordRCommand(RC, rCommand2);
            RC.voidEval(rCommand2);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String[] GetSAMSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMSigRowNames(NA)";
            String[] names = sb.getRConnection().eval(rCommand).asStrings();
            return names;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetSAMSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMSigColNames(NA)";
            String[] names = sb.getRConnection().eval(rCommand).asStrings();
            return names;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetSAMSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMSigMat(NA)";
            double[][] sigvals = sb.getRConnection().eval(rCommand).asDoubleMatrix();
            return sigvals;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double GetSAMSuggestedDelta(SessionBean1 sb) {
        try {
            String rCommand = "GetSuggestedSAMDelta(NA)";
            double delta = sb.getRConnection().eval(rCommand).asDouble();
            return delta;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static double[] getSAMDeltaRange(SessionBean1 sb) {
        try {
            String rCommand = "GetSAMDeltaRange(NA)";
            double[] deltas = sb.getRConnection().eval(rCommand).asDoubles();
            return deltas;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void InitEBAM_A0(SessionBean1 sb, String paired, String equalVar) {
        try {
            String rCommand = "EBAM.A0.Init(NA" + ", " + paired + ", " + equalVar + ")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotEBAM_A0(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotEBAM.A0(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ebam_view", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //plot EBAM images and set up sig.cmpd object so the member page can use 
    public static void InitEBAM_Cmpd(SessionBean1 sb, String method, double A0, String paired, String equalVar) {
        try {
            String rCommand = "EBAM.Cmpd.Init(NA" + ", \"" + method + "\", " + A0 + ", " + paired + ", " + equalVar + ")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //plot EBAM images and set up sig.cmpd object so the member page can use 
    public static void PlotEBAM_Cmpd(SessionBean1 sb, String imgName, String format, int dpi, double delta) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SetEBAMSigMat(NA" + ", " + delta + ");"; //also obtain the signifacnt compound information
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);

            String rCommand2 = "PlotEBAM.Cmpd(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand2);
            sb.addGraphicsCMD("ebam_imp", rCommand2);
            RC.voidEval(rCommand2);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double GetEBAMSuggestedA0(SessionBean1 sb) {
        try {
            String rCommand = "round(as.numeric(mSet$analSet$ebam.a0@suggested),4)";
            return sb.getRConnection().eval(rCommand).asDouble();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String[] GetEBAMSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetEBAMSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetEBAMSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetEBAMSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetEBAMSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetEBAMSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }
}
