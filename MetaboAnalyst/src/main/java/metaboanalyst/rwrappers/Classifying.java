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
public class Classifying {

    public static void InitRF(SessionBean1 sb, int treeNum, int tryNum, int randomOn) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "RF.Anal(NA" + ", " + treeNum + "," + tryNum + "," + randomOn + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static void PlotRFClassication(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotRF.Classify(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("rf_cls", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static void PlotRFCmpd(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotRF.VIP(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("rf_imp", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static void PlotRFOutlier(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotRF.Outlier(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("rf_outlier", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static Double GetRFOOB(SessionBean1 sb) {
        try {
            String rCommand = "GetRFOOB(NA)";
            return sb.getRConnection().eval(rCommand).asDouble();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetRFConfRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetRFConfRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetRFConfColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetRFConfColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetRFConfusionMat(SessionBean1 sb) {
        try {
            String rCommand = "GetRFConfMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetRFSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetRFSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetRFSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetRFSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetRFSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetRFSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //-------------R-SVM-----------methods
    public static void InitSVMAnal(SessionBean1 sb, String cvType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand;
            if (cvType.equals("LOO") || cvType.equals("bootstrape")) { //10 fold validation
                rCommand = "RSVM.Anal(NA" + ", \"" + cvType + "\")";
            } else {
                rCommand = "RSVM.Anal(NA" + ", " + 10 + ")";
            }
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    //frequencies of being selected in the best classifier
    public static double[][] GetSVMSigMat(SessionBean1 sb) {
        try {
            //make into decreasing oder
            String rCommand = "GetSVMSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetSVMSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSVMSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetSVMSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSVMSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void PlotSVMClassification(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotRSVM.Classification(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("svm_cls", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSVMSigCmpds(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotRSVM.Cmpd(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("svm_imp", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }
}
