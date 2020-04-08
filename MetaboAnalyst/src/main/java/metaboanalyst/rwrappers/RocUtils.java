/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author jianguox
 */
public class RocUtils {

    public static String[] getModelNames(RConnection RC) {
        try {
            String rCommand = "GetModelNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getSample1Names(RConnection RC) {
        try {
            String[] names = RC.eval("rownames(mSet$dataSet$norm)[mSet$dataSet$cls==levels(mSet$dataSet$cls)[1]]").asStrings();
            return names;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getSample2Names(RConnection RC) {
        try {
            String[] names = RC.eval("rownames(mSet$dataSet$norm)[mSet$dataSet$cls==levels(mSet$dataSet$cls)[2]]").asStrings();
            return names;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void PerformRocCVExplorer(RConnection RC, String cls, String rkOpt, int lvNum) {
        try {
            String rCommand = "PerformCV.explore(NA" + ", \"" + cls + "\", \"" + rkOpt + "\", " + lvNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PerformRocCVTester(RConnection RC, String cls, int lvNum) {
        try {
            String rCommand = "PerformCV.test(NA" + ", \"" + cls + "\", " + lvNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PerformRocPred1(RConnection RC) {
        try {
            String rCommand = "Perform.Prediction()";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PerformUnivROC(SessionBean1 sb, String featNm, String imgName, String format, int dpi, String isAUC, String isOpt, String optMethod, String isPartial, String measure, double cutoff) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Perform.UnivROC(NA" + ", \"" + featNm + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + isAUC + ", " + isOpt + ", \"" + optMethod + "\", " + isPartial + ", \"" + measure + "\", " + cutoff + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(featNm, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPredView(RConnection RC, String imgName, String format, int dpi) {
        try {
            String pdfCommand = "PlotPredView()";
            RCenter.recordRCommand(RC, pdfCommand);
            String webCommand = "PNG.PlotPredView(\"" + imgName + "\")";
            RC.voidEval(webCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotProbView(SessionBean1 sb, String imgName, String format, int dpi, int mdlInx, int showNm, int showPred) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotProbView(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + mdlInx + ", " + showNm + ", " + showPred + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("cls_prob", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotROC(SessionBean1 sb, String imgName, String format, int dpi, int mdlInx, String method, int showConf, int showHoldOut, String focus, double cutoff) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotROC(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + mdlInx + ", \"" + method + "\", " + showConf + ", " + showHoldOut + ", \"" + focus + "\", " + cutoff + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_roc", rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotROCLR(SessionBean1 sb, String imgName, String format, int dpi, int showConf) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotROC.LRmodel(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + showConf + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_roc_lr", rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int getBestModelInx(RConnection RC) {
        try {
            String rCommand = "GetBestModelIndex(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static String getModelInfo(RConnection RC, int mdlInx, String usrName) {
        try {
            String rCommand = "GetModelInfo(" + mdlInx + ", \"" + usrName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String getAccuSummary(RConnection RC) {
        try {
            String rCommand = "GetAccuracyInfo(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static void PlotImpVar(SessionBean1 sb, String imgName, String format, int dpi, int modelInx, String measure, int featNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotImpVars(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + modelInx + ", \"" + measure + "\", " + featNum + ");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_imp", rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String getConfusionMatrix(RConnection RC) {
        try {
            return RC.eval("GetCurrentConfMat(NA)").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getConfusionMatrixTest(RConnection RC) {
        try {
            return RC.eval("GetCurrentConfMatTest(NA)").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void PlotAccuracies(SessionBean1 sb, String imageName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotAccuracy(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_accu", rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotTestAccuracies(SessionBean1 sb, String imageName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotTestAccuracy(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("cls_test_accu", rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PerformPermut(RConnection RC, String measure, int num) {
        try {
            String rCommand = "Perform.Permut(NA" + ", \"" + measure + "\", " + num + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPermut(SessionBean1 sb, String imageName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.Permutation(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            sb.addGraphicsCMD("roc_perm", rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double[][] GetImpValues(RConnection RC) {
        try {

            String rCommand = "GetImpValues(NA)";
            double[][] loadings = RC.eval(rCommand).asDoubleMatrix();
            return loadings;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetImpRowName(RConnection RC) {
        try {
            String rCommand = "GetImpRowNames(NA)";
            String[] rowNames = RC.eval(rCommand).asStrings();
            return rowNames;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetImpHighLow(RConnection RC, int inx) {
        try {
            String rCommand = "GetImpHighLow(NA" + ", " + inx + ")";
            String[] rowNames = RC.eval(rCommand).asStrings();
            return rowNames;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetImpColName(RConnection RC) {
        try {
            String rCommand = "GetImpColNames(NA)";
            String[] colNames = RC.eval(rCommand).asStrings();
            return colNames;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void setCustomData(RConnection RC) {
        try {
            String rCommand = "SetCustomData(NA, selected.cmpds, selected.smpls)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static double[][] GetRocValues(RConnection RC) {
        try {

            String rCommand = "as.matrix(mSet$analSet$roc.mat)";
            double[][] loadings = RC.eval(rCommand).asDoubleMatrix();
            return loadings;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetRocColName(RConnection RC) {
        try {
            String rCommand = "colnames(mSet$analSet$roc.mat)";
            String[] colNames = RC.eval(rCommand).asStrings();
            return colNames;
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetROCcoords(RConnection RC, String queryFld, double value, String plot, String imgNm) {
        try {
            String rCommand = "GetROC.coords(NA" + ", \"" + queryFld + "\", " + value + ", plot=" + plot +", \"" + imgNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
        
    public static double[][] getUnivFeatureRankingMat(RConnection RC) {
        try {
            String rCommand = "GetFeatureRankingMat()";
            double[][] mat = RC.eval(rCommand).asDoubleMatrix();
            return mat;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getUnivRankedFeatureNames(RConnection RC) {
        try {
            String rCommand = "GetUnivRankedFeatureNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getLassoFreqs(RConnection RC) {
        try {
            String rCommand = "GetLassoFreqs()";
            return RC.eval(rCommand).asDoubles();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getLassoFreqNames(RConnection RC) {
        try {
            String rCommand = "GetLassoFreqNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

        public static void updateKmeans(RConnection RC, int clustNm) {
        try {
            String rCommand = "UpdateKmeans(NA" + ", " + clustNm + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void ComputeUnivFeatureRanking(RConnection RC) {
        try {
            String rCommand = "CalculateFeatureRanking(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PrepareROCDetails(RConnection RC, String featNm) {
        try {
            String rCommand = "PrepareROCDetails(NA" + ", \"" + featNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void setAnalysisMode(RConnection RC, String mode) {
        try {
            String rCommand = "SetAnalysisMode(NA" + ", \"" + mode + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void prepareROCData(RConnection RC) {
        try {
            String rCommand = "PrepareROCData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static int containNewSamples(RConnection RC) {
        try {
            String rCommand = "ContainNewSamples(NA)";
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            e.printStackTrace();
            return 0;
        }
    }

    public static String getLRConvergence(RConnection RC) {
        try {
            return RC.eval("GetLRConvergence()").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "FALSE";
    }

    public static String getLREquation(RConnection RC) {
        try {
            return RC.eval("GetLREquation()").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getLRmodelTable(RConnection RC) {
        try {
            return RC.eval("GetLRmodelTable()").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getLRperformanceTable(RConnection RC) {
        try {
            return RC.eval("GetLRperformTable()").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getLRclsLabel(RConnection RC) {
        try {
            return RC.eval("GetLR_clsLbl(NA)").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    // integer class label
    public static String getLRclsLabelNew(RConnection RC) {
        try {
            return RC.eval("GetLR_clsLblNew(NA)").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getLRthreshold(RConnection RC) {
        try {
            return RC.eval("GetLRthreshold()").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

}
