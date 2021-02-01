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
 * @author Leif Wilm
 */
public class Dispersal {

    public static void InitBGD(SessionBean1 sb) {
        try {
            String rCommand = "bgdispersalWegan(NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void InitBsmooth(SessionBean1 sb) {
        try {
            String rCommand = "bsmoothWegan(NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

        
        
/**      
    public static void FlipPCA(SessionBean1 sb, String axisOpt) {
        try {
            String rCommand = "PCA.Flip(NA" + ", \"" + axisOpt + "\")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double[][] GetPCALoadingScore(SessionBean1 sb) {
        try {
            String rCommand = "mSet$analSet$pca$imp.loads";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetPCALoadingRowName(SessionBean1 sb) {
        try {
            String rCommand = "rownames(mSet$analSet$pca$imp.loads)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetPCALoadingColName(SessionBean1 sb) {
        try {
            String rCommand = "colnames(mSet$analSet$pca$imp.loads)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void PlotPCALoading(SessionBean1 sb, String imgName, String format, int dpi, int pcImpInx1, int pcImpInx2, String plotType, int lblFeat) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCALoading(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcImpInx1 + "," + pcImpInx2 + ",\"" + plotType + "\", " + lblFeat + ");";
            sb.addGraphicsCMD("pca_loading", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPCA2DScore(SessionBean1 sb, String imgName, String format, int dpi, int pc1Inx, int pc2Inx, double conf, int show, int greyScale) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCA2DScore(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pc1Inx + "," + pc2Inx + "," + conf + "," + show + "," + greyScale + ")";
            sb.addGraphicsCMD("pca_score2d", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPCA3DScore(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCA3DScore(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            String rCommand_png = "PlotPCA3DScoreImg(NA" + ", \"" + imgName + "\", \"png\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ", 40)";
            sb.addGraphicsCMD("pca_score3d", rCommand_png);
            //System.out.println("============pca_score3d: " + rCommand_png);
            RCenter.recordRCommand(RC, rCommand_png);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPCAPairSummary(SessionBean1 sb, String imageName, String format, int dpi, int pcNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCAPairSummary(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + ")";
            sb.addGraphicsCMD("pca_pair", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPCABiplot(SessionBean1 sb, String imageName, String format, int dpi, int pc1Inx, int pc2Inx) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCABiplot(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pc1Inx + "," + pc2Inx + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pca_biplot", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPCAScree(SessionBean1 sb, String imageName, String format, int dpi, int pcNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPCAScree(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pca_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void InitPLS(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PLSR.Anal(NA, reg=FALSE)";
            if (sb.isKeepClsOrder()) {
                rCommand = "PLSR.Anal(NA, reg=TRUE)";
            }
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void InitSPLS(SessionBean1 sb, int compNum, int varNum, String varSpec) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SPLSR.Anal(NA" + ", " + compNum + ", " + varNum + ", \"" + varSpec + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void InitOPLS(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "OPLSR.Anal(NA, reg=FALSE)";
            if (sb.isKeepClsOrder()) {
                rCommand = "OPLSR.Anal(NA, reg=TRUE)";
            }
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotOPLS2DScore(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2, double conf, int show, int greyScale) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS2DScore(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + conf + "," + show + "," + greyScale + ")";
            sb.addGraphicsCMD("opls_score2d", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotOplsSplot(SessionBean1 sb, String imageName, String format, int dpi, String plotType) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS.Splot(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + plotType + "\");";
            sb.addGraphicsCMD("opls_splot", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void ResetCustomCmpds(RConnection RC) {
        try {
            String rCommand = "ResetCustomCmpds(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String PlotLoadingCmpdView(SessionBean1 sb, String cmpdName, String format, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotLoadingCmpd(NA" + ", \"" + cmpdName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            String imgName = RC.eval(rCommand).asString();
            return imgName;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void PlotPLSLoading(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2, String plotType, int lablFeat) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLSLoading(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + ", " + pcInx2 + ",\"" + plotType + "\", " + lablFeat + ");";
            sb.addGraphicsCMD("pls_loading", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSPLSLoading(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, String viewOpt) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLSLoading(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + ",\"" + viewOpt + "\");";
            sb.addGraphicsCMD("spls_loading", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPLS2DScore(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2, double conf, int show, int greyScale) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS2DScore(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + conf + "," + show + "," + greyScale + ")";
            sb.addGraphicsCMD("pls_score2d", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSPLS2DScore(SessionBean1 sb, String imageName, String format, int dpi, int pcInx1, int pcInx2, double conf, int show, int greyScale) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLS2DScore(NA" + ", \"" + imageName + "\", \"" + format + "\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + conf + "," + show + "," + greyScale + ")";
            sb.addGraphicsCMD("spls_score2d", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //json file generation
    public static void PlotPLS3DScore(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS3DScore(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            //String rCommand_png = "PlotPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"" + format + "\", " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ")";
            String rCommand_png = "PlotPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"png\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ", 40)";
               
            sb.addGraphicsCMD("pls_score3d", rCommand_png);
            RCenter.recordRCommand(RC, rCommand_png);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSPLS3DScore(SessionBean1 sb, String imgName, String format, int dpi, int pcInx1, int pcInx2, int pcInx3) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLS3DScore(NA" + ", \"" + imgName + "\", \"" + format + "\")";
            String rCommand_png = "PlotSPLS3DScoreImg(NA" + ", \"" + imgName + "\", \"png\", " + dpi + ", width=NA, " + pcInx1 + "," + pcInx2 + "," + pcInx3 + ", 40)";
            
            sb.addGraphicsCMD("spls_score3d", rCommand_png);
            RCenter.recordRCommand(RC, rCommand_png);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPLSPairSummary(SessionBean1 sb, String imgName, String format, int dpi, int cmpdNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLSPairSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + cmpdNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pls_pair", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSPLSPairSummary(SessionBean1 sb, String imgName, String format, int dpi, int cmpdNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLSPairSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, " + cmpdNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("spls_pair", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static int TrainPLSClassifier(SessionBean1 sb, String LorT, int cmpdNum, String choice) {
        try {
            //first perform PLSDA cross validation
            RConnection RC = sb.getRConnection();
            String rCommand = "PLSDA.CV(NA" + ", " + "\"" + LorT + "\"," + cmpdNum + ", \"" + choice + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return (0);
        }
    }

    public static int TrainSPLSClassifier(SessionBean1 sb, String LorT, int cmpdNum, String choice) {
        try {
            //first perform PLSDA cross validation
            RConnection RC = sb.getRConnection();
            String rCommand = "SPLSDA.CV(" + "\"" + LorT + "\"," + cmpdNum + ", \"" + choice + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return (0);
        }
    }

    public static String PLSPermute(SessionBean1 sb, int permutNum, String type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PLSDA.Permut(NA" + ", " + permutNum + ", \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String SPLSPermute(SessionBean1 sb, int permutNum, String type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "SPLSDA.Permut(" + permutNum + ", \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String OPLSPermute(SessionBean1 sb, int permutNum, String type) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "OPLSDA.Permut(NA" + ", " + permutNum + ", \"" + type + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    //should perform TrainPLSClassifier first
    public static int GetBestTuneNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetPLSBestTune(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int GetDefaultPLSPairNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultPLSPairComp(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int GetDefaultSPLSPairNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultSPLSPairComp(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int GetDefaultPLSCVNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultPLSCVComp(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int GetDefaultSPLSCVNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetDefaultSPLSCVComp(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int GetMaxPLSPairCompNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetMaxPLSPairComp(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int GetMaxPCACompNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetMaxPCAComp(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    //should perform TrainPLSClassifier first
    public static int GetMaxPLSCVCompNumber(SessionBean1 sb) {
        try {
            return sb.getRConnection().eval("GetMaxPLSCVComp(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static void PlotPLSClassification(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS.Classification(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pls_cv", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSPLSClassification(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLS.Classification(\"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("spls_cv", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotOplsMdlView(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS.MDL(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("opls_mdl", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotPLSPermutation(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotPLS.Permutation(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("pls_perm", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSPLSPermutation(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLS.Permutation(\"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("spls_perm", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String PlotOPLSPermutation(SessionBean1 sb, String imgName, String format, int dpi, int permNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotOPLS.Permutation(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", " + permNum + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("opls_perm", rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static void PlotPLSImp(SessionBean1 sb, String imgName, String format, int dpi, String type, String featNm, int num, String colorBW) {
        try {
            String rCommand = "PlotPLS.Imp(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\", \"" + featNm + "\", " + num + "," + colorBW + ")";
            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("pls_imp", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotSPLSImp(SessionBean1 sb, String imgName, String format, int dpi, String type, String featNm, int num, String colorBW) {
        try {
            String rCommand = "PlotSPLS.Imp(\"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA, \"" + type + "\", \"" + featNm + "\", " + num + "," + colorBW + ")";
            RConnection RC = sb.getRConnection();
            sb.addGraphicsCMD("spls_imp", rCommand);
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String[] GetPLSSigColNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetPLSSigColNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetSPLSSigColNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetSPLSSigColNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetPLSSigRowNames(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetPLSSigRowNames(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetPLSSigMat(SessionBean1 sb, String type) {
        try {
            String rCommand = "GetPLSSigMat(NA" + ", \"" + type + "\")";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetPLSCVColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetPLS_CVColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetSPLSCVColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSPLS_CVColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetPLSCVRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetPLS_CVRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetSPLSCVRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetSPLS_CVRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetPLS_CVMat(SessionBean1 sb) {
        try {
            String rCommand = "GetPLS_CVMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetSPLS_CVMat(SessionBean1 sb) {
        try {
            String rCommand = "GetSPLS_CVMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void PlotSPLSDAClassification(SessionBean1 sb, String imgName, String cvMode, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSPLSDA.Classification(NA" + ", \"" + imgName + "\", \"" + cvMode + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }
*/
}
