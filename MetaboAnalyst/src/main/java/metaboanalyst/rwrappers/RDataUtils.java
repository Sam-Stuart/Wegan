/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import java.util.List;
import java.util.StringTokenizer;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.model.SelectItem;
import metaboanalyst.models.ComponentBean;
import metaboanalyst.utils.DataUtils;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.NameBean;
import metaboanalyst.models.SampleBean;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 * Create a dataSet list object mSet$dataSet$cls : class information
 * mSet$dataSet$orig : original data mSet$dataSet$msg : message when processing
 * raw data dataSet$status : boolean value indicates whether acceptable or not
 * dataSet$proc: processed data mSet$dataSet$norm : normalized data
 *
 * @author Jeff
 */
public class RDataUtils {


    /*
     * data type: main, meta, env
     * anal type: stat, ord, plot, stat, div, disp, clust, tax, corr
     *
     * */
    public static boolean initDataObjects(RConnection RC, String dataType, String analType, boolean isPaired) {
        try {
            String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readTextData(RConnection RC, String filePath, String dataFormat, String lblType, String dataNames) {
        try {
            String rCommand = "Read.TextData(NA, \"" + filePath + "\", \"" + dataFormat + "\", \"" + lblType + "\", \"" + dataNames + "\");";
            String rCommand2 = "Read.TextData(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + dataFormat + "\", \"" + lblType + "\", \"" + dataNames + "\");";
            RCenter.recordRCommand(RC, rCommand2);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readTextDataMeta(RConnection RC, String filePath, String metaFormat, String lblType, String metaNames) {
        try {
            String rCommand = "Read.TextDataMeta(NA, \"" + filePath + "\", \"" + metaFormat + "\", \"" + lblType + "\", \"" + metaNames + "\");";
            String rCommand2 = "Read.TextDataMeta(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + metaFormat + "\", \"" + lblType + "\", \"" + metaNames + "\");";
            RCenter.recordRCommand(RC, rCommand2);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }    

    public static boolean readTextDataEnv(RConnection RC, String filePath, String envFormat, String lblType, String envNames) {
        try {
            String rCommand = "Read.TextDataEnv(NA, \"" + filePath + "\", \"" + envFormat + "\", \"" + lblType + "\", \"" + envNames + "\");";
            String rCommand2 = "Read.TextDataEnv(NA, \"" + "Replacing_with_your_file_path" + "\", \"" + envFormat + "\", \"" + lblType + "\", \"" + envNames + "\");";            
            RCenter.recordRCommand(RC, rCommand2);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    //should be in the same directory format specify sample in row or column
    public static boolean readPeakListData(RConnection RC, String filePath) {
        try {
            String rCommand = "Read.PeakListData(NA, \"" + filePath + "\");";
            String rCommand2 = "Read.PeakListData(NA, \"" + "Replacing_with_your_file_path" + "\");";
            RCenter.recordRCommand(RC, rCommand2);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean setMummichogParams(RConnection RC, double instrumentOpt, String msModeOpt, double pvalCutoff) {
        try {
            String rCommand = "UpdateMummichogParameters(NA, \"" + instrumentOpt + "\", \"" + msModeOpt + "\", " + pvalCutoff + ");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }

    //zip data is from stream, not from file
    public static boolean readZipData(RConnection RC, String inFilePath, String dataType, String rmFile) {
        try {
            String rCommand = "UnzipUploadedFile(\"" + inFilePath + "\", \"upload\", " + rmFile + ");";
            String rCommand2 = "UnzipUploadedFile(\"" + "Replacing_with_your_file_path" + "\", \"upload\", " + rmFile + ");";
            RCenter.recordRCommand(RC, rCommand2);
            if (RC.eval(rCommand).asInteger() != 1) {
                return false;
            }
            if (dataType.equals("nmrpeak") || dataType.equals("mspeak")) {
                String rCommand3 = "Read.PeakList(NA" + ", \"upload\");";
                RCenter.recordRCommand(RC, rCommand3);
                return ((RC.eval(rCommand3)).asInteger() == 1);
            }
            return true;
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readBatchCSVData(RConnection RC, String fileName, String format, String label) {
        try {
            String rCommand = "Read.BatchCSVdata(NA" + ", \"" + fileName + "\", \"" + format + "\", \"" + label + "\");";
            String rCommand2 = "Read.BatchCSVdata(NA" + ", \"" + "Replacing_with_your_file_path" + "\", \"" + format + "\", \"" + label + "\");";
            RCenter.recordRCommand(RC, rCommand2);
            return !RC.eval(rCommand).asString().equalsIgnoreCase("F");
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean resetBatchData(RConnection RC) {
        try {
            String rCommand = "ResetBatchData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return !RC.eval(rCommand).asString().equalsIgnoreCase("F");
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }

    public static boolean performBatchCorrection(RConnection RC, String imgName) {
        try {
            String rCommand = "PerformBatchCorrection(NA, \"" + imgName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString().equals("T");
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static String[] GetBatchNames(RConnection RC) {
        try {
            String rCommand = "GetAllBatchNames(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void removeUnmatchedCmpds(RConnection RC) {
        try {
            String rCommand = "RemoveUnmatchedCmpds()";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static boolean readMSspec(RConnection RC, String fileName, String profMethod, double fwhm, double bw) {
        try {
            String rCommand = "Read.MSspec(NA" + ", \'"
                    + fileName + "\', \'" + profMethod + "\', " + fwhm + ", " + bw + ");";
            String rCommand2 = "Read.MSspec(NA" + ", \'"
                    + "Replacing_with_your_file_path" + "\', \'" + profMethod + "\', " + fwhm + ", " + bw + ");";
            RCenter.recordRCommand(RC, rCommand2);
            return ((RC.eval(rCommand)).asInteger() == 1);
        } catch (Exception rse) {
            System.out.println(rse);
            return false;
        }
    }

    public static boolean processPeakList(RConnection RC, double binwidth) {
        try {
            String rCommand = "GroupPeakList(NA" + ", " + binwidth + "," + 10 + ");\n"
                    + "SetPeakList.GroupValues(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public static boolean processPeakList(RConnection RC, double binwidth, double bw) {
        try {
            String rCommand = "GroupPeakList(NA" + ", " + binwidth + "," + bw + ");\n"
                    + "SetPeakList.GroupValues(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public static boolean processMSspec(RConnection RC, double bw, String peakint) {
        try {
            String rCommand = "MSspec.rtCorrection(NA" + ", " + bw + ");\n"
                    + "MSspec.fillPeaks(NA);\n"
                    + "SetupMSdataMatrix(NA" + ", \"" + peakint + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }

    public static String[] getPeaklistProcMessage(RConnection RC) {
        try {
            return RC.eval("c(mSet$msgSet$read.msg, mSet$msgSet$proc.msg)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static boolean isSpectraProcessingOK(RConnection RC) {
        try {
            return RC.eval("IsSpectraProcessingOK(NA)").asInteger() == 1;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    //plot a boxplot and density for each compound
    public static void plotMSspecSummaryGraph(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotMS.RT(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("msrt", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String getNewSampleNames(RConnection RC) {
        try {
            String rCommand = "GetNewSampleNames(NA)";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public static String getCmpdInfo(RConnection RC, String id) {
        try {
            String rCommand = "GetMatchingDetails(NA, \"" + id + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    //------------Sanity Check------------------
    public static boolean sanityCheckData(RConnection RC) {
        try {
            String rCommand = "SanityCheckData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static boolean sanityCheckMummichogData(RConnection RC) {
        try {
            String rCommand = "SanityCheckMummichogData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static String[] getSanityCheckMessage(RConnection RC) {
        try {
            return RC.eval("mSet$msgSet$check.msg").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static boolean sanityCheckMetaData(RConnection RC, String dataNm) {
        try {
            String rCommand = "SanityCheckIndData(NA" + ", \"" + dataNm + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static String[] getMetaSanityCheckMessage(RConnection RC, String dataNm) {
        try {
            String rCommand = "GetMetaSanityCheckMsg(NA" + ", \"" + dataNm + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getMSSpectraProcessingMessage(RConnection RC) {
        try {
            return RC.eval("c(mSet$msgSet$read.msg, mSet$msgSet$xset.msg)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getUploadErrorMessage(RConnection RC) {
        try {
            return RC.eval("c(mSet$msgSet$read.msg)").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getCurrentMsg(RConnection RC) {
        try {
            return RC.eval("GetCurrentMsg()").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getMetaCheckMsg(RConnection RC) {
        try {
            return RC.eval("GetMetaCheckMsg(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    //----------------Data normalization-------------------
    //normalize the data
    public static int normalizeData(RConnection RC, String rowNorm, String transNorm,
            String scaleNorm, String refName, boolean includeRatio, int ratioNum) {
        try {
            String ratioCMD = "ratio=FALSE, ratioNum=20";
            if (includeRatio) {
                ratioCMD = "ratio=TRUE, ratioNum=" + ratioNum;
            }
            String rCommand = null;
            if (refName == null) {
                rCommand = "Normalization(NA" + ", \"" + rowNorm + "\", \"" + transNorm + "\", \"" + scaleNorm + "\", " + ratioCMD + ")";
            } else {
                rCommand = "Normalization(NA" + ", \"" + rowNorm + "\", \"" + transNorm + "\", \"" + scaleNorm + "\", \"" + refName + "\", " + ratioCMD + ")";
            }
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
            e.printStackTrace();
            return 0;
        }
    }

    public static void quantileNormalizeData(RConnection RC) {
        try {
            String rCommand = "QuantileNormalize()";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //retrieve best noramlization method
    public static String autoNormalize(RConnection RC) {
        try {
            String rCommand = "BestNormalize(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
    
    //plot a boxplot and density for each compound
    public static void plotNormSummaryGraph(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotNormSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("norm", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //plot a boxplot and density for each compound
    public static void plotSampleNormSummaryGraph(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotSampleNormSummary(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("snorm", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
//       //LOUISA ADDED THIS START!!!!!!!!!!!!
//        //perform Shapiro-Wilk test
//    public static String shapiroTest(RConnection RC) {
//        try {
//            String rCommand = "shapiroT(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            return RC.eval(rCommand).asString();
//        } catch (Exception e) {
//            e.printStackTrace();
//            return null;
//        }
//    }
//    
//            //perform Levene's test
//    public static String leveneTest(RConnection RC, String predText) {
//        try {
//            String rCommand = "levene(NA" + ", \"" + predText + ")";
//            RCenter.recordRCommand(RC, rCommand);
//            return RC.eval(rCommand).asString();
//        } catch (Exception e) {
//            e.printStackTrace();
//            return null;
//        }
//    }
//    
//    //Plot regression plus residuals
//    public static void ResidualPlot(SessionBean1 sb, String numA, String predText, String imgName, String format, int dpi) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "ResidPlot(NA" + ", \"" + numA + "\", \"" + predText + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("resid", rCommand);
//            RC.voidEval(rCommand);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//    }
//    
//        //Plot residuals vs fitted values
//    public static void Residual_fitPlot(SessionBean1 sb, String imgName, String format, int dpi) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "Resid_fitPlot(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("residFit", rCommand);
//            RC.voidEval(rCommand);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//    }
//   
//            //QQ Plot
//    public static void QQ_plot(SessionBean1 sb, String imgName, String format, int dpi) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "QQplot(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("qq", rCommand);
//            RC.voidEval(rCommand);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//    }
//       
//            //Plot density of residuals
//    public static void Density_plot(SessionBean1 sb, String imgName, String format, int dpi) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "Densityplot(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("residDen", rCommand);
//            RC.voidEval(rCommand);
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//    }
//    
//    //Extract numeric column names for residual plot
//    public static String[] AssupColumn(SessionBean1 sb){
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "AssupCol(NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            return RC.eval(rCommand).asStrings();
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        } catch (REXPMismatchException ex) {
//            Logger.getLogger(OAUtils.class.getName()).log(Level.SEVERE, null, ex);
//        }
//        return null;
//    } 
//   //LOUISA ADDED THIS END!!!!!!!!!!!!
    
    //---------------Methods for access Data information-------------
    //get data information

    public static void initPrenormData(RConnection RC) {
        try {
            RC.voidEval("InitPrenormData(NA)");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String[] getSampleNames(RConnection RC) {
        try {
            String[] names = RC.eval("rownames(mSet$dataSet$prenorm)").asStrings();
            return names;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getPrenormSampleNames(RConnection RC) {
        try {
            String[] names = RC.eval("GetPrenormSmplNms(NA)").asStrings();
            return names;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getPrenormFeatureNames(RConnection RC) {
        try {
            String[] names = RC.eval("GetPrenormFeatureNms(NA)").asStrings();
            return names;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getPrenormGroupNames(RConnection RC) {
        try {
            String[] names = RC.eval("GetPrenormClsNms(NA)").asStrings();
            return names;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getNameCheckMsgs(RConnection RC) {
        try {
            return RC.eval("mSet$msgSet$nmcheck.msg").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void setCachexiaTestSet(RConnection RC, String used) {
        String rCommand = "SetCachexiaSetUsed(NA" + ", " + used + ")";
        try {
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //pass sample specific information to R for normalization
    //note: only need ordered double values
    public static void setDesignType(RConnection RC, String design) {
        String rCommand = "SetDesignType(NA" + ", \"" + design + "\")";
        RCenter.recordRCommand(RC, rCommand);
        try {
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //pass sample specific information to R for normalization
    //note: only need ordered double values
    public static void setSampleNormFactor(RConnection RC, List<SampleBean> norms) {
        String rcmd = "norm.vec<-c(";
        for (int i = 0; i < norms.size() - 1; i++) {
            rcmd = rcmd + norms.get(i).getAdjust() + ",";
        }
        rcmd = rcmd + norms.get(norms.size() - 1).getAdjust() + ");";
        try {
            RC.eval(rcmd);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void setCompVarNumbers(RConnection RC, List<ComponentBean> comps) {
        String rcmd = "comp.var.nums<-c(";
        for (int i = 0; i < comps.size() - 1; i++) {
            rcmd = rcmd + comps.get(i).getVarNum() + ",";
        }
        rcmd = rcmd + comps.get(comps.size() - 1).getVarNum() + ");";
        try {
            RC.eval(rcmd);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void updateGraphSettings(RConnection RC, String[] cols, int[] sps) {
        try {
            String colCmd = "colVec<-" + DataUtils.createStringVector(cols);
            RCenter.recordRCommand(RC, colCmd, true);

            StringBuilder sb = new StringBuilder();
            for (int s : sps) {
                sb.append(s);
                sb.append(",");
            }
            String res = sb.toString();
            //trim the last comma
            res = res.substring(0, res.length() - 1);
            String shpCmd = "shapeVec<-c(" + res + ")";
            RCenter.recordRCommand(RC, shpCmd, true);
            String updateCmd = "UpdateGraphSettings(NA, colVec, shapeVec)";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = colCmd + "; " + shpCmd + "; " + updateCmd;
            RC.eval(rcmd);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String[] getPrenorMFeatureNames(RConnection RC) {
        try {
            return RC.eval("colnames(mSet$dataSet$prenorm)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getNormFeatureNames(RConnection RC) {
        try {
            return RC.eval("colnames(mSet$dataSet$norm)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static int getTtestSigNum(RConnection RC) {
        try {
            return RC.eval("GetTTSigNum(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int getAovSigNum(RConnection RC) {
        try {
            return RC.eval("GetAovSigNum(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int getProcFeatureNumber(RConnection RC) {
        try {
            return RC.eval("ncol(mSet$dataSet$procr)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int getNormFeatureNumber(RConnection RC) {
        try {
            return RC.eval("ncol(mSet$dataSet$norm)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int[] getMissingNumber(RConnection RC) {
        try {
            return RC.eval("apply(is.na(mSet$dataSet$preproc), 2, sum)").asIntegers();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static double[] getMissingPercent(RConnection RC) {
        try {
            return RC.eval("apply(is.na(mSet$dataSet$preproc), 2, sum)*100/nrow(mSet$dataSet$orig)").asDoubles();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static int[] getZeroNumber(RConnection RC) {
        try {
            return RC.eval("apply(mSet$dataSet$preproc==0, 2, sum, na.rm=T)").asIntegers();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static double[] getZeroPercent(RConnection RC) {
        try {
            return RC.eval("apply(mSet$dataSet$preproc==0, 2, sum, na.rm=T)*100/nrow(mSet$dataSet$orig)").asDoubles();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static double[] getCmpdMins(RConnection RC) {
        try {
            return RC.eval("round(apply(mSet$dataSet$preproc, 2, min, na.rm=T),3)").asDoubles();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static double[] getCmpdMaxs(RConnection RC) {
        try {
            return RC.eval("round(apply(mSet$dataSet$preproc, 2, max, na.rm=T),3)").asDoubles();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static double[] getCmpdMedians(RConnection RC) {
        try {
            return RC.eval("round(apply(mSet$dataSet$preproc, 2, median, na.rm=T),3)").asDoubles();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    //save Normalized data
    public static void saveAllData(RConnection RC) {
        try {
            String rCommand = "SaveTransformedData(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //deselect certain compounds from further consideration
    public static void removeFeature(RConnection RC, String cmpdName) {
        try {
            String rCommand = "RemoveFeature(\"" + cmpdName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //deselect certain compounds from further consideration
    public static int isReadyForEditor(RConnection RC) {
        try {
            String rCommand = "IsReadyForEditor(NA)";
            //RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            e.printStackTrace();
            return 0;
        }
    }

    //deselect certain compounds from further consideration
    public static int filterVariable(RConnection RC, String filter, String doQC, int rsd) {
        try {
            String rCommand = "FilterVariable(NA" + ", \"" + filter + "\", \"" + doQC + "\", " + rsd + ")";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            e.printStackTrace();
            return 0;
        }
    }

    //deselect certain compounds from further consideration
    public static void restoreFeature(RConnection RC, String cmpdName) {
        try {
            String rCommand = "RestoreFeature(\"" + cmpdName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static int updateGroupItems(RConnection RC, String[] grpNameVec) {
        try {
            String grpCmd = "grp.nm.vec <- " + DataUtils.createStringVector(grpNameVec);
            RCenter.recordRCommand(RC, grpCmd, true);
            String updateCmd = "UpdateGroupItems(NA, grp.nm.vec)";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = grpCmd + "; " + updateCmd;
            return RC.eval(rcmd).asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int updateSampleItems(RConnection RC, String[] smplNameVec) {
        try {
            String smplCmd = "smpl.nm.vec <- " + DataUtils.createStringVector(smplNameVec);
            RCenter.recordRCommand(RC, smplCmd, true);
            String updateCmd = "UpdateSampleItems(NA, smpl.nm.vec)";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = smplCmd + "; " + updateCmd;
            return RC.eval(rcmd).asInteger();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static void setCurrMapData(RConnection RC, String[] currVec) {
        try {
            String currCmd = "curr.vec <- " + DataUtils.createStringVector(currVec);
            RCenter.recordRCommand(RC, currCmd, true);
            String updateCmd = "Setup.MapData(NA" + ", curr.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = currCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    public static void setAddMapData(RConnection RC, String[] addVec) {
        try {
            String currCmd = "add.vec <- " + DataUtils.createStringVector(addVec);
            RCenter.recordRCommand(RC, currCmd, true);
            String updateCmd = "Setup.AdductData(NA" + ", add.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = currCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static int performCurrencyMapping(RConnection RC) {
        try {
            String rCommand = "PerformCurrencyMapping(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }
    
    public static int performAdductMapping(RConnection RC, String adductMode) {
        try {
            String rCommand = "PerformAdductMapping(NA, adductMode)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static int updateFeatureItems(RConnection RC, String[] smplNameVec) {
        try {
            String grpCmd = "feature.nm.vec <- " + DataUtils.createStringVector(smplNameVec);
            RCenter.recordRCommand(RC, grpCmd, true);
            String updateCmd = "UpdateFeatureItems(NA, feature.nm.vec)";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = grpCmd + "; " + updateCmd;
            return RC.eval(rcmd).asInteger();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    //deselect certain compounds from further consideration
    public static void removeVariable4Name(RConnection RC, String cmpdName) {
        try {
            String rCommand = "mSet$dataSet$preproc[," + "\"" + cmpdName + "\"]<-NULL";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void removeVariable4Percent(RConnection RC, double perc) {
        try {
            String rCommand = "RemoveMissingPercent(NA" + ", percent=" + perc + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // final sanity check to set up dataSet$proc
    public static void replaceMin(RConnection RC) {
        try {
            String rCommand = "ReplaceMin(NA);";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // final sanity check to set up dataSet$proc
    public static void imputeVariable(RConnection RC, String method) {
        try {
            String rCommand = "ImputeVar(NA" + ", method=\"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String getProcZipMessage(RConnection RC) {
        try {
            return RC.eval("procZipErrMsg").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static int getGroupNumber(RConnection RC) {
        try {
            return RC.eval("GetGroupNumber(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static String[] getPathNames(RConnection RC) {
        try {
            return RC.eval("GetPathNames(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getMatchedCmpdNames(RConnection RC) {
        try {
            return RC.eval("GetMatchedCompounds(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getErrorMsgs(RConnection RC) {
        try {
            return RC.eval("GetErrMsg()").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static void addErrMsg(RConnection RC, String msg) {
        try {
            String rCommand = "AddErrMsg(\"" + msg + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static String[] getSSPReferences(RConnection RC, String nm) {
        try {
            return RC.eval("GetSSP.Refs(NA, \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] getSSPConcs(RConnection RC, String nm) {
        try {
            return RC.eval("GetSSP.RefConc(NA, \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] getSSP_Pmids(RConnection RC, String nm) {
        try {
            //  System.out.println(RC.eval("GetSSP.Pmids("+inx+")").asStrings());
            return RC.eval("GetSSP.Pmids(NA" + ", \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] getSSP_Notes(RConnection RC, String nm) {
        try {
            return RC.eval("GetSSP.Notes(NA" + ", \"" + nm + "\")").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readMsetLibData(RConnection RC, String fileName) {
        try {
            String rCommand = "Setup.UserMsetLibData(NA" + ", \"" + fileName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static String getMsetLibCheckMsg(RConnection RC) {
        try {
            return RC.eval("GetMsetLibCheckMsg(NA)").asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readKEGGRefLibData(RConnection RC, String fileName) {
        try {
            String rCommand = "Setup.KEGGReferenceMetabolome(NA" + ", \"" + fileName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    //should be in the same directory format specify sample in row or column
    public static boolean readHMDBRefLibData(RConnection RC, String fileName) {
        try {
            String rCommand = "Setup.HMDBReferenceMetabolome(NA" + ", \"" + fileName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static String getRefLibCheckMsg(RConnection RC) {
        try {
            return RC.eval("GetRefLibCheckMsg(NA)").asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static void setMapData(RConnection RC, String[] qvec) {
        try {
            String grpCmd = "cmpd.vec<-" + DataUtils.createStringVector(qvec);
            RCenter.recordRCommand(RC, grpCmd, true);
            String updateCmd = "Setup.MapData(NA" + ", cmpd.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = grpCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static void setConcData(RConnection RC, double[] concVec) {
        try {

            StringBuilder sb = new StringBuilder();
            for (double s : concVec) {
                sb.append(s);
                sb.append(",");
            }
            String res = sb.toString();
            //trim the last comma
            res = res.substring(0, res.length() - 1);
            String grpCmd = "conc.vec<-c(" + res + ")";
            RCenter.recordRCommand(RC, grpCmd, true);
            String updateCmd = "Setup.ConcData(NA" + ", conc.vec);";
            RCenter.recordRCommand(RC, updateCmd);

            String rcmd = grpCmd + "; " + updateCmd;
            RC.voidEval(rcmd);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //to set up the single sample concentration data for SSP analysis
    public static void setupSspData(RConnection RC, String[] nmVec, double[] concVec, String type) {
        try {
            setMapData(RC, nmVec);
            setConcData(RC, concVec);
            setBiofluidType(RC, type);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void setBiofluidType(RConnection RC, String type) {
        try {
            String rCommand = "Setup.BiofluidType(NA" + ", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //set current metabolite set library for search
    public static void setCurrentMsetLib(RConnection RC, String libType, int excludeNum) {
        try {
            String rCommand = "SetCurrentMsetLib(NA" + ", \"" + libType + "\", " + excludeNum + ");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //set current metabolite set library for search
    public static void setCurrentMsetLib(RConnection RC, String libType) {
        try {
            String rCommand = "SetCurrentMsetLib(NA" + ", \"" + libType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //use filter or not
    public static void setMetabolomeFilter(RConnection RC, boolean useFilter) {
        try {
            String rCommand = "SetMetabolomeFilter(NA" + ", " + (useFilter ? "T" : "F") + ");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //first col is compound name, 2nd is concentration values, return as a Hashmap
    //with cmpd name as key and concentration as value
    public static boolean setSample(RConnection RC, String content, String type, String sep) {

        if (sep == null) {
            sep = System.getProperty("line.separator");
        }
        StringTokenizer st = new StringTokenizer(content, sep);

        try {
            ArrayList<String> nmVec = new ArrayList();
            ArrayList<Double> concVec = new ArrayList();
            while (st.hasMoreTokens()) {
                String line = st.nextToken();
                line = DataUtils.cleanString(line);
                if (line.length() == 0) { //empty line
                    continue;
                }
                String[] eles = line.split("\t");
                if (eles.length != 2) {
                    RDataUtils.addErrMsg(RC, "Each row must have two elements: compound and its concentration value, separated by a tab!");
                    return false;
                }
                String name = DataUtils.cleanString(eles[0]);
                double conc = Double.parseDouble(DataUtils.cleanString(eles[1]));
                nmVec.add(name);
                concVec.add(conc);
            }
            double[] concArray = new double[concVec.size()];
            for (int i = 0; i < concArray.length; i++) {
                concArray[i] = concVec.get(i);
            }
            setupSspData(RC, nmVec.toArray(new String[0]), concArray, type);
            return true;
        } catch (NumberFormatException e) {
            e.printStackTrace();
            RDataUtils.addErrMsg(RC, "Some concentration values are not numeric!");
            return false;
        }
    }

    public static boolean setPathLib(RConnection RC, String path) {

        if (path.equals("hsa-smpdb")) {
            return (setSmpdbPathLib(RC, "hsa"));
        } else if (path.equals("mmu-smpdb")) {
            return (setSmpdbPathLib(RC, "mmu"));
        } else {
            return (setKeggPathLib(RC, path));
        }
    }

    public static boolean setKeggPathLib(RConnection RC, String path) {
        try {
            String rCommand = "SetKEGG.PathLib(NA" + ", \"" + path + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static boolean setSmpdbPathLib(RConnection RC, String path) {
        try {
            String rCommand = "SetSMPDB.PathLib(NA" + ", \"" + path + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static String[] getKeggPathLibNames(RConnection RC) {
        try {
            return RC.eval("GetKEGG.PathNames(NA)").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static List<SampleBean> createSampleBeans(RConnection RC, boolean withNA) {
        String[] allSamples = RDataUtils.getSampleNames(RC);
        String[] allGrps = RDataUtils.getLiteralGroupNames(RC);
        int samSize = allSamples.length;
        List<SampleBean> samNABeans = new ArrayList();
        if (withNA) {
            samNABeans.add(new SampleBean("<Not set>", "NA")); // the first one is NA
        }
        for (int i = 0; i < samSize; i++) {
            samNABeans.add(new SampleBean(allSamples[i], allGrps[i]));
        }
        return samNABeans;
    }

    public static NameBean[] createProcFeatureBeans(RConnection RC, boolean withNA) {
        String[] names = RDataUtils.getPrenormFeatureNames(RC);
        int varSize = names.length;
        if (withNA) {
            NameBean[] featureNABeans = new NameBean[varSize + 1];
            featureNABeans[0] = new NameBean("<Not set>"); // the first one is NA

            for (int i = 0; i < varSize; i++) {
                featureNABeans[i + 1] = new NameBean(names[i]);
            }
            return featureNABeans;
        } else {
            NameBean[] featureBeans = new NameBean[varSize];
            for (int i = 0; i < varSize; i++) {
                featureBeans[i] = new NameBean(names[i]);
            }
            return featureBeans;
        }
    }

    public static NameBean[] createQCFeatureBeans(RConnection RC) {
        String[] names = getQCFeatureNames(RC);
        int varSize = names.length;

        NameBean[] featureNABeans = new NameBean[varSize + 1];
        featureNABeans[0] = new NameBean("Sum of all compounds"); // the first one is NA

        for (int i = 0; i < varSize; i++) {
            featureNABeans[i + 1] = new NameBean(names[i]);
        }
        return featureNABeans;
    }

    public static SelectItem[] createSelectItems(SessionBean1 sb, String[] names) {
        if (names == null || names.length == 0) {
            SelectItem[] nmBeans = new SelectItem[1];
            nmBeans[0] = new SelectItem("NA", "--Not available-");
            return nmBeans;
        }
        SelectItem[] nmBeans = new SelectItem[names.length];
        for (int i = 0; i < names.length; i++) {
            nmBeans[i] = new SelectItem(names[i], names[i]);
        }
        return nmBeans;
    }

    public static String[] getLiteralGroupNames(RConnection RC) {
        try {
            String rCommand = "GetLiteralGroupNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getGroupNames(RConnection RC) {
        try {
            String rCommand = "GetGroupNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getNormGroupNames(RConnection RC) {
        try {
            String rCommand = "GetNormGroupNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getQCFeatureNames(RConnection RC) {
        try {
            String rCommand = "GetQCCompoundNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static boolean isDataContainNegative(RConnection RC) {
        try {
            String rCommand = "IsDataContainsNegative(NA);";
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static boolean isSmallSampleSize(RConnection RC) {
        try {
            String rCommand = "IsSmallSmplSize(NA);";
            return RC.eval(rCommand).asInteger() == 1;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static int getMinGroupSize(RConnection RC) {
        try {
            String rCommand = "GetMinGroupSize(NA);";
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String getErrMsg(RConnection RC) {
        String msg = "<table>";
        if (RC != null) {
            String[] msgs = getErrorMsgs(RC);
            msg = msg + "<tr><th> Possible causes of error (last one being the most relevant): </th></tr>";
            if (msgs.length > 0) {
                for (int i = 0; i < msgs.length; i++) {
                    msg = msg + "<tr><td align=\"left\">" + msgs[i] + "</td></tr>";
                }
            } else {
                msg = msg + "An unknown error occurred, please notify web admin for help.";
            }
        } else {
            msg = "An unknown error occurred.";
        }
        msg = msg + "</table>";
        return msg;
    }

    public static String[] getNewSampleNameVec(RConnection RC) {
        try {
            return RC.eval("GetNewSampleNameVec(NA)").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static double[] getNewSampleProbs(RConnection RC) {
        try {
            return RC.eval("GetNewSampleProbs(NA)").asDoubles();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] getNewSampleGrps(RConnection RC) {
        try {
            return RC.eval("GetNewSampleGrps(NA)").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static double[][] getTtDnMat(RConnection RC) {
        try {
            String rCommand = "GetTtDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getTtUpMat(RConnection RC) {
        try {
            String rCommand = "GetTtUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getTtLnMat(RConnection RC) {
        try {
            String rCommand = "GetTtLnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTtCmpds(RConnection RC) {
        try {
            String rCommand = "GetTtCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getTtCmpdInxs(RConnection RC) {
        try {
            String rCommand = "GetTtCmpdInxs(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int getMaxTtInx(RConnection RC) {
        try {
            String rCommand = "GetMaxTtInx()";
            return RC.eval(rCommand).asInteger();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static double[][] getAnovaDnMat(RConnection RC) {
        try {
            String rCommand = "GetAnovaDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnovaUpMat(RConnection RC) {
        try {
            String rCommand = "GetAnovaUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnovaLnMat(RConnection RC) {
        try {
            String rCommand = "GetAnovaLnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAnovaCmpds(RConnection RC) {
        try {
            String rCommand = "GetAnovaCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getAnovaCmpdInxs(RConnection RC) {
        try {
            String rCommand = "GetAnovaCmpdInxs(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int getMaxAovInx(RConnection RC) {
        try {
            String rCommand = "GetMaxAnovaInx(NA)";
            return RC.eval(rCommand).asInteger();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static double[][] getVolcanoDnMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoDnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoUpMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoUpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoVlMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoVlMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoVrMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoVrMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getVolcanoHlMat(RConnection RC) {
        try {
            String rCommand = "GetVolcanoHlMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getVolcanoCmpds(RConnection RC) {
        try {
            String rCommand = "GetVolcanoCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getVolcanoCmpdInxs(RConnection RC) {
        try {
            String rCommand = "GetVolcanoCmpdInxs(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getVolcanoRangeX(RConnection RC) {
        try {
            String rCommand = "GetVolcanoRangeX(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getPCALoadCmpds(RConnection RC) {
        try {
            String rCommand = "GetPCALoadCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getPCALoadCmpdInxs(RConnection RC) {
        try {
            String rCommand = "GetPCALoadCmpdInxs(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getPCALoadMat(RConnection RC) {
        try {
            String rCommand = "GetPCALoadMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getPCALoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetPCALoadAxesSpec(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getPLSLoadCmpds(RConnection RC) {
        try {
            String rCommand = "GetPLSLoadCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getSPLSLoadCmpds(RConnection RC) {
        try {
            String rCommand = "GetSPLSLoadCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getOPLSLoadCmpds(RConnection RC) {
        try {
            String rCommand = "GetOPLSLoadCmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getOPLSLoadColNames(RConnection RC) {
        try {
            String rCommand = "GetOPLSLoadColNames(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getPLSLoadCmpdInxs(RConnection RC) {
        try {
            String rCommand = "GetPLSLoadCmpdInxs(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getSPLSLoadCmpdInxs(RConnection RC) {
        try {
            String rCommand = "GetSPLSLoadCmpdInxs(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getOPLSLoadCmpdInxs(RConnection RC) {
        try {
            String rCommand = "GetOPLSLoadCmpdInxs(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getPLSLoadMat(RConnection RC) {
        try {
            String rCommand = "GetPLSLoadMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getSPLSLoadMat(RConnection RC) {
        try {
            String rCommand = "GetSPLSLoadMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getOPLSLoadMat(RConnection RC) {
        try {
            String rCommand = "GetOPLSLoadMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getPLSLoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetPLSLoadAxesSpec(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getSPLSLoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetSPLSLoadAxesSpec(NA)";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[] getOPLSLoadAxesSpec(RConnection RC) {
        try {
            String rCommand = "GetOPLSLoadAxesSpec()";
            return RC.eval(rCommand).asDoubles();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnova2DnMat(RConnection RC) {
        try {
            String rCommand = "GetAnova2DnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnova2UpMat(RConnection RC) {
        try {
            String rCommand = "GetAnova2UpMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getAnova2LnMat(RConnection RC) {
        try {
            String rCommand = "GetAnova2LnMat(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getAnova2Cmpds(RConnection RC) {
        try {
            String rCommand = "GetAnova2Cmpds(NA)";
            return RC.eval(rCommand).asStrings();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int getMaxAov2Inx(RConnection RC) {
        try {
            String rCommand = "GetMaxAnova2Inx(NA)";
            return RC.eval(rCommand).asInteger();
        } catch (REXPMismatchException e) {
            System.out.println(e);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void setOrganism(RConnection RC, String orgCode) {
        try {
            String rCommand = "SetOrganism(NA" + ", \"" + orgCode + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static int setSelectedDataNames(RConnection RC, String[] nmVec) {
        try {
            RC.assign("nm.vec", nmVec);
            return RC.eval("SelectMultiData(NA)").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int prepareVennData(RConnection RC) {
        try {
            String rCommand = "PrepareVennData(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getMetaGroupNames(RConnection RC, String dataName) {
        try {
            String rCommand = "GetMetaGroupNames(NA" + ", \"" + dataName + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] setSelectedMetaMetaInfo(RConnection RC, String dataName, String meta0, String meta1, String isBlock) {
        try {
            String cmd = "SetSelectedMetaMetaInfo(\"" + dataName + "\", \"" + meta0 + "\", \"" + meta1 + "\", " + isBlock + ")";
            RCenter.recordRCommand(RC, cmd);
            return RC.eval(cmd).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String prepareMetaDataTable(RConnection RC, int dataCount) {
        try {
            return RC.eval("PrepareMetaDataTable()").asString();
        } catch (Exception e) {
        }
        return null;
    }

    public static int annotateMetaData(RConnection RC, String dataName, String featureType, String lvlOpt) {
        try {
            String rCommand = "AnnotateMetaData(\"" + dataName + "\", \"" + featureType + "\", \"" + lvlOpt + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getGroupLabels(RConnection RC, String dataName, String classLbl) {
        try {
            return RC.eval("GetGroupLabels(\"" + dataName + "\", \"" + classLbl + "\")").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int prepareMetaHeatmapVis(RConnection RC, String heatmapName) {
        try {
            String rCommand = "PrepareMetaHeatmapJSON(\"" + heatmapName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getMetaMetaInfo(RConnection RC, String dataName) {
        try {
            return RC.eval("GetMetaMetaInfo(\"" + dataName + "\")").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String[] mapGeneToProteins(RConnection RC, String listNm, String enIDs, String org, String type) {
        try {
            String rCommand = "performGene2ProteinMapping(\"" + listNm + "\", \"" + enIDs + "\", \"" + org + "\", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    //only for single gene expression
    public static int setCurrentData(RConnection RC, String nm) {
        try {
            return RC.eval("SetCurrentData(\"" + nm + "\")").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static String[] getArraySampleNames(RConnection RC) {
        try {
            String rcmd = "GetArraySampleNames()";
            RCenter.recordRCommand(RC, rcmd);
            return RC.eval(rcmd).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //list of samples with class label
    public static String[] getSampleInfo(RConnection RC, String dataName, String clsLbl) {
        try {
            return RC.eval("GetSampleInfo(\"" + dataName + "\", \"" + clsLbl + "\")").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int setSampleInfo(RConnection RC, String dataName, String clsLbl, String grpText, String smplText) {

        String lineSep = System.getProperty("line.separator");

        StringTokenizer gt = new StringTokenizer(grpText, lineSep);
        ArrayList<String> clsVec = new ArrayList<String>();
        while (gt.hasMoreTokens()) {
            String line = gt.nextToken();
            line = DataUtils.cleanString(line);
            if (line.length() == 0) { //empty line
                continue;
            }
            clsVec.add(DataUtils.cleanString(line));
        }

        StringTokenizer st = new StringTokenizer(smplText, lineSep);
        ArrayList<String> nmVec = new ArrayList<String>();
        while (st.hasMoreTokens()) {
            String line = st.nextToken();
            line = DataUtils.cleanString(line);
            if (line.length() == 0) { //empty line
                continue;
            }
            String[] smplInfo = line.split("\t");
            nmVec.add(DataUtils.cleanString(smplInfo[0]));
        }
        try {
            RC.assign("class.vec", clsVec.toArray(new String[0]));
            RC.assign("smpl.vec", nmVec.toArray(new String[0]));
            String rCommand = "UpdateSampleInfo(\"" + dataName + "\", \"" + clsLbl + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    //for multiple class
    public static String[] readInDataForMetaInfo(RConnection RC, String dataName) {
        try {
            String rCommand = "ReadDataForMetaInfo(\"" + dataName + "\")";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void removeData(RConnection RC, String dataName) {
        try {
            String rCommand = "RemoveData(\"" + dataName + "\")";
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static int setupSingleMetaRes(RConnection RC) {
        try {
            String rCommand = "SetupSingleMetaRes();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int[] getInputGeneListStat(RConnection RC, String listNm) {
        try {
            String rCommand = "GetInputGeneListStat(\"" + listNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
        }
        return null;
    }

    public static String[] getAvailableDataName(RConnection RC) {
        try {
            return RC.eval("GetAllDataNames()").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int[] getDataDims(RConnection RC, String dataName) {
        try {
            String rCommand = "GetDataDims(NA" + ", \"" + dataName + "\")";
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //type metab, all
    public static void setKeggPathLib(RConnection RC, String orgCode, String type) {
        try {
            RC.voidEval("LoadKEGGLib(\"" + orgCode + "\", \"" + type + "\")");
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    //return dataName (strip .zip or txt) to be consistent java/R
    public static int readIndExpressTable(RConnection RC, String dataName, String dataFormat) {
        try {
            String rCommand = "ReadIndData(NA" + ", \"" + dataName + "\", \"" + dataFormat + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int preparePCAData(RConnection RC) {
        try {
            String rCommand = "PreparePCAData();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static void updatePhenoInfo(RConnection RC, String[] phenoInf) {
        try {
            RC.assign("tmp.vec", phenoInf);
            String rCommand = "SetupGroupLabel(tmp.vec);";
            RC.voidEval(rCommand);
            //compound names for hypergeometric test
            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
        }
    }

    public static int checkMetaDataConsistency(RConnection RC, String adjustBatch) {
        try {
            String rCommand = "CheckMetaDataConsistency(NA" + ", " + adjustBatch + ");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int checkIntegDataConsistency(RConnection RC) {
        try {
            String rCommand = "CheckIntegDataIntegrity();";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int getSelectedDataNumber(RConnection RC) {
        try {
            String rCommand = "GetSelectedDataNumber(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static String getSelectedDataNames(RConnection RC) {
        try {
            String rCommand = "GetSelectedDataNames(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
        }
        return null;
    }

    public static String getVennGeneNames(RConnection RC, String areas) {
        try {
            String rCommand = "GetVennGeneNames(NA" + ", \"" + areas + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
        }
        return null;
    }

    public static int processIndData(RConnection RC, String dataName, String featureType) {
        try {
            String rCommand = "ProcessIndData(\"" + dataName + "\", \"" + featureType + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static void performDetailSearch(RConnection RC, String query) {
        try {
            String rCommand = "PerformDetailMatch(NA, \"" + query + "\");";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String genPathwayJSON(RConnection RC, String name) {
        try {
            String rCommand = "GeneratePathwayJSON(\"" + name + "\")";
            //String imgName = RC.eval(rCommand).asString();
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
        }
        return null;
    }

//    public static void readTextData(RConnection RC, String testRocPath, String rowu, String disc, String afalse, String afalse0) {
//        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
//    }

}
