/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

// GPS ADDED:
import java.util.logging.Level;
import java.util.logging.Logger;
import org.rosuda.REngine.REXPMismatchException;

// for Stats module (rwrapper: UniVarTests, controller: UnivBean)
// have group_name added to : InitPairedFC, InitUnpairedFC, performTtests,   performVolcano, performANOVA, PlotCmpdView

/**
 *
 * @author Jeff
 */
public class UniVarTests {

// FOLD-CHANGE (FC)    
   
    public static void InitPairedFC(SessionBean1 sb, double fcThresh, double pairThresh, int cmpType
             , String group_name
    ) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FC.Anal.paired(NA" + ", " 
                    + fcThresh + ", " 
                    + pairThresh + ", " 
                    + cmpType + ", \"" 
                    + group_name + "\" )";
//                    + cmpType + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void InitUnpairedFC(SessionBean1 sb, double fcThresh, int cmpType
               , String group_name
    ) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FC.Anal.unpaired(NA" + ", " 
                    + fcThresh + ", "
                    + cmpType + ", \"" 
                    + group_name + "\" )";
//                    + cmpType + ")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotFC(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            String rCommand = "PlotFC(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("fc", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void ComputeUnivReport(RConnection RC) {
        try {
            String rCommand = "GetUnivReport(NA)";
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double[][] GetFCSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetFCSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetFCSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetFCSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetFCSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetFCSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

// T-TEST
 
    public static int performTtests(SessionBean1 sb, String nonpar, double pthresh, String paired, String equalVar
            , String group_name
    ) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Ttests.Anal(NA" + ", " 
                    + nonpar + ", " 
                    + pthresh + ", " 
                    + paired + ", " 
//                    + equalVar + ")";
                    + equalVar + ", \"" 
                    + group_name + "\" )";//"p" or "u"
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }
//    RC.voidEval(rCommand);
    
    
    // from CAUtiles ; Correlation helper functions
    // based on GetDataColumns (using lin.reg.columns(NA) from correlation_linear.R
    
    // correlation_linear.R
    public static String[] GetFacColumns(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "fac.names(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(OAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    

    public static void PlotT(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotTT(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("tt", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static double[][] GetTTSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetTTSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] GetCorSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetCorSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetTTSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetTTSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetTTSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetTTSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }


// VOLCANO PLOT      
    public static void performVolcano(SessionBean1 sb, String paired, double fcThresh, int cmpType, double countThresh, String nonpar, double pThresh, String varEqual, String vcPvalType
    ,String group_name
    ) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Volcano.Anal(NA" + ", " 
                    + paired + ", " 
                    + fcThresh + ", " 
                    + cmpType + ", " 
                    + countThresh + "," 
                    + nonpar + ", " 
                    + pThresh + ", " 
                    + varEqual + ", \"" 
                    + vcPvalType + ", \"" 
                    + group_name + "\" )";        
//                    + vcPvalType + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotVolcano(SessionBean1 sb, String imgName, int plotLbl, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotVolcano(NA" + ", \"" + imgName + "\"," + plotLbl + ", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("volcano", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //threshx is fc, threshy is pvalue
    public static double[][] GetVolcanoSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetVolcanoSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetVolcanoSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetVolcanoSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static boolean ContainsInfValuesVolcano(SessionBean1 sb) {
        try {
            String rCommand = "ContainInfiniteVolcano(NA)";
            return sb.getRConnection().eval(rCommand).asString().equalsIgnoreCase("TRUE");
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static boolean ContainsInfValuesTT(SessionBean1 sb) {
        try {
            String rCommand = "ContainInfiniteTT(NA)";
            return sb.getRConnection().eval(rCommand).asString().equalsIgnoreCase("TRUE");
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

// ANOVA    
    public static int performANOVA(SessionBean1 sb, String nonPar, double thresh, String postType
    , String group_name
    ) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "ANOVA.Anal(NA" + ", " 
                    + nonPar + ", " 
                    + thresh + ", \"" 
                    + postType + ", \"" 
                    + group_name + "\" )";        
//                  + postType + "\")";    
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
            return 0;
        }
    }

    public static void PlotAOV(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotANOVA(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("aov", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    //threshx is fc, threshy is pvalue
    public static double[][] GetAovSigMat(SessionBean1 sb) {
        try {
            String rCommand = "GetAovSigMat(NA)";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetAovSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetAovSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetAovSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetAovSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetAovPostHocSigNames(SessionBean1 sb) {
        try {
            String rCommand = "GetAovPostHocSig(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    
// PATTERN MATCHING    
    public static String PlotCmpdSummary(SessionBean1 sb, String cmpdName, String format, String dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCmpdSummary(NA" + ", \"" + cmpdName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("cmpd", rCommand);
            String imgName = RC.eval(rCommand).asString();
            return imgName;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String PlotCmpdView(RConnection RC, String cmpdName,
//            String group_name,
            String format, String dpi) {
        try {
            String rCommand = "PlotCmpdView(NA" + ", \"" 
                    + cmpdName + "\", \"" 
//                    + group_name + "\", \"" 
                    + format + "\", " 
                    + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            String imgName = RC.eval(rCommand).asString();
            return imgName;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static boolean matchPattern(SessionBean1 sb, String dist, String pattern) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Match.Pattern(NA" + ", \"" + dist + "\", \"" + pattern + "\")";
            RCenter.recordRCommand(RC, rCommand);
            if (RC.eval(rCommand).asInteger() == 1) {
                return true;
            } else {
                return false;
            }
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static boolean matchFeaturePattern(SessionBean1 sb, String dist, String featName) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "FeatureCorrelation(NA" + ", \"" + dist + "\", \"" + featName + "\")";
            RCenter.recordRCommand(RC, rCommand);
            if (RC.eval(rCommand).asInteger() == 1) {
                return true;
            } else {
                return false;
            }
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

    public static void plotMatchedFeatures(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCorr(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ptn", rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static String[] GetCorSigRowNames(SessionBean1 sb) {
        try {
            String rCommand = "GetCorSigRowNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetCorSigColNames(SessionBean1 sb) {
        try {
            String rCommand = "GetCorSigColNames(NA)";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getTempateNames(SessionBean1 sb) {
        try {
            String rCommand = "GenerateTemplates(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static void PlotCorrHeatMap(SessionBean1 sb, String imgName, String format, int dpi, String target, String smplDist,
            String colors, String viewOpt, String fix, String clst, String top, int topNum) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotCorrHeatMap(NA" + ", \"" + imgName + "\", \"" + format + "\", "
                    + dpi + ", width=NA, \"" + target + "\", \"" + smplDist + "\", \"" + colors + "\", \"" + viewOpt + "\", " + fix + ", "
                    + clst + ", " + top + ", " + topNum + ")";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    
// GET FILENAMES    
    public static String GetCorrSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetCorrSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String GetTtestSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetTtestSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String GetAnovaSigFileName(SessionBean1 sb) {
        try {
            String rCommand = "GetAnovaSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
}