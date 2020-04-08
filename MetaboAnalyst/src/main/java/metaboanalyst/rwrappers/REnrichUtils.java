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
public class REnrichUtils {

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static boolean hypergeomTest(RConnection RC) {
        try {
            String rCommand = "CalculateHyperScore(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static String[] getORAColorBar(RConnection RC) {
        try {
            return RC.eval("GetORA.colorBar(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getORARowNames(RConnection RC) {
        try {
            return RC.eval("GetORA.rowNames(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static double[][] getORAMat(RConnection RC) {
        try {
            return RC.eval("GetORA.mat(NA)").asDoubleMatrix();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static void doSSPTest(RConnection RC) {
        try {
            String rCommand = "CalculateSSP(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String[] getSSP_HMDB(RConnection RC) {
        try {
            return RC.eval("GetSSP.HMDB(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getSSPRefConcs(RConnection RC) {
        try {
            return RC.eval("GetSSP.RefConcs(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getSSPStates(RConnection RC) {
        try {
            return RC.eval("GetSSP.States(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getSSPdetails(RConnection RC) {
        try {
            return RC.eval("GetSSP.Details(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getSSPNames(RConnection RC) {
        try {
            return RC.eval("GetSSP.Names(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getSSPConcs(RConnection RC) {
        try {
            return RC.eval("GetSSP.Concs(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static boolean performGlobalTest(RConnection RC) {
        try {
            String rCommand = "CalculateGlobalTestScore(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger() == 1);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static String[] getQEAColorBar(RConnection RC) {
        try {
            return RC.eval("GetQEA.colorBar(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getQEARowNames(RConnection RC) {
        try {
            return RC.eval("GetQEA.rowNames(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static double[][] getQEAMat(RConnection RC) {
        try {
            return RC.eval("GetQEA.mat(NA)").asDoubleMatrix();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getHTMLMetSet(RConnection RC, String msetNm) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetHTMLMetSet(NA, \"" + msetNm + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getIntegHTMLPathSet(RConnection RC, String pathName) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetIntegHTMLPathSet(NA, \"" + pathName + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getHTMLPathSet(RConnection RC, String pathName) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetHTMLPathSet(NA, \"" + pathName + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getMummichogHTMLPathSet(RConnection RC, String pathName) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetMummichogHTMLPathSet(NA, \"" + pathName + "\")";
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getMetSetName(RConnection RC, int setInx) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetMetSetName(NA, " + setInx + ")";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getSMPDBImgName(RConnection RC, int setInx) {
        try {
            //remove the html tag
            //setNM = setNM.replaceAll("\\<.*?>", "");
            String rCommand = "GetSMPDBimg(NA, " + setInx + ")";
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    //assign query list to R, note, direct "<-" only pass reference, not value
    public static boolean doPathOraTest(SessionBean1 sb, String topoCode, String method) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CalculateOraScore(NA, \"" + topoCode + "\", \"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand);
            if (RC.eval(rCommand).asInteger() == 1) {
                return true;
            }
            return false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static String[] getORApathNames(RConnection RC) {
        try {
            return RC.eval("GetORA.pathNames(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getORAKeggIDs(RConnection RC) {
        try {
            return RC.eval("GetORA.keggIDs(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getORASMPDBIDs(RConnection RC) {
        try {
            return RC.eval("GetORA.smpdbIDs(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static boolean doPathQeaTest(SessionBean1 sb, String topoCode, String method) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "CalculateQeaScore(NA" + ", \"" + topoCode + "\", \"" + method + "\")";
            RCenter.recordRCommand(RC, rCommand);
            if (RC.eval(rCommand).asInteger() == 1) {
                return true;
            }
            return false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static String[] getQEApathNames(RConnection RC) {
        try {
            return RC.eval("GetQEA.pathNames(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getQEAKeggIDs(RConnection RC) {
        try {
            return RC.eval("GetQEA.keggIDs(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getQEASMPDBIDs(RConnection RC) {
        try {
            return RC.eval("GetQEA.smpdbIDs(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String plotCmpdConcRange(RConnection RC, String nm, String format, int dpi) {
        try {
            String rCommand = "PlotConcRange(NA, \"" + nm + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static void plotORA(SessionBean1 sb, String imgOpt, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotORA(NA" + ", \"" + imgName + "\", \"" + imgOpt + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ora", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void plotQEA(SessionBean1 sb, String imgOpt, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotQEA.Overview(NA" + ", \"" + imgName + "\", \"" + imgOpt + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("qea", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void prepareSifDownload(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PrepareSifDownloads(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static String plotQeaMset(SessionBean1 sb, String nm, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotQEA.MetSet(NA" + ", \"" + nm + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int readAdductData (RConnection RC, String adductList) {
        try {
            String rCommand = "Read.AdductData(NA" + ", \"" + adductList + "\",);";
            // for local run, need to get the list from a txt file, not from web interface
            String rcmd1 = "adductListFile<-\"replace_with_your_file_name\"";
            RCenter.recordRCommand(RC, rcmd1);
            String rcmd2 = "adductList<-readChar(adductListFile, file.info(adductListFile)$size)";
            RCenter.recordRCommand(RC, rcmd2);
            String rcmd3 = "Read.AdductData(NA, adductList);";
            RCenter.recordRCommand(RC, rcmd3);
            System.out.println(rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            System.out.println("failed");
        }
        return 0;
    }

    public static boolean performMummichog(SessionBean1 sb, String libOpt, String enrichOpt, String pvalOpt) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PerformMummichog(NA, \"" + libOpt + "\", \"" + enrichOpt + "\", \"" + pvalOpt + "\")";
            RCenter.recordRCommand(RC, rCommand);
            if (RC.eval(rCommand).asInteger() == 1) {
                return true;
            }
            return false;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static double[][] getMummiMat(RConnection RC) {
        try {
            return RC.eval("GetMummiResMatrix(NA)").asDoubleMatrix();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getMummiPathNames(RConnection RC) {
        try {
            return RC.eval("GetMummiResRowNames(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getMummiKeggIDs(RConnection RC) {
        try {
            return RC.eval("GetMummi.keggIDs(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static int set_Currency(RConnection RC, String userCurr) {
        try {
            String rCommand = "set_currency(NA, userCurr);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());

        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static String getCurrencyMsg(RConnection RC) {
        try {
            return RC.eval("GetCurrencyMsg(NA").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
    
    public static String getAdductMsg(RConnection RC) {
        try {
            return RC.eval("GetAdductMsg(NA").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

}
