/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
public class RAnalUtils {

    public static int plotDataProfiles(RConnection RC, String dataName, String boxName, String pcaName) {
        try {
            String rCommand = "PlotDataProfile(\"" + dataName + "\", \"" + boxName + "\", \"" + pcaName + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static void plotSelectedFeature(RConnection RC, String symb) {
        try {
            String rCommand = "PlotSelectedFeature(NA" + ", \"" + symb + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static int performVoteCounting(RConnection RC, double sigLvl, double minVote) {
        try {
            String rCommand = "PerformVoteCounting(NA" + ", " + sigLvl + ", " + minVote + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int performPvalCombination(RConnection RC, String method, double sigLvl) {
        try {
            String rCommand = "PerformPvalCombination(NA" + ", \"" + method + "\", " + sigLvl + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int performMetaEffectSize(RConnection RC, String method, double sigLvl) {
        try {
            String rCommand = "PerformMetaEffectSize(\"" + method + "\", " + sigLvl + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static int performMetaMerge(RConnection RC, double sigLvl) {
        try {
            String rCommand = "PerformMetaMerge(NA" + ", " + sigLvl + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return 0;
    }

    public static void setAnalMethod(RConnection RC, String method) {
        try {
            String rCommand = "SetAnalMethod(\"" + method + "\")";
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    public static String getMetaGeneIDType(RConnection RC) {
        try {
            String rCommand = "GetMetaGeneIDType()";
            String nms = RC.eval(rCommand).asString();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMetaResColNames(RConnection RC) {
        try {
            String rCommand = "GetMetaResultColNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    //entrez ids
    public static String[] getMetaResGeneIDs(RConnection RC) {
        try {
            String rCommand = "GetMetaResultGeneIDs()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMetaResGeneSymbols(RConnection RC) {
        try {
            String rCommand = "GetMetaResultGeneSymbols()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getMetaResGeneIDLinks(RConnection RC) {
        try {
            String rCommand = "GetMetaResultGeneIDLinks()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    // the result matrix
    public static double[][] getMetaResMatrix(RConnection RC, String indFld) {
        try {
            String rCommand = "GetMetaResultMatrix(NA" + ", \"" + indFld + "\")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asDoubleMatrix();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

        public static int[] performLimmaDE(RConnection RC, String dataName, double sigLevel, double fcLvl) {
        try {
            String rCommand = "PerformLimmaDE(NA" + ", \"" + dataName + "\", " + sigLevel + ", " + fcLvl +");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
        }
        return new int[0];
    }
        
    public static int performIndNormalization(RConnection RC, String dataName, String opt, int autoOpt) {
        try {
            String rCommand = "PerformIndNormalization(NA" + ", \"" + dataName + "\", \"" + opt + "\", " + autoOpt + ");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int doNetEnrichmentTest(RConnection RC, String fileNm, String libNm, String IDs) {
        try {
            String rCommand = "PerformNetEnrichment(\"" + fileNm + "\", \"" + libNm + "\", \"" + IDs + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int doHeatmapEnrichmentTest(RConnection RC, String fileNm, String libNm, String IDs) {
        try {
            String rCommand = "PerformHeatmapEnrichment(\"" + fileNm + "\", \"" + libNm + "\", \"" + IDs + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

}
