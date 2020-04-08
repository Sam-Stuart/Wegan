/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
public class RIntegUtils {

    public static String[] getKeggPathLibNames2(RConnection RC) {
        try {
            return RC.eval("GetKEGG.PathNames2()").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static int performGeneMapping(RConnection RC, String geneList, String org, String idType) {
        try {
            String rCommand = "PerformIntegGeneMapping(NA, \"" + geneList + "\", \"" + org + "\", \"" + idType + "\");";

            // for local run, need to get the list from a txt file, not from web interface
            String rcmd1 = "geneListFile<-\"replace_with_your_file_name\"";
            RCenter.recordRCommand(RC, rcmd1);
            String rcmd2 = "geneList<-readChar(geneListFile, file.info(geneListFile)$size)";
            RCenter.recordRCommand(RC, rcmd2);
            String rcmd3 = "PerformIntegGeneMapping(NA, geneList, \"" + org + "\", \"" + idType + "\");";
            RCenter.recordRCommand(RC, rcmd3);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int performCmpdMapping(RConnection RC, String cmpdList, String org, String idType) {
        try {
            String rCommand = "PerformIntegCmpdMapping(NA" + ", \"" + cmpdList + "\", \"" + org + "\", \"" + idType + "\");";
            // for local run, need to get the list from a txt file, not from web interface
            String rcmd1 = "cmpdListFile<-\"replace_with_your_file_name\"";
            RCenter.recordRCommand(RC, rcmd1);
            String rcmd2 = "cmpdList<-readChar(cmpdListFile, file.info(cmpdListFile)$size)";
            RCenter.recordRCommand(RC, rcmd2);
            String rcmd3 = "PerformIntegCmpdMapping(NA, cmpdList, \"" + org + "\", \"" + idType + "\");";
            RCenter.recordRCommand(RC, rcmd3);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int performIntegPathwayAnalysis(RConnection RC, String topoOpt, String enrichOpt, String libOpt) {
        try {
            String rCommand = "PerformIntegPathwayAnalysis(NA" + ", \"" + topoOpt + "\", \"" + enrichOpt + "\", \"" + libOpt + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int removeCmpd(RConnection RC, int inx) {
        try {
            String rCommand = "RemoveCmpd(NA" + ", " + inx + ");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int removeGene(RConnection RC, int inx) {
        try {
            String rCommand = "RemoveGene(NA" + ", " + inx + ");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static int prepareIntegData(RConnection RC) {
        try {
            String rCommand = "PrepareIntegData(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static String[] getPathMapColNames(RConnection RC) {
        try {
            String rCommand = "GetPathMapColNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getPathMapRowNames(RConnection RC) {
        try {
            String rCommand = "GetPathMapRowNames()";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getPathMapMatrix(RConnection RC) {
        try {
            String rCommand = "GetPathMapMatrix()";
            return RC.eval(rCommand).asDoubleMatrix();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getIntegResColNames(RConnection RC) {
        try {
            String rCommand = "GetIntegResultColNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getIntegResMatrix(RConnection RC) {
        try {
            String rCommand = "GetIntegResultMatrix(NA)";
            return RC.eval(rCommand).asDoubleMatrix();

        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getIntegPathNames(RConnection RC) {
        try {
            String rCommand = "GetIntegResultPathNames(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getIntegPathIDs(RConnection RC) {
        try {
            String rCommand = "GetIntegResultPathIDs(NA)";
            String[] nms = RC.eval(rCommand).asStrings();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static double[][] getPathBarStats(RConnection RC) {
        try {
            String rCommand = "GetBarParams(NA)";
            return RC.eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] getPathBarNames(RConnection RC) {
        try {
            return RC.eval("GetBarNames(NA)").asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static String plotInmexPath(RConnection RC, String pathID, int width, int height) {
        try {
            String rCommand = "PlotInmexPath(NA" + ", \"" + pathID + "\", " + width + ", " + height + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
            return null;
        }
    }

    public static boolean drawKEGGGraph(RConnection RC, String imgName, double width, double height, double zoomPercent) {
        try {
            String rCommand = "RerenderKEGGGraph(NA" + ", \"" + imgName + "\"," + width + ", " + height + ", " + zoomPercent + ")";
            //RCenter.recordRCommand(RC, rCommand);
            int res = RC.eval(rCommand).asInteger();
            if (res == 1) {
                return true;
            }
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return false;
    }

}
