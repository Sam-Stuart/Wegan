/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author soufanom
 */
public class RNetworkUtils {

    //Network methods
    public static int performKOmapping(RConnection RC, String geneList, String type) {
        try {
            String rCommand = "PerformKOmapping(\"" + geneList + "\", \"" + type + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static int doEnrichmentTest_KO01100(RConnection RC, String dbType, String fileNm) {
        try {
            String rCommand = "PerformKOEnrichAnalysis_KO01100(NA" + ", \"" + dbType + "\", \"" + fileNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asInteger();
        } catch (Exception e) {
        }
        return 0;
    }

    public static void prepareQueryJson(RConnection RC) {
        try {
            String rCommand = "PrepareQueryJson(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (Exception rse) {
            System.out.println(rse);
        }
    }

    //parameter for network
    public static int prepareNetworkData(RConnection RC) {
        try {
            String rCommand = "PrepareNetworkData(NA);";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static String getShortestPaths(RConnection RC, String from, String to) {
        try {
            String rCommand = "GetShortestPaths(\"" + from + "\", \"" + to + "\")";
            String nms = RC.eval(rCommand).asString();
            return nms;
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int[] searchNetDB(RConnection RC, String dbType, String dbName, Double minScore) {
        try {
            String useExp = "FALSE";
            String rCommand = "SearchNetDB(NA" + ", \"" + dbType + "\", \"" + dbName + "\", " + useExp + ", " + minScore + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return new int[0];
    }

    public static int[] createIGraph(RConnection RC) {
        try {
            String rCommand = "CreateGraph(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int prepareNetwork(RConnection RC, String netNm, String jsonNm) {
        try {
            return RC.eval("PrepareNetwork(\"" + netNm + "\", \"" + jsonNm + "\")").asInteger();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return (0);
    }

    public static String[] getNetsNames(RConnection RC) {
        try {
            String rCommand = "GetNetsName();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static String getNetsNamesString(RConnection RC) {
        try {
            String rCommand = "GetNetsNameString();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
        }
        return null;
    }

    //return the number of components
    public static String detectCommunities(RConnection RC, String method) {
        try {
            String rCommand = "FindCommunities(\"" + method + "\");";
            RCenter.recordRCommand(RC, rCommand);
            String coms = RC.eval(rCommand).asString();
            return coms;
        } catch (Exception e) {
        }
        return "NA";
    }

    public static String[] getNodeIDs(RConnection RC) {
        try {
            String rCommand = "GetNodeIDs();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static String[] getNodeNames(RConnection RC) {
        try {
            String rCommand = "GetNodeNames();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
        }
        return null;
    }

    public static int[] getNodeDegrees(RConnection RC) {
        try {
            String rCommand = "GetNodeDegrees();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
        }
        return null;
    }

    public static double[] getNodeBetweenness(RConnection RC) {
        try {
            String rCommand = "GetNodeBetweenness();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asDoubles();
        } catch (Exception e) {
        }
        return null;
    }

    public static String getNodeEntrezIDs(RConnection RC, String unipID) {
        try {
            String rCommand = "GetNodeEntrezIDs(\"" + unipID + "\");";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
        }
        return null;
    }

    public static String getNodeEmblEntrezIDs(RConnection RC, String emblprotein) {
        try {
            String rCommand = "GetNodeEmblEntrezIDs(\"" + emblprotein + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asString();
        } catch (Exception e) {
        }
        return null;
    }

    public static int[] getNetsNodeNum(RConnection RC) {
        try {
            String rCommand = "GetNetsNodeNum();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
        }
        return null;
    }

    public static int[] getNetsEdgeNum(RConnection RC) {
        try {
            String rCommand = "GetNetsEdgeNum();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
        }
        return null;
    }

    public static int[] getNetsQueryNum(RConnection RC) {
        try {
            String rCommand = "GetNetsQueryNum();";
            //RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asIntegers();
        } catch (Exception e) {
        }
        return null;
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

    public static String excludeNodes(RConnection RC, String nodeIDs, String fileNm) {
        try {
            String rCommand = "ExcludeNodes(\"" + nodeIDs + "\", \"" + fileNm + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String updateNetworkLayout(RConnection RC, String algo, String jsonNm) {
        try {
            return RC.eval("UpdateNetworkLayout(\"" + algo + "\", \"" + jsonNm + "\")").asString();
        } catch (Exception rse) {
            rse.printStackTrace();
            return null;
        }
    }

    public static void exportNetwork(RConnection RC, String name) {
        try {
            RC.voidEval("ExportNetwork(\"" + name + "\")");
        } catch (Exception rse) {
            rse.printStackTrace();
        }
    }

    public static String extractModule(RConnection RC, String nodeIDs) {
        try {
            String rCommand = "ExtractModule(\"" + nodeIDs + "\")";
            return RC.eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static int updateIntegPathwayAnalysis(RConnection RC, String qlist, String fileNm, String topoOpt, String enrichOpt, String libOpt) {
        try {
            String rCommand = "UpdateIntegPathwayAnalysis(NA" + ", \"" + qlist + "\", \"" + fileNm + "\", \"" + topoOpt + "\", \"" + enrichOpt + "\", \"" + libOpt + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
        }
        return 0;
    }

    public static void prepareSubnetDownload(RConnection RC, String name) {
        try {
            String rCommand = "PrepareSubnetDownloads(\"" + name + "\")";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
}
