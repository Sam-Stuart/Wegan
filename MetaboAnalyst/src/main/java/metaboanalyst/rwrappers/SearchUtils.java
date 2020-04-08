/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Jeff
 */
public class SearchUtils {

    public static void searchLibrary(RConnection RC, String query, String type) {
        try {
            String rCommand = "SearchMsetLibraries(NA" + ", \"" + query + "\",\"" + type + "\");";
            RC.eval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String[] searchMsetByName(RConnection RC, String query) {
        try {
            String rCommand = "SearchMsetLibByName(\"" + query + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getMsetLibSearchResult(RConnection RC) {
        try {
            String rCommand = "GetMsetLibSearchResult(NA);";
            String[] rawResults = RC.eval(rCommand).asStrings();
            RCenter.recordRCommand(RC, rCommand);
            return rawResults;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void crossReferenceExactGeneral(RConnection RC, String query_type) {
        try {
            String rCommand = "CrossReferencing(NA" + ", \"" + query_type + "\", T, T, T, T, T" + ");";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void crossReferenceExact(RConnection RC, String query_type) {
        try {
            String rCommand = "CrossReferencing(NA" + ", \"" + query_type + "\");";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //when editor edit the library, the metset.table need to be updated accordingly
    public static String[] getNameMapTable(RConnection RC) {
        try {
            String rCommand = "CreateMappingResultTable(NA)";
            String[] resTable = RC.eval(rCommand).asStrings();
            RCenter.recordRCommand(RC, rCommand);
            return resTable;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String[] getGeneNameMapTable(RConnection RC) {
        try {
            String rCommand = "GetGeneMappingResultTable(NA)";
            String[] resTable = RC.eval(rCommand).asStrings();
            // RCenter.recordRCommand(RC, rCommand);
            return resTable;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
    
    // Prepares table matchings of query genes with KEGG orthologs
    public static String[] getNetworkGeneNameMapTable(RConnection RC) {
        try {
            String rCommand = "GetNetworkGeneMappingResultTable(NA)";
            String[] resTable = RC.eval(rCommand).asStrings();
            RCenter.recordRCommand(RC, rCommand);
            return resTable;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    //when editor edit the library, the metset.table need to be updated accordingly
    public static int getNameMapRowNumber(RConnection RC) {
        try {
            return RC.eval("GetHitsRowNumber(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    //when editor edit the library, the metset.table need to be updated accordingly
    public static int getGeneNameMapRowNumber(RConnection RC) {
        try {
            return RC.eval("GetGeneHitsRowNumber(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
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

    //when editor edit the library, the metset.table need to be updated accordingly
    public static int getCanListRowNumber(RConnection RC) {
        try {
            return RC.eval("GetCanListRowNumber(NA)").asInteger();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    public static String[] getCandidateList(RConnection RC) {
        try {
            String rCommand = "GetCandidateList(NA);";
            String[] rawResults = RC.eval(rCommand).asStrings();

            RCenter.recordRCommand(RC, rCommand);
            return rawResults;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static int setCandidate(RConnection RC, String qNm, String cNm) {
        try {
            String rCommand = "SetCandidate(NA" + ", \"" + qNm + "\", \"" + cNm + "\");";
            RCenter.recordRCommand(RC, rCommand);
            return (RC.eval(rCommand).asInteger());
        } catch (Exception e) {
            e.printStackTrace();
            return -1;
        }
    }

    public static String getQuery(RConnection RC, int inx) {
        try {
            return (RC.eval("GetQuery(NA" + ", " + inx + ")").asString());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static String getBestHit(RConnection RC) {
        try {
            return (RC.eval("getBestHit(NA)").asString());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
}
