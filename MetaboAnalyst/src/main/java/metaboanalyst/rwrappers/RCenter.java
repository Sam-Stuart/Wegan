/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import java.util.ArrayList;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author Jeff
 */
public class RCenter {

    // public static final String RserveIP = "129.128.246.14";
    public static final String RserveIP = "127.0.0.1"; //to be edited if not local
    public static final int Rport = 6311;    //define ports for Rserve, default 6311 is for MetaboAnalyst

    //return an RConnection object and specify the home directory
    public static RConnection getRConnection(String homeDir, String scriptPath, String moduleNm) {

        try {
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            String rCommand = "setwd(\"" + homeDir + "\")\n"
                    + "unlink(dir(), recursive=T)\n"
                    + "file.create(\"Rhistory.R\")\n";
            RC.voidEval(rCommand);
            //loadRscripts(RC, DataUtils.getFileNames(scriptPath, true));
            RC.voidEval("source(\"" + scriptPath + "\")");
            RC.voidEval("LoadScripts(\"" + moduleNm + "\")");
            return RC;
        } catch (RserveException e) {
            return null;
        }
    }

    public static boolean compileRScripts(String homeDir, String scriptPath) {
        try {
            //System.out.println("=============" + homeDir);
            RConnection RC = new RConnection(RCenter.RserveIP, RCenter.Rport);
            RC.voidEval("print(getwd());");
            RC.voidEval("setwd(\"" + homeDir + "\"); source(\"" + scriptPath + "\")");
            String res = RC.eval("CompileScripts()").asString();
            return res.equals("TRUE");
        } catch (Exception e) {
            return false;
        }
    }

    public static String getRCWorkingDir(RConnection RC) {
        String msg = null;
        try {
            msg = RC.eval("getwd()").asString();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return msg;
    }

    //return a name exist in the current working dir
    public static boolean isHomeDirEmpty(RConnection RC) {
        try {
            return RC.eval("length(dir())>0").asString().equalsIgnoreCase("FALSE");
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static boolean isFileExist(RConnection RC, String fileName) {
        try {
            return RC.eval("file.exists(\"" + fileName + "\")").asString().equalsIgnoreCase("TRUE");
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    //return data files exist in the current working dir
    public static ArrayList<String> getExistingFileNames(RConnection RC) {
        try {
            String[] files = RC.eval("dir()").asStrings();
            ArrayList<String> fileVec = new ArrayList();
            for (int i = 0; i < files.length; i++) {
                String fileName = files[i];
                if (fileName.endsWith(".png")
                        || //    fileName.endsWith(".pdf") ||
                        fileName.endsWith(".R")
                        || fileName.endsWith(".csv")) {
                    fileVec.add(fileName);
                }
            }
            return fileVec;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    // clean all files in the current dir
    // create new Rhistory file
    public static void clearUserDir(RConnection RC) {
        try {
            String rCommand = "ClearUserDir(NA)";
            RC.voidEval(rCommand);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //this should be the better one
    public static void recordRCommand(RConnection RC, String rCommand, Boolean asIs) {
        try {
            rCommand = cleanRCmd(rCommand);
            if (asIs) {
                RC.voidEval("RecordRCommand(NA" + ", \"" + rCommand + "\")");
            } else {
                recordRCommand(RC, rCommand);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //this should be the better one
    public static void saveCurrentSession(RConnection RC) {
        try {
            RC.voidEval("SaveCurrentSession()");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void recordRCommand(RConnection RC, String rCommand) {
        //public static void recordRCommand(SessionBean1 sb, String rCommand) {
        try {
            rCommand = cleanRCmd(rCommand);
            //for local MetaboAnalystR package
            if (rCommand.contains("(NA")) {
                rCommand = rCommand.replace("(NA", "(mSet");
                rCommand = "mSet<-" + rCommand;
            }
            if (rCommand.contains("InitDataObjects")) {
                rCommand = "mSet<-" + rCommand;
            }
            RC.voidEval("RecordRCommand(NA" + ", \"" + rCommand + "\")");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static String cleanRCmd(String rCommand) {
        int inx = 0;
        while (rCommand.indexOf("\"", inx) != -1) {
            inx = rCommand.indexOf("\"", inx);
            StringBuffer strB = new StringBuffer(rCommand).insert(inx, "\\");
            rCommand = strB.toString();
            inx = inx + 2; //move one step, plus the position of backslash
        }
        return rCommand;
    }

    public static String[] getRCommandHistory(RConnection RC) {
        try {
            return RC.eval("GetRCommandHistory(NA)").asStrings();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void loadReporterFuns(RConnection RC, String module) {
        try {
            String rCommand = "LoadReporter(\"" + module + "\");";
            RC.voidEval(rCommand);
        } catch (Exception e) {
        }
    }

    public static boolean prepareReport(RConnection RC, String usrName) {
        try {
            if (usrName.endsWith("tmp")) {
                usrName = usrName.substring(0, usrName.length() - 3);
            }
            String rCommand = "PreparePDFReport(NA" + ", \"" + usrName + "\")\n";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            return true;
        } catch (Exception e) {//Catch exception if any
            System.err.println("Error: " + e.getMessage());
            return false;
        }
    }

    public static String getBashFullPath(RConnection RC) {
        try {
            return RC.eval("GetBashFullPath()").asString();
        } catch (Exception rse) {
            rse.printStackTrace();
            return null;
        }
    }

    public static void showMemoryUsage(RConnection RC) {
        try {
            String rCommand = "ShowMemoryUse();";
            RC.voidEval(rCommand);
            //compound names for hypergeometric test
            //RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
        }
    }

    //load certain heavy but rarely used functions 
    public static void loadRareFunctions(RConnection RC, String nm) {
        try {
            String rCommand = "LoadRareFunctions(\"" + nm + "\");";
            RC.voidEval(rCommand);
            //compound names for hypergeometric test
            //RCenter.recordRCommand(RC, rCommand);
        } catch (Exception e) {
        }
    }
}
