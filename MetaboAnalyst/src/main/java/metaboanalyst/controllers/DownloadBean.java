/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers;

import java.io.File;
import java.io.FilenameFilter;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import metaboanalyst.controllers.ordination.OARDABean;
import metaboanalyst.models.ResultBean;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "downloader")
public class DownloadBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private ResultBean[] downloads;

    public ResultBean[] getDownloads() {
        return downloads;
    }
    
    public void setupDownloadTable() {

        //to prevent ajax call trigger this event
        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }

        User usr = sb.getCurrentUser();
        String usrName = usr.getName();
        RConnection RC = sb.getRConnection();
        RDataUtils.saveAllData(RC);

        File folder = new File(usr.getHomeDir());
        DataUtils.deleteFile(usr, "Download.zip");
        File[] listOfFiles = folder.listFiles(new OnlyExt(true));

        if (listOfFiles.length == 0) {
            downloads = new ResultBean[1];
            downloads[0] = new ResultBean("Empty Folder", "");
        } else {
            // first create zip files
            DataUtils.createZipFile(listOfFiles, usr.getHomeDir());

            ArrayList<String> fileNames = new ArrayList();
            fileNames.add("Download.zip");

            listOfFiles = folder.listFiles(new OnlyExt(false));

            for (int i = 0; i < listOfFiles.length; i++) {
                fileNames.add(listOfFiles[i].getName());
            }

            reportURL = null;

            if (fileNames.contains("Rhistory.R")) {
                fileNames.remove("Rhistory.R");
                fileNames.add(1, "Rhistory.R");
            }
            int fileSize = fileNames.size();
            boolean added = false;
            if (fileSize % 2 > 0) {
                fileSize = fileSize + 1;
                added = true;
            }

            int rowNum = fileSize / 2;
            downloads = new ResultBean[rowNum];
            String fileNMA, fileNMB;
            for (int i = 0; i < rowNum; i++) {
                fileNMA = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileNames.get(i) + "\">" + fileNames.get(i) + "</a>";
                if (i == rowNum - 1 && added) {
                    fileNMB = "";
                } else {
                    fileNMB = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileNames.get(rowNum + i) + "\">" + fileNames.get(rowNum + i) + "</a>";
                }
                downloads[i] = new ResultBean(fileNMA, fileNMB);
            }
        }
    }

    private String reportURL = null;

    public String getReportURL() {
        return reportURL;
    }

    public void setReportURL(String reportURL) {
        this.reportURL = reportURL;
    }

    //try to redo it using bash, assuming Analysis_Report.tex exists
    public void generateReport() {
        //System.out.println("========== download file number:2 ");
        //if (reportURL == null) {
        //   System.out.println("======= regenerating reports");
        //first remove the file in case
        User usr = sb.getCurrentUser();
        RConnection RC = sb.getRConnection();
        //DataUtils.deleteFile(usr, "Analysis_Report.pdf");

        //loading R functions for sweave report generation
        setupPDFImages();
        RCenter.loadReporterFuns(sb.getRConnection(), sb.getAnalType());
        RCenter.prepareReport(RC, usr.getName());

        //now we always use Bash external rather than RSweave (due to fatigue reason?)
        RCenter.saveCurrentSession(sb.getRConnection());
        String cmdPath = RCenter.getBashFullPath(sb.getRConnection());
        String rScriptHome = ab.getRscriptsPath();
        String userDir = usr.getHomeDir();
        boolean res = DataUtils.generateReportCMD(cmdPath, rScriptHome, userDir);

        //File f = new File(userDir + "/Analysis_Report.pdf");
        //if (f.exists()) {
        if (res) {
            reportURL = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usr.getName() + "/Analysis_Report.pdf" + "\">Analysis Report</a>";
        } else {
            reportURL = "";
            sb.updateMsg("Error", "Unknown error happened during report generation.");
        }
        //}
    }

    public void setupPDFImages() {
        HashMap<String, String> graphicsMap = sb.getGraphicsMap();
        Iterator keyIt = graphicsMap.keySet().iterator();
        String key, rcmd;
        RConnection RC = sb.getRConnection();
        while (keyIt.hasNext()) {
            key = keyIt.next().toString();
            rcmd = graphicsMap.get(key);
            rcmd = rcmd.replace("png", "pdf");
            rcmd = rcmd.replace("width=NA", "width=0");
            try {
                //this is workaround to make around for Rserve bugs, to make sure all device are closed
                //the theory is that if plot error, the dev.off() may not be called
                //RCenter.recordRCommand(RC, rcmd);
                RC.voidEval(rcmd);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public class OnlyExt implements FilenameFilter {

        private boolean showAll = false;

        public OnlyExt(boolean isall) {
            showAll = isall;
        }

        public boolean accept(File dir, String name) {
            if (showAll) {
                return name.endsWith(".csv")
                        || name.endsWith(".png")
                        || name.endsWith(".txt")
                        || name.endsWith(".R") //return R history
                        || name.endsWith(".pdf");
            } else {
                return name.endsWith(".csv")
                        || name.endsWith(".png")
                        || name.endsWith(".txt")
                        || name.endsWith(".R")
                        || name.endsWith("Report.pdf");
            }
        }
    }
}
