/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import metaboanalyst.models.NameMapBean;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.SearchUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "utilBean")
@SessionScoped
public class UtilsBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String dataOpt = "row";

    public String getDataOpt() {
        return dataOpt;
    }

    public void setDataOpt(String dataOpt) {
        this.dataOpt = dataOpt;
    }

    private UploadedFile batchFile;

    public UploadedFile getBatchFile() {
        return batchFile;
    }

    public void setBatchFile(UploadedFile batchFile) {
        this.batchFile = batchFile;
    }

    private String dataName = "";

    public String getDataName() {
        return dataName;
    }

    public void setDataName(String dataName) {
        this.dataName = dataName;
    }
    private String isomerOpt = "y";

    public String getIsomerOpt() {
        return isomerOpt;
    }

    public void setIsomerOpt(String isomerOpt) {
        this.isomerOpt = isomerOpt;
    }

    private UploadedFile lipidFile;

    public UploadedFile getLipidFile() {
        return lipidFile;
    }

    public void setLipidFile(UploadedFile lipidFile) {
        this.lipidFile = lipidFile;
    }

    private String utilOpt = "convert";

    public String getUtilOpt() {
        return utilOpt;
    }

    public void setUtilOpt(String utilOpt) {
        this.utilOpt = utilOpt;
    }

    public String batchURL = "";

    public String getBatchURL() {
        return batchURL;
    }

    public String setUtilOpt() {
        if (sb.doLogin("NA", "utils", false, false)) {
            switch (utilOpt) {
                case "convert":
                    return "ID Input";
                case "lipid":
                    return "Lipidomics";
                case "batch":
                    return "Batch Upload";
            }
        }
        sb.updateMsg("Error", "Failed to initiate the service");
        return null;
    }

    public void performBatchUpload() {

        resultInfo = "";
        if (batchFile.getSize() == 0) {
            sb.updateMsg("Error", "File is empty");
            return;
        }

        if (!(batchFile.getFileName().endsWith(".csv") || batchFile.getFileName().endsWith(".txt"))) {
            sb.updateMsg("Error", "Only comma separated format (*.csv) or tab delimited (.txt) will be accepted!");
            return;
        }

        RConnection RC = sb.getRConnection();

        String label = dataName;

        String fileName = DataUtils.uploadFile(batchFile, sb, null, ab.isOnPublicServer());
        if (label.equalsIgnoreCase("")) {
            label = fileName.substring(0, fileName.length() - 4);
        }
        dataName = ""; // reset the name
        if (RDataUtils.readBatchCSVData(RC, fileName, dataOpt, label)) {
            String msg = "Data <u>" + fileName + "</u> was uploaded successfully!"
                    + " It is renamed to <u>" + label + "</u>. "
                    + "You can click the <b>Finish</b> button to process your data "
                    + " or click the <b>Upload</b> button to upload more data.";
            sb.updateMsg("OK", msg);
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.updateMsg("Error", "Failed to read in the CSV file." + err);
        }
    }

    public void resetBatchData() {
        RDataUtils.resetBatchData(sb.getRConnection());
    }

    public String performBatchCorrection() {
        String[] names = RDataUtils.GetBatchNames(sb.getRConnection());
        if (names.length < 2) {
            sb.updateMsg("Error", "At least two batches are required!");
            return null;
        }

        if (RDataUtils.performBatchCorrection(sb.getRConnection(), sb.getNewImage("pca_batch"))) {
            batchURL = "<b>You can download the corrected data </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                    + "/MetaboAnalyst_batch_data.csv\"><b>" + "here" + "</b></a>";
            return ("Batch View");
        } else {
            sb.updateMsg("Error", "Failed to perform the procedure!");
            return null;
        }
    }

    public String getBatchImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("pca_batch") + "dpi72.png";
    }

    private String[] selectedIDs;

    public String[] getSelectedIDs() {
        return selectedIDs;
    }

    public void setSelectedIDs(String[] selectedIDs) {
        this.selectedIDs = selectedIDs;
    }

    private String inputType;

    public String getInputType() {
        return inputType;
    }

    public void setInputType(String inputType) {
        this.inputType = inputType;
    }

    private String queryList = null;

    public String getQueryList() {
        return queryList;
    }

    public void setQueryList(String queryList) {
        this.queryList = queryList;
    }

    private UploadedFile nmFile;

    public UploadedFile getNmFile() {
        return nmFile;
    }

    public void setNmFile(UploadedFile nmFile) {
        this.nmFile = nmFile;
    }

    public String performNameMapping() {

        String[] qVec = null;
        if (queryList != null && queryList.trim().length() > 0) {
            qVec = DataUtils.getQueryNames(queryList, null);
        } else if (nmFile.getSize() > 0) {
            qVec = DataUtils.getQueryNames(nmFile, null);
        } else {
            sb.updateMsg("Error", "Empty input?");
            return null;
        }

        RConnection RC = sb.getRConnection();
        try {
            RDataUtils.setMapData(RC, qVec);
            SearchUtils.crossReferenceExactGeneral(RC, inputType);
            setupNameMaps();
            return "Map Result";
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }

    }

    private String resultInfo = "";

    public String getResultInfo() {
        return resultInfo;
    }

    public void setResultInfo(String resultInfo) {
        this.resultInfo = resultInfo;
    }

    public void performLipidAnalysis() {

        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        resultInfo = "";
        if (lipidFile.getSize() == 0) {
            sb.updateMsg("Error", "File is empty");
            return;
        }

        if (!lipidFile.getFileName().endsWith(".csv")) {
            sb.updateMsg("Error", "Only comma separated format (*.csv) will be accepted!");
            return;
        }

        RConnection RC = sb.getRConnection();
        String fileName = DataUtils.uploadFile(lipidFile, sb, null, ab.isOnPublicServer());
        try {
            String rcmd = "analyze.lipids(\"" + fileName + "\", iso=\"" + isomerOpt + "\")";
            RCenter.recordRCommand(RC, rcmd, Boolean.TRUE);
            RC.voidEval(rcmd);
            setupFileDownloadTable(sb.getCurrentUser().getHomeDir());
        } catch (Exception e) {
            sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
        }

    }

    private void setupFileDownloadTable(String homeDir) {
        User usr = sb.getCurrentUser(); // BHAN: added for deleteFile
        ArrayList<String> fileNames = new ArrayList();
        String tmpName = DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir());
        File folder = new File(homeDir);
        DataUtils.deleteFile(usr, "Download.zip"); // BHAN, added because it makes diskfull
        File[] listOfFiles = folder.listFiles();

        DataUtils.createZipFile(listOfFiles, homeDir);

        for (File listOfFile : listOfFiles) {
            fileNames.add(listOfFile.getName());
        }

        fileNames.add(0, "Download.zip");

        String str = "<table width=\"250\" border=\"1\" cellpadding=\"3\">";
        for (int i = 0; i < fileNames.size(); i++) {
            str = str + "<tr><td>" + "<a href = \"/MetaboAnalyst/resources/users/" + tmpName + File.separator + fileNames.get(i) + "\">" + fileNames.get(i) + "</a>" + "</td></tr>";
        }
        str = str + "</table>";
        str = str + "<p> "
                + "Note: the \"Download.zip\" contains all the data. The \"_uplimit.csv\" contains the upper limit concentrations "
                + "for each corresponding lipid class. The \"_prob.csv\" contains the most probable conetration values."
                + "</p>";
        resultInfo = str;
    }

    private NameMapBean[] nameMaps = null;

    public NameMapBean[] getNameMapResult() {
        return nameMaps;
    }

    private String downloadMsg = "";

    public String getDownloadMsg() {
        return downloadMsg;
    }

    public void setDownloadMsg(String downloadMsg) {
        this.downloadMsg = downloadMsg;
    }

    private void setupNameMaps() {
        RConnection RC = sb.getRConnection();
        String[] mapRes = SearchUtils.getNameMapTable(RC);
        int row_count = SearchUtils.getNameMapRowNumber(RC);

        nameMaps = new NameMapBean[row_count];
        NameMapBean nameMap;
        for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
            nameMap = new NameMapBean();

            //walk through in order
            int count = 0;
            nameMap.setQuery(mapRes[i + count * row_count]);

            count++;
            nameMap.setHit(mapRes[i + count * row_count]);

            count++;
            nameMap.setHmdb_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setPubchem_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setChebi_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setKegg_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setMetlin_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setDetails(mapRes[i + count * row_count]);
            nameMaps[i] = nameMap;
        }
        downloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private NameMapBean[] candidateMaps;
    private String preTarget = "";

    public NameMapBean[] getCandidateMaps() {
        if (!target.equals(preTarget)) {
            RConnection RC = sb.getRConnection();
            SearchUtils.performDetailSearch(RC, target);
            String[] canList = SearchUtils.getCandidateList(RC);
            int row_count = SearchUtils.getCanListRowNumber(RC);
            candidateMaps = new NameMapBean[row_count];

            for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
                NameMapBean nameMap = new NameMapBean();
                //walk through in order
                nameMap.setQuery(target);
                int count = 0;
                nameMap.setHit(canList[i + count * row_count]);
                count++;
                nameMap.setHmdb_id(canList[i + count * row_count]);
                count++;
                nameMap.setPubchem_id(canList[i + count * row_count]);
                count++;
                nameMap.setChebi_id(canList[i + count * row_count]);
                count++;
                nameMap.setKegg_id(canList[i + count * row_count]);
                count++;
                nameMap.setMetlin_id(canList[i + count * row_count]);

                candidateMaps[i] = nameMap;
            }
            preTarget = target;
        }
        return candidateMaps;
    }

    private String selectedHit;

    public String getSelectedHit() {
        return selectedHit;
    }

    public void setSelectedHit(String selectedHit) {
        this.selectedHit = selectedHit;
    }

    private String target = "";

    public String getTarget() {
        return target;
    }

    public void setCurrentCmpd(String cmpd) {
        target = DataUtils.getStringHTMLTag(cmpd);
    }

    public void selectCandidate() {
        for (NameMapBean candidateMap : candidateMaps) {
            if (candidateMap.isSelected() && !(candidateMap.getHit().equals("None of the above") || candidateMap.getHit().equals(""))) {
                int current_hitInx = SearchUtils.setCandidate(sb.getRConnection(), target, candidateMap.getHit());
                nameMaps[current_hitInx - 1] = candidateMap;
                return;
            }
        }
    }
}
