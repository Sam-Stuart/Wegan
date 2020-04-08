/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.meta;

import metaboanalyst.models.DataModel;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.FacesContext;
import metaboanalyst.models.User;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "loadBean")
@SessionScoped
public class MetaLoadBean implements Serializable{

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    RConnection RC;
    /**
     * Record the currently selected data
     */

    private int dataNum = 1;

    public int getDataNum() {
        return dataNum;
    }

    public void setDataNum(int dataNum) {
        this.dataNum = dataNum;
    }

    private String dataFormat = "colu";

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    private String selectedTestData = "Test1";

    public String getSelectedTestData() {
        return selectedTestData;
    }

    public void setSelectedTestData(String selectedTestData) {
        this.selectedTestData = selectedTestData;
    }

    private DataModel selectedData;

    public DataModel getSelectedData() {
        return selectedData;
    }

    public void setSelectedData(DataModel selectedData) {
        this.selectedData = selectedData;
    }

    public DataModel getData4Vis() {
        return selectedData;
    }

    public void setData4Vis(DataModel selectedData) {
        this.selectedData = selectedData;
        selectedData.plotDataProfile();
    }

    private boolean allDataConsistent = false;

    public boolean isAllDataConsistent() {
        return allDataConsistent;
    }

    public void setAllDataConsistent(boolean allDataConsistent) {
        this.allDataConsistent = allDataConsistent;
    }
    private String integCheckMsg;

    public String getIntegCheckMsg() {
        return integCheckMsg;
    }

    private String analMethod;

    public String getAnalMethod() {
        return analMethod;
    }

    public void setAnalMethod(String analMethod) {
        this.analMethod = analMethod;
    }

    private DataModel selectedDataSet;

    public DataModel getSelectedDataSet() {
        return selectedDataSet;
    }

    public void setSelectedDataSet(DataModel dm) {
        if (!selectedDataSet.getName().equals(dm.getName())) {
            this.selectedDataSet = dm;
            RDataUtils.setCurrentData(RC, selectedDataSet.getName());
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "OK",
                            "Current data is: " + selectedDataSet.getName() + ", ready for analysis."));
        }
    }

    private int currentDeNum = 0;

    public int getCurrentDeNum() {
        return currentDeNum;
    }

    public void setCurrentDeNum(int currentDeNum) {
        this.currentDeNum = currentDeNum;
    }

    private boolean statsOnlyMode = false;

    public boolean isStatsOnlyMode() {
        return statsOnlyMode;
    }

    public void setStatsOnlyMode(boolean statsOnlyMode) {
        this.statsOnlyMode = statsOnlyMode;
    }

    public void addData() {
        addNewData("Upload");
    }

    public void deleteData() {
        deleteData(selectedData);
    }

    /**
     * Handle file upload Use simple mode, as other fancy mode does not work in
     * all browsers (IE)
     */
    private UploadedFile file;

    public UploadedFile getFile() {
        return file;
    }
    private int count;

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    public void increment() {
        count++;
    }

    public void setFile(UploadedFile file) {
        this.file = file;
    }

    private void checkLogIn() {
        if (!loggedIn) {
            sb.doLogin("conc", "metadata", false, false);
            RC = sb.getRConnection();

            //need to update all dataSets created for meta and multi
            List<DataModel> dataSets = getDataSets();
            for (int i = 0; i < dataSets.size(); i++) {
                dataSets.get(i).setRC(RC);
            }
            loggedIn = true;
        }

    }
    private boolean loggedIn = false;

    public boolean handleFileUpload() {

        if (file == null || file.getSize() == 0) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Empty file?"));
            return false;
        }

        if (ab.isOnPublicServer()) { // size limit will apply only on public server
            if (file.getSize() > 50000000) {
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "The file size exceeds limit: 50M"));
                file = null;
                return false;
            }
        }

        String fileName = file.getFileName();
        if (!fileName.endsWith(".csv") && !fileName.endsWith(".txt")) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Only .txt or .csv file is acceptable!"));
            file = null;
            return false;
        }

        checkLogIn();

        fileName = DataUtils.uploadFile(file, sb, null, ab.isOnPublicServer());
        int res = RDataUtils.readIndExpressTable(RC, fileName, dataFormat);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res == 0) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error:", msg));
            //remove the file
            file = null;
            return false;
        } else {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", fileName + " is uploaded and parsed out." + msg));
            selectedData.setUploaded(true);
            selectedData.setName(fileName);
            selectedData.processMetaData();
            file = null;
            return true;
        }

    }

    private boolean adjustBatch = false;

    public boolean isAdjustBatch() {
        return adjustBatch;
    }

    public void setAdjustBatch(boolean adjustBatch) {
        this.adjustBatch = adjustBatch;
    }

    public void performMetaIntegrityCheck() {
        allDataConsistent = false;
        //first get datasets
        ArrayList<String> selectedDataSets = getCurrentDataSets();
        integCheckMsg = "";

        if (selectedDataSets.isEmpty()) {
            integCheckMsg = "Integrity Check Failed. No data was found.";
            return;
        }

        //check if the summary view is checked
        for (String selectedDataSet : selectedDataSets) {
            DataModel dm = getData(selectedDataSet);

            setStatsOnlyMode(true);

            if (!dm.isAllDone()) {
                integCheckMsg = "Integrity Check Failed. Please specify or confirm group comparison for data: " + dm.getFullName()
                        + " using the <b>View</b> option under <u>Data Summary</u>";
                return;
            }
        }
        if (RDataUtils.setSelectedDataNames(RC, selectedDataSets.toArray(new String[0])) == 0) {
            integCheckMsg = "Integrity Check Failed. Cannot locate the selected data!";
            return;
        }
        int res = RDataUtils.checkMetaDataConsistency(RC, adjustBatch ? "T" : "F");
        String msg = RDataUtils.getCurrentMsg(RC);
        switch (res) {
            case 0:
                integCheckMsg = "Integrity Check Failed. " + msg;
                break;
            case -1:
                integCheckMsg = "Integrity Check Failed. " + msg + ". You can use the Annotation "
                        + "cell to edit group labels.";
                break;
            default:
                allDataConsistent = true;
                if (res == 1) {
                    setSingleMode(false);
                    integCheckMsg = "OK, all datasets passed intergrity check. "
                            + "Click <b>Next</b> button to next page.";
                } else {
                    setSingleMode(true);
                    integCheckMsg = "OK, the dataset passed intergrity check. "
                            + "Click <b>Next</b> button to next page. "
                            + "Note, only a single dataset is found. "
                            + "You can only perform regular differential expression analysis "
                            + "using <b><u>Direct Merging</u></b> method.";
                }
                break;
        }
        //meta analysis default all selected

        RDataUtils.setSelectedDataNames(RC, selectedDataSets.toArray(new String[0]));
        setDataNum(selectedDataSets.size());
    }

    //do default analysis for testing purpose
    public void doDefaultMetaAnalysis() {

        checkLogIn();

        User currentUser = sb.getCurrentUser();
        List<DataModel> metaDataSets = getDataSets();

        //reset data 
        metaDataSets.clear();
        String format = "colu";

        //first data
        String inpath = ab.getDataSet1Path();
        String name = DataUtils.getJustFileName(inpath);
        String outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.copyFile(new File(inpath), new File(outpath));
        addNewData("Upload");
        RDataUtils.readIndExpressTable(RC, name, format);
        selectedData = metaDataSets.get(0);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        //2nd data
        inpath = ab.getDataSet2Path();
        name = DataUtils.getJustFileName(inpath);
        outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.copyFile(new File(inpath), new File(outpath));
        addNewData("Upload");
        RDataUtils.readIndExpressTable(RC, name, format);

        selectedData = metaDataSets.get(1);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        //3rd data
        inpath = ab.getDataSet3Path();
        name = DataUtils.getJustFileName(inpath);
        outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.copyFile(new File(inpath), new File(outpath));
        addNewData("Upload");
        RDataUtils.readIndExpressTable(RC, name, format);

        selectedData = metaDataSets.get(2);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        //
        inpath = ab.getDataSet4Path();
        name = DataUtils.getJustFileName(inpath);
        outpath = currentUser.getHomeDir() + File.separator + name;
        DataUtils.copyFile(new File(inpath), new File(outpath));
        addNewData("Upload");
        RDataUtils.readIndExpressTable(RC, name, format);

        selectedData = metaDataSets.get(3);
        selectedData.setName(name);
        //selectedData.setIsTestData(true);
        perfromDefaultMetaProcess(selectedData);

        FacesMessage msg = new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", "Three datasets were uploaded and processed on the server.");
        FacesContext.getCurrentInstance().addMessage(null, msg);
    }

    private void perfromDefaultMetaProcess(DataModel dm) {
        dm.setUploaded(true);
        dm.processMetaData();
        dm.setProcessed(true);
        dm.setNormOpt("log");
        dm.setAutoscaleOpt(true);
        dm.performNormalization();
        dm.setNormed(true);

        //DE
        dm.setSigLevel(0.05);
        dm.setFcLevel(0.0);
        dm.performDEAnalysis();
        dm.setAnalDone(true);
        dm.setAllDone();
    }

    //record data in each mode
    private List<DataModel> dataSets;
    private List<DataModel> mDataSets; //the contains meta

    public List<DataModel> getDataSets() {
        if (dataSets == null) {
            dataSets = new ArrayList();
            dataSets.add(new DataModel(RC, "Upload"));
        }
        return dataSets;
    }

    public List<DataModel> getMetaDataSets() {
        if (mDataSets == null) {
            mDataSets = new ArrayList();
            for (int i = 0; i < dataSets.size(); i++) {
                mDataSets.add(dataSets.get(i));
            }
            //now add the meta-dataSet
            DataModel dm = new DataModel(RC, "meta_dat");
            dm.setDeNum(currentDeNum);
            //dm.setDe
            mDataSets.add(dm);
        }
        return mDataSets;
    }

    public DefaultStreamedContent getMetaDataTable() {
        return sb.getDownloadFile("MetaboAnalyst_merged_data.csv");
    }

    public DefaultStreamedContent getMetaResFile() {
        return sb.getDownloadFile("meta_sig_features_" + analMethod + ".csv");
    }

    public void setDataSets(List<DataModel> dataSets) {
        this.dataSets = dataSets;
    }

    //only meta-analysis
    public void addNewData(String dataName) {
        dataSets.add(new DataModel(RC, dataName));
    }

    public void deleteData(DataModel selectedData) {
        dataSets.remove(selectedData);
        //remove data from R inmex.vec 
        RDataUtils.removeData(RC, selectedData.getFullName());
        String homeDir = sb.getCurrentUser().getHomeDir();
        String fileName = selectedData.getFullName();
        File rmFile = new File(homeDir + File.separator + fileName);
        rmFile.delete();

        FacesContext.getCurrentInstance().addMessage(null,
                new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", "The selected data entry is deleted"));
    }

    public ArrayList<String> getCurrentDataSets() {

        ArrayList<String> currentDataNms = new ArrayList();
        for (int i = 0; i < dataSets.size(); i++) {
            if (dataSets.get(i).isInclude()) {
                currentDataNms.add(dataSets.get(i).getFullName());
            }
        }
        return currentDataNms;
    }

    //see if only single dataset to be analyzed
    private boolean singleMode = false;

    public boolean isSingleMode() {
        return singleMode;
    }

    public void setSingleMode(boolean singleMode) {
        this.singleMode = singleMode;
    }

    public DataModel getData(String dataName) {
        for (DataModel dm : dataSets) {
            if (dm.getFullName().equals(dataName)) {
                return dm;
            }
        }
        return null;
    }

    public String prepareVennView() {

        ArrayList<String> selectedDataNms = new ArrayList();
        for (int i = 0; i < mDataSets.size(); i++) {
            DataModel dm = mDataSets.get(i);
            if (dm.isVennInclude()) {
                selectedDataNms.add(dm.getFullName());
            }
        }
        if (selectedDataNms.size() > 4) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Venn diagram supports at most 4 datasets."));
            RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
            return null;
        }

        RDataUtils.setSelectedDataNames(RC, selectedDataNms.toArray(new String[0]));
        int res = RDataUtils.prepareVennData(RC);
        if (res == 1) {
            return "Venn diagram";
        }
        //}
        RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
        String msg = RDataUtils.getCurrentMsg(RC);
        FacesContext.getCurrentInstance().addMessage(null,
                new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", msg));
        return null;
    }
}
