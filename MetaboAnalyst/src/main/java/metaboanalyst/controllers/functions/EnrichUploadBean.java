/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.functions;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.event.ValueChangeEvent;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.SearchUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "enrichLoader")
public class EnrichUploadBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    /*
     * ORA analysis
     */
    private String msetOraList;

    public String getMsetOraList() {
        return msetOraList;
    }

    public void setMsetOraList(String msetOraList) {
        this.msetOraList = msetOraList;
    }

    private String cmpdIDType;

    public String getCmpdIDType() {
        return cmpdIDType;
    }

    public void setCmpdIDType(String cmpdIDType) {
        this.cmpdIDType = cmpdIDType;
    }

    private boolean useMsetListExample = false;

    public boolean isUseMsetListExample() {
        return useMsetListExample;
    }

    public void setUseMsetListExample(boolean useMsetListExample) {
        this.useMsetListExample = useMsetListExample;
    }

    public void updateOraArea() {
        if (useMsetListExample) {
            msetOraList = ab.getTestORA();
        } else {
            msetOraList = "";
        }
    }

    public String handleOraListUpload() {
        if (!sb.doLogin("conc", "msetora", false, false)) {
            sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }
        if (msetOraList != null && msetOraList.trim().length() > 0) {
            String[] qVec = DataUtils.getQueryNames(msetOraList, null);
            RDataUtils.setMapData(sb.getRConnection(), qVec);
            SearchUtils.crossReferenceExact(sb.getRConnection(), cmpdIDType);
            sb.setDataUploaded(true);
            return "Name check";
        } else {
            sb.updateMsg("Error", "Error: the input is empty!");
            return null;
        }
    }

    /*
     * SSP analysis
     */
    private String msetSspData = "";

    public String getMsetSspData() {
        return msetSspData;
    }

    public void setMsetSspData(String msetSspData) {
        this.msetSspData = msetSspData;
    }

    private boolean useMsetSspExample = false;

    public boolean isUseMsetSspExample() {
        return useMsetSspExample;
    }

    public void setUseMsetSspExample(boolean useMsetSspExample) {
        this.useMsetSspExample = useMsetSspExample;
    }

    public String biofluidType;

    public String getBiofluidType() {
        return biofluidType;
    }

    public void setBiofluidType(String biofluidType) {
        this.biofluidType = biofluidType;
    }

    public void updateSspArea() {
        if (useMsetSspExample) {
            msetSspData = ab.getTestSSP();
        } else {
            msetSspData = "";
        }
    }

    public void msetSspTestCB_processValueChange(ValueChangeEvent event) {
        if (useMsetSspExample) {
            msetSspData = ab.getTestSSP();
        } else {
            msetSspData = "";
        }
    }

    public String handleSspDataUpload() {
        if (!sb.doLogin("conc", "msetssp", false, false)) {
            sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }
        if (msetSspData != null && msetSspData.trim().length() > 0) {
            if (!RDataUtils.setSample(sb.getRConnection(), msetSspData, biofluidType, null)) {
                return null;
            }
            SearchUtils.crossReferenceExact(sb.getRConnection(), cmpdIDType);
            sb.setDataUploaded(true);
            return "Name check";
        }
        return null;
    }

    /*
     * QEA analysis
     */
    private UploadedFile csvFile;

    public UploadedFile getCsvFile() {
        return csvFile;
    }

    public void setCsvFile(UploadedFile csvFile) {
        this.csvFile = csvFile;
    }

    private String qeaClsOpt = "disc";

    public String getQeaClsOpt() {
        return qeaClsOpt;
    }

    public void setQeaClsOpt(String qeaClsOpt) {
        this.qeaClsOpt = qeaClsOpt;
    }

    private String dataFormat;

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    private String qeaTestDataOpt = "msetDis";

    public String getQeaTestDataOpt() {
        return qeaTestDataOpt;
    }

    public void setQeaTestDataOpt(String qeaTestDataOpt) {
        this.qeaTestDataOpt = qeaTestDataOpt;
    }

    public String handleQeaDataUpload() {
        try {
            if (!sb.doLogin("conc", "msetqea", qeaClsOpt.equals("cont"), false)) {
                sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
                return null;
            }
            String fileName = DataUtils.uploadFile(csvFile, sb, null, ab.isOnPublicServer());
            sb.setDataUploaded(true);
            return processMsetQeaData(fileName, qeaClsOpt, dataFormat, cmpdIDType);
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public String msetQeaTestBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        String lblType, clsType, fileName;
        if (qeaTestDataOpt.equals("msetDis")) {
            lblType = "name";
            clsType = "disc";
            fileName = ab.getDisreteDataPath();
        } else {
            lblType = "pubchem";
            clsType = "cont";
            fileName = ab.getContDataPath();
        }
        if (!sb.doLogin("conc", "msetqea", clsType.equals("cont"), false)) {
            sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }
        sb.setDataUploaded(true);
        RDataUtils.setCachexiaTestSet(sb.getRConnection(), "TRUE");
        return processMsetQeaData(fileName, clsType, "rowu", lblType);        
    }

    private String processMsetQeaData(String fileName, String clsType, String dataFormat, String lblType) {
          RConnection RC = sb.getRConnection();
        if (RDataUtils.readTextData(RC, fileName, dataFormat, clsType)) {
            //double check for multiple class, this will be issue for large sample
            if (RDataUtils.getGroupNumber(RC) > 2) {
                    sb.updateMsg("Error", "Enrichment analysis for multiple-group data is not well-defined. Please subset your data to two groups to proceed!");
                    return null;
            }
            SearchUtils.crossReferenceExact(sb.getRConnection(), lblType);
            return "Name check";
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.updateMsg("Error", "Failed to read in the CSV file." + err);
            return null;
        }
    }
}
