/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package wegan.controllers.Wegan;

import metaboanalyst.controllers.stats.*;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "Statload")
@ViewScoped
public class StatloadBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    /*
     * Handle file upoad (.csv or .txt)
     */
    private String dataType = "main";

    public String getDataType() {
        return dataType;
    }

    public void setDataType(String dataType) {
        this.dataType = dataType;
    }
    
    private String dataFormat = "rowu";

    public String getDataFormat() {
        return dataFormat;
    }
        
    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }
    
    private String metaFormat = "rowu";

    public String getMetaFormat() {
        return metaFormat;
    }

    public void setMetaFormat(String metaFormat) {
        this.metaFormat = metaFormat;
    }
    
    private String envFormat = "rowu";

    public String getEnvFormat() {
        return envFormat;
    }

    public void setEnvFormat(String envFormat) {
        this.envFormat = envFormat;
    }

    private UploadedFile dataFile;

    public UploadedFile getDataFile() {
        return dataFile;
    }

    public void setDataFile(UploadedFile dataFile) {
        this.dataFile = dataFile;
    }

    private UploadedFile dataFileMeta;

    public UploadedFile getDataFileMeta() {
        return dataFileMeta;
    }

    public void setDataFileMeta(UploadedFile dataFileMeta) {
        this.dataFileMeta = dataFileMeta;
    }
    
    private UploadedFile dataFileEnv;

    public UploadedFile getDataFileEnv() {
        return dataFileEnv;
    }

    public void setDataFileEnv(UploadedFile dataFileEnv) {
        this.dataFileEnv = dataFileEnv;
    }
    
    private String dataNames = "colOnly";

    public String getDataNames() {
        return dataNames;
    }

    public void setDataNames(String dataNames) {
        this.dataNames = dataNames;
    }
    
    private String metaNames = "colOnly";

    public String getMetaNames() {
        return metaNames;
    }

    public void setMetaNames(String metaNames) {
        this.metaNames = metaNames;
    }
    
    private String envNames = "colOnly";

    public String getEnvNames() {
        return envNames;
    }

    public void setEnvNames(String envNames) {
        this.envNames = envNames;
    }
    
    
    /*
    Data upload for ordination module
     */
    public String handleFileUpload() {

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }

        if (sb.doLogin(dataType, "stat", false, paired)) {
            try {
                RConnection RC = sb.getRConnection();
                String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
                String fileNameMeta = DataUtils.uploadFile(dataFileMeta, sb, null, ab.isOnPublicServer());
                String fileNameEnv = DataUtils.uploadFile(dataFileEnv, sb, null, ab.isOnPublicServer());

                if (fileName == null) {
                    return null;
                }
                if (fileNameMeta != null){
                    RDataUtils.readTextDataMeta(RC, fileNameMeta, metaFormat, "disc", metaNames);
                }
                if (fileNameEnv != null){
                    RDataUtils.readTextDataEnv(RC, fileNameEnv, envFormat, "disc", envNames);
                }
                if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc", dataNames)) {
                    sb.setDataUploaded(true);
                    sb.updateMsg("Error", "Data Uploaded successfully");
                    return "Data check";
                } else {
                    String err = RDataUtils.getErrMsg(RC);
                    sb.updateMsg("Error", "Failed to read in the data file." + err);
                    return null;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");

        return null;
    }

    
    /*
     * Handle zip file examples (containing csv or txt files)
     */
    private UploadedFile zipFile;

    public UploadedFile getZipFile() {
        return zipFile;
    }

    public void setZipFile(UploadedFile zipFile) {
        this.zipFile = zipFile;
    }

    private String zipDataType = "nmrpeak";

    public String getZipDataType() {
        return zipDataType;
    }

    public void setZipDataType(String zipDataType) {
        this.zipDataType = zipDataType;
    }

    private String zipFormat;

    public String getZipFormat() {
        return zipFormat;
    }

    public void setZipFormat(String zipFormat) {
        this.zipFormat = zipFormat;
    }

    private UploadedFile pairFile;

    public UploadedFile getPairFile() {
        return pairFile;
    }

    public void setPairFile(UploadedFile file) {
        this.pairFile = file;
    }

    public String handleZipFileUpload() {

        //disable ms spectra 
       if (ab.isOnPublicServer() && zipDataType.equals("msspec")) {
       //if (zipDataType.equals("msspec")) {
            sb.updateMsg("Error", "Raw spectral processing is disabled on the public server. "
                    + "Install MetaboAnalyst locally or use XCMSOnline for such task.");
            return null;
        }
        boolean paired = false;
        if (pairFile != null && pairFile.getSize() > 0) {
            paired = true;
        }

        if (sb.doLogin(zipDataType, "stat", false, paired)) {
            try {
                RConnection RC = sb.getRConnection();
                //String homeDir = sb.getCurrentUser().getHomeDir();
                DataUtils.uploadFile(zipFile, sb, null, ab.isOnPublicServer());
                if (paired) {
                    DataUtils.uploadFile(pairFile, sb, "pairs.txt", ab.isOnPublicServer());
                }
                if (RDataUtils.readZipData(RC, zipFile.getFileName(), zipDataType, "T")) {
                    sb.setDataUploaded(true);
                    return zipDataType;
                } else {
                    sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                    return null;
                }
            } catch (Exception e) {
                e.printStackTrace();
                return null;
            }
        }

        sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
        return null;
    }


        /*
     * Handle test examples for ordination module
     */
    private String testDataOpt;
    
    public String getTestDataOpt() {
        return testDataOpt;
    }

    public void setTestDataOpt(String testDataOpt) {
        this.testDataOpt = testDataOpt;
    }
    
    //----------------------------------------------------------------- Test loader 
    public String handleOrdinationTestFileUpload() {

        String testFile = null;
        String metaTestFile = null;
        String envTestFile = null;
        
        if (testDataOpt == null) {
            sb.updateMsg("Error", "No data set is selected!");
            return null;
        }

        else if (testDataOpt.equals("Dune")) {
            dataType = "main";
            testFile = ab.getTestDune();
            dataFormat = "rowu";
            dataNames = "colOnly";
            metaTestFile = null;
            envTestFile = ab.getTestDuneEnv();
            envFormat = "rowu";
            envNames = "colOnly";
        } 
        
        else if (testDataOpt.equals("Iris")) {
            dataType = "main";
            testFile = ab.getTestIrisOrd();
            dataFormat = "rowu";       
            dataNames = "colOnly";
            metaTestFile = ab.getTestIrisMeta();
            metaFormat = "rowu";
            metaNames = "bothNames";
            envTestFile = null;
        } 
        
        else if (testDataOpt.equals("Pitlatrine")) {
            dataType = "main";
            testFile = ab.getTestPitlatrine();
            dataFormat = "colu";
            dataNames = "bothNames";
            metaTestFile = ab.getTestPitlatrineMeta();
            metaFormat = "rowu";
            metaNames = "bothNames";
            envTestFile = ab.getTestPitlatrineEnv();
            envFormat = "rowu";
            envNames = "bothNames";
        }
        
        else {
            sb.updateMsg("Error", "Unknown data selected?");
            return null;
        }
        
        if (!sb.doLogin(dataType, "stat", false, false)) {
            //sb.updateMsg("Error", "No login return null?");
            return null;
        }
        
        sb.setDataUploaded(true);
        RConnection RC = sb.getRConnection();
        RDataUtils.readTextData(RC, testFile, dataFormat, "disc", dataNames);
        if (metaTestFile != null) {
            RDataUtils.readTextDataMeta(RC, metaTestFile, metaFormat, "disc", metaNames);
        }
        if (envTestFile != null) {
            RDataUtils.readTextDataEnv(RC, envTestFile, envFormat, "disc", envNames);
        }
        return "Data check";
    }
    
//    Checkbox for viewing supplemental data loader panel
    public boolean checkboxValue;

    public void setCheckboxValue(boolean checkboxValue) {
        this.checkboxValue = checkboxValue;
    }

    public boolean getCheckboxValue() {
        return this.checkboxValue;
    }

}