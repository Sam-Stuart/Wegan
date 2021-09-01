/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package wegan.controllers.Wegan;

import metaboanalyst.controllers.stats.*;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
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
@ManagedBean(name = "Ordinationload")
public class OrdinationloadBean implements Serializable {

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

        if (sb.doLogin(dataType, "ord", false, paired)) {
            try {
                RConnection RC = sb.getRConnection();
                String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
                String fileNameMeta = DataUtils.uploadFile(dataFileMeta, sb, null, ab.isOnPublicServer());
                String fileNameEnv = DataUtils.uploadFile(dataFileEnv, sb, null, ab.isOnPublicServer());
                
                if (fileName == null) {
                    return null;
                }
                if (fileNameMeta != null){
//                    RDataUtils.readTextDataMeta(RC, fileNameMeta, dataFormat, "disc", doRowNames, doColNames);
                    RDataUtils.readTextDataMeta(RC, fileNameMeta, metaFormat, "disc", metaNames);
                }
                if (fileNameEnv != null){
//                    RDataUtils.readTextDataEnv(RC, fileNameEnv, dataFormat, "disc", doRowNames, doColNames);
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

        if (sb.doLogin(zipDataType, "ord", false, paired)) {
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


    
    //WEGAN FUCNTIONS 
    
    //*********------------------------------------------------------
    
    private String NMDSTestDataOpt;
    
    public String getNMDSTestDataOpt() {
        return NMDSTestDataOpt;
    }

    public void setNMDSTestDataOpt(String NMDSTestDataOpt) {
        this.NMDSTestDataOpt = NMDSTestDataOpt;
    }
    

    //*********------------------------------------------------------
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
        
        if (testDataOpt == null) {
            sb.updateMsg("Error", "No data set is selected!");
            return null;
        }

        if (testDataOpt.equals("Dune")) {
            dataType = "main";
            testFile = ab.getTestDune();
            dataFormat = "rowu";
            dataNames = "colOnly";
        } 
        
        if (testDataOpt.equals("Iris")) {
            dataType = "main";
            testFile = ab.getTestIris();
            dataFormat = "rowu";       
            dataNames = "colOnly";
        } 
        
        if (testDataOpt.equals("BCI")) {
            dataType = "main";
            testFile = ab.getTestBCI();
            dataFormat = "rowu";
            dataNames = "colOnly";
        }
        
//        else {
//            sb.updateMsg("Error", "Unknown data selected?");
//            return null;
//        }
        
        if (!sb.doLogin(dataType, "ord", false, false)) {
            //sb.updateMsg("Error", "No login return null?");
            return null;
        }
        
        sb.setDataUploaded(true);
        RConnection RC = sb.getRConnection();
        RDataUtils.readTextData(RC, testFile, dataFormat, "disc", dataNames);
        return "Data check";
    }
    
    
    
//    public boolean runOrdinationR(String inputData,String ext){
//        RConnection RC = sb.getRConnection();
//        try {
//            //String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";
//
//            //String rCommand = "CAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\"  )";
//
//            String rCommand = "OrdinationWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\", \"" + ext + "\"   )";
//            RC.voidEval(rCommand);
//            RCenter.recordRCommand(RC, rCommand);
//
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//        //;
//        return true ;
//            
//    }    

    
    /*
    Handle data for power analysis
     */
//    private boolean useExample = false;
//
//    public boolean isUseExample() {
//        return useExample;
//    }
//
//    public void setUseExample(boolean useExample) {
//        this.useExample = useExample;
//    }
//
//    public String uploadPilotData() {
//        //check if data is uploaded
//        if (useExample) {
//            return handlePowerTestFileUpload();
//        }
//
//        if (dataFile.getSize() == 0) {
//            sb.updateMsg("Error", "File is empty");
//            return null;
//        }
//
//        boolean paired = false;
//        if (dataFormat.endsWith("p")) {
//            paired = true;
//        }
//        if (sb.doLogin(dataType, "power", false, paired)) {
//            RConnection RC = sb.getRConnection();
//            String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
//            if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc", doRowNames, doColNames)) {
//                sb.setDataUploaded(true);
//                return "Data check";
//            } else {
//                sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
//                return null;
//            }
//        }
//        return null;
//    }
//
//    public String handlePowerTestFileUpload() {
//        if (!sb.doLogin("conc", "power", false, false)) {
//            return null;
//        }
//        RConnection RC = sb.getRConnection();
//        RDataUtils.readTextData(RC, ab.getTestPowerPath(), dataFormat, "disc", doRowNames, doColNames);
//        sb.setDataUploaded(true);
//        return "Data check";
//    }
//
//    /*
//    ROC data upload
//     */
//    private String dataOpt = "data1";
//
//    public String getDataOpt() {
//        return dataOpt;
//    }
//
//    public void setDataOpt(String dataOpt) {
//        this.dataOpt = dataOpt;
//    }
//
//    public String uploadRocData() {
//        //check if data is uploaded
//        if (useExample) {
//            return handleRocTestFileUpload();
//        }
//
//        if (dataFile.getSize() == 0) {
//            sb.updateMsg("Error", "File is empty");
//            return null;
//        }
//
//        if (sb.doLogin(dataType, "roc", false, false)) {
//            RConnection RC = sb.getRConnection();
//            String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
//            if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc", doRowNames, doColNames)) {
//                sb.setDataUploaded(true);
//                return "Data check";
//            } else {
//                sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
//                return null;
//            }
//        }
//        return null;
//    }
//
//    public String handleRocTestFileUpload() {
//        if (!sb.doLogin("conc", "roc", false, false)) {
//            return null;
//        }
//        RConnection RC = sb.getRConnection();
//        if (dataOpt.equals("data1")) {
//            RDataUtils.readTextData(RC, ab.getTestRocPath(), dataFormat, "disc", doRowNames, doColNames);
//        } else {
//            RDataUtils.readTextData(RC, ab.getTestRocNewPath(), dataFormat, "disc", doRowNames, doColNames);
//        }
//        sb.setDataUploaded(true);
//        return "Data check";
//    }
}
