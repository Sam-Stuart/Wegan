/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "uploader")
public class UploadBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    /*
     * Handle file upoad (.csv or .txt)
     */
    private String dataType = "conc";

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

    private UploadedFile dataFile;

    public UploadedFile getDataFile() {
        return dataFile;
    }

    public void setDataFile(UploadedFile dataFile) {
        this.dataFile = dataFile;
    }

    /*
    Data upload for statistics module
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
                if (fileName == null) {
                    return null;
                }

                if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                    sb.setDataUploaded(true);
                    return "Data check";
                } else {
                    String err = RDataUtils.getErrMsg(RC);
                    sb.updateMsg("Error", "Failed to read in the CSV file." + err);
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
     * Handle test examples for statistics mode
     */
    private String testDataOpt;
    
    
    
    
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
    public String getTestDataOpt() {
        return testDataOpt;
    }

    public void setTestDataOpt(String testDataOpt) {
        this.testDataOpt = testDataOpt;
    }

    public String handleStatTestFileUpload() {
        String format = "";
        boolean paired = false;
        boolean isZip = false;
        String testFile = null;

        
        
        if (testDataOpt == null) {
            //sb.updateMsg("Error", "No data set is selected!");
            return null;
        }

        if (testDataOpt.equals("conccancer")) {
            dataType = "conc";
            testFile = ab.getTestConcHsaPath();
            format = "rowu";
        }
        
        //DUNE DATA SELECTED*********************************************************
        else if (testDataOpt.equals("Dune")) {
            dataType = "Dune";
            //sb.updateMsg("Error", "Dune data selected");

            testFile = ab.getTestamf();
            format = "rowu";
            
            
        //****************************************************************************   
        } else if (testDataOpt.equals("Iris")) {
            dataType = "iris";
            testFile = ab.getTestIris();
            format = "rowu";
        
        } else if (testDataOpt.equals("conccow")) {
            dataType = "conc";
            testFile = ab.getTestConcCowPath();
            format = "rowu";
        } else if (testDataOpt.equals("nmrspecbin")) {
            dataType = "specbin";
            testFile = ab.getTestNMRbinPath();
            format = "rowu";
        } else if (testDataOpt.equals("concpair")) {
            dataType = "conc";
            paired = true;
            format = "colp";
            testFile = ab.getTestConcPairPath();
        } else if (testDataOpt.equals("mspkint")) {
            dataType = "pktable";
            testFile = ab.getTestMSTable();
            format = "colu";
        } else if (testDataOpt.equals("nmrpeaklist")) {
            dataType = "nmrpeak";
            isZip = true;
            testFile = ab.getTestNMRpeakPath();
        } else if (testDataOpt.equals("lcmsspec")) {
            dataType = "msspec";
            isZip = true;
            testFile = ab.getTestLCMSspecPath();
        } else if (testDataOpt.equals("mspklist")) {
            dataType = "mspeak";
            isZip = true;
            testFile = ab.getTestMSpeakPath3Col();
        } else if (testDataOpt.equals("gcmsspec")) {
            dataType = "msspec";
            isZip = true;
            testFile = ab.getTestGCMSspecPath();
        } else {
            sb.updateMsg("Error", "Unknown data selected?");
            return null;
        }

        if (!sb.doLogin(dataType, "stat", false, paired)) {
            return null;
        }

        RConnection RC = sb.getRConnection();
        if (isZip) {
            if (!RDataUtils.readZipData(RC, testFile, dataType, "F")) {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        } else {
            
            //Tested cahnging Disc to cont
            if (!RDataUtils.readTextData(RC, testFile, format, "cont")) {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        sb.setDataUploaded(true);
        if (dataType.equals("conc") || dataType.equals("pktable") || dataType.equals("specbin")) {
            return "Data check";
        }
        return dataType;
    }

    
    
    
    
    
    
    
    //END IMPORTANT FUNCS***********************************************************************
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    /*
    Handle data for power analysis
     */
    private boolean useExample = false;

    public boolean isUseExample() {
        return useExample;
    }

    public void setUseExample(boolean useExample) {
        this.useExample = useExample;
    }

    public String uploadPilotData() {
        //check if data is uploaded
        if (useExample) {
            return handlePowerTestFileUpload();
        }

        if (dataFile.getSize() == 0) {
            sb.updateMsg("Error", "File is empty");
            return null;
        }

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }
        if (sb.doLogin(dataType, "power", false, paired)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
            if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                sb.setDataUploaded(true);
                return "Data check";
            } else {
                sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        return null;
    }

    public String handlePowerTestFileUpload() {
        if (!sb.doLogin("conc", "power", false, false)) {
            return null;
        }
        RConnection RC = sb.getRConnection();
        RDataUtils.readTextData(RC, ab.getTestPowerPath(), "rowu", "disc");
        sb.setDataUploaded(true);
        return "Data check";
    }

    /*
    ROC data upload
     */
    private String dataOpt = "data1";

    public String getDataOpt() {
        return dataOpt;
    }

    public void setDataOpt(String dataOpt) {
        this.dataOpt = dataOpt;
    }

    public String uploadRocData() {
        //check if data is uploaded
        if (useExample) {
            return handleRocTestFileUpload();
        }

        if (dataFile.getSize() == 0) {
            sb.updateMsg("Error", "File is empty");
            return null;
        }

        if (sb.doLogin(dataType, "roc", false, false)) {
            RConnection RC = sb.getRConnection();
            String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
            if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                sb.setDataUploaded(true);
                return "Data check";
            } else {
                sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        return null;
    }

    public String handleRocTestFileUpload() {
        if (!sb.doLogin("conc", "roc", false, false)) {
            return null;
        }
        RConnection RC = sb.getRConnection();
        if (dataOpt.equals("data1")) {
            RDataUtils.readTextData(RC, ab.getTestRocPath(), "rowu", "disc");
        } else {
            RDataUtils.readTextData(RC, ab.getTestRocNewPath(), "rowu", "disc");
        }
        sb.setDataUploaded(true);
        return "Data check";
    }
}
