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
@ManagedBean(name = "Clusterload")
public class ClusteringloadBean implements Serializable {

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

    private String dataFormat = "colu";

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
                    sb.updateMsg("Error", "Data Uploaded successfully");
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
            sb.updateMsg("Error", "Dune data selected");

            testFile = ab.getTestamf();
            format = "rowu";
            
           
        } else if (testDataOpt.equals("BCI")) {
            testFile = ab.getTestBCI();
            format = "rowu";
        }

        if (!sb.doLogin(dataType, "DCA", false, paired)) {
            //sb.updateMsg("Error", "No login return null?");
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
            if (!RDataUtils.readTextData(RC, testFile, format, "disc")) {
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
    
    /*

        public String handleDCAFileUpload() {

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }

        if (sb.doLogin(dataType, "nmds", false, paired)) {
            
            try {
                RConnection RC = sb.getRConnection();
                String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
                if (fileName == null) {
                    return null;
                }
                
                //Gets if the file is in Csv or Txt format, allow for use of proper R reader later
                //Already know it must be one of those based on uploading it to the server without error
                String fileExt = fileName.substring(fileName.length() - 4);
                
                
                if(runDCaR(fileName,fileExt)){
                    //sb.updateMsg("Error", "CA run successfully");
                    return "DCA";
                    
                }else{
                    sb.updateMsg("Error", "DCA not run succesffully");

                    return "";
                }
                
                
                /*
                //RDataUtils.readTextData(RC, fileName, dataFormat, "disc")
                if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
                    sb.setDataUploaded(true);
                    return "Download";
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
    
    
    */
  
    
    //----------------------------------------------------------------- Test loader 
    public String handleClusterTestFileUpload() {
        String format = "";
        boolean paired = false;
        boolean isZip = false;
        String testFile = null;

        
        if (testDataOpt == null) {
                    

            sb.updateMsg("Error", "No data set is selected!");
            return null;
        }


        
        //DUNE DATA SELECTED*********************************************************
        else if (testDataOpt.equals("Dune")) {
            dataType = "Dune";
            //sb.updateMsg("Error", "Dune data selected");
            sb.updateMsg("Hello", "Leif was here");
            testFile = ab.getTestDune();
            format = "rowu";
            
        } else if (testDataOpt.equals("BCI")) {
            testFile = ab.getTestBCI();
            format = "rowu";
        } else {
            sb.updateMsg("Error", "Unknown data selected?");
            return null;
        }
        if (!sb.doLogin(dataType, "nmds", false, paired)) {
            //sb.updateMsg("Error", "No login return null?");
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
            if (!RDataUtils.readTextData(RC, testFile, format, "disc")) {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        sb.setDataUploaded(true);
        //RC.Eval;
        /*try {
            //String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";
            //String rCommand = "NMDSWegan(\"" + dataType + "\")";
            
            //String rCommand = "DCAWegan(\"" + dataType + "\", \"" + sb.getPath2()+ "\"  )";
            
            
            //RC.voidEval(rCommand);
            //RCenter.recordRCommand(RC, rCommand);
            
        } catch (RserveException rse) {
            System.out.println(rse);
            return "";
        }*/
        //;
        return "Data check";
    }
    
    
    
    public boolean runDCaR(String inputData,String ext){
        RConnection RC = sb.getRConnection();
        try {
            //String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";

            //String rCommand = "CAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\"  )";

            String rCommand = "DCAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\", \"" + ext + "\"   )";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);

        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
        //;
        return true ;
            
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
