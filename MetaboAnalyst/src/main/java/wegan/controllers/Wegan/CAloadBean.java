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
@ManagedBean(name = "Caload")
public class CAloadBean implements Serializable {

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

    private UploadedFile dataFile;

    public UploadedFile getDataFile() {
        return dataFile;
    }

    public void setDataFile(UploadedFile dataFile) {
        this.dataFile = dataFile;
    }

        
    private String dataNames = "colOnly";

    public String getDataNames() {
        return dataNames;
    }

    public void setDataNames(String dataNames) {
        this.dataNames = dataNames;
    }
    
    /*
    Data upload for statistics module
     */
    public String handleFileUpload() {

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }

        if (sb.doLogin(dataType, "ca", false, paired)) {
            try {
                RConnection RC = sb.getRConnection();
                String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
                if (fileName == null) {
                    return null;
                }
                //RDataUtils.readTextData(RC, fileName, dataFormat, "disc")
                if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc", dataNames)) {
                    sb.setDataUploaded(true);
                    return "Data Check";
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
     
    public String getTestDataOpt() {
        return testDataOpt;
    }

    public void setTestDataOpt(String testDataOpt) {
        this.testDataOpt = testDataOpt;
    }
 
    
    
    //Function for grrabbing the Test files for CA
    public String handleCATestFileUpload() {
        boolean paired = false;
        boolean isZip = false;
        String testFile = null;
        String testWeightFile = null;
        
        if (testDataOpt == null) {                   
            sb.updateMsg("Error", "No data set is selected!");
            return null;
        } else if (testDataOpt.equals("Iris")) {
            dataType = "Iris";
            testFile = ab.getTestIris();
            dataFormat = "rowu";    
            dataNames = "colOnly";
        } else if (testDataOpt.equals("WolvesElk")) {
            testFile = ab.getTestWolvesElk();
            dataFormat = "rowu";
            dataNames = "bothNames";
        } else {
            sb.updateMsg("Error", "Unknown data selected?");
            return null;
        }
        if (!sb.doLogin(dataType, "ca", false, paired)) {
            //sb.updateMsg("Error", "No login return null?");
            return null;
        }

        RConnection RC = sb.getRConnection();
        if (isZip) {
            if (!RDataUtils.readZipData(RC, testFile, dataType, "F")) {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        } 
        else {
            if (!RDataUtils.readTextData(RC, testFile, dataFormat, "disc", dataNames)) {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
//            if (!RDataUtils.readTextData(RC, testWeightFile, format, "disc")) {
//                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//                return null;
//            }
        }
        sb.setDataUploaded(true);
        return "Data check";
    }
    
    
//     
//     
//     
//        //Handling CA user file upload---------------
//    public String handleCAFileUpload() {
//
//        boolean paired = false;
//        if (dataFormat.endsWith("p")) {
//            paired = true;
//        }
//
//        if (sb.doLogin(dataType, "ca", false, paired)) {
//            
//            try {
//                RConnection RC = sb.getRConnection();
//                String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
//                if (fileName == null) {
//                    return null;
//                }
//                
//                //Gets if the file is in Csv or Txt format, allow for use of proper R reader later
//                //Already know it must be one of those based on uploading it to the server without error
//                String fileExt = fileName.substring(fileName.length() - 4);
//                
//                
//                if(runCaR(fileName,fileExt)){
//                    //sb.updateMsg("Error", "CA run successfully");
//                    return "CA";
//                    
//                }else{
//                    sb.updateMsg("Error", "CA not run succesffully");
//
//                    return "";
//                }
//                
//                
//                /*
//                //RDataUtils.readTextData(RC, fileName, dataFormat, "disc")
//                if (RDataUtils.readTextData(RC, fileName, dataFormat, "disc")) {
//                    sb.setDataUploaded(true);
//                    return "Download";
//                } else {
//                    String err = RDataUtils.getErrMsg(RC);
//                    sb.updateMsg("Error", "Failed to read in the CSV file." + err);
//                    return null;
//                }*/
//            } catch (Exception e) {
//                e.printStackTrace();
//            }
//        }
//        sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
//
//        return null;
//    }
//        
//        
//        
//        
//        
//        public boolean runCaR(String inputData,String ext){
//            RConnection RC = sb.getRConnection();
//            try {
//                //String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";
//            
//                //String rCommand = "CAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\"  )";
//                
//                String rCommand = "CAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\", \"" + ext + "\"   )";
//                RC.voidEval(rCommand);
//                RCenter.recordRCommand(RC, rCommand);
//            
//            } catch (RserveException rse) {
//                System.out.println(rse);
//                return false;
//            }
//            //;
//            return true ;
//            
//        }
       
}
