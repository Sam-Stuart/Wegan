/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package wegan.controllers.Wegan;


import wegan.controllers.Wegan.*;
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
@ManagedBean(name = "Plottingload")
public class PlottingloadBean implements Serializable {

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
    Data upload for plotting module
     */
    public String handleFileUpload() {

        boolean paired = false;
        if (dataFormat.endsWith("p")) {
            paired = true;
        }

        if (sb.doLogin(dataType, "plotting", false, paired)) {
            try {
                RConnection RC = sb.getRConnection();
                String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
                
                if (fileName == null) {
                    return null;
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

        if (sb.doLogin(zipDataType, "plotting", false, paired)) {
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
     * Handle test examples for plotting module
     */


    private UploadedFile testFile;

    public UploadedFile getTestFile() {
        return testFile;
    }

    public void setTestFile(UploadedFile testFile) {
        this.testFile = testFile;
    }
        

    private String testDataOpt;
        
    public String getTestDataOpt() {
        return testDataOpt;
    }

    public void setTestDataOpt(String testDataOpt) {
        this.testDataOpt = testDataOpt;
    }

//<<<<<<< HEAD
//    public String handleStatTestFileUpload() {
//        String format = "";
//        boolean paired = false;
//        boolean isZip = false;
//        String testFile = null;
//
//        
//        
//        if (testDataOpt == null) {
//            //sb.updateMsg("Error", "No data set is selected!");
//            return null;
//        }
//
//        if (testDataOpt.equals("conccancer")) {
//            dataType = "conc";
//            testFile = ab.getTestConcHsaPath();
//            format = "rowu";
//        }
//        
//        //DUNE DATA SELECTED*********************************************************
//        else if (testDataOpt.equals("Dune")) {
//            dataType = "Dune";
//            sb.updateMsg("Error", "Dune data selected");
//
//            testFile = ab.getTestamf();
//            format = "rowu";
//          
//        } else if (testDataOpt.equals("BCI")) {
//            testFile = ab.getTestBCI();
//            format = "rowu";
//            
//        } else if (testDataOpt.equals("Iris")) {
//            dataType = "Dune";
//            testFile = ab.getTestIris();
//            format = "rowu";            
//        }
//
//        if (!sb.doLogin(dataType, "DCA", false, paired)) {
//            //sb.updateMsg("Error", "No login return null?");
//            return null;
//        }
//
//        RConnection RC = sb.getRConnection();
//        if (isZip) {
//            if (!RDataUtils.readZipData(RC, testFile, dataType, "F")) {
//                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//                return null;
//            }
//        } else {
//            
//            //Tested cahnging Disc to cont
//            if (!RDataUtils.readTextData(RC, testFile, format, "disc")) {
//                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//                return null;
//            }
//        }
//        sb.setDataUploaded(true);
//        if (dataType.equals("conc") || dataType.equals("pktable") || dataType.equals("specbin")) {
//            return "Data check";
//        }
//        return dataType;
//    }
//=======
    
//>>>>>>> 6d2a238d3aaa5638d6a34c5d8864a89f961edc0f
   
    //----------------------------------------------------------------- Test loader 
    public String handlePlottingTestFileUpload() {

        String testFile = null;
        
        if (testDataOpt == null) {
            sb.updateMsg("Error", "No data set is selected!");
            return null;
        }

        else if (testDataOpt.equals("Linear")) {
            dataType = "main";
            testFile = ab.getTestLinear();
            dataFormat = "rowu";
            dataNames = "colOnly";
        } 
        
        else if (testDataOpt.equals("Dune")) {
            dataType = "main";
            testFile = ab.getTestDune();
            dataFormat = "rowu";       
            dataNames = "colOnly";
        } 
        
        else if (testDataOpt.equals("BCI")) {
            dataType = "main";
            testFile = ab.getTestBCI();
//<<<<<<< HEAD
//            format = "rowu";
//        } else if (testDataOpt.equals("Iris")) {
//            dataType = "Dune";
//            testFile = ab.getTestIris();
//            format = "rowu";            
//        
//        } else if (testDataOpt.equals("Linear")) {
//            //System.out.print(" Linear Data selected");
//            dataType = "Linear";
//            //sb.updateMsg("Error", "Dune data selected");
//
//            testFile = ab.getTestLinear();
//            format = "rowu";
//            
//        }else {
//=======
            dataFormat = "rowu";
            dataNames = "colOnly";
        }
        
        else {
//>>>>>>> 6d2a238d3aaa5638d6a34c5d8864a89f961edc0f
            sb.updateMsg("Error", "Unknown data selected?");
            return null;
        }
        
        if (!sb.doLogin(dataType, "plotting", false, false)) {
            //sb.updateMsg("Error", "No login return null?");
            return null;
        }
        
        sb.setDataUploaded(true);
//<<<<<<< HEAD
////        return "Data check";  // Change back to 'Data check'    
////    }
////    
////    public boolean runDCaR(String inputData,String ext){
//=======
//>>>>>>> 6d2a238d3aaa5638d6a34c5d8864a89f961edc0f
        RConnection RC = sb.getRConnection();
        RDataUtils.readTextData(RC, testFile, dataFormat, "disc", dataNames);
        return "Data check";
    }
}   

    
    
