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
@ManagedBean(name = "Nmdsload")
public class NMDSloadBean implements Serializable {

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
    
    
    
    
    
    
    
    //WEGAN FUNCS ----------------------------------------------
    
    
    
    
    
    //Handle the uploading of the MetaData for NMDS analysis
    private UploadedFile metaData;

    public UploadedFile getMetaData() {
        return metaData;
    }

    public void setMetaData(UploadedFile metaData) {
        this.metaData = metaData;
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
   
    
    
    
    public String handleNMDSFileUpload() {

        boolean paired = false;

        
        if (sb.doLogin(dataType, "nmds", false, paired)) {
            
            try {
                RConnection RC = sb.getRConnection();
                String fileName = DataUtils.uploadFile(dataFile, sb, null, ab.isOnPublicServer());
                if (fileName == null) {
                    sb.updateMsg("Error", "Data file Uploaded failed, make sure its .txt or .csv format");

                    return null;
                }
                String MetaName = null;
                String Metaext = null;

                if (metaData != null){
                     MetaName = DataUtils.uploadFile(metaData, sb, null, ab.isOnPublicServer());
                     Metaext = MetaName.substring(MetaName.length() - 4);
                    
                    
                }
                
                //Gets if the file is in Csv or Txt format, allow for use of proper R reader later
                //Already know it must be one of those based on uploading it to the server without error
                String fileExt = fileName.substring(fileName.length() - 4);
                
                if(runNMDSR(fileName,fileExt,MetaName,Metaext)){
                    sb.updateMsg("Error", "CA run successfully");
                    return "NMDS";
                    
                }else{
                    sb.updateMsg("Error", "NMDS not run succesffully");

                    return "";
                }
                
            } catch (Exception e) {
                e.printStackTrace();
            }
            return "NMDSMeta";
        }
        sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");

        return null;
    }
   
        
    
    //Loads the test data selected by the user
    //----------------------------------------------------------------- 
    
     public String handleNMDSTestFileUpload() {
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

            testFile = ab.getTestamf();
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
//        if (isZip) {
//            if (!RDataUtils.readZipData(RC, testFile, dataType, "F")) {
//                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//                return null;
//            }
//        } else {
//            
//            //Tested cahnging Disc to cont
//            if (!RDataUtils.readTextData(RC, testFile, format, "cont")) {
//                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//                return null;
//            }
//        }
        
        sb.setDataUploaded(true);
        //RC.Eval;
        try {
            //String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";
            //String rCommand = "NMDSWegan(\"" + dataType + "\")";
            String rCommand = "NMDSWegan(\"" + dataType + "\", \"" + sb.getPath2()+ "\"  )";
            RC.voidEval(rCommand);
            RCenter.recordRCommand(RC, rCommand);
            
        } catch (RserveException rse) {
            System.out.println(rse);
            return "";
        }
        //;
        return"NMDS";
    }
    
    
    
    public boolean runNMDSR(String inputData,String ext,String metaData, String metaExt){
        RConnection RC = sb.getRConnection();
        try {
            //String rCommand = "InitDataObjects(\"" + dataType + "\", \"" + analType + "\", " + (isPaired ? "TRUE" : "FALSE") + ")";

            //String rCommand = "CAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\"  )";

            //String rCommand = "NMDSWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\", \"" + ext + "\", \"" + Metadata + "\"   )";
            String rCommand = "NMDSWegan(\"" + inputData + "\", \"" + ext + "\", \"" + metaData + "\", \"" + metaExt+ "\"   )";
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

}
