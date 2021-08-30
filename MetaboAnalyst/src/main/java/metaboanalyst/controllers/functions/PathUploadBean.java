/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.functions;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
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
@ManagedBean(name = "pathLoader")
public class PathUploadBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private boolean usePathListExample;

    public boolean isUsePathListExample() {
        return usePathListExample;
    }

    public void setUsePathListExample(boolean usePathListExample) {
        this.usePathListExample = usePathListExample;
    }

    private String oraCmpdIDType = "na";

    public String getOraCmpdIDType() {
        return oraCmpdIDType;
    }

    public void setOraCmpdIDType(String oraCmpdIDType) {
        this.oraCmpdIDType = oraCmpdIDType;
    }

    private String pathOraList;

    public String getPathOraList() {
        return pathOraList;
    }

    public void setPathOraList(String pathOraList) {
        this.pathOraList = pathOraList;
    }

    public void updatePathListArea() {
        if (usePathListExample) {
            pathOraList = ab.getTestORA();
            oraCmpdIDType = "name";
        } else {
            pathOraList = "";
            oraCmpdIDType = "na";
        }
    }

    public String handlePathListUpload() {
        if (!sb.doLogin("conc", "pathora", false, false)) {
            sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
            return null;
        }
        if (pathOraList == null || pathOraList.trim().length() == 0) {
            sb.updateMsg("Error", "Empty input!");
            return null;
        } else {
            if (oraCmpdIDType.equals("na")) {
                sb.updateMsg("Error", "Please specify the ID type for your data input!");
                return null;
            } else {
                RConnection RC = sb.getRConnection();
                String[] qVec = DataUtils.getQueryNames(pathOraList, null);
                RDataUtils.setMapData(RC, qVec);
                SearchUtils.crossReferenceExact(RC, oraCmpdIDType);
                sb.setDataUploaded(true);
                return "Name check";
            }
        }
    }

    private UploadedFile csvFile;

    public UploadedFile getCsvFile() {
        return csvFile;
    }

    public void setCsvFile(UploadedFile csvFile) {
        this.csvFile = csvFile;
    }

    private String clsOpt;

    public String getClsOpt() {
        return clsOpt;
    }

    private String dataFormat;

    public String getDataFormat() {
        return dataFormat;
    }

    public void setDataFormat(String dataFormat) {
        this.dataFormat = dataFormat;
    }

    public void setClsOpt(String clsOpt) {
        this.clsOpt = clsOpt;
    }

    private String qeaCmpdIDType;

    public String getQeaCmpdIDType() {
        return qeaCmpdIDType;
    }

    public void setQeaCmpdIDType(String qeaCmpdIDType) {
        this.qeaCmpdIDType = qeaCmpdIDType;
    }

    private boolean usePathDataExample;

    public boolean isUsePathDataExample() {
        return usePathDataExample;
    }

    public void setUsePathDataExample(boolean usePathDataExample) {
        this.usePathDataExample = usePathDataExample;
    }

        
    private String dataNames = "colOnly";

    public String getDataNames() {
        return dataNames;
    }

    public void setDataNames(String dataNames) {
        this.dataNames = dataNames;
    }
    
    public String pathQeaBn_action() {
        if (usePathDataExample) {
            if (!sb.doLogin("conc", "pathqea", false, false)) {
                sb.updateMsg("Error", "Failed to log in!");
                return null;
            }
            sb.setDataUploaded(true);
            return processPathQeaData(ab.getDisreteDataPath(), "name", "rowu", "disc", "colOnly");
        } else {
            if (qeaCmpdIDType.equals("na")) {
                sb.updateMsg("Error", "Please specify the ID type for your data input!");
                return null;
            }
            if (clsOpt.equals("na")) {
                sb.updateMsg("Error", "Please specify the class label type");
                return null;
            }
            if (!sb.doLogin("conc", "pathqea", clsOpt.equals("cont"), false)) {
                sb.updateMsg("Error", "Failed to log in!");
                return null;
            }
            try {
                if (csvFile.getSize() == 0) {
                    sb.updateMsg("Error", "File is empty!");
                    return null;
                }

                String fileName = DataUtils.getJustFileName(csvFile.getFileName());
                DataUtils.uploadFile(csvFile, sb, null, ab.isOnPublicServer());
                sb.setDataUploaded(true);
                return processPathQeaData(fileName, qeaCmpdIDType, dataFormat, clsOpt, dataNames);
            } catch (Exception e) {
                e.printStackTrace();
                return null;
            }
        }
    }

    private String processPathQeaData(String fileName, String cmpdType, String dataFormat, String lblType, String dataNames) {
        RConnection RC = sb.getRConnection();
        if (RDataUtils.readTextData(RC, fileName, dataFormat, lblType, dataNames)) {
            if (RDataUtils.getGroupNumber(RC) > 2) {
                sb.updateMsg("Error", "Enrichment analysis for multiple-group data is "
                                + "not well-defined. Please subset your data to two groups to proceed!");
                return null;
            }
            SearchUtils.crossReferenceExact(sb.getRConnection(), cmpdType);
            return "Name check";
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.updateMsg("Error", "Failed to read in the CSV file." + err);
            return null;
        }
    }
}
