/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.taxonomy;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.TaxonomyUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Xin (David) Zhao
 */
@ManagedBean(name = "ordPCOABean")
public class OAPCOABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    
    //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    
    private String file2DSampleScores = "pcoa_2D_sample_scores.csv";
    private String file2DSampleScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DSampleScores + "\">" + file2DSampleScores + "</a>";
 
    public String getFile2DSampleScoresPath() {
        return file2DSampleScoresPath;
    }
        
    public void setFile2DSampleScoresPath(String file2DSampleScoresPath) {
        this.file2DSampleScoresPath = file2DSampleScoresPath;
    } 
    
    
    
    private String file2DColScores = "pcoa_2D_variable_scores.csv";
    private String file2DColScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DColScores + "\">" + file2DColScores + "</a>";
 
    public String getFile2DColScoresPath() {
        return file2DColScoresPath;
    }
        
    public void setFile2DColScoresPath(String file2DColScoresPath) {
        this.file2DColScoresPath = file2DColScoresPath;
    } 
    
    
    private String file2DEnvScores = "pcoa_2D_constraining_variable_scores.csv"; //TABLE SHOULD ONLY POP UP IF ENVDATA IS UPLOADED
    private String file2DEnvScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DEnvScores + "\">" + file2DEnvScores + "</a>";
 
    public String getFile2DEnvScoresPath() {
        return file2DEnvScoresPath;
    }
        
    public void setFile2DEnvScoresPath(String file2DEnvScoresPath) {
        this.file2DEnvScoresPath = file2DEnvScoresPath;
    } 
    
    
    private String file3DSampleScores = "pcoa_3D_sample_scores.csv";
    private String file3DSampleScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file3DSampleScores + "\">" + file3DSampleScores + "</a>";
 
    public String getFile3DSampleScoresPath() {
        return file3DSampleScoresPath;
    }
        
    public void setFile3DSampleScoresPath(String file3DSampleScoresPath) {
        this.file3DSampleScoresPath = file3DSampleScoresPath;
    } 
    
    
    private String file3DColScores = "pcoa_3D_variable_scores.csv";
    private String file3DColScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file3DColScores + "\">" + file3DColScores + "</a>";
 
    public String getFile3DColScoresPath() {
        return file3DColScoresPath;
    }
        
    public void setFile3DColScoresPath(String file3DColScoresPath) {
        this.file3DColScoresPath = file3DColScoresPath;
    } 
    
    
    private String fileScree = "pcoa_scree_data.csv";
    private String fileScreePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileScree + "\">" + fileScree + "</a>";
 
    public String getFileScreePath() {
        return fileScreePath;
    }
        
    public void setFileScreePath(String fileScreePath) {
        this.fileScreePath = fileScreePath;
    } 
    
    //TEXT BOX  
//    private String envDataCol = " ";
//        
//    public String getEnvDataCol() {
//        return envDataCol;
//    }
//
//    public void setEnvDataCol(String envDataCol) {
//        this.envDataCol = envDataCol;
//    } 
    
    
    //CHECKBOX
    private boolean doCustomNorm = false; 
    
    public boolean isDoCustomNorm() {
        return doCustomNorm;
    }
    
    public void setDoCustomNorm(boolean doCustomNorm) {
        this.doCustomNorm = doCustomNorm;
    }

    //STATIC DROPDOWN
    private String vegdistOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.vegdistMeasureOpts
    
    public String getVegdistOpts() {
        return vegdistOpts;
    }

    public void setVegdistOpts(String vegdistOpts) {
        this.vegdistOpts = vegdistOpts;
    }

    
    private String ordStressDimOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.ordStressDimensionOpts
    
    public String getOrdStressDimOpts() {
        return ordStressDimOpts;
    }

    public void setOrdStressDimOpts(String ordStressDimOpts) {
        this.ordStressDimOpts = ordStressDimOpts;
    }
//
//
//    private String ordColorOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.ordColorPaletteOpts
//    
//    public String getOrdColorOpts() {
//        return ordColorOpts;
//    }
//
//    public void setOrdColorOpts(String ordColorOpts) {
//        this.ordColorOpts = ordColorOpts;
//    }
//    
//
//    
//    //DYNAMIC DROPDOWN 
//    private SelectItem[] pcoaMetaColumnOpts = null;
//    
//    public SelectItem[] getPcoaMetaColumnOpts(){
//        String[] columns = OAUtils.GetPCOAMetaColumns(sb);
//        int columnsLen = columns.length;
//        pcoaMetaColumnOpts = new SelectItem[columnsLen];
//        List<String> columnNames = Arrays.asList(columns);
//        for (int i = 0; i < columnsLen; i++) {
//            pcoaMetaColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
//        }
//        return pcoaMetaColumnOpts;
//    }
//    
//    private String pcoaMetaColumnName = getPcoaMetaColumnOpts()[0].getLabel();
//    
//    public String getPcoaMetaColumnName() {
//        return pcoaMetaColumnName;
//    }
//
//    public void setPcoaMetaColumnName(String pcoaMetaColumnName) {
//        this.pcoaMetaColumnName = pcoaMetaColumnName;
//    }
//
//    
//    private SelectItem[] pcoaScreeDimOpts = null;
//    
//    public SelectItem[] getPcoaScreeDimOpts(){
//        String[] dims = OAUtils.GetPCOAScreeDims(sb);
//        int dimsLen = dims.length;
//        pcoaScreeDimOpts = new SelectItem[dimsLen];
//        List<String> dimension = Arrays.asList(dims);
//        for (int i = 0; i < dimsLen; i++) {
//            pcoaScreeDimOpts[i] = new SelectItem(dimension.get(i), dimension.get(i));
//        }
//        return pcoaScreeDimOpts;
//    }
//    
//    private String pcoaScreeDimNum = getPcoaScreeDimOpts()[0].getLabel();
//    
//    public String getPcoaScreeDimNum() {
//        return pcoaScreeDimNum;
//    }
//
//    public void setPcoaScreeDimNum(String pcoaScreeDimNum) {
//        this.pcoaScreeDimNum = pcoaScreeDimNum;
//    }

//// ACTION BUTTONS //
//    public void pcoaUpdate_action() {
//        OAUtils.CreatePCOAOrdination(sb, doOriginal, vegdistOpts, doBinary, doAbundance, envDataCol);
//        OAUtils.PlotPCOA2DOrdination(sb, ellipseOpts, var_arrowsOpts, env_arrowsOpts, env_centOpts, sampleNamesOpts, ordColorOpts, pcoaMetaColumnName, sb.getNewImage("ord_pcoa_2D"), "png", 72);
//        OAUtils.PlotPCOA3DOrdination(sb, ordColorOpts, var_arrowsOpts, pcoaMetaColumnName, sb.getNewImage("ord_pcoa_3D"));
//        OAUtils.PlotPCOAstressOrdination(sb, sb.getNewImage("ord_pcoa_stress"), "png", 72);
//        OAUtils.PlotPCOAscreeOrdination(sb, sb.getNewImage("ord_pcoa_scree"), "png", 72);    
//    }
    public void pcoa_2D_Update_action() {
        TaxonomyUtils.MakeWGCNAData(sb, doCustomNorm, 1);
        TaxonomyUtils.PlotWGCNAGeneNetwork(sb, doCustomNorm, 0, usrName, usrName, 0, usrName, 0, 0);
        OAUtils.PlotPCOA2DOrdination(sb, ellipseOpts, var_arrowsOpts, env_arrowsOpts, env_centOpts, sampleNamesOpts, ordColorOpts, pcoaMetaColumnName, sb.getNewImage("ord_pcoa_2D"), "png", 72);
    }
    
    public void pcoa_3D_Update_action() {
        OAUtils.CreatePCOAOrdination(sb, doOriginal, vegdistOpts, doBinary, doAbundance, envDataCol);
        OAUtils.PlotPCOA3DOrdination(sb, ordColorOpts, var_arrowsOpts, pcoaMetaColumnName, sb.getNewImage("ord_pcoa_3D"));
    }
    
    public void pcoa_stress_Update_action() {
        OAUtils.CreatePCOAOrdination(sb, doOriginal, vegdistOpts, doBinary, doAbundance, envDataCol);
        OAUtils.PlotPCOAstressOrdination(sb, sb.getNewImage("ord_pcoa_stress"), "png", 72);
    }
    public void pcoa_scree_Update_action() {
        OAUtils.CreatePCOAOrdination(sb, doOriginal, vegdistOpts, doBinary, doAbundance, envDataCol);
        OAUtils.PlotPCOAscreeOrdination(sb, pcoaScreeDimNum, sb.getNewImage("ord_pcoa_scree"), "png", 72);    
    }
}



